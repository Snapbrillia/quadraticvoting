{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module CLI.Tx where


-- IMPORTS
-- {{{
import qualified Control.Monad.Fail          as M
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( FromJSON(..)
                                             , (.:)
                                             , (.:?) )
import           Data.Aeson.Types            ( Parser )
import           Data.Text                   ( Text )
import qualified Data.Text                   as Text
import qualified Ledger.Ada                  as Ada
import qualified Plutus.V1.Ledger.Value      as Value
import           Plutus.V1.Ledger.Value      ( CurrencySymbol(..)
                                             , TokenName(..) )
import           Plutus.V2.Ledger.Api        ( TxOutRef(..)
                                             , TxInInfo(..)
                                             , TxOut(..) )
import qualified Plutus.V2.Ledger.Api        as Ledger
import qualified PlutusTx
import           Prelude

import           CLI.Utils
import           Data.Datum
import           Utils
-- }}}


-- ASSET
-- {{{
data Asset = Asset
  { assetSymbol    :: CurrencySymbol
  , assetTokenName :: TokenName
  } deriving (Eq)

instance Show Asset where
  -- {{{
  show a =
    let
      CurrencySymbol sym = assetSymbol a
      TokenName      tn  = assetTokenName a
    in
    builtinByteStringToString sym ++
      (if tn == "" then "" else "." ++ builtinByteStringToString tn)
  -- }}}

readAsset :: String -> Maybe Asset
readAsset a =
  -- {{{
  case span (/= '.') a of
    (sym, [])     -> do
      bbs <- hexStringToBuiltinByteString sym
      return $ Asset (CurrencySymbol bbs) emptyTokenName
    (sym, _ : tn) -> do
      symBBS <- hexStringToBuiltinByteString sym
      tnBBS  <- hexStringToBuiltinByteString tn
      return $ Asset (CurrencySymbol symBBS) (TokenName tnBBS)
  -- }}}
-- }}}


-- TX OUTPUT
-- {{{
data Output = Output
  { oAddress    :: Ledger.Address
  , oAddressStr :: String
  , oLovelace   :: Integer
  , oForScript  :: Maybe (Asset, Integer, QVFDatum)
  } deriving (Eq)

instance Show Output where
  show Output{..} =
    -- {{{
       "{\"address\":"    ++ "\"" ++      oAddressStr ++ "\""
    ++ ",\"lovelace\":"   ++         show oLovelace
    ++ ( case oForScript of
           Just (oAsset, oAssetCount, oDatum) ->
             -- {{{
                ",\"asset\":"      ++ "\"" ++        show oAsset      ++ "\""
             ++ ",\"assetCount\":" ++                show oAssetCount
             ++ ",\"datumCBOR\":"  ++ "\"" ++ typedToCBOR oDatum      ++ "\""
             -- }}}
           Nothing                            ->
             -- {{{
             ""
             -- }}}
       )
    ++ "}"
    -- }}}
-- }}}


-- TX INPUT
-- {{{
data Input = Input
  { iTxOutRef :: TxOutRef
  , iResolved :: Output
  } deriving (Eq)

instance Show Input where
  show Input{..} =
       init (show iResolved) -- WARNING: Used `init` with caution.
    ++ ",\"utxo\":" ++ showTxOutRef iTxOutRef
    ++ "}"

instance FromJSON Input where
  -- parseJSON :: Value -> Parser Input
  parseJSON = A.withObject "Input" $ \v -> do
    -- {{{
    initUTxO   <- v .:  "utxo"
    initAddr   <- v .:  "address"
    mInitAsset <- v .:? "asset"
    mInitCBOR  <- v .:? "datumCBOR"
    case initUTxO of
      A.String utxo ->
        -- {{{
        case initAddr of
          A.String addr ->
            -- {{{
            let
              fromOut out = Input <$> utxoParser utxo <*> out
            in
            case (mInitAsset, mInitCBOR) of
              (Just (A.String asset), Just (A.String cbor)) ->
                -- {{{
                let
                  forScript =
                    -- {{{
                        (,,)
                    <$> assetParser asset
                    <*> v .: "assetCount"
                    <*> qvfDatumParser cbor
                    -- }}}
                  out       =
                    -- {{{
                        Output
                    <$> addressParser addr
                    <*> v .: "address"
                    <*> v .: "lovelace"
                    <*> (Just <$> forScript)
                    -- }}}
                in
                fromOut out
                -- }}}
              (Nothing              , Nothing             ) ->
                -- {{{
                let
                  out =
                    -- {{{
                        Output
                    <$> addressParser addr
                    <*> v .: "address"
                    <*> v .: "lovelace"
                    <*> return Nothing
                    -- }}}
                in
                fromOut out
                -- }}}
              _                                             ->
                -- {{{
                M.fail "Invalid asset and/or CBOR."
                -- }}}
            -- }}}
          _             ->
            -- {{{
            M.fail "Invalid address."
            -- }}}
        -- }}}
      _             ->
        -- {{{
        M.fail "Invalid UTxO."
        -- }}}
    -- }}}
-- }}}


-- UTILS
-- {{{
showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef{..} =
  -- {{{
     builtinByteStringToString (Ledger.getTxId txOutRefId)
  ++ "#"
  ++ show txOutRefIdx
  -- }}}


utxoParser :: Text -> Parser TxOutRef
utxoParser txt =
  -- {{{
  case readTxOutRef (Text.unpack txt) of
    Just utxo ->
      return utxo
    Nothing   ->
      M.fail "Invalid TxOutRef."
  -- }}}


assetParser :: Text -> Parser Asset
assetParser txt =
  -- {{{
  case readAsset (Text.unpack txt) of
    Just a  ->
      return a
    Nothing ->
      M.fail "Invalid Asset."
  -- }}}


addressParser :: Text -> Parser Ledger.Address
addressParser txt =
  -- {{{
  case tryReadAddress txt of
    Just addr ->
      return addr
    Nothing   ->
      M.fail "Invalid Address."
  -- }}}


qvfDatumParser :: Text -> Parser QVFDatum
qvfDatumParser txt =
  -- {{{
  case cborStringToData (Text.unpack txt) of
    Just d  ->
      case PlutusTx.fromData d of
        Just qvfD ->
          return qvfD
        Nothing   ->
          M.fail "Can't convert Data to QVFDatum"
    Nothing ->
      M.fail "Invalid Data."
  -- }}}


inputToTxInInfo :: Input -> TxInInfo
inputToTxInInfo Input{..} =
  -- {{{
  TxInInfo iTxOutRef $ outputToTxOut iResolved
  -- }}}


outputToTxOut :: Output -> TxOut
outputToTxOut Output{..} =
  -- {{{
  let
    (extraVal, d) =
      case oForScript of
        Just (Asset{..}, assetCount, qvfD) ->
          -- {{{
          ( Value.singleton assetSymbol assetTokenName assetCount
          , qvfDatumToInlineDatum qvfD
          )
          -- }}}
        Nothing                            ->
          -- {{{
          (mempty, Ledger.NoOutputDatum)
          -- }}}
  in
  TxOut oAddress (Ada.lovelaceValueOf oLovelace <> extraVal) d Nothing
  -- }}}


txOutToScriptOutput :: String -> TxOut -> Maybe Output
txOutToScriptOutput addrStr o@TxOut{..} = do
  -- {{{
  inAddr <- tryReadAddress $ Text.pack addrStr
  if inAddr == txOutAddress then
    -- {{{
    case (Value.flattenValue txOutValue, getInlineDatum o) of
      ([(sym', tn', amt'), (_, _, lovelaces)], qvfD) ->
        -- {{{
        Just $ Output
          { oAddress    = txOutAddress
          , oAddressStr = addrStr
          , oLovelace   = lovelaces
          , oForScript  = Just (Asset sym' tn', amt', qvfD)
          }
        -- }}}
      _                                              ->
        -- {{{
        Nothing
        -- }}}
    -- }}}
  else
    -- {{{
    Nothing
    -- }}}
  -- }}}


getAssetFromInput :: Input -> Maybe Asset
getAssetFromInput i =
  -- {{{
  case i of
    Input{iResolved = Output{oForScript = Just (a, _, _)}} -> Just a
    _                                                      -> Nothing
  -- }}}
-- }}}


-- IO
-- {{{
-- | Attempts decoding given strings into `Input` values, and applies the
--   given IO action in case of success.
fromGovAndInputs :: String
                 -> String
                 -> Maybe String
                 -> (Input -> [Input] -> [Input] -> IO ())
                 -> IO ()
fromGovAndInputs govInputStr inputsStr mRefsStr action =
  -- {{{
  let
    mGov    = decodeString @Input   govInputStr
    mInputs = decodeString @[Input] inputsStr
  in
  case mRefsStr of
    Just refsStr ->
      -- {{{
      case (mGov, mInputs, decodeString @[Input] refsStr) of
        (Just govInput, Just inputs, Just refs) ->
          action govInput inputs refs
        _                                       ->
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ govInputStr
            ++ "\n\t" ++ inputsStr
            ++ "\n\t" ++ refsStr
      -- }}}
    Nothing      ->
      -- {{{
      case (mGov, mInputs) of
        (Just govInput, Just inputs) ->
          action govInput inputs []
        _                            ->
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ govInputStr
            ++ "\n\t" ++ inputsStr
      -- }}}
  -- }}}
-- }}}


