{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CLI.Tx where


-- IMPORTS
-- {{{
import qualified Control.Monad.Fail          as M
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( FromJSON(..)
                                             , (.:) )
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


-- SCRIPT OUTPUT
-- {{{
data ScriptOutput = ScriptOutput
  { soAddress    :: Ledger.Address
  , soAddressStr :: String
  , soLovelace   :: Integer
  , soAsset      :: Asset
  , soAssetCount :: Integer
  , soDatum      :: QVFDatum
  } deriving (Eq)

instance Ord ScriptOutput where
  compare u0 u1 = compare (soDatum u0) (soDatum u1)

instance Show ScriptOutput where
  show ScriptOutput{..} =
    -- {{{
       "{\"address\":"    ++ "\"" ++ soAddressStr            ++ "\""
    ++ ",\"lovelace\":"   ++         show soLovelace
    ++ ",\"asset\":"      ++ "\"" ++ show soAsset            ++ "\""
    ++ ",\"assetCount\":" ++         show soAssetCount
    ++ ",\"datumCBOR\":"  ++ "\"" ++ untypedToCBOR soDatum   ++ "\""
    ++ "}"
    -- }}}
-- }}}


-- SCRIPT INPUT
-- {{{
data ScriptInput = ScriptInput
  { siTxOutRef :: TxOutRef
  , siResolved :: ScriptOutput
  } deriving (Eq)

instance Ord ScriptInput where
  compare u0 u1 = compare (siResolved u0) (siResolved u1)

instance FromJSON ScriptInput where
  -- parseJSON :: Value -> Parser ScriptInput
  parseJSON = A.withObject "ScriptInput" $ \v -> do
    -- {{{
    initUTxO   <- v .: "utxo"
    initAddr   <- v .: "address"
    initAsset  <- v .: "asset"
    initCBOR   <- v .: "datumCBOR"
    case initUTxO of
      A.String utxo ->
        -- {{{
        case initAddr of
          A.String addr ->
            -- {{{
            case initAsset of
              A.String asset ->
                -- {{{
                case initCBOR of
                  A.String cbor ->
                    -- {{{
                    let
                      out =
                            ScriptOutput
                        <$> addressParser addr
                        <*> v .: "address"
                        <*> v .: "lovelace"
                        <*> assetParser asset
                        <*> v .: "assetCount"
                        <*> qvfDatumParser cbor
                    in
                    ScriptInput <$> utxoParser utxo <*> out
                    -- }}}
                  _             ->
                    -- {{{
                    M.fail "Invalid datum CBOR."
                    -- }}}
                -- }}}
              _              ->
                -- {{{
                M.fail "Invalid asset."
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


scriptInputToTxInInfo :: ScriptInput -> TxInInfo
scriptInputToTxInInfo ScriptInput{..} =
  -- {{{
  TxInInfo siTxOutRef $ scriptOutputToTxOut siResolved
  -- }}}


scriptOutputToTxOut :: ScriptOutput -> TxOut
scriptOutputToTxOut ScriptOutput{..} =
  -- {{{
  TxOut
    soAddress
    (    Ada.lovelaceValueOf soLovelace
      <> Value.singleton
           (assetSymbol soAsset)
           (assetTokenName soAsset)
           soAssetCount
    )
    (Ledger.OutputDatum $ qvfDatumToDatum soDatum)
    Nothing
  -- }}}


txOutToScriptOutput :: String -> TxOut -> Maybe ScriptOutput
txOutToScriptOutput addrStr o@TxOut{..} = do
  -- {{{
  inAddr <- tryReadAddress $ Text.pack addrStr
  if inAddr == txOutAddress then
    -- {{{
    case (Value.flattenValue txOutValue, getInlineDatum o) of
      ([(sym', tn', amt'), (_, _, lovelaces)], qvfD) ->
        -- {{{
        Just $ ScriptOutput
          { soAddress    = txOutAddress
          , soAddressStr = addrStr
          , soLovelace   = lovelaces
          , soAsset      = Asset sym' tn'
          , soAssetCount = amt'
          , soDatum      = qvfD
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
-- }}}





