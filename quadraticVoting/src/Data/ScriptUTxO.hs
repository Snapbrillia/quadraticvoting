{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Data.ScriptUTxO where


-- IMPORTS
-- {{{
import qualified Control.Monad.Fail          as M
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( FromJSON(..)
                                             , (.:) )
import           Data.Aeson.Types            ( Parser )
import           Data.Text                   ( Text )
import qualified Data.Text                   as Text
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


-- SCRIPT UTXO
-- {{{
data ScriptUTxO = ScriptUTxO
  { suUTxO       :: TxOutRef
  , suAddress    :: Ledger.Address
  , suAddressStr :: String
  , suLovelace   :: Integer
  , suAsset      :: Asset
  , suAssetCount :: Integer
  , suDatum      :: QVFDatum
  } deriving (Eq)

instance Ord ScriptUTxO where
  compare u0 u1 = compare (suDatum u0) (suDatum u1)

instance Show ScriptUTxO where
  show ScriptUTxO{..} =
    -- {{{
       "{\"utxo\":"       ++ "\"" ++ showTxOutRef suUTxO ++ "\""
    ++ ",\"address\":"    ++ "\"" ++ suAddressStr        ++ "\""
    ++ ",\"lovelace\":"   ++         show suLovelace
    ++ ",\"asset\":"      ++ "\"" ++ show suAsset        ++ "\""
    ++ ",\"assetCount\":" ++         show suAssetCount
    ++ "}"
    -- }}}

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

instance FromJSON ScriptUTxO where
  -- parseJSON :: Value -> Parser ScriptUTxO
  parseJSON = A.withObject "ScriptUTxO" $ \v -> do
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
                        ScriptUTxO
                    <$> utxoParser utxo
                    <*> addressParser addr
                    <*> v .: "address"
                    <*> v .: "lovelace"
                    <*> assetParser asset
                    <*> v .: "assetCount"
                    <*> qvfDatumParser cbor
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


toTxInInfo :: ScriptUTxO -> TxInInfo
toTxInInfo ScriptUTxO{..} =
  -- {{{
  TxInInfo suUTxO $ TxOut
    suAddress
    (    Ada.lovelaceOf suLovelace
      <> Value.singleton
           (assetSymbol suAsset)
           (assetTokenName suAsset)
           suAssetCount
    )
    (Ledger.OutputDatum $ Ledger.Datum $ Ledger.toBuiltinData suDatum)
    Nothing
  -- }}}






