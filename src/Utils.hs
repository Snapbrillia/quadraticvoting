-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- }}}


module Utils where


-- IMPORTS
-- {{{
import           Ledger
import qualified Ledger.Typed.Scripts        as Scripts
import qualified Ledger.Ada                  as Ada
import qualified Plutonomy
import           Plutus.Contract
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified Plutus.V1.Ledger.Interval   as Interval
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.AssocMap           (Map)
import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude            hiding (unless)
import           PlutusTx.Prelude            (BuiltinByteString, (<>))
import           PlutusTx.Sqrt               (Sqrt (..), isqrt)
import qualified Prelude                     as P
-- }}}


-- OFF-CHAIN FUNCTIONS 
-- {{{
lovelaceToAda :: Integer -> P.Double
lovelaceToAda lovelace = P.fromIntegral lovelace P./ 1_000_000
-- }}}


-- ON-CHAIN FUNCTIONS 
-- {{{
{-# INLINABLE emptyTokenName #-}
emptyTokenName :: TokenName
emptyTokenName = TokenName emptyByteString


{-# INLINABLE addressToBuiltinByteString #-}
addressToBuiltinByteString :: Address -> BuiltinByteString
addressToBuiltinByteString Address {..} =
  -- {{{
  case addressCredential of
    PubKeyCredential (PubKeyHash bbs) ->
      bbs
    ScriptCredential (ValidatorHash bbs) ->
      bbs
  -- }}}


{-# INLINABLE pluck #-}
pluck :: (a -> Bool) -> [a] -> Maybe (a, [a])
pluck _ [] = Nothing
pluck p xs =
  -- {{{
  let
    go []       a = a
    go (y : ys) (_, soFar)
      | p y       = (Just y, ys ++ soFar)
      | otherwise = go ys (Nothing, y : soFar)
  in
  case go xs (Nothing, []) of
    (Nothing, _)      -> Nothing
    (Just y, otherYs) -> Just (y, otherYs)
  -- }}}


{-# INLINABLE takeSqrt #-}
-- | Calls `traceError` if a negative input is given.
takeSqrt :: Integer -> Integer
takeSqrt val =
  -- {{{
  case isqrt val of
    Imaginary ->
      traceError "E0"
    Exactly sqrt ->
      sqrt
    Approximately sqrt ->
      sqrt
  -- }}}


{-# INLINABLE lovelaceFromValue #-}
lovelaceFromValue :: Value -> Integer
lovelaceFromValue = Ada.getLovelace . Ada.fromValue


{-# INLINABLE updateIfWith #-}
-- | If an element of the given list satisfies the
--   predicate, that single element is updated,
--   and the new list is returned (applies to the
--   leftmost item only). If no items satify the
--   predicate, @Nothing@ is returned.
updateIfWith :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
updateIfWith predicate fn xs =
  -- {{{
  let
    -- go :: ([a], Maybe a, [a]) -> ([a], Maybe a, [a])
    go acc@([], Nothing, _) = acc
    go acc@(_ , Just _ , _) = acc
    go (p : ps, Nothing, acc)
      | predicate p = (ps, Just $ fn p, acc)
      | otherwise   = go (ps, Nothing, p : acc)
  in
  case go (xs, Nothing, []) of
    (_, Nothing, _) ->
      Nothing
    (leftXs, Just updatedX, rightXs) ->
      Just $ leftXs ++ (updatedX : rightXs)
  -- }}}


{-# INLINABLE getDatumFromUTxO #-}
getDatumFromUTxO :: FromData a
                 => TxOut
                 -> (DatumHash -> Maybe Datum)
                 -> Maybe a
getDatumFromUTxO TxOut {..} converter = do
  -- {{{
  dh      <- txOutDatumHash
  Datum d <- converter dh
  return d
  -- }}}

-- | Helper function to count a specific token in a UTxO.
{-# INLINABLE getTokenCountIn #-}
getTokenCountIn :: AssetClass -> TxOut -> Integer
getTokenCountIn tokenAsset utxo =
  -- {{{
  assetClassValueOf (txOutValue utxo) tokenAsset
  -- }}}


-- | Checks if exactly one asset is being sent to a specific script address,
--   and also that this output UTxO has a retreivable datum attached. Returns
--   the datum.
{-# INLINABLE checkOutputToScriptAndGetDatum #-}
checkOutputToScriptAndGetDatum :: FromData a
                               => TxInfo
                               -> CurrencySymbol
                               -> TokenName
                               -> ValidatorHash
                               -> a
checkOutputToScriptAndGetDatum txInfo currencySymbol tokenName validatorHash =
  -- {{{
  case scriptOutputsAt validatorHash info of
    [(outputDatumHash, outputValue)] ->
      -- {{{
      let
        mOutputDatum =
          -- {{{
          outputDatumHash >>= (`findDatum` info) >>= PlutusTx.fromBuiltinData
          -- }}}
        oneTokenOut =
          -- {{{
          case flattenValue outputValue of
            [(symbol, tn, amt)] ->
              -- {{{
                 symbol == ownSymbol
              && tn     == tokenName
              && amt    == 1
              -- }}}
            _                   ->
              -- {{{
              False
              -- }}}
          -- }}}
      in
      case mOutputDatum of
        Nothing ->
          -- {{{
          traceError "Datum either missing, or invalid."
          -- }}}
        Just d  ->
          -- {{{
          d
          -- }}}
      -- }}}
    _                                ->
      -- {{{
      traceError
        "There should be exactly one output to the project script address."
      -- }}}
  -- }}}
-- }}}


