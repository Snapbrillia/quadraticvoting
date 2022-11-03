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
{-# LANGUAGE LambdaCase            #-}
-- }}}


-- MODULE
-- {{{
module Utils where
-- }}}


-- IMPORTS
-- {{{
import qualified Cardano.Api                 as Cardano
import           Cardano.Api.Shelley         ( PlutusScript(PlutusScriptSerialised) )
import qualified Codec.Serialise             as Codec
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.String                 ( fromString )
import qualified Ledger.Ada                  as Ada
import           Plutus.V1.Ledger.Value      ( flattenValue )
import           Plutus.V2.Ledger.Api
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.Prelude
import           PlutusTx.Sqrt               ( Sqrt (..)
                                             , isqrt )
import qualified Prelude                     as P

import           Data.Datum
-- }}}


-- OFF-CHAIN FUNCTIONS 
-- {{{
lovelaceToAda :: Integer -> P.Double
lovelaceToAda lovelace = P.fromIntegral lovelace P./ 1_000_000


scriptToCardanoApiScript :: Script -> Cardano.Script Cardano.PlutusScriptV2
scriptToCardanoApiScript =
  -- {{{
    Cardano.PlutusScript Cardano.PlutusScriptV2
  . PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  . Codec.serialise
  -- }}}


mintingPolicyToSymbol :: MintingPolicy -> CurrencySymbol
mintingPolicyToSymbol =
  -- {{{
    fromString
  . BS8.unpack
  . Cardano.serialiseToRawBytesHex
  . Cardano.hashScript
  . scriptToCardanoApiScript
  . getMintingPolicy
  -- }}}
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


-- TODO: Remove.
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


-- TODO: Remove.
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


{-# INLINABLE utxoIsGettingSpent #-}
utxoIsGettingSpent :: [TxInInfo] -> TxOutRef -> Bool
utxoIsGettingSpent inputs oref =
  -- {{{
  any ((== oref) . txInInfoOutRef) inputs
  -- }}}


{-# INLINABLE orefToTokenName #-}
orefToTokenName :: TxOutRef -> TokenName
orefToTokenName TxOutRef{txOutRefId = TxId txHash, txOutRefIdx = txIndex} =
  -- {{{
  TokenName $ sha2_256 $ consByteString txIndex txHash
  -- }}}


{-# INLINABLE getInlineDatum #-}
-- | Extracts the inline datum from a given UTxO.
--
--   Raises exception upon failure.
getInlineDatum :: TxOut -> QVFDatum
getInlineDatum utxo =
  -- {{{
  case txOutDatum utxo of
    OutputDatum (Datum d) ->
      case fromBuiltinData d of
        Just qvfDatum ->
          qvfDatum
        Nothing       ->
          traceError "Provided datum didn't have a supported structure."
    _                     ->
      traceError "Bad inline datum."
  -- }}}


{-# INLINABLE getInputGovernanceUTxOFrom #-}
-- | Abstraction to find a given "governance" UTxO.
--
--   Raises exception upon failure.
getInputGovernanceUTxOFrom :: CurrencySymbol
                           -> TokenName
                           -> [TxInInfo]
                           -> TxOut
getInputGovernanceUTxOFrom sym tn inputs =
  -- {{{
  case filter (utxoHasOnlyX sym tn . txInInfoResolved) inputs of
    [txIn] ->
      -- {{{
      txInInfoResolved txIn
      -- }}}
    _      ->
      -- {{{
      traceError
        "Governance asset missing."
      -- }}}
  -- }}}


{-# INLINABLE validateGovUTxO #-}
-- | Checks if a given UTxO carries a single "governance" asset. If it doesn't,
--   the returned value will simply be @False@. But if it does carry the asset,
--   unless its address and datum comply with the given ones, raises exception.
validateGovUTxO :: Value
                -> Address
                -> QVFDatum
                -> TxOut
                -> Bool
validateGovUTxO govVal origAddr updatedDatum utxo =
  -- {{{
  if utxoHasValue govVal utxo then
    -- {{{
    if txOutAddress utxo == origAddr then
      if utxosDatumMatchesWith updatedDatum utxo then
        True
      else
        traceError
          "Output governance UTxO must have a properly updated datum."
    else
      traceError
        "Governance token must be sent back to the same address from which it's getting consumed."
    -- }}}
  else
    -- {{{
    False
    -- }}}
  -- }}}


{-# INLINABLE utxosDatumMatchesWith #-}
-- | Checks if a UTxO carries a specific inline datum.
--
--   Raises exception upon failure of getting the inline datum.
utxosDatumMatchesWith :: QVFDatum -> TxOut -> Bool
utxosDatumMatchesWith newDatum =
  -- {{{
  (newDatum ==) . getInlineDatum
  -- }}}


{-# INLINABLE utxoXCount #-}
-- | Finds how much X asset is present in the given UTxO.
utxoXCount :: CurrencySymbol -> TokenName -> TxOut -> Integer
utxoXCount sym tn =
  -- {{{
    ( \case
        Just (_, _, amt') -> amt'
        Nothing           -> 0
    )
  . find (\(sym', tn', _) -> sym' == sym && tn' == tn)
  . flattenValue
  . txOutValue
  -- }}}


{-# INLINABLE utxoHasValue #-}
utxoHasValue :: Value -> TxOut -> Bool
utxoHasValue val =
  -- {{{
  (== val) . txOutValue
  -- }}}


{-# INLINABLE utxoHasOnlyXWithLovelaces #-}
-- | Checks if a given UTxO has *only* 1 of asset X, and a given amount of
--   Lovelaces.
utxoHasOnlyXWithLovelaces :: CurrencySymbol
                          -> TokenName
                          -> Integer
                          -> TxOut
                          -> Bool
utxoHasOnlyXWithLovelaces sym tn lovelaces =
  -- {{{
    ( \case
        [(sym', tn', amt'), (_, _, amt)] ->
          amt == lovelaces && sym' == sym && tn' == tn && amt' == 1
        _                                ->
          False
    )
  . flattenValue
  . txOutValue
  -- }}}


{-# INLINABLE utxoHasOnlyX #-}
-- | Checks if a given UTxO has *only* 1 of asset X, and some Lovelaces.
utxoHasOnlyX :: CurrencySymbol
             -> TokenName
             -> TxOut
             -> Bool
utxoHasOnlyX sym tn =
  -- {{{
    ( \case
        [(sym', tn', amt'), _] ->
          sym' == sym && tn' == tn && amt' == 1
        _                      ->
          False
    )
  . flattenValue
  . txOutValue
  -- }}}


{-# INLINABLE utxoHasX #-}
-- | Checks if a given UTxO has exactly 1 of asset X.
utxoHasX :: CurrencySymbol -> Maybe TokenName -> TxOut -> Bool
utxoHasX sym mTN =
  -- {{{
    isJust
  . find
      ( \(sym', tn', amt') ->
             sym' == sym
          && ( case mTN of
                 Just tn -> tn' == tn
                 Nothing -> True
             )
          && amt' == 1
      )
  . flattenValue
  . txOutValue
  -- }}}


{-# INLINABLE utxoHasLovelaces #-}
-- | Helper function to see if the given UTxO carries the given amount of
--   Lovelaces.
utxoHasLovelaces :: Integer -> TxOut -> Bool
utxoHasLovelaces lovelaces txOut =
  -- {{{
  Ada.fromValue (txOutValue txOut) == Ada.lovelaceOf lovelaces
  -- }}}


{-# INLINABLE foldDonationInputs #-}
-- | Collects all the donation UTxOs (to one project, specified in the token
--   name) into a @Map@ value: mapping their public key hashes to their donated
--   Lovelace count. It also counts the number of donations.
--
--   Allows mixture of `Donation` and `Donations` datums (TODO?).
--
--   There is also no validation for assuring the UTxO is coming from the
--   script address (TODO?).
--
--   Ignores UTxOs that don't have the specified donation asset, but raises
--   exception if it finds the asset with a datum other than `Donation` or
--   `Donations`.
foldDonationInputs :: CurrencySymbol
                   -> TokenName
                   -> [TxInInfo]
                   -> ( Integer                -- ^ Count of folded donations
                      , Integer                -- ^ Total donated Lovelaces
                      , Map PubKeyHash Integer -- ^ Record of donors and their contributions.
                      )
foldDonationInputs donationSymbol donationTN inputs =
  -- {{{
  let
    foldFn TxInInfo{txInInfoResolved = o} (count, total, dMap) =
      -- {{{
      let
        xCount               = utxoXCount donationSymbol donationTN o
        lovelaces            = lovelaceFromValue $ txOutValue o
        helperFn mapForUnion =
          -- {{{
          ( count + xCount
          , total + lovelaces
          , Map.unionWith (+) mapForUnion dMap
          )
          -- }}}
      in
      if xCount > 0 then
        -- {{{
        case getInlineDatum o of
          Donation donor  ->
            -- {{{
            -- TODO: Here we have an implicit assumption that since the
            --       UTxO has a `Donation` datum, it must also carry
            --       exactly 1 donation asset.
            helperFn $ Map.singleton donor lovelaces
            -- }}}
          Donations soFar ->
            -- {{{
            helperFn soFar
            -- }}}
          _               ->
            -- {{{
            traceError "Unexpected UTxO encountered."
            -- }}}
        -- }}}
      else
        -- {{{
        (count, total, dMap)
        -- }}}
      -- }}}
  in
  foldr foldFn (0, 0, Map.empty) inputs
  -- }}}
-- }}}


-- CONSTANTS
-- {{{
governanceLovelaces :: Integer
governanceLovelaces = 1_500_000

registrationFee :: Integer
registrationFee = 3_000_000

halfOfTheRegistrationFee :: Integer
halfOfTheRegistrationFee = 1_500_000

maxDonationInputsForPhaseOne :: Integer
maxDonationInputsForPhaseOne = 120

maxDonationInputsForPhaseTwo :: Integer
maxDonationInputsForPhaseTwo = 250

maxTotalDonationCount :: Integer
maxTotalDonationCount =
  maxDonationInputsForPhaseOne * maxDonationInputsForPhaseTwo

minKeyHolderFee :: Integer
minKeyHolderFee = 1_000_000

minDonationAmount :: Integer
minDonationAmount = 2_000_000
-- }}}
