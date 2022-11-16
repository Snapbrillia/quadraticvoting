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


{-# INLINABLE indexToTokenName #-}
indexToTokenName :: Integer -> TokenName
indexToTokenName i = TokenName $ consByteString i mempty


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
          traceError "E111"
    _                     ->
      traceError "E112"
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
      traceError "E113"
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
          "E114"
    else
      traceError
        "E115"
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
            traceError "E116"
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


{-# INLINABLE findProjectsWonLovelaces #-}
findProjectsWonPortion :: Integer -> Integer -> Integer -> Integer
findProjectsWonPortion matchPool sumW w =
  -- {{{
  (w * matchPool) `divide` sumW
  -- }}}


{-# INLINABLE findQVFDenominator #-}
findQVFDenominator :: [(BuiltinByteString, (Integer, Integer, Integer))]
                   -> Integer
findQVFDenominator =
  -- {{{
  foldr (\acc (_, (_, _, w)) -> acc + w) 0
  -- }}}


-- | Finds the summation of all the prize weights, and also finds how far each
--   project is from their funding goal, and the project (if any) which has
--   achieved the smallest percentage (less than 100%, including the raised
--   donations), is removed from the map and the updated map along with the
--   eliminated project are returned.
--
--   If, however, no such project is found (i.e. all projects have either
--   reached or exceeded their funding goals), the project count, along with
--   the computed "denominator" is returned—as the subsequent updated datum has
--   to carry this value and this approach prevents a re-calculation.
{-# INLINABLE eliminateOneProject #-}
eliminateOneProject :: Integer
                    -> Map BuiltinByteString (Integer, Integer, Integer)
                    -> Either
                         (Integer, Integer)
                         ( Map BuiltinByteString (Integer, Integer, Integer)
                         , BuiltinByteString
                         )
eliminateOneProject matchPool ws =
  -- {{{
  let
    kvs                                        = Map.toList ws
    den                                        = findQVFDenominator kvs
    helper (req, dons, w)                      =
      -- {{{
      let
        won = (1_000_000_000 * matchPool * w) `divide` den + dons
      in
      won `divide` req
      -- }}}
    foldFn (count, p, minSoFar) (projID, info) =
      -- {{{
      let
        ratio = helper info
      in
      if ratio < minSoFar then
        (count + 1, projID, ratio   )
      else
        (count + 1, p     , minSoFar)
      -- }}}
  in
  case kvs of
    (p, t) : rest ->
      -- {{{
      let
        (pCount, toBeEliminated, ratio) = foldr foldFn (1, p, helper t) rest
      in
      if ratio < 1_000_000_000 then
        Right (Map.delete toBeEliminated ws, toBeEliminated)
      else
        Left (pCount, den)
      -- }}}
    _             ->
      -- {{{
      -- TODO.
      traceError "E1"
      -- }}}
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

keyHolderFeePercentage :: Integer
keyHolderFeePercentage = 5

minDonationAmount :: Integer
minDonationAmount = 2_000_000

minRequestable :: Integer
minRequestable = 5_000_000
-- }}}


-- ERROR CODES
-- {{{
-- E0  : Negative value passed to square root.
-- E1  : No projects found.
-- E2  : Can not request 0 or less Lovelaces.
-- E000: Bad deadline token name.
-- E001: Bad main token name.
-- E002: Exactly 1 deadline token must be minted.
-- E003: Exactly 1 main token must be minted.
-- E004: Exactly 2 type of assets must be minted.
-- E005: Invalid value for the deadline UTxO.
-- E006: Invalid value for the main UTxO.
-- E007: Deadline must match with the provided parameter.
-- E008: Funding round must start with 0 registered projects.
-- E009: Either invalid datums produced, or produced in wrong order.
-- E010: The 2 minted tokens must be split among 2 UTxOs.
-- E011: UTxO not consumed.
-- E012: Deadline has passed.
-- E013: Invalid datum for project registration.
-- E014: The produced governance UTxO must have untouched value.
-- E015: Invalid value for project's reference UTxO.
-- E016: Invalid value for project's main UTxO.
-- E017: First project output must carry its static info.
-- E018: Second project output must carry its record of donations.
-- E019: Specified UTxO must be consumed.
-- E020: Project owner's signature is required.
-- E021: There should be exactly 1 governance, and 2 project UTxOs produced.
-- E022: Invalid project info UTxO provided.
-- E023: Escrow must be depleted before refunding the registration fee.
-- E024: Transaction must be signed by the project owner.
-- E025: Invalid datum for the second project input.
-- E026: First project input must be the info UTxO.
-- E027: Exactly 2 project inputs are expected.
-- E028: Unauthorized.
-- E029: Invalid datum for donation count.
-- E030: The first UTxO produced at the script address must be the updated project UTxO.
-- E031: Invalid value for the donation UTxO.
-- E032: Produced donation UTxO must carry donor's public key hash as an inlinde datum.
-- E033: Donation amount is too small.
-- E034: This project has reached the maximum number of donations.
-- E035: Donor's signature is required.
-- E036: There should be exactly 1 project, and 1 donation UTxOs produced.
-- E037: Invalid updated value for the project UTxO.
-- E038: Invalid updated value for the project UTxO.
-- E039: Folded UTxO must be produced at its originating address.
-- E040: All donations must be included in the final folding transaction.
-- E041: Invalid outputs pattern.
-- E042: Donation count is too large for direct burning.
-- E043: All donation tokens must be folded before burning.
-- E044: Project UTxO must carry the proper datum to allow burning of its donation tokens.
-- E045: Unauthorized.
-- E046: Unauthorized.
-- E047: Current UTxO is unauthentic.
-- E048: Missing reference input.
-- E049: Incorrect number of donations included for the first phase of folding.
-- E050: Donations output must carry the folded donations.
-- E051: Project output must be properly updated.
-- E052: Donations output must carry all the donation Lovelaces.
-- E053: Project output must preserve its Lovelaces.
-- E054: Missing proper outputs for the first phase of folding donations.
-- E055: Project asset not found.
-- E056: Unexpected UTxO encountered (expected a `PrizeWeight` and `ProjectInfo`).
-- E057: Either an invalid datum attached to the processed prize weight UTxO, or its value is improper, or the corresponding input/reference UTxO is not coming from the script address.
-- E058: Invalid prize weight UTxO is being produced.
-- E059: Excessive number of prize weight inputs are provided.
-- E060: Updated governance UTxO must carry the same value as its consumed counterpart.
-- E061: Invalid datum attached to the produced governance datum.
-- E062: Expected consumed governance UTxO must come from this script address.
-- E063: Improper correspondence between input and output prize weights, and projects' info reference inputs.
-- E064: This funding round is over.
-- E065: Invalid deadline datum.
-- E066: This funding round is still in progress.
-- E067: Invalid deadline datum.
-- E068: Invalid asset is getting minted/burnt.
-- E069: There should be exactly 2 project assets minted/burnt.
-- E070: Only one project asset must be minted/burnt.
-- E071: Invalid Lovelace count at output.
-- E072: Invalid output datum.
-- E073: Unauthentic output UTxO.
-- E074: There should be exactly 1 UTxO going back to the script.
-- E075: New deadline has already passed.
-- E076: Missing authentication asset.
-- E077: There should be exactly 1 donation asset minted.
-- E078: The project UTxO must also be getting consumed.
-- E079: All donation assets must be burnt.
-- E080: All donation assets must be burnt.
-- E081: The concluded folding project UTxO must also be getting consumed.
-- E082: The main UTxO must also be getting consumed.
-- E083: Unauthentic governance UTxO provided.
-- E084: Invalid Lovelace count at the produced governance UTxO.
-- E085: Governance datum not updated properly.
-- E086: Governance UTxO not produced.
-- E087: Key holder fees must be paid accurately.
-- E088: Bad project info reference provided.
-- E089: Escrow must carry the excess reward.
-- E090: Escrow's inline datum is invalid.
-- E091: Escrow UTxO was not produced.
-- E092: Prize not paid.
-- E093: Bad datum attached to a project UTxO.
-- E094: Couldn't find the corresponding input UTxO of a provided reference project UTxO.
-- E095: The impossible happened.
-- E096: Current UTxO is unauthentic.
-- E097: Impossible 2.0 happened.
-- E098: Governance UTxO is not getting updated properly.
-- E099: Prize Lovelaces are not properly withdrawn.
-- E100: Missing output governance UTxO.
-- E101: The project UTxO must also be getting consumed.
-- E102: Bad reference project datum provided.
-- E103: Unauthentic escrow UTxO.
-- E104: Insufficient funds.
-- E105: Missing project owner's signature.
-- E106: Unauthentic escrow UTxO.
-- E107: The bounty winner must be imbursed.
-- E108: Not eligible for bounty withdrawal.
-- E109: Can not conclude with withstanding beneficiaries.
-- E110: Invalid transaction.
-- E111: Provided datum didn't have a supported structure.
-- E112: Bad inline datum.
-- E113: Governance asset missing.
-- E114: Output governance UTxO must have a properly updated datum.
-- E115: Governance token must be sent back to the same address from which it's getting consumed.
-- E116: Unexpected UTxO encountered.
-- E117: Couldn't find UTxO.
-- E118: Mismatch of input project UTxO addresses.
-- E119: No Other UTxOs from the contract can be spent.
-- E120: Input project and reference project UTxOs don't match up.
-- E121: Invalid order of the inputs compared to the reference inputs.
-- E122: Output governance UTxO must carry the match pool and the authentication token.
-- E123: Output governance UTxO must have a proper `DistributionProgress` datum.
-- E124: Exactly 1 UTxO must be produced at the script.
-- E125: Exactly 1 project UTxO must be getting spent.
-- E126: Could not find the project's information UTxO referenced.
-- E127: Invalid input datums.
-- E128: Exactly two UTxOs must be produced at the script.
-- E129: Produced governance UTxO must carry the key holder fee.
-- E130: Invalid datum attached to the produced governance UTxO.
-- E131: Project owner not paid properly.
-- E132: Produced project UTxO must carry its asset with half of the registration fee.
-- E133: Produced project UTxO must have a locked `Escrow` datum attached.
-- E134: 
-- E135: 
-- E136: 
-- E137: 
-- E138: 
-- E139: 
-- E140: 
-- E141: 
-- E142: 
-- E143: 
-- E144: 
-- E145: 
-- E146: 
-- E147: 
-- E148: 
-- E149: 
-- }}}
