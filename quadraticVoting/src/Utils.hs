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
{-# LANGUAGE NamedFieldPuns        #-}
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
import qualified Plutus.V1.Ledger.Value      as Value
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


{-# INLINABLE valuePaidToFromOutputs #-}
-- | Similar to `valuePaidTo` from Plutus, but takes in a list of outputs
--   rather than the whole transaction info.
--
--   Looking at the source code for `valuePaidTo`, this implementation seems to
--   traverse the output list one less time. Github link:
--   https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/V2/Contexts.hs
valuePaidToFromOutputs :: [TxOut] -> PubKeyHash -> Value
valuePaidToFromOutputs outputs pkh =
  -- {{{
  let
    foldFn :: TxOut -> Value -> Value
    foldFn txOut acc =
      case txOut of
        TxOut{txOutAddress = Address (PubKeyCredential pkh') _, txOutValue} ->
          if pkh == pkh' then acc <> txOutValue else acc
        _                                                                   ->
          acc
  in
  foldr foldFn mempty outputs
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


{-# INLINABLE qvfDatumToInlineDatum #-}
qvfDatumToInlineDatum :: QVFDatum -> OutputDatum
qvfDatumToInlineDatum = OutputDatum . Datum . toBuiltinData


{-# INLINABLE makeAuthenticValue #-}
makeAuthenticValue :: Integer
                   -> CurrencySymbol
                   -> TokenName
                   -> Integer
                   -> Value
makeAuthenticValue lovelaceCount sym tn amt =
  -- {{{
  Ada.lovelaceValueOf lovelaceCount <> Value.singleton sym tn amt
  -- }}}


{-# INLINABLE getTokenNameOfUTxO #-}
-- | Tries to find a singular asset with a given symbol inside the given
--   UTxO, and returns its token name.
getTokenNameOfUTxO :: CurrencySymbol -> TxOut -> Maybe TokenName
getTokenNameOfUTxO sym utxo =
  -- {{{
  case flattenValue (txOutValue utxo) of
    [(sym', tn', amt'), _] ->
      -- {{{
      if sym' == sym && amt' == 1 then
        Just tn'
      else
        Nothing
      -- }}}
    _                      ->
      -- {{{
      Nothing
      -- }}}
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


-- | Given the match pool's Lovelace count, sum of all the prize weights, and
--   a project's prize weight, this function finds the portion of the match
--   pool won by the project.
{-# INLINABLE findProjectsWonPortion #-}
findProjectsWonPortion :: Integer -> Integer -> Integer -> Integer
findProjectsWonPortion matchPool sumW w =
  -- {{{
  (w * matchPool) `divide` sumW
  -- }}}


-- | Returns the sum of all the prize weights included in the `EliminationInfo`
--   values.
{-# INLINABLE findQVFDenominator #-}
findQVFDenominator :: [(BuiltinByteString, EliminationInfo)]
                   -> Integer
findQVFDenominator =
  -- {{{
  foldr (\(_, EliminationInfo _ _ w) acc -> acc + w) 0
  -- }}}


-- | Checks that there are both UTxOs of a specific project present in inputs
--   and reference inputs. If they are found, the provided function is applied
--   to find the proper transaction outputs, along with the information for
--   paying the project owner.
--
--   Raises exception upon pattern match failure.
{-# INLINABLE findOutputsFromProjectUTxOs #-}
findOutputsFromProjectUTxOs :: CurrencySymbol
                            -> TokenName
                            -> [TxInInfo]
                            -> [TxInInfo]
                            -> (TxOut -> TxOut -> a)
                            -> a
findOutputsFromProjectUTxOs projSym projTN inputs refs validation =
  -- {{{
  case filter (utxoHasOnlyX projSym projTN . txInInfoResolved) inputs of
    [TxInInfo{txInInfoResolved = inP}] ->
      -- {{{
      case filter (utxoHasOnlyX projSym projTN . txInInfoResolved) refs of
        [TxInInfo{txInInfoResolved = infoUTxO}] ->
          -- {{{
          if txOutAddress inP == txOutAddress infoUTxO then
            validation inP infoUTxO
          else
            traceError "E090"
          -- }}}
        _                                       ->
          -- {{{
          traceError "E126"
          -- }}}
      -- }}}
    _                                  ->
      -- {{{
      traceError "E125"
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
-- E000: Exactly 2 governance tokens must be minted.
-- E001: Bad token name.
-- E002: Exactly 2 governance tokens must be getting burnt.
-- E003: All prizes must be sent out before concluding a funding round.
-- E004: Exactly 1 type of asset must be minted.
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
-- E057: Unauthentic governance UTxO provided.
-- E058: Invalid prize weight UTxO is being produced.
-- E059: Provided governance UTxO has invalid value.
-- E060: Input governance UTxO has an improper datum.
-- E061: Excessive number of prize weight inputs are provided.
-- E062: Invalid pattern between inputs and references.
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
-- E083: Invalid value in the prodced governance UTxO.
-- E084: Invalid datum attached to the produced governance UTxO.
-- E085: Only one UTxO must be produced at the script address.
-- E086: Invalid script outputs.
-- E087: Invalid script outputs.
-- E088: Invalid input datums.
-- E089: Unauthentic governance datum is getting spent.
-- E090: Projects UTxOs must share the same address.
-- E091: Project UTxOs must be from the script address.
-- E092: The redeemer is not pointing to this UTxO.
-- E093: 
-- E094: Project owner must be paid accurately.
-- E095: Both governance UTxOs must be getting spent.
-- E096: Invalid governance UTxOs are getting spent.
-- E097: 
-- E098: 
-- E099: 
-- E100: The governance UTxO must also be getting consumed.
-- E101: The governance UTxO must also be getting consumed.
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
-- E120: Input project and reference project UTxOs don't belong to the same project, or are not coming from the same address as the governance UTxO.
-- E121: Invalid order of the inputs compared to the reference inputs.
-- E122: Invalid outputs.
-- E123: Project owner must be properly paid.
-- E124: Exactly 1 UTxO must be produced at the script.
-- E125: Exactly 1 project UTxO must be getting spent.
-- E126: Could not find the project's information UTxO referenced.
-- E127: Project UTxOs must be from the script address.
-- E128: Invalid input datums.
-- E129: Unauthentic governance UTxO.
-- E130: 
-- E131: 
-- E132: 
-- E133: 
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
-- E150: 
-- E151: 
-- E152: 
-- E153: 
-- E154: 
-- E155: 
-- E156: 
-- E157: 
-- E158: 
-- E159: 
-- }}}
