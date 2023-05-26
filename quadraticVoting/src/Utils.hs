-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Plutus.V1.Ledger.Scripts    ( Redeemer(..) )
import qualified Plutus.V1.Ledger.Value      as Value
import           Plutus.V1.Ledger.Value      ( flattenValue
                                             , valueOf
                                             )
import           Plutus.V2.Ledger.Api
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.Prelude
import           PlutusTx.Sqrt               ( Sqrt(..)
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


{-# INLINABLE addressToPubKeyHash #-}
addressToPubKeyHash :: Address -> Maybe PubKeyHash
addressToPubKeyHash Address{..} =
  -- {{{
  case addressCredential of
    PubKeyCredential pkh ->
      Just pkh
    ScriptCredential _   ->
      Nothing
  -- }}}


{-# INLINABLE addressToBuiltinByteString #-}
addressToBuiltinByteString :: Address -> BuiltinByteString
addressToBuiltinByteString Address{..} =
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
    (Nothing, _      ) -> Nothing
    (Just y , otherYs) -> Just (y, otherYs)
  -- }}}


{-# INLINABLE pluckMap #-}
pluckMap :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
pluckMap _ [] = Nothing
pluckMap p xs =
  -- {{{
  let
    go []       a          = a
    go (y : ys) (_, soFar) =
      case p y of
        Nothing -> go ys (Nothing, y : soFar)
        justY'  -> (justY', ys ++ soFar)
  in
  case go xs (Nothing, []) of
    (Nothing, _      ) -> Nothing
    (Just y , otherYs) -> Just (y, otherYs)
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


{-# INLINABLE keepOnlyLovelacesOf #-}
keepOnlyLovelacesOf :: Value -> Value
keepOnlyLovelacesOf = Ada.toValue . Ada.fromValue


-- TODO: Remove.
{-# INLINABLE updateIfWith #-}
-- | If an element of the given list satisfies the predicate, that single
--   element is updated, and the new list is returned (applies to the leftmost
--   item only). If no items satify the predicate, @Nothing@ is returned.
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


{-# INLINABLE findMap2 #-}
-- | First 2 from left are favored. Preserves the order of predicates.
findMap2 :: (a -> Maybe b)
         -> (a -> Maybe b)
         -> [a]
         -> (Maybe b, Maybe b)
findMap2 pred0 pred1 elems =
  -- {{{
  let
    go [] acc = acc
    go _ acc@(Just _, Just _) = acc
    go (e : es) acc@(Nothing, Nothing) =
      case pred0 e of
        e@Just _ -> go es (e, Nothing)
        Nothing  ->
          case pred1 e of
            e@Just _ -> go es (Nothing, e)
            Nothing  -> go es acc
    go (e : es) acc@(e0@Just _, Nothing) =
      case pred1 e of
        e@Just _ -> (e0, e)
        Nothing  -> go es acc
    go (e : es) acc@(Nothing, e1@Just _) =
      case pred0 e of
        e@Just _ -> (e, e1)
        Nothing  -> go es acc
  in
  go elems (Nothing, Nothing)
  -- }}}


{-# INLINABLE findMap3 #-}
-- | First 3 from left are favored. Preserves the order of predicates.
findMap3 :: (a -> Maybe b)
         -> (a -> Maybe b)
         -> (a -> Maybe b)
         -> [a]
         -> (Maybe b, Maybe b, Maybe b)
findMap3 pred0 pred1 pred2 elems =
  -- {{{
  let
    go [] acc = acc
    go _ acc@(Just _, Just _, Just _) = acc
    go (e : es) acc@(Nothing, Nothing, Nothing) =
      case pred0 e of
        e@Just _ -> go es (e, Nothing, Nothing)
        Nothing  ->
          case pred1 e of
            e@Just _ -> go es (Nothing, e, Nothing)
            Nothing  ->
              case pred2 e of
                e@Just _ -> go es (Nothing, Nothing, e)
                Nothign  -> go es acc
    go (e : es) acc@(e0@Just _, Nothing, Nothing) =
      case pred1 e of
        e@Just _ -> go es (e0, e, Nothing)
        Nothing  ->
          case pred2 e of
            e@Just _ -> go es (e0, Nothing, e)
            Nothing  -> go es acc
    go (e : es) acc@(Nothing, e1@Just _, Nothing) =
      case pred0 e of
        e@Just _ -> go es (e, e1, Nothing)
        Nothing  ->
          case pred2 e of
            e@Just _ -> go es (Nothing, e1, e)
            Nothing  -> go es acc
    go (e : es) acc@(Nothing, Nothing, e2@Just _) =
      case pred0 e of
        e@Just _ -> go es (e, Nothing, e2)
        Nothing  ->
          case pred1 e of
            e@Just _ -> go es (Nothing, e, e2)
            Nothing  -> go es acc
    go (e : es) acc@(Nothing, e1@Just _, e2@Just _) =
      case pred0 e of
        e@Just _ -> (e, e1, e2)
        Nothing  -> go es acc
    go (e : es) acc@(e0@Just _, Nothing, e2@Just _) =
      case pred1 e of
        e@Just _ -> (e0, e, e2)
        Nothing  -> go es acc
    go (e : es) acc@(e0@Just _, e1@Just _, Nothing) =
      case pred2 e of
        e@Just _ -> (e0, e1, e)
        Nothing  -> go es acc
  in
  go elems (Nothing, Nothing, Nothing)
  -- }}}


{-# INLINABLE resolveIfRefEquals #-}
resolveIfRefEquals :: TxOutRef -> TxInInfo -> Maybe TxOut
resolveIfRefEquals ref TxInInfo{..} =
  if txInInfoOutRef == ref then Just txInInfoResolved else Nothing


{-# INLINABLE keepInputsFrom #-}
keepInputsFrom :: Address -> [TxInInfo] -> [TxInInfo]
keepInputsFrom addr = filter ((== addr) . txOutAddress . txInInfoResolved)


{-# INLINABLE keepOutputsFrom #-}
keepOutputsFrom :: Address -> [TxOut] -> [TxOut]
keepOutputsFrom addr = filter ((== addr) . txOutAddress)


{-# INLINABLE utxoIsGettingSpent #-}
utxoIsGettingSpent :: [TxInInfo] -> TxOutRef -> Bool
utxoIsGettingSpent inputs oref =
  -- {{{
  any ((== oref) . txInInfoOutRef) inputs
  -- }}}


{-# INLINABLE integerToBuiltinByteString #-}
integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString i =
  -- {{{
  let
    go x soFar
      | x <= 0    = soFar
      | otherwise = 
        let
          (d, r) = divMod x 256
        in
        go d $ consByteString r soFar
  in
  go i mempty
  -- }}}


{-# INLINABLE indexToTokenName #-}
indexToTokenName :: Integer -> TokenName
indexToTokenName = TokenName . integerToBuiltinByteString


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


{-# INLINABLE getRedeemerOf #-}
getRedeemerOf :: forall r. FromData r
              => ScriptPurpose
              -> Map ScriptPurpose Redeemer
              -> Maybe r
getRedeemerOf purpose txInfoRedeemers = do
  -- {{{
  rawRedeemer <- Map.lookup purpose txInfoRedeemers
  fromBuiltinData @r rawRedeemer
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


{-# INLINABLE getGovernanceUTxOFrom #-}
-- | Abstraction to find a given "governance" UTxO.
--
--   Raises exception upon failure.
getGovernanceUTxOFrom :: TxOutRef
                      -> CurrencySymbol
                      -> TokenName
                      -> [TxInInfo]
                      -> TxOut
getGovernanceUTxOFrom oref sym tn inputs =
  -- {{{
  case find ((== oref) . txInInfoOutRef) inputs of
    Just TxInInfo{txInInfoResolved = utxo} ->
      if utxoHasOnlyX sym tn utxo then utxo else traceError "E113"
    Nothing                                ->
      traceError "E118"
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
validateGovUTxO govVal origAddr updatedDatum utxo@TxOut{..} =
  -- {{{
  if txOutValue == govVal then
    -- {{{
    if txOutAddress == origAddr then
      if utxosDatumMatchesWith updatedDatum utxo then
        True
      else
        traceError "E114"
    else
      traceError "E115"
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
-- | Tries to find a singular asset with a given symbol inside the given UTxO,
--   and returns its token name.
getTokenNameOfUTxO :: CurrencySymbol -> TxOut -> Maybe TokenName
getTokenNameOfUTxO sym TxOut{txOutValue = v} = do
  -- {{{
  case flattenValue v of
    lst@[_, _] ->
      case filter (\(sym', _, _) -> sym' == sym lst of
        [(_, tn', amt')] -> if amt' == 1 then Just tn' else Nothing
        _                -> Nothing
    _          -> Nothing
  -- }}}


{-# INLINABLE utxoXCount #-}
-- | Finds how much X asset is present in the given UTxO.
utxoXCount :: CurrencySymbol -> TokenName -> TxOut -> Integer
utxoXCount sym tn TxOut{txOutValue = v} =
  -- {{{
  valueOf v sym tn
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
utxoHasOnlyXWithLovelaces sym tn lovelaces TxOut{txOutValue = v} =
  -- {{{
  v == makeAuthenticValue lovelaces sym tn 1
  -- }}}


{-# INLINABLE valueHasOnlyX #-}
-- | Checks if a given UTxO has *only* 1 of asset X, and some Lovelaces.
valueHasOnlyX :: CurrencySymbol
              -> TokenName
              -> Value
              -> Bool
valueHasOnlyX sym tn v = valueOf v sym tn == 1 && length (flattenValue v) == 2


{-# INLINABLE utxoHasOnlyX #-}
-- | Checks if a given UTxO has *only* 1 of asset X, and some Lovelaces.
utxoHasOnlyX :: CurrencySymbol
             -> TokenName
             -> TxOut
             -> Bool
utxoHasOnlyX sym tn = valueHasOnlyX sym tn . txOutValue


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
utxoHasLovelaces lovelaces TxOut{txOutValue = v} =
  -- {{{
  Ada.fromValue v == Ada.lovelaceOf lovelaces
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
{-# INLINABLE findMatchPoolPortion #-}
findMatchPoolPortion :: Integer -> Integer -> Integer -> Integer
findMatchPoolPortion matchPool sumW w =
  -- {{{
  (w * matchPool) `divide` sumW
  -- }}}


{-# INLINABLE findFundedToRequestedRatio #-}
findFundedToRequestedRatio :: Integer
                           -> Integer
                           -> EliminationInfo
                           -> Integer
findFundedToRequestedRatio
  matchPool
  denominator
  (EliminationInfo req raised w) =
  -- {{{
  let
    won =
        decimalMultiplier
      * (findMatchPoolPortion matchPool denominator w + raised)
  in
  won `divide` req
  -- }}}


{-# INLINABLE findLeastFundedProject #-}
findLeastFundedProject :: Integer
                       -> Integer
                       -> [(BuiltinByteString, EliminationInfo)]
                       -> (Integer, BuiltinByteString, Integer)
findLeastFundedProject matchPool denominator elimInfos =
  -- {{{
  case elimInfos of
    (p, t) : rest ->
      -- {{{
      let
        foldFn (projID, info) (count, p', minSoFar) =
          -- {{{
          let
            theRatio = findFundedToRequestedRatio matchPool denominator info
          in
          if theRatio < minSoFar then
            (count + 1, projID, theRatio)
          else
            (count + 1, p'    , minSoFar)
          -- }}}
      in
      foldr
        foldFn
        (1, p, findFundedToRequestedRatio matchPool denominator t)
        rest
      -- }}}
    _             ->
      -- {{{
      -- TODO.
      traceError "E1"
      -- }}}
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
--   and reference inputs. If they are found, and they share the same address,
--   the provided function is applied. Project UTxO found in the @inputs@ is
--   passed as the first argument, while the one found from @refs@ will be the
--   second argument.
--
--   Raises exception upon pattern match failure.
{-# INLINABLE findOutputsFromProjectUTxOs #-}
findOutputsFromProjectUTxOs :: CurrencySymbol
                            -> TokenName
                            -> TxOutRef
                            -> TxOutRef
                            -> [TxInInfo]
                            -> [TxInInfo]
                            -> (TxOut -> TxOut -> a)
                            -> a
findOutputsFromProjectUTxOs
  projSym
  projTN
  projRef
  infoRef
  inputs
  refs
  validation =
  -- {{{
  case (mapMaybe (resolveIfRefEquals projRef) inputs, mapMaybe (resolveIfRefEquals infoRef) refs) of
    ([pUTxO], [iUTxO]) ->
      -- {{{
      if utxoHasOnlyX projSym projTN inP && utxoHasOnlyX projSym projTN iUTxO then
        validation pUTxO iUTxO
      -- TODO: This check might also be needed.
      -- if txOutAddress inP == txOutAddress infoUTxO then
      --   validation inP infoUTxO
      else
        traceError "E090"
      -- }}}
    _                   ->
      -- {{{
      traceError "E125"
      -- }}}
  -- }}}
-- }}}


-- CONSTANTS
-- {{{
minDeadlineThreshold :: POSIXTime
minDeadlineThreshold = POSIXTime 86_400_000

deadlineLovelaces :: Integer
deadlineLovelaces = 1_500_000

governanceLovelaces :: Integer
governanceLovelaces = 5_000_000

registrationFee :: Integer
registrationFee = 3_000_000

halfOfTheRegistrationFee :: Integer
halfOfTheRegistrationFee = 1_500_000

keyHolderFeePercentage :: Integer
keyHolderFeePercentage = 5

minDonationAmount :: Integer
minDonationAmount = 2_000_000

minRequestable :: Integer
minRequestable = 5_000_000

decimalMultiplier :: Integer
decimalMultiplier = 1_000_000_000
-- }}}


-- ERROR CODES
-- {{{
-- E0  : Negative value passed to square root.
-- E1  : No projects found (TODO).
-- E2  : Can not request 0 or less Lovelaces.
-- E000: Number of minted governance assets must be exactly 2 plust the specified count of multi-donation UTxOs.
-- E001: Exactly 2 project tokens must be getting minted.
-- E002: Exactly 2 governance tokens must be getting burnt.
-- E003: All prizes must be sent out before concluding a funding round.
-- E004: Outputs of project registration are either invalid or badly ordered.
-- E005: Invalid value for the deadline UTxO.
-- E006: Invalid value for the main UTxO.
-- E007: Deadline must match with the provided parameter.
-- E008: Funding round must start with 0 registered projects.
-- E009: Specified project UTxO was not found in the inputs.
-- E010: Exactly two UTxOs must be produced, each containing one minted token, in correct order (deadline, main).
-- E011: UTxO not consumed.
-- E012: Deadline must be at least one day in the future.
-- E013: Invalid datum for project registration.
-- E014: Invalid datums are attached to the project UTxOs.
-- E015: Values of the provided governance and project UTxOs are not valid.
-- E016: Governance and project UTxOs missing.
-- E017: Values of the provided project UTxOs are not valid.
-- E018: Project UTxOs missing.
-- E019: Specified UTxO must be consumed.
-- E020: Project owner's signature is required.
-- E021: There should be exactly 1 governance, and 2 project UTxOs produced.
-- E022: All provided donation assets must be getting burnt.
-- E023: Invalid produced project output.
-- E024: Transaction must be signed by the project owner.
-- E025: Invalid donation datum encountered.
-- E026: Invalid datums are attached to the project UTxOs.
-- E027: The provided redeemer for spending the governance UTxO is not valid.
-- E028: Unauthorized.
-- E029: Invalid datum for donation count.
-- E030: The first UTxO produced at the script address must be the updated project UTxO.
-- E031: Invalid value for the donation UTxO.
-- E032: Produced donation UTxO must carry donor's public key hash as an inlinde datum.
-- E033: Donation amount is too small.
-- E034: Only one UTxO must be produced at the source script address.
-- E035: Donor's signature is required.
-- E036: The provided redeemer for spending the governance UTxO is not valid.
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
-- E056: Script attachment is not allowed.
-- E057: Only two UTxOs from the script can be spent.
-- E058: Invalid prize weight UTxO is being produced.
-- E059: The provided redeemer for spending the governance UTxO is not valid.
-- E060: Input governance UTxO has an improper datum.
-- E061: Excessive number of prize weight inputs are provided.
-- E062: Invalid pattern between inputs and references.
-- E063: Improper correspondence between input and output prize weights, and projects' info reference inputs.
-- E064: This funding round is over.
-- E065: Invalid deadline datum.
-- E066: This funding round is still in progress.
-- E067: Invalid deadline datum.
-- E068: Asset getting minted/burnt doesn't have a project symbol
-- E069: There should be exactly 2 project assets minted/burnt.
-- E070: Only one project asset must be minted/burnt.
-- E071: Invalid Lovelace count at output.
-- E072: Invalid output datum.
-- E073: Unauthentic output UTxO.
-- E074: There should be exactly 1 UTxO going back to the script.
-- E075: New deadline has already passed.
-- E076: Missing authentication asset.
-- E077: Only one kind of donation asset can be minted/burnt.
-- E078: The project UTxO must also be getting consumed.
-- E079: 
-- E080: 
-- E081: The concluded folding project UTxO must also be getting consumed.
-- E082: The main UTxO must also be getting consumed.
-- E083: Missing authentication asset.
-- E084: Current deadline is too close.
-- E085: Only two UTxOs from the script can be spent.
-- E086: Invalid script outputs.
-- E087: Invalid script outputs.
-- E088: Invalid input datums.
-- E089: Unauthentic governance datum is getting spent.
-- E090: Projects UTxOs are not authentic.
-- E091: Project UTxOs must be from the script address.
-- E092: The redeemer is not pointing to this UTxO.
-- E093: Could not find the specified governance TxOutRef.
-- E094: Project owner must be paid accurately.
-- E095: Specified governance UTxOs could not be found.
-- E096: Invalid governance UTxOs are getting spent.
-- E097: The governance UTxO must have the proper datum attached.
-- E098: Invalid output governance UTxO.
-- E099: There should be a single governance UTxO produced, along with refund of the registration fee to the project owner.
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
-- E118: Couldn't find input with given TxOutRef.
-- E119: Found governance UTxO is not authentic.
-- E120: The provided redeemer for spending the governance UTxO is not valid.
-- E121: Project inputs and info references don't match up.
-- E122: Invalid outputs.
-- E123: Project owner must be properly paid.
-- E124: Exactly 1 UTxO must be produced at the script.
-- E125: Project UTxOs (either state, info, or both) were not found)
-- E126: The appointed UTxO is unauthentic.
-- E127: Outputs going back to the incoming script address don't match the expected outputs.
-- E128: Invalid input datums.
-- E129: Unauthentic governance UTxO.
-- E130: Project owner must be the recepient of the refund.
-- E131: Project owner must receive exactly the registration fee minus the transaction fee.
-- E132: Transaction fee is set too high.
-- E133: The main UTxO must also be getting consumed.
-- E134: Invalid datum attached to the provided project UTxO.
-- E135: The specified input was not found.
-- E136: Found donation UTxO doesn't carry the proper asset.
-- E137: New donor's public key hash must be bigger than the last donor.
-- E138: New donor's public key hash must be bigger than the specified donor.
-- E139: The datum attached to the provided donation's UTxO is not valid.
-- E140: Asset getting minted/burnt doesn't have a donation symbol
-- E141: Token name of the asset getting minted/burnt doesn't match that of the spending UTxO.
-- E142: Either 1 donation asset can be getting minted (donation), or a negative quantity must be burnt (folding).
-- E143: Project UTxO has an invalid datum.
-- E144: Unauthentic project UTxO encountered.
-- E145: 
-- E146: 
-- E147: Project owner's address has to be a payment address (script address was given).
-- E148: 
-- E149: 
-- E150: New donor's public key hash must be smaller than current head's.
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
