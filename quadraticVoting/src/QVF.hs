-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- }}}


-- MODULE
-- {{{
module QVF where
-- }}}


-- IMPORTS
-- {{{
import           Ledger.Ada                           ( lovelaceValueOf )
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutonomy
-- import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V1.Ledger.Address             ( scriptHashAddress )
import qualified Plutus.V1.Ledger.Interval            as Interval
import qualified Plutus.V1.Ledger.Value               as Value
import           Plutus.V1.Ledger.Value               ( flattenValue
                                                      , AssetClass(..) )
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import qualified PlutusTx.AssocMap                    as Map
import           PlutusTx.AssocMap                    ( Map )
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Maybe                       ( mapMaybe )
import           PlutusTx.Prelude                     ( Bool(..)
                                                      , Either(..)
                                                      , Integer
                                                      , Maybe(..)
                                                      , BuiltinByteString
                                                      , Eq((==))
                                                      , Semigroup((<>))
                                                      , Ord(max, (>=), (<), min, (>), (<=))
                                                      , AdditiveGroup((-))
                                                      , AdditiveSemigroup((+))
                                                      , MultiplicativeSemigroup((*))
                                                      , ($)
                                                      , (.)
                                                      , (&&)
                                                      , any
                                                      , const
                                                      , find
                                                      , fmap
                                                      , foldr
                                                      , filter
                                                      , isJust
                                                      , length
                                                      , negate
                                                      , divide
                                                      , traceError
                                                      , traceIfFalse )
import           PlutusTx.Sqrt                        ( Sqrt(..)
                                                      , isqrt )
import           Prelude                              ( Show
                                                      , show )
import qualified Prelude                              as P

import           Data.Datum
import           Data.DonationInfo
import           Data.Redeemers
import qualified Minter.Governance                    as Gov
import           Minter.Governance                    ( qvfTokenName )
import qualified Minter.Registration
import           Utils
-- }}}


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder      :: PubKeyHash
  , qvfSymbol         :: CurrencySymbol
  , qvfProjectSymbol  :: CurrencySymbol
  , qvfDonationSymbol :: CurrencySymbol
  }

instance Show QVFParams where
  show QVFParams{..} =
         "QVFParams"
    P.++ "\n  { qvfKeyHolder      = " P.++ show qvfKeyHolder
    P.++ "\n  , qvfSymbol         = " P.++ show qvfSymbol
    P.++ "\n  , qvfProjectSymbol  = " P.++ show qvfProjectSymbol
    P.++ "\n  , qvfDonationSymbol = " P.++ show qvfDonationSymbol
    P.++ "\n  }"

PlutusTx.makeLift ''QVFParams
-- }}}


-- QVF VALIDATOR 
-- {{{
{-# INLINABLE mkQVFValidator #-}
mkQVFValidator :: QVFParams
               -> QVFDatum
               -> QVFRedeemer
               -> ScriptContext
               -> Bool
mkQVFValidator QVFParams{..} datum action ctx =
  -- {{{
  let
    -- {{{
    info = scriptContextTxInfo ctx

    inputs      = txInfoInputs info
    refs        = txInfoReferenceInputs info
    txRedeemers = txInfoRedeemers info

    -- | The UTxO currently being validated.
    currUTxO                             :: TxOut
    currUTxO@TxOut{txOutValue = currVal} =
      -- {{{
      case findOwnInput ctx of
        Nothing ->
          traceError "E117"
        Just i  ->
          txInInfoResolved i
      -- }}}

    -- | Script's address.
    ownAddr :: Address
    ownAddr =
      -- {{{
      txOutAddress currUTxO
      -- }}}

    -- | Checks if a given UTxO is in fact from this contract.
    utxoSitsAtScript :: TxOut -> Bool
    utxoSitsAtScript TxOut{txOutAddress = addr} = addr == ownAddr

    -- | Checks for key holder's signature. Induced laziness.
    signedByKeyHolder :: Bool
    signedByKeyHolder =
      -- {{{
      traceIfFalse "E046" $ txSignedBy info qvfKeyHolder
      -- }}}

    -- | Checks if the UTxO currently being validated carries a single X asset.
    currUTxOHasX :: CurrencySymbol -> TokenName -> Bool
    currUTxOHasX sym tn =
      -- {{{
      utxoHasOnlyX sym tn currUTxO
      -- }}}

    -- | Tries to find a singular asset with a given symbol inside the UTxO
    --   that is currently being validated, and returns its token name.
    --
    --   Raises exception upon failure.
    getCurrTokenName :: CurrencySymbol -> TokenName
    getCurrTokenName sym =
      -- {{{
      case getTokenNameOfUTxO sym currUTxO of
        Just tn -> tn
        Nothing -> traceError "E047"
      -- }}}

    -- | Looks inside the reference inputs and extracts the inline datum
    --   attached to one of which carries the given asset.
    --
    --   Raises exception if the reference input, or the datum is not found.
    getDatumFromRefX :: CurrencySymbol -> TokenName -> QVFDatum
    getDatumFromRefX sym tn =
      -- {{{
      case find (utxoHasOnlyX sym tn . txInInfoResolved) refs of
        Just txIn ->
          -- {{{
          getInlineDatum (txInInfoResolved txIn)
          -- }}}
        Nothing   ->
          -- {{{
          traceError "E048"
          -- }}}
      -- }}}

    -- | Looks for the presence of a UTxO from the script in the input list
    --   with 1 X asset, and a datum that complies with the given predicate.
    xInputWithSpecificDatumAndRedeemerExists :: CurrencySymbol
                                             -> TokenName
                                             -> TxOutRef
                                             -> (QVFDatum -> Bool)
                                             -> (QVFRedeemer -> Bool)
                                             -> Bool
    xInputWithSpecificDatumAndRedeemerExists sym tn ref dPred rPred =
      -- {{{
      case find ((== ref) . txInInfoOutRef) inputs of
        Just TxInInfo{txInInfoResolved = o} ->
          -- {{{
          case getRedeemerOf @QVFRedeemer (Spending ref) txRedeemers of
            Just r  ->
                 utxoHasOnlyX sym tn o
              && utxoSitsAtScript o
              && dPred (getInlineDatum o)
              && rPred r
            Nothing -> False
          -- }}}
        _                                   -> False
      -- }}}

    -- | Collection of validations for consuming a set number of donation
    --   UTxOs, along with the project's UTxO. Outputs are expected to be
    --   project's UTxO with an updated datum (given), ang also a single
    --   `Donations` UTxO.
    --
    --   Raises exception on @False@.
    foldDonationsPhaseOne :: Integer -> QVFDatum -> Bool
    foldDonationsPhaseOne requiredDonationCount psUpdatedDatum =
      -- {{{
      let
        tn                    = getCurrTokenName qvfProjectSymbol
        (ds, total, finalMap) = foldDonationInputs qvfDonationSymbol tn inputs
        outputs               = getContinuingOutputs ctx
        mOD                   = find (utxoHasX qvfDonationSymbol $ Just tn) outputs
        mOP                   = find (utxoHasX qvfProjectSymbol $ Just tn) outputs
      in
      case (mOD, mOP) of
        (Just od, Just op) ->
          -- {{{
             traceIfFalse
               "E049"
               (ds == requiredDonationCount)
          && traceIfFalse
               "E050"
               (utxosDatumMatchesWith (Donations finalMap) od)
          && traceIfFalse
               "E051"
               (utxosDatumMatchesWith psUpdatedDatum op)
          && traceIfFalse
               "E052"
               (utxoHasLovelaces total od)
          && traceIfFalse
               "E053"
               (utxoHasLovelaces halfOfTheRegistrationFee op)
          && canFoldOrDistribute
          && signedByKeyHolder
          -- }}}
        _                  ->
          -- {{{
          traceError "E054"
          -- }}}
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   is still in progress.
    --
    --   Raises exception on @False@.
    canRegisterOrDonate :: Bool
    canRegisterOrDonate =
      -- {{{
      case getDatumFromRefX qvfSymbol qvfTokenName of
        DeadlineDatum dl ->
          traceIfFalse "E064" $ deadlineNotReached dl
        _                ->
          traceError "E065"
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   has ended.
    --
    --   Raises exception on @False@.
    canFoldOrDistribute :: Bool
    canFoldOrDistribute =
      -- {{{
      case getDatumFromRefX qvfSymbol qvfTokenName of
        DeadlineDatum dl ->
          traceIfFalse "E066" $ deadlineReached dl
        _                ->
          traceError "E067"
      -- }}}

    -- | Checks whether project registrations and donations are still allowed.
    deadlineNotReached :: POSIXTime -> Bool
    deadlineNotReached dl =
      -- {{{
      Interval.to dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Checks whether the distribution can be triggered.
    deadlineReached :: POSIXTime -> Bool
    deadlineReached dl =
      -- {{{
      Interval.from dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Raises exception on @False@.
    projectMintIsPresent :: Bool -> Bool
    projectMintIsPresent shouldMint =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(sym', _, amt')] ->
             traceIfFalse
               "E068"
               (sym' == qvfProjectSymbol)
          && traceIfFalse
               "E069"
               (if shouldMint then amt' == 2 else amt' == negate 2)
        _                 ->
          traceError "E070"
      -- }}}

    -- | Raises exception on @False@.
    donationMintIsPresent :: CurrencySymbol -> Bool -> Bool
    donationMintIsPresent sym shouldMint =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(sym', tn', amt')] ->
             traceIfFalse "E140" (sym' == qvfDonationSymbol)
          && traceIfFalse "E141" (getCurrTokenName sym)
          && traceIfFalse
               "E142"
               (if shouldMint then amt' == 1 else amt' < 0)
        _                   ->
          traceError "E077"
      -- }}}

    -- | Expects a single continuing output, and validates given predicates.
    validateSingleOutput :: Integer
                         -> QVFDatum
                         -> CurrencySymbol
                         -> TokenName
                         -> Bool
    validateSingleOutput outputLovelaces updatedDatum sym tn =
      -- {{{
      case getContinuingOutputs ctx of
        [o] ->
          -- {{{
             traceIfFalse "E071" (utxoHasLovelaces outputLovelaces o)
          && traceIfFalse "E072" (utxosDatumMatchesWith updatedDatum o)
          && traceIfFalse "E073" (utxoHasOnlyX sym tn o)
          && traceIfFalse "E056" (txOutReferenceScript o == Nothing)
          -- }}}
        _   ->
          -- {{{
          traceError "E074"
          -- }}}
      -- }}}
    -- }}}
  in
  case (datum, action) of
    -- {{{ GOVERNANCE INTERACTIONS 
    (DeadlineDatum oldDl                          , UpdateDeadline newDl         ) ->
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E084"
           (deadlineNotReached $ oldDl - minDeadlineThreshold)
      && traceIfFalse
           "E075"
           (deadlineNotReached newDl)
      && traceIfFalse
           "E076"
           (currUTxOHasX qvfSymbol qvfTokenName)
      && validateSingleOutput
           deadlineLovelaces
           (DeadlineDatum newDl)
           qvfSymbol
           qvfTokenName
      -- }}}

    (DeadlineDatum _                              , ConcludeFundingRound govRef  ) ->
      -- Conclusion of a Funding Round
      -- (Delegation to the governance UTxO.)
      -- {{{
        traceIfFalse "E002"
      $ xInputWithSpecificDatumAndRedeemerExists qvfSymbol qvfTokenName govRef
      $ \case
          DistributionProgress _ _ _ -> True
          _                          -> False
      $ \case
          ConcludeFundingRound _ -> True
          _                      -> False
      -- }}}

    (RegisteredProjectsCount _                    , Contribute contribution      ) ->
      -- Match Pool Contribution
      -- {{{
         traceIfFalse
           "E083"
           (currUTxOHasX qvfSymbol qvfTokenName)
      && validateSingleOutput
           (lovelaceFromValue currVal + contribution)
           datum
           qvfSymbol
           qvfTokenName
      -- }}}

    (RegisteredProjectsCount _                    , RegisterProject              ) ->
      -- Project Registration
      -- {{{
      projectMintIsPresent True && canRegisterOrDonate
      -- }}}

    (RegisteredProjectsCount _                    , AccumulatePrizeWeights govRef) ->
      -- Formation of the Prize Weight Map
      -- (Same logic as `PrizeWeightAccumulation` with this redeemer)
      -- {{{ 
      let
        validOutputs =
          accumulatePrizeWeights qvfSymbol qvfProjectSymbol govRef inputs refs
      in
      traceIfFalse "E086" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (PrizeWeightAccumulation _ _                  , AccumulatePrizeWeights govRef) ->
      -- Accumulation of Computed Prize Weights
      -- (Same logic as `RegisteredProjectsCount` with this redeemer)
      -- {{{ 
      let
        validOutputs =
          accumulatePrizeWeights qvfSymbol qvfProjectSymbol govRef inputs refs
      in
      traceIfFalse "E087" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (ProjectEliminationProgress mp wMap           , EliminateProject _ pRef iRef ) ->
      -- Elimination of Non-Eligible Projects
      -- {{{
      let
        (validOutputs, mEliminated, _) =
          eliminateProject
            qvfSymbol
            qvfProjectSymbol
            currUTxO
            inputs
            refs
            mp
            wMap
            pRef
            iRef
      in
         traceIfFalse "E122" (validOutputs == getContinuingOutputs ctx)
      && ( case mEliminated of
             Just (eliminated, raised) ->
               traceIfFalse
                 "E123"
                 (valuePaidTo info eliminated == lovelaceValueOf raised)
             Nothing                   -> True
         )
      -- }}}

    (DistributionProgress mp remaining den        , DistributePrize pID _ pR iR  ) ->
      -- Handing Out Prize of a Specific Project
      -- {{{
      let
        projTN = TokenName pID
        (validOutputs, winner, won, _) =
          distributePrize
            qvfSymbol
            qvfProjectSymbol
            projTN
            currUTxO
            inputs
            refs
            mp
            remaining
            den
            pR
            iR
      in
         traceIfFalse "E122" (validOutputs == getContinuingOutputs ctx)
      && traceIfFalse
           "E094"
           -- Since outputs back to the script are already validated to make
           -- sure they carry correct values, we can securely validate as long
           -- as the value sent to the winner is equal or greater than the
           -- prize (to accomodate change outputs).
           --
           -- Since a collateral UTxO is already needed, there is no need to
           -- cover the situation where the transaction fee is also covered by
           -- the prize itself.
           (valuePaidTo info winner >= lovelaceValueOf won)
      -- }}}

    (RegisteredProjectsCount _                    , ConcludeProject _            ) ->
      -- Removal of a Donation-less Project
      -- {{{
      projectMintIsPresent False && canFoldOrDistribute
      -- }}}

    (PrizeWeightAccumulation _ _                  , ConcludeProject _            ) ->
      -- Removal of a Donation-less Project
      -- (During/after prize weight accumulation, therefore no need to check
      -- the deadline)
      -- {{{
      projectMintIsPresent False
      -- }}}

    (DistributionProgress _ remaining _           , ConcludeFundingRound         ) ->
      -- Conclusion of a Funding Round
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E079"
           (txInfoMint info == Value.singleton qvfSymbol qvfTokenName (negate 2))
      && traceIfFalse "E080" (remaining == 0)
      -- }}}
    -- }}}

    -- {{{ PROJECT INTERACTIONS 
    (ProjectDonations _                           , DonateToProject              ) ->
      -- Project Donation
      -- {{{
      donationMintIsPresent qvfProjectSymbol True && canRegisterOrDonate
      -- }}}

    (ProjectDonations Nothing                     , ConcludeProject govRef       ) ->
      -- Removal of a Donation-less Project
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E133"
      $ xInputWithSpecificDatumAndRedeemerExists qvfSymbol qvfTokenName govRef
      $ \case
          RegisteredProjectsCount _   -> True
          PrizeWeightAccumulation _ _ -> True
          _                           -> False
      $ \case
          ConcludeProject _ -> True
          _                 -> False
      -- }}} 

    (ProjectDonations _                           , FoldDonations _              ) ->
      -- Folding Donations
      -- {{{
      donationMintIsPresent qvfProjectSymbol False && canFoldOrDistribute
      -- }}}

    (DonationFoldingProgress _ _                  , FoldDonations _              ) ->
      -- Folding Donations
      -- (No need to check the deadline.)
      -- {{{
      donationMintIsPresent qvfProjectSymbol False
      -- }}}

    (PrizeWeight _ False                          , AccumulatePrizeWeights govRef) ->
      -- Accumulation of Prize Weights
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E082"
      $ xInputWithSpecificDatumAndRedeemerExists qvfSymbol qvfTokenName govRef
      $ \case
          RegisteredProjectsCount _   -> True
          PrizeWeightAccumulation _ _ -> True -- tot > length (Map.toList ws)
          _                           -> False
      $ \case
          AccumulatePrizeWeights govRef' -> govRef == govRef'
          _                              -> False
      -- }}} 

    (PrizeWeight _ True                           , EliminateProject govRef _ _  ) ->
      -- Elimination of Non-Eligible Projects
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E100"
      $ xInputWithSpecificDatumAndRedeemerExists qvfSymbol qvfTokenName govRef
      $ \case
          ProjectEliminationProgress _ _ -> True
          _                              -> False
      $ \case
          EliminateProject _ _ _ -> True
          _                      -> False
      -- }}} 

    (PrizeWeight _ True                           , DistributePrize pID gR _ _   ) ->
      -- Distribution of a Project's Prize
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
         traceIfFalse
           "E092"
           (getCurrTokenName qvfProjectSymbol == TokenName pID)
      && traceIfFalse "E101"
           ( xInputWithSpecificDatumAndRedeemerExists
               qvfSymbol
               qvfTokenName
               gR
               ( \case
                   DistributionProgress {} -> True
                   _                       -> False
               )
               ( \case
                   DistributePrize pID' gR' _ _ -> pID == pID' && gR == gR'
                   _                            -> False
               )
           )
      -- }}} 

    --      v-----------v is there a better term?
    (Escrow beneficiaries                         , UnlockEscrowFor ben amt      ) ->
      -- Giving Withdrawal Rights to a Bounty Winner
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        available     = currLovelaces - halfOfTheRegistrationFee
        tn            = getCurrTokenName qvfProjectSymbol
        projectOwner  =
          -- {{{
          case getDatumFromRefX qvfProjectSymbol tn of
            ProjectInfo ProjectDetails{..} -> pdPubKeyHash
            _                              -> traceError "E102"
          -- }}}
      in
         traceIfFalse "E103" (currUTxOHasX qvfProjectSymbol tn)
      && traceIfFalse "E104" (amt <= available)
      && traceIfFalse "E105" (txSignedBy info projectOwner)
      && validateSingleOutput
           currLovelaces
           (   Just
             $ Escrow
             $ Map.unionWith (+) (Map.singleton ben amt) beneficiaries
           )
           qvfSymbol
           qvfTokenName
      -- }}}

    (Escrow beneficiaries                         , WithdrawBounty winner        ) ->
      -- Bounty Collection from Escrow Account
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        -- | Implicit authenticity check for the input escrow UTxO.
        projTN = getCurrTokenName qvfProjectSymbol
      in
      signedByKeyHolder && -- For development. TODO: REMOVE.
      case (Map.lookup winner beneficiaries of
        Just bounty ->
          -- {{{
             traceIfFalse
               "E107"
               (lovelaceFromValue (valuePaidTo info winner) == bounty)
          && validateSingleOutput
               -- No need to make sure this value is greater than or equal to
               -- `halfOfTheRegistrationFee` because of the validation at
               -- `UnlockEscrowFor`.
               (currLovelaces - bounty)
               (Escrow $ Map.delete winner beneficiaries)
               qvfProjectSymbol
               projTN
          -- }}}
        Nothing     ->
          -- {{{
          traceError "E108"
          -- }}}
      -- }}}

    (Escrow beneficiaries                         , ConcludeProject _            ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
         traceIfFalse "E109" (Map.null beneficiaries)
      && projectMintIsPresent False
      -- }}}

    (ProjectInfo _                                , ConcludeProject _            ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
      projectMintIsPresent False
      -- }}}
    -- }}}

    -- {{{ DONATION INTERACTIONS 
    (Donation _                                   , FoldDonations projRef        ) ->
      -- Folding Donations
      -- To avoid excessive transaction fees, this endpoint delegates its logic
      -- to its corresponding project UTxO by checking that it is in fact being
      -- spent.
      -- {{{ 
      let
        -- | Finds the token name of the current donation UTxO being spent.
        --   Uses this value to check the presence of relevant project UTxO.
        --
        --   Raises exception upon failure.
        tn = getCurrTokenName qvfDonationSymbol
      in
        traceIfFalse "E078"
      $ xInputWithSpecificDatumAndRedeemerExists qvfProjectSymbol tn projRef
      $ \case
          ReceivedDonationsCount _    -> True
          DonationFoldingProgress _ _ -> True
          _                           -> False
      $ \case
          FoldDonations _ -> True
          _               -> False
      -- }}} 
    -- }}}

    (_                                            , QVFDev                       ) ->
      -- For development. TODO: REMOVE.
      signedByKeyHolder
    (_                                            , _                            ) ->
      traceError "E110"
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = QVFDatum
  type RedeemerType QVF = QVFRedeemer


typedQVFValidator :: QVFParams -> PSU.V2.TypedValidator QVF
typedQVFValidator =
  let
    wrap = PSU.V2.mkUntypedValidator @QVFDatum @QVFRedeemer
  in
  PSU.V2.mkTypedValidatorParam @QVF
    $$(PlutusTx.compile [|| mkQVFValidator ||])
    $$(PlutusTx.compile [|| wrap ||])


qvfValidator :: QVFParams -> Validator
qvfValidator =
    Plutonomy.optimizeUPLC
  . PSU.V2.validatorScript
  . typedQVFValidator


qvfValidatorHash :: QVFParams -> ValidatorHash
qvfValidatorHash = PSU.V2.validatorHash . typedQVFValidator


qvfAddress :: QVFParams -> Address
qvfAddress = PSU.V2.validatorAddress . typedQVFValidator
-- }}}
-- }}}


-- UTILS
-- {{{
{-# INLINABLE separateKeyHoldersFeeFrom #-}
-- | Returns the calculated fee, and the total value minus the fee.
separateKeyHoldersFeeFrom :: Integer -> (Integer, Integer)
separateKeyHoldersFeeFrom total =
  -- {{{
  let
    khFee = max 0 $ keyHolderFeePercentage * total `divide` 100
  in
  (khFee, total - khFee)
  -- }}}


{-# INLINABLE findEscrowLovelaces #-}
findEscrowLovelaces :: Integer -> Integer -> Integer
findEscrowLovelaces portion requested =
  -- {{{
  max halfOfTheRegistrationFee $
    halfOfTheRegistrationFee + portion - requested
  -- }}}


{-# INLINABLE accumulatePrizeWeights #-}
accumulatePrizeWeights :: CurrencySymbol
                       -> CurrencySymbol
                       -> TxOutRef
                       -> [TxInInfo]
                       -> [TxInInfo]
                       -> [TxOut]
accumulatePrizeWeights qvfSym pSym govRef initInputs initRefs =
  -- {{{
  let
    projMapFn :: TxInInfo -> Maybe (BuiltinByteString, Integer, TxOut)
    projMapFn TxInInfo{txInInfoResolved = i} =
      -- {{{
      case (getTokenNameOfUTxO pSym i, getInlineDatum i) of
        (Just (TokenName projID), PrizeWeight w False) -> Just (projID, w, i)
        _                                              -> Nothing
      -- }}}

    infoMapFn :: TxInInfo -> Maybe (BuiltinByteString, Integer)
    infoMapFn TxInInfo{txInInfoResolved = i} =
      -- {{{
      case (getTokenNameOfUTxO pSym i, getInlineDatum i) of
        (Just (TokenName projID), ProjectInfo ProjectDetails{..}) ->
          Just (projID, pdRequested)
        _                                                         ->
          Nothing
      -- }}}

    -- | Using `pluck` to both avoid (potentially) traversing the whole list,
    --   and also reduce the second traversal by 1 element.
    (mGov, inputs') =
      -- {{{
      case pluck ((== govRef) . txInInfoOutRef) initInputs of
        Just (TxInInfo{txInInfoResolved = gO}, ins) ->
          -- {{{
          if utxoHasOnlyX qvfSym qvfTokenName gO then
            case getInlineDatum gO of
              RegisteredProjectsCount tot    -> (Just (tot, Map.empty, i), ins)
              PrizeWeightAccumulation tot ws -> (Just (tot, ws       , i), ins)
              _                              -> traceError "E060"
          else
            traceError "E119"
          -- }}}
        Nothing                                     -> traceError "E093"
      -- }}}

    go [] [] (c, wMap, outs) =
      -- {{{
      case mGov of
        Just (tot, ws, govUTxO@TxOut{txOutValue = govVal}) ->
          -- {{{
          let
            remaining = tot - length (Map.toList ws)
            finalWs   = Map.unionWith const ws wMap
            updatedD  =
              -- {{{
              | c < remaining  = PrizeWeightAccumulation tot finalWs
              | c == remaining =
                ProjectEliminationProgress
                  (lovelaceFromValue govVal - governanceLovelaces)
                  finalWs
              | otherwise      = traceError "E061"
              -- }}}
          in
          govUTxO {txOutDatum = qvfDatumToInlineDatum updatedD} : outs
          -- }}}
        _                                                  ->
          -- {{{
          traceError "E059"
          -- }}}
      -- }}}
    go ((projID, w, pIn) : ins) ((infoID, requested) : rs) (c, wMap, outs) =
      -- {{{
      if projID == infoID then
        -- {{{
        let
          raised = lovelaceFromValue (txOutValue pIn) - halfOfTheRegistrationFee
        in
        go
          ins
          rs
          ( c + 1
          , Map.insert projID (EliminationInfo requested raised w) wMap
          , pIn {txOutDatum = qvfDatumToInlineDatum $ PrizeWeight w True} : outs
          )
        -- }}}
      else
        -- {{{
        traceError "E121"
        -- }}}
      -- }}}
    go _ _ _ = traceError "E062"

    inputs =
      sortBy
        (\(t, _, _) (t', _, _) -> compare t t')
        (mapMaybe projMapFn inputs')

    refs = sort $ mapMaybe infoMapFn initRefs
  in
  go inputs refs (0, Map.empty, [])
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
{-# INLINABLE eliminateProject #-}
eliminateProject :: CurrencySymbol
                 -> CurrencySymbol
                 -> TxOut
                 -> [TxInInfo]
                 -> [TxInInfo]
                 -> Integer
                 -> Map BuiltinByteString EliminationInfo
                 -> TxOutRef
                 -> TxOutRef
                 -> ([TxOut], Maybe (PubKeyHash, Integer), Integer)
eliminateProject
  qvfSym
  pSym
  currUTxO@TxOut{txOutAddress = scriptAddr, txOutValue = currVal}
  inputs
  refs
  matchPool
  ws
  projRef
  infoRef =
  -- {{{
  let
    kvs = Map.toList ws
    den = findQVFDenominator kvs
  in
  if utxoHasOnlyX qvfSym qvfTokenName currUTxO then
    -- {{{
    let
      (pCount, toBeEliminated, r) = findLeastFundedProject matchPool den kvs
    in
    if r < decimalMultiplier then
      -- {{{
      let
        newMap   = Map.delete toBeEliminated ws
        projTN   = TokenName toBeEliminated
        filtered = keepInputsFrom scriptAddr inputs
      in
      case filtered of
        [_, _] ->
          findOutputsFromProjectUTxOs pSym projTN filtered refs projRef infoRef $
            \inP@TxOut{txOutValue = pVal, txOutAddress = pAddr} infoUTxO ->
              -- {{{
              case (getInlineDatum inP, getInlineDatum infoUTxO) of
                (PrizeWeight _ True, ProjectInfo ProjectDetails{..}) ->
                  -- {{{
                  let
                    raised   =
                      lovelaceFromValue pVal - halfOfTheRegistrationFee
                    (khF, b) = separateKeyHoldersFeeFrom raised
                    outputS  =
                      -- {{{
                      currUTxO
                        { txOutDatum =
                            qvfDatumToInlineDatum $
                              ProjectEliminationProgress matchPool newMap
                        , txOutValue = currVal <> lovelaceValueOf khF
                        }
                      -- }}}
                    outputP  =
                      -- {{{
                      inP
                        { txOutDatum = qvfDatumToInlineDatum $ Escrow Map.empty
                        , txOutValue =
                            makeAuthenticValue
                              halfOfTheRegistrationFee
                              pSym
                              projTN
                              1
                        }
                      -- }}}
                  in
                  ([outputS, outputP], Just (pdPubKeyHash, b), r)
                  -- }}}
                _                                                    ->
                  -- {{{
                  traceError "E128"
                  -- }}}
              -- }}}
        _      ->
          -- {{{
          traceError "E057"
          -- }}}
      -- }}}
    else
      -- {{{
      -- If all projects are eligible, then there should only be a single UTxO
      -- going back to the script address, which is the governance UTxO with an
      -- untouched value, and a properly updated datum.
      ( [ currUTxO
            { txOutDatum =
                qvfDatumToInlineDatum $
                  DistributionProgress matchPool pCount den
            }
        ]
      , Nothing
      , r
      )
      -- }}}
    -- }}}
  else
    -- {{{
    traceError "E129"
    -- }}}
  -- }}}


{-# INLINABLE distributePrize #-}
distributePrize :: CurrencySymbol
                -> CurrencySymbol
                -> TokenName
                -> TxOut
                -> [TxInInfo]
                -> [TxInInfo]
                -> Integer
                -> Integer
                -> Integer
                -> TxOutRef -- Intermediary values for development purposes.
                -> TxOutRef --                    v-------v
                -> ([TxOut], PubKeyHash, Integer, [Integer])
distributePrize
  qvfSym
  pSym
  pTN
  currUTxO@TxOut{txOutAddress = scriptAddr, txOutValue = currVal}
  inputs
  refs
  matchPool
  remaining
  den
  projRef
  infoRef =
  -- {{{
  let
    filtered = keepInputsFrom scriptAddr inputs
  in
  case filtered of
    [_, _] ->
      findOutputsFromProjectUTxOs pSym pTN filtered refs projRef infoRef $
        \inP@TxOut{txOutValue = pVal, txOutAddress = pAddr} infoUTxO ->
          case (getInlineDatum inP, getInlineDatum infoUTxO) of
            (PrizeWeight w True, ProjectInfo ProjectDetails{..}) ->
              -- {{{
              let
                -- mpPortion     = (matchPool * w) `divide` den
                mpPortion     = findMatchPoolPortion matchPool den w
                raised        = lovelaceFromValue pVal - halfOfTheRegistrationFee
                (donFee, don) = separateKeyHoldersFeeFrom raised
                (mpFee, won)  = separateKeyHoldersFeeFrom mpPortion
                belonging     = don + won
                excess        = max 0 $ belonging - pdRequested
                shouldBePaid  = belonging - excess
                outputS       =
                  -- {{{
                  currUTxO
                    { txOutDatum =
                        qvfDatumToInlineDatum $
                          DistributionProgress matchPool (remaining - 1) den
                    , txOutValue =
                        currVal <> lovelaceValueOf (donFee - won)
                    }
                  -- }}}
                outputP       =
                  -- {{{
                  inP
                    { txOutDatum = qvfDatumToInlineDatum $ Escrow Map.empty
                    , txOutValue =
                        makeAuthenticValue
                          (halfOfTheRegistrationFee + excess)
                          pSym
                          pTN
                          1
                    }
                  -- }}}
              in
              if utxoHasOnlyX qvfSym qvfTokenName currUTxO then
                if pAddr == scriptAddr then
                  ( [outputS, outputP]
                  , pdPubKeyHash
                  , shouldBePaid
                  , [ mpPortion
                    , raised
                    , donFee
                    , don
                    , mpFee
                    , won
                    , belonging
                    , excess
                    , shouldBePaid
                    ]
                  )
                else
                  traceError "E091"
              else
                traceError "E089"
              -- }}}
            _                                                    ->
              -- {{{
              traceError "E088"
              -- }}}
    _      ->
      -- {{{
      traceError "E085"
      -- }}}
  -- }}}
-- }}}



-- data ScriptPurpose
--   = Minting    CurrencySymbol
--   | Spending   TxOutRef
--   | Rewarding  StakingCredential
--   | Certifying DCert

-- TxInfo	= TxInfo
--   { txInfoInputs          :: [TxInInfo]
--   , txInfoReferenceInputs :: [TxInInfo]
--   , txInfoOutputs         :: [TxOut]
--   , txInfoFee             :: Value
--   , txInfoMint            :: Value
--   , txInfoDCert           :: [DCert]
--   , txInfoWdrl            :: Map StakingCredential Integer
--   , txInfoValidRange      :: POSIXTimeRange
--   , txInfoSignatories     :: [PubKeyHash]
--   , txInfoRedeemers       :: Map ScriptPurpose Redeemer 
--   , txInfoData            :: Map DatumHash Datum 
--   , txInfoId              :: TxId
--   }

-- TxInInfo = TxInInfo
--   { txInInfoOutRef    :: TxOutRef
--   , txInInfoResolved  :: TxOut
--   }

-- TxOut = TxOut
--   { txOutAddress         :: Address
--   , txOutValue           :: Value
--   , txOutDatum           :: OutputDatum
--   , txOutReferenceScript :: Maybe ScriptHash
--   }

-- OutputDatum
--   = NoOutputDatum	 
--   | OutputDatumHash DatumHash	 
--   | OutputDatum     Datum

