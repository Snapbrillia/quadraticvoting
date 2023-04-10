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
import           Data.Redeemer
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
    info   = scriptContextTxInfo ctx

    inputs = txInfoInputs info
    refs   = txInfoReferenceInputs info

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
    utxoSitsAtScript =
      -- {{{
      (== ownAddr) . txOutAddress
      -- }}}

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
    xInputWithSpecificDatumExists :: CurrencySymbol
                                  -> TokenName
                                  -> (QVFDatum -> Bool)
                                  -> Bool
    xInputWithSpecificDatumExists sym tn datumPred =
      -- {{{
      let
        predicate TxInInfo{txInInfoResolved = txOut} =
          -- {{{
             utxoHasOnlyX sym tn txOut
          && utxoSitsAtScript txOut
          && datumPred (getInlineDatum txOut)
          -- }}}
      in
      case inputs of
        txIn : _ ->
          predicate txIn
        _        ->
          False
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

    -- | Validates the minting value.
    mintIsPresent :: CurrencySymbol -> TokenName -> Integer -> Bool
    mintIsPresent sym tn amt = txInfoMint info == Value.singleton sym tn amt

    projectMintIsPresent :: Bool -> Bool
    projectMintIsPresent mint =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(sym', _, amt')] ->
             traceIfFalse
               "E068"
               (sym' == qvfProjectSymbol)
          && traceIfFalse
               "E069"
               (if mint then amt' == 2 else amt' == negate 2)
        _                 ->
          traceError "E070"
      -- }}}

    -- | Expects a single continuing output, and validates given predicates.
    validateSingleOutput :: Maybe Integer
                         -> Maybe QVFDatum
                         -> Maybe (CurrencySymbol, TokenName)
                         -> Bool
    validateSingleOutput mExpectedLovelaces mExpectedDatum mExpectedAsset =
      -- {{{
      case getContinuingOutputs ctx of
        [o] ->
          -- {{{
             traceIfFalse
               "E071"
               ( case mExpectedLovelaces of
                   Just outputLovelaces ->
                     utxoHasLovelaces outputLovelaces o
                   Nothing              ->
                     True
               )
          && traceIfFalse
               "E072"
               ( case mExpectedDatum of
                   Just updatedDatum ->
                     utxosDatumMatchesWith updatedDatum o
                   Nothing           ->
                     True
               )
          && traceIfFalse
               "E073"
               ( case mExpectedAsset of
                   Just (sym, tn) ->
                     utxoHasOnlyX sym tn o
                   Nothing        ->
                     True
               )
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
    (DeadlineDatum _                              , UpdateDeadline newDl   ) ->
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E075"
           (deadlineNotReached newDl)
      && traceIfFalse
           "E076"
           (currUTxOHasX qvfSymbol qvfTokenName)
      && validateSingleOutput
           (Just deadlineLovelaces)
           (Just $ DeadlineDatum newDl)
           (Just (qvfSymbol, qvfTokenName))
      -- }}}

    (DeadlineDatum _                              , ConcludeFundingRound   ) ->
      -- Conclusion of a Funding Round
      -- {{{
         signedByKeyHolder
      && traceIfFalse "E002" (mintIsPresent qvfSymbol qvfTokenName (negate 2))
      -- }}}

    (RegisteredProjectsCount _                    , Contribute contribution) ->
      -- Match Pool Contribution
      -- {{{
      validateSingleOutput
        (Just $ lovelaceFromValue currVal + contribution)
        (Just datum)
        (Just (qvfSymbol, qvfTokenName))
      -- }}}

    (RegisteredProjectsCount _                    , RegisterProject        ) ->
      -- Project Registration
      -- {{{
      projectMintIsPresent True && canRegisterOrDonate
      -- }}}

    (RegisteredProjectsCount _                    , AccumulatePrizeWeights ) ->
      -- Formation of the Prize Weight Map
      -- {{{ 
      let
        validOutputs =
          accumulatePrizeWeights qvfSymbol qvfProjectSymbol inputs projRefs
      in
      traceIfFalse "E086" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (PrizeWeightAccumulation _ _                  , AccumulatePrizeWeights ) ->
      -- Accumulation of Computed Prize Weights
      -- (Same logic as `RegisteredProjectsCount` with this redeemer, TODO?)
      -- {{{ 
      let
        validOutputs =
          accumulatePrizeWeights qvfSymbol qvfProjectSymbol inputs projRefs
      in
      traceIfFalse "E087" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (ProjectEliminationProgress mp wMap           , EliminateOneProject    ) ->
      -- Elimination of Non-Eligible Projects
      -- {{{
      let
        (validOutputs, mEliminated, _) =
          eliminateOneProject
            qvfSymbol
            qvfProjectSymbol
            currUTxO
            inputs
            refs
            mp
            wMap
      in
         traceIfFalse "E122" (validOutputs == getContinuingOutputs ctx)
      && ( case mEliminated of
             Just (eliminated, raised) ->
               -- {{{
               traceIfFalse
                 "E123"
                 (valuePaidTo info eliminated == lovelaceValueOf raised)
               -- }}}
             Nothing                   ->
               -- {{{
               True
               -- }}}
         )
      -- }}}

    (DistributionProgress mp remaining den        , DistributePrize projID ) ->
      -- Handing Out Prize of a Specific Project
      -- {{{
      let
        projTN                         = TokenName projID
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
      in
         traceIfFalse "E122" (validOutputs == getContinuingOutputs ctx)
      && traceIfFalse
           "E094"
           (valuePaidTo info winner == lovelaceValueOf won)
      -- }}}

    (RegisteredProjectsCount _                    , ConcludeProject        ) ->
      -- Removal of a Donation-less Project
      -- {{{
      projectMintIsPresent False && canFoldOrDistribute
      -- }}}

    (PrizeWeightAccumulation _ _                  , ConcludeProject        ) ->
      -- Removal of a Donation-less Project
      -- (During/after prize weight accumulation, therefore no need to check
      -- the deadline)
      -- {{{
      projectMintIsPresent False
      -- }}}

    (DistributionProgress _ remaining _           , ConcludeFundingRound   ) ->
      -- Conclusion of a Funding Round
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E002"
           (mintIsPresent qvfSymbol qvfTokenName (negate 2))
      && traceIfFalse
           "E003"
           (remaining == 0)
      -- }}}
    -- }}}

    -- {{{ PROJECT INTERACTIONS 
    (ReceivedDonationsCount _                     , DonateToProject        ) ->
      -- Project Donation
      -- {{{
         traceIfFalse
           "E077"
           ( mintIsPresent
               qvfDonationSymbol
               (getCurrTokenName qvfProjectSymbol)
               1
           )
      && canRegisterOrDonate
      -- }}}

    (ReceivedDonationsCount _                     , ConcludeProject        ) ->
      -- Removal of a Donation-less Project
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E133"
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          RegisteredProjectsCount _   -> True
          PrizeWeightAccumulation _ _ -> True
          _                           -> False
      -- }}} 

    (ReceivedDonationsCount tot                   , FoldDonations          ) ->
      -- Folding Donations
      -- {{{
      if tot <= maxDonationInputsForPhaseTwo then
        -- No need for a two phase folding.
        let
          tn = getCurrTokenName qvfProjectSymbol
        in
        -- foldDonationsPhaseTwo tot
           traceIfFalse
             "E079"
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute
        && signedByKeyHolder
      else
        -- Folding should happen in two phases.
        foldDonationsPhaseOne
          maxDonationInputsForPhaseOne -- The number of donation assets expected in inputs.
          ( DonationFoldingProgress
              tot                                  -- Total number of donations.
              (tot - maxDonationInputsForPhaseOne) -- Donations folded so far.
          )
      -- }}}

    (DonationFoldingProgress tot soFar            , FoldDonations          ) ->
      -- Folding Donations
      -- {{{
      let
        remaining = tot - soFar
      in
      if remaining == 0 then
        let
          tn = getCurrTokenName qvfProjectSymbol
        in
        -- foldDonationsPhaseTwo tot
           traceIfFalse
             "E080"
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute
        && signedByKeyHolder
      else
        let
          expected = min remaining maxDonationInputsForPhaseOne
        in
        foldDonationsPhaseOne
          expected
          (DonationFoldingProgress tot (soFar + expected))
      -- }}}

    (ConsolidationProgress remaining wsSoFar      , FoldDonations          ) ->
      -- Reducing Donations to a Single Value
      -- {{{
      traceError "TODO."
      -- }}}

    (PrizeWeight _ False                          , AccumulatePrizeWeights ) ->
      -- Accumulation of Prize Weights
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E082"
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          RegisteredProjectsCount _      -> True
          PrizeWeightAccumulation tot ws -> tot > length (Map.toList ws)
          _                              -> False
      -- }}} 

    (PrizeWeight _ True                           , EliminateOneProject    ) ->
      -- Elimination of Non-Eligible Projects
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E100"
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          ProjectEliminationProgress _ _ -> True
          _                              -> False
      -- }}} 

    (PrizeWeight _ True                           , DistributePrize projID ) ->
      -- Distribution of a Project's Prize
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
         traceIfFalse
           "E092"
           (getCurrTokenName qvfProjectSymbol == TokenName projID)
      && traceIfFalse "E101"
           ( xInputWithSpecificDatumExists
               qvfSymbol
               qvfTokenName
               ( \case
                   DistributionProgress {} -> True
                   _                       -> False
               )
           )
      -- }}} 

    --      v-----------v is there a better term?
    (Escrow beneficiaries                         , UnlockEscrowFor ben amt) ->
      -- Giving Withdrawal Rights to a Bounty Winner
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        available     = currLovelaces - halfOfTheRegistrationFee
        tn            = getCurrTokenName qvfProjectSymbol
        projectOwner  =
          -- {{{
          case getDatumFromRefX qvfProjectSymbol tn of
            ProjectInfo ProjectDetails{..} ->
              pdPubKeyHash
            _                              ->
              traceError "E102"
          -- }}}
      in
         traceIfFalse
           "E103"
           (currUTxOHasX qvfProjectSymbol tn)
      && traceIfFalse
           "E104"
           (amt <= available)
      && traceIfFalse
           "E105"
           (txSignedBy info projectOwner)
      && validateSingleOutput
           (Just currLovelaces)
           (   Just
             $ Escrow
             $ Map.unionWith (+) (Map.singleton ben amt) beneficiaries
           )
           (Just (qvfSymbol, qvfTokenName))
      -- }}}

    (Escrow beneficiaries                         , WithdrawBounty winner  ) ->
      -- Bounty Collection from Escrow Account
      -- {{{
      let
        currLovelaces = lovelaceFromValue $ txOutValue currUTxO
        tn            = getCurrTokenName qvfProjectSymbol
      in
      signedByKeyHolder && -- For development. TODO: REMOVE.
      case Map.lookup winner beneficiaries of
        Just bounty ->
          -- {{{
          let
            remainingBounty = currLovelaces - bounty
          in
          if remainingBounty > halfOfTheRegistrationFee then
            -- {{{
               traceIfFalse
                 "E106"
                 (currUTxOHasX qvfProjectSymbol tn)
            && traceIfFalse
                 "E107"
                 (lovelaceFromValue (valuePaidTo info winner) == bounty)
            && validateSingleOutput
                 (Just remainingBounty)
                 (Just $ Escrow $ Map.delete winner beneficiaries)
                 (Just (qvfSymbol, qvfTokenName))
            -- }}}
          else 
            -- {{{
            projectMintIsPresent False
            -- }}}
          -- }}}
        Nothing     ->
          -- {{{
          traceError "E108"
          -- }}}
      -- }}}

    (Escrow beneficiaries                         , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
         traceIfFalse
           "E109"
           (Map.null beneficiaries)
      && projectMintIsPresent False
      -- }}}

    (ProjectInfo _                                , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
      projectMintIsPresent False
      -- }}}
    -- }}}

    -- {{{ DONATION INTERACTIONS 
    (Donation _                                   , FoldDonations          ) ->
      -- Folding Donations
      -- To avoid excessive transaction fees, this endpoint delegates its
      -- logic to the @P@ UTxO by checking that it is in fact being spent.
      -- {{{ 
      let
        -- | Finds the token name of the current donation UTxO being spent.
        --   Uses this value to check the presence of relevant project UTxO.
        --
        --   Raises exception upon failure.
        tn = getCurrTokenName qvfDonationSymbol
      in
        traceIfFalse "E078"
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          ReceivedDonationsCount _          -> True
          DonationFoldingProgress tot soFar -> tot > soFar
          _                                 -> False
      -- }}} 

    (Donations _                                  , FoldDonations          ) ->
      -- Second Phase of Folding Donations
      -- Similar to `Donation`, this endpoint also delegates its logic. This
      -- time, specifically to `DonationFoldingProgress`.
      -- {{{ 
      let
        tn = getCurrTokenName qvfDonationSymbol
      in
        traceIfFalse "E081"
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          DonationFoldingProgress tot soFar -> tot == soFar
          _                                 -> False
      -- }}} 
    -- }}}

    (_                                            , Dev                    ) ->
      -- For development. TODO: REMOVE.
      signedByKeyHolder
    (_                                            , _                      ) ->
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
                       -> [TxInInfo]
                       -> [TxInInfo]
                       -> [TxOut]
accumulatePrizeWeights qvfSym pSym initInputs initRefs =
  -- {{{
  let
    projMapFn :: TxInInfo -> Maybe (BuiltinByteString, Integer, TxOut)
    projMapFn TxInInfo{txInInfoResolved = i} =
      -- {{{
      case (getTokenNameOfUTxO pSym i, getInlineDatum i) of
        (Just (TokenName projID), PrizeWeight w False) ->
          Just (projID, w, i)
        _                                              ->
          Nothing
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

    govMapFn :: TxInInfo
             -> Maybe (Integer, Map BuiltinByteString EliminationInfo, TxOut)
    govMapFn TxInInfo{txInInfoResolved = i@TxOut{txOutValue}} =
      -- {{{
      if utxoHasOnlyX qvfSym qvfTokenName i then
        case getInlineDatum i of
          RegisteredProjectsCount tot    -> Just (tot, Map.empty, i)
          PrizeWeightAccumulation tot ws -> Just (tot, ws       , i)
          _                              -> traceError "E060"
      else
        Nothing
      -- }}}

    govs = mapMaybe govMapFn  initInputs

    go [] [] (c, wMap, outs) =
      -- {{{
      case govs of
        [(tot, ws, govUTxO@TxOut{txOutValue = govVal})] ->
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
        _                                               ->
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
        (mapMaybe projMapFn initInputs)

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
{-# INLINABLE eliminateOneProject #-}
eliminateOneProject :: CurrencySymbol
                    -> CurrencySymbol
                    -> TxOut
                    -> [TxInInfo]
                    -> [TxInInfo]
                    -> Integer
                    -> Map BuiltinByteString EliminationInfo
                    -> ([TxOut], Maybe (PubKeyHash, Integer), Integer)
eliminateOneProject
  qvfSym
  pSym
  currUTxO@TxOut{txOutAddress = scriptAddr, txOutValue = currVal}
  inputs
  refs
  matchPool
  ws =
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
        newMap = Map.delete toBeEliminated ws
        projTN = TokenName toBeEliminated
      in
      findOutputsFromProjectUTxOs pSym projTN inputs refs $
        \inP@TxOut{txOutValue = pVal, txOutAddress = pAddr} infoUTxO ->
          -- {{{
          -- Equality of project addresses is checked by
          -- `findOutputsFromProjectUTxOs`.
          if pAddr == scriptAddr then
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
          else
            -- {{{
            traceError "E127"
            -- }}}
          -- }}}
      -- }}}
    else
      -- {{{
      -- If all projects are eligible, then there should only be a single
      -- UTxO going back to the script address, which is the governance
      -- UTxO with an untouched value, and a properly updated datum.
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
                -> Integer -- Intermediary values for development purposes.
                -> Integer --                     v-------v
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
  den =
  -- {{{
  findOutputsFromProjectUTxOs pSym pTN inputs refs $
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
  -- }}}
-- }}}


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

