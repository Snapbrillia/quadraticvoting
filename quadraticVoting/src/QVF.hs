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
import           Plutus.V1.Ledger.Value               ( flattenValue
                                                      , AssetClass(..) )
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import qualified PlutusTx.AssocMap                    as Map
import           PlutusTx.AssocMap                    ( Map )
import qualified PlutusTx.Builtins                    as Builtins
import PlutusTx.Prelude                               ( Bool(..)
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

    -- | Traverses transaction inputs, reference inputs, and outputs to
    --   validate the proper correspondence between input `PrizeWeight` datums
    --   and their "processed" couterparts. And also to validate the updated
    --   datum.
    --
    --   Note that the inputs are expected to be "aligned" with the reference
    --   and output UTxOs. And the governance input/output is expected to be at
    --   the ends of respective lists:
    --       inputs:          [i0, i1, ..., in, iGov, rest of inputs]
    --       references:      [r0, r1, ..., rn]
    --       cont. outputs:   [o0, o1, ..., on, oGov]
    --
    --   The output @Map@ maps project IDs to @EliminationInfo@.
    --
    --   Raises exception upon failure.
    --
    --   TODO: A potential optimization is to store the number of processed
    --         projects as another argument of the datum constructor so that
    --         the weights list is not traversed in this function.
    -- traversePrizeWeights :: Integer
    --                      -> Map BuiltinByteString EliminationInfo
    --                      -> Bool
    -- traversePrizeWeights totPs wsSoFar =
    --   -- {{{
    --   let
    --     mp              = lovelaceFromValue currVal - governanceLovelaces
    --     psSoFar         = length $ Map.toList wsSoFar
    --     remaining       = totPs - psSoFar
    --     contOuts        = getContinuingOutputs ctx
    --     -- TODO: Can this filtered version be averted?
    --     projRefs        = filter (utxoHasX qvfProjectSymbol Nothing . txInInfoResolved) refs
    --     go
    --       (TxInInfo{txInInfoResolved = i@TxOut{txOutValue = inVal}} : _)
    --       []
    --       [o]
    --       acc@(c, wMap) =
    --       -- {{{
    --       let
    --         ws           = Map.unionWith const wsSoFar wMap
    --         updatedDatum =
    --           -- {{{
    --           if c < remaining then
    --             -- {{{
    --             PrizeWeightAccumulation totPs ws
    --             -- }}}
    --           else if c == remaining then
    --             -- {{{
    --             ProjectEliminationProgress mp ws
    --             -- }}}
    --           else
    --             -- {{{
    --             traceError "E059"
    --             -- }}}
    --           -- }}}
    --         isValid      =
    --           -- {{{
    --              traceIfFalse "E060" (utxoHasValue inVal o)
    --           && traceIfFalse "E061" (utxosDatumMatchesWith updatedDatum o)
    --           && (txOutAddress i == ownAddr)
    --           -- }}}
    --       in
    --       if isValid then
    --         acc
    --       else
    --         traceError "E062"
    --       -- }}}
    --     go
    --       (TxInInfo{txInInfoResolved = i@TxOut{txOutValue = inVal}} : ins)
    --       (TxInInfo{txInInfoResolved = r} : rs)
    --       (o : os)
    --       (c, wMap)     =
    --       -- {{{
    --       case (getTokenNameOfUTxO qvfProjectSymbol i, getTokenNameOfUTxO qvfProjectSymbol r) of
    --         (Just iTN@(TokenName projID), Just rTN) ->
    --           -- {{{
    --           if iTN == rTN then
    --             -- {{{
    --             case (getInlineDatum i, getInlineDatum r) of
    --               (PrizeWeight w False, ProjectInfo (ProjectDetails{..})) ->
    --                 -- {{{
    --                 let
    --                   oIsValid =
    --                        utxoHasValue inVal o
    --                     && utxosDatumMatchesWith (PrizeWeight w True) o
    --                     && txOutAddress i == ownAddr
    --                 in
    --                 if oIsValid then
    --                   go
    --                     ins
    --                     rs
    --                     os
    --                     ( c + 1
    --                     , Map.insert
    --                         projID
    --                         ( EliminationInfo
    --                             pdRequested
    --                             (   lovelaceFromValue inVal
    --                               - halfOfTheRegistrationFee
    --                             )
    --                             w
    --                         )
    --                         wMap
    --                     )
    --                 else
    --                   traceError "E057"
    --                 -- }}}
    --               _                                                       ->
    --                 -- {{{
    --                 traceError "E056"
    --                 -- }}}
    --             -- }}}
    --           else
    --             -- {{{
    --             traceError "E120"
    --             -- }}}
    --           -- }}}
    --         _                                       ->
    --           -- {{{
    --           traceError "E121"
    --           -- }}}
    --       -- }}}
    --     go _ _ _ _      = traceError "E063"
    --   in
    --   -- TODO: Is this evaluation enforcements redundant?
    --   case go inputs projRefs contOuts (0, Map.empty) of
    --     (_, _) -> True
    --   -- }}}

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
    mintIsPresent sym tn amt =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(sym', tn', amt')] ->
          sym' == sym && tn' == tn && amt' == amt
        _                   ->
          False
      -- }}}

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

    validateProjectUTxOs :: TokenName
                         -> (TxOut -> TxOut -> Bool)
                         -> Bool
    validateProjectUTxOs projTN validation =
      -- {{{
      case filter (utxoHasOnlyX qvfProjectSymbol projTN . txInInfoResolved) inputs of
        [TxInInfo{txInInfoResolved = inP}] ->
          -- {{{
          case filter (utxoHasOnlyX qvfProjectSymbol projTN . txInInfoResolved) refs of
            [TxInInfo{txInInfoResolved = infoUTxO}] ->
              -- {{{
              validation inP infoUTxO
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
  in
  case (datum, action) of
    -- {{{ GOVERNANCE INTERACTIONS 
    (DeadlineDatum _                              , UpdateDeadline newDl   ) ->
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E075"
           (deadlineReached newDl)
      && traceIfFalse
           "E076"
           (currUTxOHasX qvfSymbol qvfTokenName)
      && validateSingleOutput
           Nothing
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
      case getContinuingOutputs ctx of
        [o] ->
          -- {{{
             traceIfFalse
               "E083"
               ( utxoHasOnlyXWithLovelaces
                   qvfSymbol
                   qvfTokenName
                   (lovelaceFromValue currVal + contribution)
                   o
               )
          && traceIfFalse
               "E084"
               (utxosDatumMatchesWith datum o)
          -- }}}
        _   ->
          -- {{{
          traceError "E085"
          -- }}}
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
        projRefs     =
          -- TODO: Can this filtration be averted?
          filter (utxoHasX qvfProjectSymbol Nothing . txInInfoResolved) refs
        validOutputs =
          accumulatePrizeWeights
            ownAddr
            qvfSymbol
            qvfProjectSymbol
            inputs 
            projRefs
      in
      traceIfFalse "E086" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (PrizeWeightAccumulation _ _                  , AccumulatePrizeWeights ) ->
      -- Accumulation of Computed Prize Weights
      -- (Same logic as `RegisteredProjectsCount` with this redeemer, TODO?)
      -- {{{ 
      let
        projRefs     =
          filter (utxoHasX qvfProjectSymbol Nothing . txInInfoResolved) refs
        validOutputs =
          accumulatePrizeWeights
            ownAddr
            qvfSymbol
            qvfProjectSymbol
            inputs 
            projRefs
      in
      traceIfFalse "E087" (validOutputs == getContinuingOutputs ctx)
      -- }}} 

    (ProjectEliminationProgress mp wMap           , EliminateProject       ) ->
      -- Elimination of Non-Eligible Projects
      -- {{{
      case eliminateOneProject mp wMap of
        Right (newMap, removedProjID) ->
          -- {{{
          let
            projTN = TokenName removedProjID
          in
          validateProjectUTxOs projTN $
            \inP@TxOut{txOutValue = pVal} infoUTxO ->
              -- {{{
              case (getInlineDatum inP, getInlineDatum infoUTxO) of
                (PrizeWeight _ True, ProjectInfo ProjectDetails{..}) ->
                  -- {{{
                  case getContinuingOutputs ctx of
                    [s, p] ->
                      -- {{{
                      let
                        raised             =
                          lovelaceFromValue pVal - halfOfTheRegistrationFee
                        (khFee, belonging) = separateKeyHoldersFeeFrom raised
                        paidAmount         =
                          lovelaceFromValue (valuePaidTo info pdPubKeyHash)
                      in
                         traceIfFalse
                           "E129"
                           ( utxoHasValue
                               (currVal <> lovelaceValueOf khFee)
                               s
                           )
                      && traceIfFalse
                           "E130"
                           ( utxosDatumMatchesWith
                               (ProjectEliminationProgress mp newMap)
                               p
                           )
                      && traceIfFalse
                           "E131"
                           (paidAmount == belonging)
                      && traceIfFalse
                           "E132"
                           ( utxoHasOnlyXWithLovelaces
                               qvfProjectSymbol
                               projTN
                               halfOfTheRegistrationFee
                               p
                           )
                      && traceIfFalse
                           "E133"
                           (utxosDatumMatchesWith (Escrow Map.empty) p)
                      -- }}}
                    _      ->
                      -- {{{
                      traceError "E128"
                      -- }}}
                  -- }}}
                _                                                    ->
                  -- {{{
                  traceError "E127"
                  -- }}}
              -- }}}
          -- }}}
        Left (pCount, denominator)    ->
          -- {{{
          case getContinuingOutputs ctx of
            [o] ->
              -- {{{
                 traceIfFalse
                   "E122"
                   (utxoHasValue currVal o)
              && traceIfFalse
                   "E123"
                   ( utxosDatumMatchesWith
                       ( DistributionProgress
                           (lovelaceFromValue currVal - governanceLovelaces)
                           pCount
                           denominator
                       )
                       o
                   )
              -- }}}
            _   ->
              -- {{{
              traceError "E124"
              -- }}}
          -- }}}
      -- }}}

    (DistributionProgress mp remaining den        , DistributePrize projID ) ->
      -- Handing Out Prize of a Specific Project
      -- {{{
      let
        projTN = TokenName projID
      in
      validateProjectUTxOs projTN $
        \inP@TxOut{txOutValue = pVal} infoUTxO ->
          case (getInlineDatum inP, getInlineDatum infoUTxO) of
            (PrizeWeight w True, ProjectInfo ProjectDetails{..}) ->
              -- {{{
              let
                matchPoolPortion   = mp * w `divide` den
                raised             =
                  lovelaceFromValue pVal - halfOfTheRegistrationFee
                (khFee, belonging) =
                  separateKeyHoldersFeeFrom $ raised + matchPoolPortion
                paidAmount         =
                  lovelaceFromValue (valuePaidTo info pdPubKeyHash)
                excess             = max 0 $ belonging - pdRequested
                shouldBePaid       = belonging - excess
              in
              case getContinuingOutputs ctx of
                [s, p] ->
                  -- {{{
                     traceIfFalse
                       "E090"
                       ( utxoHasValue
                           (    currVal
                             <> lovelaceValueOf (khFee - matchPoolPortion)
                           )
                           s
                       )
                  && traceIfFalse
                       "E091"
                       ( utxosDatumMatchesWith
                           (DistributionProgress mp (remaining - 1) den)
                           s
                       )
                  && traceIfFalse
                       "E092"
                       ( utxoHasOnlyXWithLovelaces
                           qvfProjectSymbol
                           projTN
                           (halfOfTheRegistrationFee + excess)
                           p
                       )
                  && traceIfFalse
                       "E093"
                       (utxosDatumMatchesWith (Escrow Map.empty) p)
                  && traceIfFalse
                       "E094"
                       (paidAmount == shouldBePaid)
                  -- }}}
                _      ->
                  -- {{{
                  traceError "E089"
                  -- }}}
              -- }}}
            _                                                    ->
              -- {{{
              traceError "E088"
              -- }}}
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

    (PrizeWeight _ True                           , EliminateProject       ) ->
      -- Elimination of Non-Eligible Projects
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
      let
        tn = getCurrTokenName qvfProjectSymbol
      in
        traceIfFalse "E100"
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          ProjectEliminationProgress _ _ -> True
          _                              -> False
      -- }}} 

    (PrizeWeight _ True                           , DistributePrize projID ) ->
      -- Distribution of a Project's Prize
      -- (Delegation of logic to the governance UTxO.)
      -- {{{ 
        traceIfFalse "E101"
      $ xInputWithSpecificDatumExists qvfProjectSymbol (TokenName projID)
      $ \case
          DistributionProgress {} -> True
          _                       -> False
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
accumulatePrizeWeights :: Address
                       -> CurrencySymbol
                       -> CurrencySymbol
                       -> [TxInInfo]
                       -> [TxInInfo]
                       -> [TxOut]
accumulatePrizeWeights scriptAddr qvfSym pSym inputs refs =
  -- {{{
  let
    go
      (TxInInfo{txInInfoResolved = i@TxOut{txOutValue = inVal}} : _)
      []
      (c, wMap, outs) =
      -- scriptAddr is not validated here. TODO?
      -- {{{
      case flattenValue inVal of
        [(sym', tn', amt'), (_, _, mpWithGov)] ->
          -- {{{
          if sym' == qvfSym && tn' == qvfTokenName && amt' == 1 then
            -- {{{
            let
              fromTotAndWs tot ws =
                -- {{{
                let
                  remaining = tot - length (Map.toList ws)
                  finalWs   = Map.unionWith const ws wMap
                  updatedD  =
                    -- {{{
                    if c < remaining then
                      -- {{{
                      PrizeWeightAccumulation tot finalWs
                      -- }}}
                    else if c == remaining then
                      -- {{{
                      ProjectEliminationProgress
                        (mpWithGov - governanceLovelaces)
                        finalWs
                      -- }}}
                    else
                      -- {{{
                      traceError "E061"
                      -- }}}
                    -- }}}
                in
                i {txOutDatum = OutputDatum $ qvfDatumToDatum updatedD}
                : outs
                -- }}}
            in
            case getInlineDatum i of
              RegisteredProjectsCount tot         ->
                fromTotAndWs tot Map.empty
              PrizeWeightAccumulation tot wsSoFar ->
                fromTotAndWs tot wsSoFar
              _                                   ->
                traceError "E060"
            -- }}}
          else
            -- {{{
            traceError "E057"
            -- }}}
          -- }}}
        _                                      ->
          -- {{{
          traceError "E059"
          -- }}}
      -- }}}
    go
      (TxInInfo{txInInfoResolved = i@TxOut{txOutValue = inVal, txOutAddress = iAddr}} : ins)
      (TxInInfo{txInInfoResolved = r@TxOut{txOutAddress = rAddr}} : rs)
      (c, wMap, outs) =
      -- {{{
      case (getTokenNameOfUTxO pSym i, getTokenNameOfUTxO pSym r) of
        (Just iTN@(TokenName projID), Just rTN) ->
          -- {{{
          if iTN == rTN && scriptAddr == iAddr && iAddr == rAddr then
            -- {{{
            case (getInlineDatum i, getInlineDatum r) of
              (PrizeWeight w False, ProjectInfo (ProjectDetails{..})) ->
                -- {{{
                go
                  ins
                  rs
                  ( c + 1
                  , Map.insert
                      projID
                      ( EliminationInfo
                          pdRequested
                          (lovelaceFromValue inVal - halfOfTheRegistrationFee)
                          w
                      )
                      wMap
                  , i { txOutDatum =
                          OutputDatum $ qvfDatumToDatum $ PrizeWeight w True
                      }
                    : outs
                  )
                -- }}}
              _                                                       ->
                -- {{{
                traceError "E056"
                -- }}}
            -- }}}
          else
            -- {{{
            traceError "E120"
            -- }}}
          -- }}}
        _                                       ->
          -- {{{
          traceError "E121"
          -- }}}
      -- }}}
    go _ _ _          = traceError "E062"
  in
  go inputs refs (0, Map.empty, [])
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

