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
                                                      , find
                                                      , foldr
                                                      , filter
                                                      , isJust
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
import           Minter.Governance                    ( qvfTokenName
                                                      , deadlineTokenName )
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
               -> QVFAction
               -> ScriptContext
               -> Bool
mkQVFValidator QVFParams{..} datum action ctx =
  -- {{{
  let
    -- {{{
    info   = scriptContextTxInfo ctx

    inputs = txInfoInputs info
    refs   = txInfoRedeemers info

    -- | The UTxO currently being validated.
    currUTxO :: TxOut
    currUTxO =
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
      case find (utxoHasX sym (Just tn) . txInInfoResolved) refs of
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
    --   and their depleted couterparts. And also to validate the updated
    --   datum.
    --
    --   Raises exception upon failure.
    --
    --   TODO: A potential optimization is to store the number of processed
    --         projects as another argument of the datum constructor so that
    --         the weights list is not traversed in this function.
    traversePrizeWeights :: Integer
                         -> Integer
                         -> Map BuiltinByteString (Integer, Integer)
                         -> Bool
    traversePrizeWeights matchPool totPs wsSoFar =
      -- {{{
      let
        psSoFar           = length $ Map.toList wsSoFar
        remaining         = totPs - psSoFar
        go
          []
          []
          [o]
          acc@(c, wMap)   =
          -- TODO
          -- {{{
          let
            ws           = Map.unionWith const wsSoFar wMap
            updatedDatum =
              -- {{{
              if c < remaining then
                -- {{{
                DonationAccumulationProgress mp totPs ws
                -- }}}
              else if c == remaining then
                -- {{{
                ProjectEliminationProgress mp ws
                -- }}}
              else
                -- {{{
                traceError "E059"
                -- }}}
              -- }}}
            isValid      =
              -- {{{
                 traceIfFalse
                   "E060"
                   ( utxoHasOnlyXWithLovelaces
                       qvfSymbol
                       qvfTokenName
                       (lovelaces + governanceLovelaces)
                       o
                   )
              && traceIfFalse
                   "E061"
                   (utxosDatumMatchesWith updatedDatum o)
              -- }}}
          in
          if isValid then
            acc
          else
            -- Failure in case of missing main authentication asset.
            traceError "E062"
          -- }}}
        go
          (TxInInfo{txInInfoResolved = i@TxOut{txOutValue = inVal}} : ins)
          (TxInInfo{txInInfoResolved = r} : rs)
          (o : os)
          acc@(c, wMap)   =
          -- {{{
          case (getTokenNameOfUTxO qvfProjectSymbol i, getTokenNameOfUTxO qvfProjectSymbol r) of
            (Just iTN@TokenName projID, Just rTN) ->
              -- {{{
              if iTN == rTN then
                -- {{{
                case (getInlineDatum i, getInlineDatum r) of
                  (PrizeWeight w False, ProjectInfo (ProjectDetails{..})) ->
                    -- {{{
                    let
                      oIsValid =
                           utxoHasValue inVal o
                        && utxosDatumMatchesWith (PrizeWeight w True) o
                    in
                    if oIsValid then
                      go
                        ins
                        rs
                        os
                        ( c + 1
                        , Map.insert projID (pdRequested, w) wMap
                        )
                    else
                      traceError "E057"
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
            Nothing                               ->
              -- {{{
              traceError "E121"
              -- }}}
          -- }}}
        go _ _ _ _        = traceError "E122"
        (outputPs, _)     =
          -- {{{
          outputGo (getContinuingOutputs ctx) (0, wsSoFar)
          -- }}}
      in
      traceIfFalse
        "E063"
        (inputPs == outputPs)
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   is still in progress.
    --
    --   Raises exception on @False@.
    canRegisterOrDonate :: Bool
    canRegisterOrDonate =
      -- {{{
      case getDatumFromRefX qvfSymbol deadlineTokenName of
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
      case getDatumFromRefX qvfSymbol deadlineTokenName of
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
                     utxoHasX sym (Just tn) o
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
    (DeadlineDatum _                              , UpdateDeadline newDl   ) ->
      -- {{{
         signedByKeyHolder
      && traceIfFalse
           "E075"
           (deadlineReached newDl)
      && traceIfFalse
           "E076"
           (currUTxOHasX qvfSymbol deadlineTokenName)
      && validateSingleOutput
           Nothing
           (Just $ DeadlineDatum newDl)
           (Just (qvfSymbol, deadlineTokenName))
      -- }}}

    (RegisteredProjectsCount _                    , RegisterProject        ) ->
      -- Project Registration
      -- {{{
      projectMintIsPresent True && canRegisterOrDonate
      -- }}}

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
          expected  = min remaining maxDonationInputsForPhaseOne
        in
        foldDonationsPhaseOne
          expected
          (DonationFoldingProgress tot (soFar + expected))
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

    (PrizeWeight _ False                          , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- (Delegation of logic to the main UTxO.)
      -- {{{ 
        traceIfFalse "E082"
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          RegisteredProjectsCount _                ->
            True
          DonationAccumulationProgress tot soFar _ ->
            tot > length (Map.toList soFar)
          _                                        ->
            False
      -- }}} 

    (RegisteredProjectsCount tot                  , AccumulateDonations    ) ->
      -- First Accumulation of Donations
      -- {{{ 
      let
        currVal = lovelaceFromValue $ txOutValue currUTxO
        mp      = currVal - governanceLovelaces
      in
      -- Note that the last argument *does include* the governance Lovelaces.
      traversePrizeWeights mp tot Map.empty currVal
      -- }}} 

    (DonationAccumulationProgress mp tot ws       , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- {{{ 
      traversePrizeWeights mp tot ws (lovelaceFromValue $ txOutValue currUTxO)
      -- }}} 

    (ProjectEliminationProgress mp ws             , PayKeyHolderFee        ) ->
      -- Key Holder Fee Collection
      -- {{{
      let
        (khFee, updatedDatum) = findDatumAfterPayingKeyHoldersFee ps ds den
        keyHolderImbursed     =
          lovelaceFromValue (valuePaidTo info qvfKeyHolder) == khFee
      in
         traceIfFalse
           "E083"
           (currUTxOHasX qvfSymbol qvfTokenName)
      && ( case getContinuingOutputs ctx of
             [o] ->
               -- {{{
               let
                 inVal         = txOutValue currUTxO
                 desiredOutVal = inVal <> lovelaceValueOf (negate khFee)
               in
                  traceIfFalse
                    "E084"
                    (utxoHasValue desiredOutVal o)
               && traceIfFalse
                    "E085"
                    (utxosDatumMatchesWith updatedDatum o)
               -- }}}
             _   ->
               -- {{{
               traceError "E086"
               -- }}}
         )
      && traceIfFalse
           "E087"
           keyHolderImbursed
      -- }}}

    (ProjectEliminationProgress mp ws             , DistributePrizes       ) ->
      -- Depending on the Prize Distribution
      -- {{{
      let
        scriptOutputs    = getContinuingOutputs ctx

        -- | Folds all the reference project info UTxOs in a @Map@ from their
        --   token names to their details.
        recepientsInfoMap =
          -- {{{
          foldr
            ( \TxInInfo{txInInfoResolved = txOut} acc ->
                case getTokenNameOfUTxO qvfProjectSymbol txOut of
                  Just tn ->
                    -- {{{
                    case getInlineDatum txOut of
                      ProjectInfo dets ->
                        Map.insert tn dets acc
                      _                ->
                        traceError "E088"
                    -- }}}
                  Nothing ->
                    -- {{{
                    acc
                    -- }}}
            )
            Map.empty
            refs
          -- }}}

        -- | Folding function to go over all the input UTxOs. For each project
        --   `PrizeWeight` input it finds (such that its info is also present
        --   in the reference inputs), checks to see if the project owner is
        --   paid his/her rightful portion.
        foldFn TxInInfo{txInInfoResolved = txOut} acc@(accP, accPaid) =
          -- {{{
          case getTokenNameOfUTxO qvfProjectSymbol txOut of
            Just tn ->
              -- {{{
              case Map.lookup tn recepientsInfoMap of
                Just ProjectDetails{..} ->
                  -- {{{
                  case getInlineDatum txOut of
                    PrizeWeight w True ->
                      -- {{{
                      let
                        portion          = findProjectsWonLovelaces ds den w
                        prize            = min pdRequested portion
                        paidAmount       =
                          -- {{{
                          lovelaceFromValue (valuePaidTo info pdPubKeyHash)
                          -- }}}
                        prizeIsPaid      = paidAmount == prize
                        mEscrowOutput    =
                          -- {{{
                          find
                            (utxoHasX qvfProjectSymbol $ Just tn)
                            scriptOutputs
                          -- }}}
                        escrowIsProduced =
                          -- {{{
                          case mEscrowOutput of
                            Just o  ->
                              -- {{{
                                 traceIfFalse
                                   "E089"
                                   ( utxoHasLovelaces
                                       ( findEscrowLovelaces
                                           portion
                                           pdRequested
                                       )
                                       o
                                   )
                              && traceIfFalse
                                   "E090"
                                   (utxosDatumMatchesWith (Escrow Map.empty) o)
                              -- }}}
                            Nothing ->
                              -- {{{
                              traceError "E091"
                              -- }}}
                          -- }}}
                      in
                      if prizeIsPaid && escrowIsProduced then
                        (accP + 1, accPaid + prize)
                      else
                        traceError "E092"
                      -- }}}
                    _                  ->
                      -- {{{
                      traceError "E093"
                      -- }}}
                  -- }}}
                Nothing                 ->
                  -- {{{
                  traceError
                    "E094"
                  -- }}}
              -- }}}
            Nothing ->
              -- {{{
              acc
              -- }}}
          -- }}}

        (projectsAccountedFor, prizesPaid) = foldr foldFn (0, 0) inputs
      in
      if projectsAccountedFor > ps then -- TODO: Is this redundant?
        traceError "E095"
      else
        case find (utxoHasX qvfSymbol (Just qvfTokenName)) scriptOutputs of
          Just o  ->
            -- {{{
            let
              currLovelaces      = lovelaceFromValue $ txOutValue currUTxO
              remainingLovelaces = currLovelaces - prizesPaid
            in
               traceIfFalse
                 "E096"
                 (currUTxOHasX qvfSymbol qvfTokenName)
            && traceIfFalse
                 "E097" -- TODO: Is this necessary?
                 (remainingLovelaces >= governanceLovelaces)
            && traceIfFalse
                 "E098"
                 ( utxosDatumMatchesWith
                     ( DonationAccumulationConcluded
                         (ps - projectsAccountedFor)
                         ds
                         den
                         True
                     )
                     o
                 )
            && traceIfFalse
                 "E099"
                 (utxoHasLovelaces remainingLovelaces o)
            -- }}}
          Nothing ->
            -- {{{
            traceError "E100"
            -- }}}
      -- }}}

    (PrizeWeight _ True                           , DistributePrizes       ) ->
      -- Prize Distribution
      -- (Delegation of logic to the project's UTxO.)
      -- {{{ 
      let
        tn = getCurrTokenName qvfProjectSymbol
      in
        traceIfFalse "E101"
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          DonationAccumulationConcluded _ _ _ True -> True
          _                                        -> False
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
  type RedeemerType QVF = QVFAction


typedQVFValidator :: QVFParams -> PSU.V2.TypedValidator QVF
typedQVFValidator =
  let
    wrap = PSU.V2.mkUntypedValidator @QVFDatum @QVFAction
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
{-# INLINABLE findDatumAfterPayingKeyHoldersFee #-}
-- | Returns the calculated fee, and the updated datum.
findDatumAfterPayingKeyHoldersFee :: Integer
                                  -> Integer
                                  -> Integer
                                  -> (Integer, QVFDatum)
findDatumAfterPayingKeyHoldersFee ps totalLovelaces denominator =
  -- {{{
  let
    forKH = max minKeyHolderFee $ 5 * totalLovelaces `divide` 100
  in
  ( forKH
  , DonationAccumulationConcluded ps (totalLovelaces - forKH) denominator True
  )
  -- }}}


{-# INLINABLE findProjectsWonLovelaces #-}
findProjectsWonLovelaces :: Integer -> Integer -> Integer -> Integer
findProjectsWonLovelaces pool sumW w =
  -- {{{
  (w * pool) `divide` sumW
  -- }}}


{-# INLINABLE findEscrowLovelaces #-}
findEscrowLovelaces :: Integer -> Integer -> Integer
findEscrowLovelaces portion requested =
  -- {{{
  max halfOfTheRegistrationFee $
    halfOfTheRegistrationFee + portion - requested
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

