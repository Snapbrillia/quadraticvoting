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
import           Data.RegistrationInfo
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
    info   = scriptContextTxInfo ctx

    inputs = txInfoInputs info

    -- | The UTxO currently being validated.
    currUTxO :: TxOut
    currUTxO =
      -- {{{
      case findOwnInput ctx of
        Nothing ->
          traceError "Couldn't find UTxO."
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
    signedByKeyHolder :: () -> Bool
    signedByKeyHolder _ =
      -- {{{
      traceIfFalse "Unauthorized." $ txSignedBy info qvfKeyHolder
      -- }}}

    -- | Checks if the UTxO currently being validated carries a single X asset.
    currUTxOHasX :: CurrencySymbol -> TokenName -> Bool
    currUTxOHasX sym tn =
      -- {{{
      utxoHasX sym (Just tn) currUTxO
      -- }}}

    -- | Tries to find a singular asset with a given symbol inside the given
    --   UTxO, and returns its token name.
    --
    --   TODO: Presumes the given UTxO has only Lovelaces and the desired
    --         asset, and that Lovelaces show up first after calling
    --         `flattenValue`.
    getTokenNameOfUTxO :: CurrencySymbol -> TxOut -> Maybe TokenName
    getTokenNameOfUTxO sym utxo =
      -- {{{
      case flattenValue (txOutValue utxo) of
        [_, (sym', tn', amt')] ->
          -- {{{
          if sym' == sym && amt' == 1 then
            Just tn'
          else
            Nothing
          -- }}}
        _                   ->
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
        Nothing -> traceError "Current UTxO is unauthentic."
      -- }}}

    -- | Tries to find an input from the script that carries a single X asset.
    --
    --   Expects to find exactly 1. Raises exception otherwise.
    getXInputUTxO :: CurrencySymbol -> TokenName -> TxOut
    getXInputUTxO sym tn =
      -- {{{
      case filter (utxoHasX sym (Just tn) . txInInfoResolved) inputs of
        [TxInInfo{txInInfoResolved = inUTxO}] ->
          if utxoSitsAtScript inUTxO then
            inUTxO
          else
            traceError "Input authentication asset must come from the script."
        _      ->
          traceError "There should be exactly 1 authentication asset in input."
      -- }}}

    -- | Looks inside the reference inputs and extracts the inline datum
    --   attached to one of which carries the given asset.
    --
    --   Raises exception if the reference input, or the datum is not found.
    getDatumFromRefX :: CurrencySymbol -> TokenName -> QVFDatum
    getDatumFromRefX sym tn =
      -- {{{
      case find (utxoHasX sym (Just tn) . txInInfoResolved) (txInfoReferenceInputs info) of
        Just txIn ->
          -- {{{
          getInlineDatum (txInInfoResolved txIn)
          -- }}}
        Nothing   ->
          -- {{{
          traceError "Missing reference input."
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
             utxoHasX sym (Just tn) txOut
          && utxoSitsAtScript txOut
          && datumPred (getInlineDatum txOut)
          -- }}}
      in
      isJust $ find predicate inputs
      -- }}}
  
    -- | Checks 1 X comes from the script, and 1 X goes back to the script,
    --   with enough added Lovelaces and properly updated datum.
    --
    --   Raises exception on @False@.
    xIsPresent :: CurrencySymbol -- ^ X's currency symbol
               -> TokenName      -- ^ X's token name
               -> Integer        -- ^ Increase in output's Lovelace count
               -> QVFDatum       -- ^ Updated datum
               -> Bool
    xIsPresent sym tn increaseInLovelace newDatum =
      -- {{{
      case filter (utxoHasX sym $ Just tn) (getContinuingOutputs ctx) of
        [txOut@TxOut{txOutValue = outVal}] ->
          -- {{{
          let
            TxOut{txOutValue = inVal} = getXInputUTxO sym tn
            desiredOutVal             =
              inVal <> lovelaceValueOf increaseInLovelace
          in
             traceIfFalse
               "Authenticated output doesn't have enough Lovelaces"
               (outVal == desiredOutVal)
          && traceIfFalse
               "Invalid datum attached to the authenticated output."
               (utxosDatumMatchesWith newDatum txOut)
          -- }}}
        _       ->
          -- {{{
          traceError "There must be exactly 1 authentication asset produced."
          -- }}}
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
               "Incorrect number of donations included for the first phase of folding."
               (ds == requiredDonationCount)
          && traceIfFalse
               "Donations output must carry the folded donations."
               (utxosDatumMatchesWith (Donations finalMap) od)
          && traceIfFalse
               "Project output must be properly updated."
               (utxosDatumMatchesWith psUpdatedDatum op)
          && traceIfFalse
               "Donations output must carry all the donation Lovelaces."
               (utxoHasLovelaces total od)
          && traceIfFalse
               "Project output must preserve its Lovelaces."
               (utxoHasLovelaces halfOfTheRegistrationFee op)
          && canFoldOrDistribute ()
          && signedByKeyHolder ()
          -- }}}
        _                  ->
          -- {{{
          traceError "Missing proper outputs for the first phase of folding donations."
          -- }}}
      -- }}}

    -- | Traverses transaction inputs and outputs to validate the proper
    --   correspondence between input `PrizeWeight` datums and their
    --   depleted couterparts.
    --
    --   Raises exception upon failure.
    traversePrizeWeights :: Integer -> Integer -> Integer -> Integer -> Bool
    traversePrizeWeights totPs psSoFar lovelacesSoFar wSoFar =
      -- {{{
      let
        remaining = totPs - psSoFar
        inputFoldFn TxInInfo{txInInfoResolved = o} acc@(pCount, sumL, sumW, wMap) =
          -- {{{
          if utxoHasX qvfProjectSymbol Nothing o then
            -- {{{
            case getInlineDatum o of
              PrizeWeight w False ->
                -- {{{
                case getTokenNameOfUTxO qvfProjectSymbol o of
                  Just tn ->
                    ( pCount + 1
                    , sumL + lovelaceFromValue (txOutValue o)
                    , sumW + w
                    , Map.insert tn w wMap
                    )
                  Nothing ->
                    traceError "Project asset not found."
                -- }}}
              _                   ->
                -- {{{
                traceError "Unexpected UTxO encountered (expected a `PrizeWeight`."
                -- }}}
            -- }}}
          else
            -- {{{
            acc
            -- }}}
          -- }}}
        (inputPs, sumOfTheirLovelaces, sumOfTheirWs, mapFromTNsToWs) =
          foldr inputFoldFn (0, 0, 0, Map.empty) inputs
        outputFoldFn o acc =
          -- {{{
          case getTokenNameOfUTxO qvfProjectSymbol o of
            Just tn ->
              -- {{{
              case Map.lookup tn mapFromTNsToWs of
                Just validW ->
                  -- {{{
                  let
                    isValid =
                         utxosDatumMatchesWith (PrizeWeight validW True) o
                      && utxoHasLovelaces halfOfTheRegistrationFee o
                  in
                  if isValid then
                    acc + 1
                  else
                    traceError
                      "Invalid datum attached to depleted prize weight UTxO."
                  -- }}}
                Nothing         ->
                  -- {{{
                  traceError
                    "Invalid prize weight UTxO is being produced."
                  -- }}}
              -- }}}
            Nothing ->
              -- {{{
              let
                lovelaces    = lovelacesSoFar + sumOfTheirLovelaces
                w            = wSoFar + sumOfTheirWs
                datumIsValid =
                  -- {{{
                  if inputPs < remaining then
                    -- {{{
                    utxosDatumMatchesWith
                      ( DonationAccumulationProgress
                          totPs
                          (psSoFar + inputPs)
                          lovelaces
                          w
                      )
                      o
                    -- }}}
                  else if inputPs == remaining then
                    -- {{{
                    utxosDatumMatchesWith
                      ( DonationAccumulationConcluded
                          totPs
                          lovelaces
                          w
                          False
                      )
                      o
                    -- }}}
                  else
                    -- {{{
                    traceError
                      "Excessive number of prize weight inputs are provided."
                    -- }}}
                  -- }}}
                isValid      =
                  -- {{{
                     utxoHasX qvfSymbol (Just qvfTokenName) o
                  && traceIfFalse
                       "Main UTxO should carry all the donations."
                       (utxoHasLovelaces lovelaces o)
                  && traceIfFalse
                       "Invalid datum attached to the produced main datum."
                       datumIsValid
                  -- }}}
              in
              if isValid then
                acc
              else
                -- Failure in case of missing main authentication asset.
                traceError "Invalid UTxO getting produced at the script."
              -- }}}
          -- }}}
        outputPs =
          -- {{{
          foldr outputFoldFn 0 $ getContinuingOutputs ctx
          -- }}}
      in
      traceIfFalse
        "Improper correspondence between input and output prize weights."
        (inputPs == outputPs)
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   is still in progress.
    --
    --   Raises exception on @False@.
    canRegisterOrDonate :: () -> Bool
    canRegisterOrDonate _ =
      -- {{{
      case getDatumFromRefX qvfSymbol qvfTokenName of
        DeadlineDatum dl ->
          traceIfFalse "This funding round is over." $ deadlineNotReached dl
        _                ->
          traceError "Invalid deadline datum."
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   has ended.
    --
    --   Raises exception on @False@.
    canFoldOrDistribute :: () -> Bool
    canFoldOrDistribute _ =
      -- {{{
      case getDatumFromRefX qvfSymbol qvfTokenName of
        DeadlineDatum dl ->
          traceIfFalse "This funding round is still in progress." $ deadlineReached dl
        _                ->
          traceError "Invalid deadline datum."
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
      any
        (\(sym', tn', amt') -> sym' == sym && tn' == tn && amt' == amt)
        (flattenValue $ txInfoMint info)
      -- }}}

    projectMintIsPresent :: Bool -> Bool
    projectMintIsPresent mint =
      let
        tn = getCurrTokenName qvfProjectSymbol
      in
      traceIfFalse
        "There should be exactly 2 project assets minted/burnt."
        (mintIsPresent qvfProjectSymbol tn $ if mint then 2 else negate 2)

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
               "Invalid Lovelace count at output."
               ( case mExpectedLovelaces of
                   Just outputLovelaces ->
                     utxoHasLovelaces outputLovelaces o
                   Nothing              ->
                     True
               )
          && traceIfFalse
               "Invalid output datum."
               ( case mExpectedDatum of
                   Just updatedDatum ->
                     utxosDatumMatchesWith updatedDatum o
                   Nothing           ->
                     True
               )
          && traceIfFalse
               "Unauthentic output UTxO."
               ( case mExpectedAsset of
                   Just (sym, tn) ->
                     utxoHasX sym (Just tn) o
                   Nothing        ->
                     True
               )
          -- }}}
        _   ->
          -- {{{
          traceError
            "There should be exactly 1 UTxO going back to the script."
          -- }}}
      -- }}}
  in
  case (datum, action) of
    (DeadlineDatum _                              , UpdateDeadline newDl   ) ->
      -- {{{
         signedByKeyHolder ()
      && traceIfFalse
           "New deadline has already passed."
           (deadlineReached newDl)
      && traceIfFalse
           "Missing authentication asset."
           (currUTxOHasX qvfSymbol qvfTokenName)
      && validateSingleOutput
           Nothing
           (Just $ DeadlineDatum newDl)
           (Just (qvfSymbol, qvfTokenName))
      -- }}}

    (RegisteredProjectsCount _                    , RegisterProject        ) ->
      -- Project Registration
      -- {{{
      projectMintIsPresent True && canRegisterOrDonate ()
      -- }}}

    (ReceivedDonationsCount _                     , DonateToProject donInfo) ->
      -- Project Donation
      -- {{{
      let
        tn = TokenName $ diProjectId donInfo
      in
         traceIfFalse
           "There should be exactly 1 donation asset minted."
           (mintIsPresent qvfDonationSymbol tn 1)
      && canRegisterOrDonate ()
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
        traceIfFalse "The project UTxO must also be getting consumed."
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
             "All donation assets must be burnt."
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute ()
        && signedByKeyHolder ()
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
             "All donation assets must be burnt."
             (mintIsPresent qvfDonationSymbol tn (negate tot))
        && canFoldOrDistribute ()
        && signedByKeyHolder ()
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
        traceIfFalse "The concluded folding project UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          DonationFoldingProgress tot soFar -> tot == soFar
          _                                 -> False
      -- }}} 

    (PrizeWeight _ False                          , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- (Delegation of logic to the main UTxO.)
      -- {{{ 
        traceIfFalse "The main UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfSymbol qvfTokenName
      $ \case
          RegisteredProjectsCount _                  -> True
          DonationAccumulationProgress tot soFar _ _ -> tot > soFar
          _                                          -> False
      -- }}} 

    (RegisteredProjectsCount tot                  , AccumulateDonations    ) ->
      -- First Accumulation of Donations
      -- {{{ 
      traversePrizeWeights tot 0 0 0
      -- }}} 

    (DonationAccumulationProgress tot ps ds ws    , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      -- {{{ 
      traversePrizeWeights tot ps ds ws
      -- }}} 

    (DonationAccumulationConcluded ps ds den False, PayKeyHolderFee        ) ->
      -- Key Holder Fee Collection
      -- {{{
      let
        (khFee, updatedDatum) = findDatumAfterPayingKeyHoldersFee ps ds den
        keyHolderImbursed     =
          lovelaceFromValue (valuePaidTo info qvfKeyHolder) == khFee
      in
         xIsPresent qvfSymbol qvfTokenName (negate khFee) updatedDatum
      && traceIfFalse
           "Key holder fees must be paid accurately."
           keyHolderImbursed
      -- }}}

    (DonationAccumulationConcluded ps ds den True , DistributePrizes       ) ->
      -- Prize Distribution
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
                        traceError "Bad project info reference provided."
                    -- }}}
                  Nothing ->
                    -- {{{
                    acc
                    -- }}}
            )
            Map.empty
            (txInfoReferenceInputs info)
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
                        paidAmount       =
                          -- {{{
                          lovelaceFromValue (valuePaidTo info pdPubKeyHash)
                          -- }}}
                        prizeIsPaid      = paidAmount == portion
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
                                   "Escrow must carry the excess reward."
                                   ( utxoHasLovelaces
                                       ( max halfOfTheRegistrationFee $
                                             halfOfTheRegistrationFee
                                           + portion
                                           - pdRequested
                                       )
                                       o
                                   )
                              && traceIfFalse
                                   "Escrow's inline datum is invalid."
                                   (utxosDatumMatchesWith (Escrow Map.empty) o)
                              -- }}}
                            Nothing ->
                              -- {{{
                              traceError "Escrow UTxO was not produced."
                              -- }}}
                          -- }}}
                      in
                      if prizeIsPaid && escrowIsProduced then
                        (accP + 1, accPaid + portion)
                      else
                        traceError "Prize not paid."
                      -- }}}
                    _                  ->
                      -- {{{
                      traceError "Bad datum attached to a project UTxO."
                      -- }}}
                  -- }}}
                Nothing                 ->
                  -- {{{
                  traceError
                    "Couldn't find the corresponding input UTxO of a provided reference project UTxO."
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
        traceError "The impossible happened."
      else
        case find (utxoHasX qvfSymbol (Just qvfTokenName)) scriptOutputs of
          Just o  ->
            -- {{{
            let
              currLovelaces      = lovelaceFromValue $ txOutValue currUTxO
              remainingLovelaces = currLovelaces - prizesPaid
            in
               traceIfFalse
                 "Current UTxO is unauthentic."
                 (currUTxOHasX qvfSymbol qvfTokenName)
            && traceIfFalse
                 "Impossible 2.0 happened." -- TODO: Is this necessary?
                 (remainingLovelaces >= governanceLovelaces)
            && traceIfFalse
                 "Governance UTxO is not getting updated properly."
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
                 "Prize Lovelaces are not properly withdrawn."
                 (utxoHasLovelaces remainingLovelaces o)
            -- }}}
          Nothing ->
            -- {{{
            traceError "Missing output governance UTxO."
            -- }}}
      -- }}}

    (PrizeWeight _ True                           , DistributePrizes       ) ->
      -- Prize Distribution
      -- (Delegation of logic to the project's UTxO.)
      -- {{{ 
      let
        tn = getCurrTokenName qvfProjectSymbol
      in
        traceIfFalse "The project UTxO must also be getting consumed."
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
              traceError "Bad reference project datum provided."
          -- }}}
      in
         traceIfFalse
           "Unauthentic escrow UTxO."
           (currUTxOHasX qvfProjectSymbol tn)
      && traceIfFalse
           "Insufficient funds."
           (amt <= available)
      && traceIfFalse
           "Missing project owner's signature."
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
                 "Unauthentic escrow UTxO."
                 (currUTxOHasX qvfProjectSymbol tn)
            && traceIfFalse
                 "The bounty winner must be imbursed."
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
          traceError "Not eligible for bounty withdrawal."
          -- }}}
      -- }}}

    (Escrow beneficiaries                         , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
         traceIfFalse
           "Can not conclude with withstanding beneficiaries."
           (Map.null beneficiaries)
      && projectMintIsPresent False
      -- }}}

    (ProjectInfo _                                , ConcludeProject        ) ->
      -- Project Conclusion and Refund of the Registration Fee
      -- {{{
      projectMintIsPresent False
      -- }}}

    (_                                            , _                      ) ->
      traceError "Invalid transaction."
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

