-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- }}}


-- MODULE
-- {{{
module Minter.Governance where
-- }}}


-- IMPORTS
-- {{{
import           Ledger.Value as Value                ( flattenValue
                                                      , valueOf
                                                      )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Interval            as Interval
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            ( ownCurrencySymbol
                                                      , txSignedBy )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.Datum
import           Utils
-- }}}


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = emptyTokenName


data GovernanceRedeemer
  = Initiate POSIXTime Integer
  | Conclude
  | BurnMultiDons
  | Dev

PlutusTx.makeIsDataIndexed ''GovernanceRedeemer
  [ ('Initiate     , 0 )
  , ('Conclude     , 1 )
  , ('BurnMultiDons, 2 )
  , ('Dev          , 20)
  ]


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: PubKeyHash
            -> TxOutRef
            -> GovernanceRedeemer
            -> ScriptContext
            -> Bool
mkQVFPolicy pkh oref r ctx =
  -- {{{
  let
    info :: TxInfo
    info   = scriptContextTxInfo ctx

    inputs :: [TxInInfo]
    inputs = txInfoInputs info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in
  case r of
    Initiate deadline multiDonCount ->
      -- {{{
      let
        checkMintedAmount :: Bool
        checkMintedAmount =
          -- {{{
          traceIfFalse
            "E000"
            ( (==)
                (valueOf (txInfoMint info) ownSym qvfTokenName
                (2 + multiDonCount)
            )
          -- }}}

        -- | Helper function for going over all the produced multi-don UTxOs.
        validateMultiDonOutput :: TxOut -> Bool
        validateMultiDonOutput o =
          -- {{{
             traceIfFalse "E001" (deadlineOrMultiDonValueIsValid o)
          && traceIfFalse
               "E009"
               (utxosDatumMatchesWith EmptyMultiDonationRecord o)
          -- }}}

        -- | Helper function for validating outputs after pattern matching.
        validateOutputs :: TxOut -> TxOut -> [TxOut] -> Bool
        validateOutputs o0 o1 os =
          -- {{{
             traceIfFalse "E005" (deadlineOrMultiDonValueIsValid o0)
          && traceIfFalse
               "E006"
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   qvfTokenName
                   governanceLovelaces
                   o1
               )
          && traceIfFalse
               "E007"
               (utxosDatumMatchesWith (DeadlineDatum deadline multiDonCount) o0)
          && traceIfFalse
               "E008"
               (utxosDatumMatchesWith (RegisteredProjectsCount 0) o1)
          && all validateMultiDonOutput os
          && traceIfFalse "E004" (length os == multiDonCount)
          && traceIfFalse "E134" (multiDonCount <= maxMultiDonationUTxOCount)
          -- }}}

        -- | Allows change output(s) to be produced at the first index(es) of
        --   the transaction. The rest should be carrying the minted assets
        --   along with proper datums.
        validOutputsPresent :: Bool
        validOutputsPresent =
          -- {{{
          case txInfoOutputs info of
            o0 : o1 : os         -> validateTwoOutputs o0 o1 os
            _ : o0 : o1 : os     -> validateTwoOutputs o0 o1 os
            _ : _ : o0 : o1 : os -> validateTwoOutputs o0 o1 os
            _                    -> traceError "E010"
          -- }}}
      in
         checkMintedAmount
      && validOutputsPresent
      && traceIfFalse "E011" (utxoIsGettingSpent inputs oref)
      && traceIfFalse
           "E012"
           (Interval.to deadline `Interval.contains` txInfoValidRange info)
      -- }}}
    BurnMultiDons                   ->
      -- {{{
      let
        updatedD :: (QVFDatum, Integer)
        (updatedD, toBurn) = burnMultiDonUTxOs inputs

        validateDLOutput :: TxOut -> Bool
        validateDLOutput o =
             traceIfFalse "E139" (deadlineOrMultiDonValueIsValid o)
          && traceIfFalse "E140" (utxosDatumMatchesWith updatedD o)
          && traceIfFalse
               "E143"
               (txInfoMint info == Value.singleton ownSym qvfTokenName toBurn)
      in
      case txInfoOutputs info of
        [o]       -> validateDLOutput o
        [_, o]    -> validateDLOutput o
        [_, _, o] -> validateDLOutput o
        _         -> traceError "E141"
      -- }}}
    Conclude                        ->
      -- {{{
      let
        validateGovAndDL remDist remBurn =
             traceIfFalse "E003" (remDist == 0)
          && traceIfFalse "E142" (remBurn == 0)
          && traceIfFalse
               "E144"
               (txInfoMint info == Value.singleton ownSym qvfTokenName (-2))
      in
      case filter (utxoHasOnlyX ownSym qvfTokenName . txInInfoResolved) inputs of
        [TxInInfo{txInInfoResolved = i0}, TxInInfo{txInInfoResolved = i1}] ->
          -- {{{
          case (getInlineDatum i0, getInlineDatum i1) of
            (DeadlineDatum _ remMultis, DistributionProgress _ remPrizes _) ->
              -- {{{
              validateGovAndDL remPrizes remMultis
              -- }}}
            (DistributionProgress _ remPrizes _, DeadlineDatum _ remMultis) ->
              -- {{{
              validateGovAndDL remPrizes remMultis
              -- }}}
            _                                                               ->
              -- {{{
              traceError "E096"
              -- }}}
          -- }}}
        _                                                                  ->
          -- {{{
          traceError "E095"
          -- }}}
      -- }}}
    -- For development. TODO: REMOVE.
    Dev      ->
      traceIfFalse "E028" $ txSignedBy info pkh
  -- }}}


qvfPolicy :: PubKeyHash -> TxOutRef -> MintingPolicy
qvfPolicy pkh oref =
  -- {{{
  let
    wrap :: (GovernanceRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' -> wrap $ mkQVFPolicy pkh' oref' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
  -- }}}


-- TODO: Commented out as it seems to generate a different symbol compared to
--       the one computed by `Cardano.Api`.
-- qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
-- qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline


-- UTILS
-- {{{
{-# INLINABLE deadlineOrMultiDonValueIsValid #-}
-- | Since the deadline UTxO and all the multi-donation UTxOs must carry the
--   same value, this function helps making this check a bit more convenient.
deadlineOrMultiDonValueIsValid :: TxOut -> Bool
deadlineOrMultiDonValueIsValid o =
  -- {{{
  utxoHasOnlyXWithLovelaces ownSym qvfTokenName deadlineAndMultiDonLovelaces
  -- }}}


{-# INLINABLE burnMultiDonUTxOs #-}
-- | Goes over the inputs, makes sure the deadline has passed, and returns the
--   valid updated datum to be attached to the output deadlien UTxO, and the
--   number of assets that must be burnt.
burnMultiDonUTxOs :: [TxInInfo] -> (QVFDatum, Integer)
burnMultiDonUTxOs inputs =
  -- {{{
  let
    -- | Function to traverse the inputs. Returns the deadline, number of
    --   remaining multi-don UTxOs to be burnt before this transaction, and
    --   the number of provided multi-don UTxOs to be burnt.
    --
    --   Raises exception upon encountering bad inputs or absence of the
    --   deadline input.
    go :: (Maybe (POSIXTime, Integer), Integer)
       -> [TxInInfo]
       -> (POSIXTime, Integer, Integer)
    go acc@(mRem, burnCount) (TxInInfo{txInInfoResolved = o} : is) =
      -- {{{
      if deadlineOrMultiDonValueIsValid o then
        -- {{{
        case getInlineDatum o of
          DeadlineDatum dl rem     ->
            -- {{{
            if Interval.from dl `Interval.contains` txInfoValidRange info then
              findDeadlineAndMultis (Just (dl, rem), burnCount) is
            else
              traceError "E136"
            -- }}}
          EmptyMultiDonationRecord ->
            -- {{{
            findDeadlineAndMultis (mRem, burnCount + 1) is
            -- }}}
          _                        ->
            -- {{{
            traceError "E137"
            -- }}}
        -- }}}
      else
        -- {{{
        findDeadlineAndMultis acc is
        -- }}}
      -- }}}
    go (Just (dl, rem), bC) []                                     =
      -- {{{
      (DeadlineDatum dl (rem - bC), negate bC)
      -- }}}
    go _                    []                                     =
      -- {{{
      traceError "E138"
      -- }}}
  in
  go (Nothing, 0) inputs
  -- }}}
-- }}}
