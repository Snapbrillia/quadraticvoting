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
  = Initiate
  | Conclude
  | Dev

PlutusTx.makeIsDataIndexed ''GovernanceRedeemer
  [ ('Initiate, 0 )
  , ('Conclude, 1 )
  , ('Dev     , 20)
  ]


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: PubKeyHash
            -> TxOutRef
            -> POSIXTime
            -> Integer
            -> GovernanceRedeemer
            -> ScriptContext
            -> Bool
mkQVFPolicy pkh oref multiDonCount deadline r ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in
  case r of
    Initiate ->
      -- {{{
      let
        hasUTxO :: Bool
        hasUTxO =
          -- {{{
          utxoIsGettingSpent (txInfoInputs info) oref
          -- }}}

        deadlineIsValid :: Bool
        deadlineIsValid =
          -- {{{
          Interval.to deadline `Interval.contains` txInfoValidRange info
          -- }}}

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

        validateMultiDonOutput o =
          -- {{{
             traceIfFalse
               "E001"
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   qvfTokenName
                   deadlineAndMultiDonLovelaces
                   o
               )
          && traceIfFalse
               "E009"
               (utxosDatumMatchesWith EmptyMultiDonationRecord o)
          -- }}}

        validateOutputs o0 o1 os =
          -- {{{
             traceIfFalse
               "E005"
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   qvfTokenName
                   deadlineAndMultiDonLovelaces
                   o0
               )
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
               (utxosDatumMatchesWith (DeadlineDatum deadline) o0)
          && traceIfFalse
               "E008"
               (utxosDatumMatchesWith (RegisteredProjectsCount 0) o1)
          && all validateMultiDonOutput os
          && traceIfFalse "E004" (length os == multiDonCount)
          -- }}}

        validOutputsPresent :: Bool
        validOutputsPresent =
          -- {{{
          case txInfoOutputs info of
            o0 : o1 : os         ->
              -- {{{
              validateTwoOutputs o0 o1 os
              -- }}}
            _ : o0 : o1 : os     ->
              -- {{{
              validateTwoOutputs o0 o1 os
              -- }}}
            _ : _ : o0 : o1 : os ->
              -- {{{
              validateTwoOutputs o0 o1 os
              -- }}}
            _                    ->
              -- {{{
              traceError "E010"
              -- }}}
          -- }}}
      in
         traceIfFalse "E011" hasUTxO
      && traceIfFalse "E012" deadlineIsValid
      && checkMintedAmount
      && validOutputsPresent
      -- }}}
    Conclude ->
      -- {{{
      case filter (utxoHasOnlyX ownSym qvfTokenName . txInInfoResolved) (txInfoInputs info) of
        [TxInInfo{txInInfoResolved = i0}, TxInInfo{txInInfoResolved = i1}] ->
          -- {{{
          case (getInlineDatum i0, getInlineDatum i1) of
            (DeadlineDatum _, DistributionProgress _ remaining _) ->
              -- {{{
              traceIfFalse "E003" (remaining == 0)
              -- }}}
            _                                                     ->
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


qvfPolicy :: PubKeyHash -> TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy pkh oref deadline =
  -- {{{
  let
    wrap :: (GovernanceRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' deadline' -> wrap $ mkQVFPolicy pkh' oref' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
  -- }}}


-- TODO: Commented out as it seems to generate a different symbol compared to
--       the one computed by `Cardano.Api`.
-- qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
-- qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline


