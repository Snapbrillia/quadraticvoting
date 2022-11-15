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
import           Ledger.Value as Value                ( flattenValue )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Interval            as Interval
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            ( ownCurrencySymbol )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.Datum
import           Utils
-- }}}


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = emptyTokenName


{-# INLINABLE deadlineTokenName #-}
deadlineTokenName :: TokenName
deadlineTokenName = TokenName "D"


data TempRedeemer
  = Validate
  | Dev

PlutusTx.makeIsDataIndexed ''TempRedeemer
  [ ('Validate, 11)
  , ('Dev     , 20)
  ]


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef
            -> POSIXTime
            -> TokenName
            -> TokenName
            -> TempRedeemer
            -> ScriptContext
            -> Bool
mkQVFPolicy oref deadline dlTN tn r ctx =
  -- {{{
  -- For development. TODO: REMOVE.
  case r of
    Dev ->
      True
    _   ->
  ---------------------------------
      let
        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownSym :: CurrencySymbol
        ownSym = ownCurrencySymbol ctx

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
          case flattenValue (txInfoMint info) of
            [(_, dlTN', dlAmt'), (_, tn', amt')] ->
              -- {{{
                 traceIfFalse
                   "E000"
                   (dlTN' == dlTN)
              && traceIfFalse
                   "E001"
                   (tn' == tn)
              && traceIfFalse
                   "E002"
                   (dlAmt' == 1)
              && traceIfFalse
                   "E003"
                   (amt' == 1)
              -- }}}
            _              ->
              -- {{{
              traceError "E004"
              -- }}}
          -- }}}

        validateTwoOutputs o0 o1 =
          -- {{{
             traceIfFalse
               "E005"
               (utxoHasOnlyXWithLovelaces ownSym dlTN governanceLovelaces o0)
          && traceIfFalse
               "E006"
               (utxoHasOnlyXWithLovelaces ownSym tn governanceLovelaces o1)
          && ( case (getInlineDatum o0, getInlineDatum o1) of
                 (DeadlineDatum dl, RegisteredProjectsCount count) ->
                   -- {{{
                      traceIfFalse "E007" (dl == deadline)
                   && traceIfFalse "E008" (count == 0)
                   -- }}}
                 _                                                 ->
                   -- {{{
                   traceError "E009"
                   -- }}}
             )
          -- }}}

        validOutputsPresent :: Bool
        validOutputsPresent =
          -- {{{
          case txInfoOutputs info of
            [o0, o1]       ->
              -- {{{
              validateTwoOutputs o0 o1
              -- }}}
            [_, o0, o1]    ->
              -- {{{
              validateTwoOutputs o0 o1
              -- }}}
            [_, _, o0, o1] ->
              -- {{{
              validateTwoOutputs o0 o1
              -- }}}
            _              ->
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


qvfPolicy :: TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy oref deadline =
  -- {{{
  let
    wrap :: (TempRedeemer -> ScriptContext -> Bool) -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' deadline' dlTN' tn' -> wrap $ mkQVFPolicy oref' deadline' dlTN' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadlineTokenName
    `PlutusTx.applyCode`
    PlutusTx.liftCode qvfTokenName
  -- }}}


-- TODO: Commented out as it seems to generate a different symbol compared to
--       the one computed by `Cardano.Api`.
-- qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
-- qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline


