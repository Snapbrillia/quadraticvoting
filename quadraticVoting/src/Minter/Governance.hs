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


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef
            -> POSIXTime
            -> TokenName
            -> TokenName
            -> Integer
            -> ScriptContext
            -> Bool
mkQVFPolicy oref deadline dlTN tn r ctx =
  -- {{{
  -- For development. TODO: REMOVE.
  if r == 1 then
    True
  else
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
               "Bad deadline token name."
               (dlTN' == dlTN)
          && traceIfFalse
               "Bad main token name."
               (tn' == tn)
          && traceIfFalse
               "Exactly 1 deadline token must be minted."
               (dlAmt' == 1)
          && traceIfFalse
               "Exactly 1 main token must be minted"
               (amt' == 1)
          -- }}}
        _              ->
          -- {{{
          traceError "Exactly 2 type of assets must be minted."
          -- }}}
      -- }}}

    validateTwoOutputs o0 o1 =
      -- {{{
         traceIfFalse
           "Invalid value for the deadline UTxO."
           (utxoHasOnlyXWithLovelaces ownSym dlTN governanceLovelaces o0)
      && traceIfFalse
           "Invalid value for the main UTxO."
           (utxoHasOnlyXWithLovelaces ownSym tn governanceLovelaces o1)
      && ( case (getInlineDatum o0, getInlineDatum o1) of
             (DeadlineDatum dl, RegisteredProjectsCount count) ->
               -- {{{
                  traceIfFalse
                    "Deadline must match with the provided parameter."
                    (dl == deadline)
               && traceIfFalse
                    "Funding round must start with 0 registered projects."
                    (count == 0)
               -- }}}
             _                                                 ->
               -- {{{
               traceError
                 "Either invalid datums produced, or produced in wrong order."
               -- }}}
         )
      -- }}}

    validOutputsPresent :: Bool
    validOutputsPresent =
      -- {{{
      case txInfoOutputs info of
        [o0, o1]    ->
          -- {{{
          validateTwoOutputs o0 o1
          -- }}}
        [_, o0, o1] ->
          -- {{{
          validateTwoOutputs o0 o1
          -- }}}
        _           ->
          -- {{{
          traceError "The 2 minted tokens must be split among 2 UTxOs."
          -- }}}
      -- }}}

    -- validDeadlineOutput :: Bool
    -- validMainOutput     :: Bool
    -- (validDeadlineOutput, validMainOutput) =
    --   -- {{{
    --   let
    --     go []            acc                     = acc
    --     go (utxo : rest) acc@(dlFound, rpcFound) =
    --       if utxoHasX ownSym (Just tn) utxo then
    --         -- {{{
    --         if dlFound then
    --           -- {{{
    --           case getInlineDatum utxo of
    --             RegisteredProjectsCount count ->
    --               -- {{{
    --               ( dlFound
    --               ,    traceIfFalse
    --                      "Funding round must start with 0 registered projects."
    --                      (count == 0)
    --                 && traceIfFalse
    --                      "Governance UTxO must carry the required Lovelaces."
    --                      (utxoHasLovelaces governanceLovelaces utxo)
    --               )
    --               -- }}}
    --             _                             ->
    --               -- {{{
    --               traceError "Invalid datum for the second UTxO."
    --               -- }}}
    --           -- }}}
    --         else
    --           -- {{{
    --           case getInlineDatum utxo of
    --             DeadlineDatum dl ->
    --               -- {{{
    --               let
    --                 cond =
    --                      traceIfFalse
    --                        "Deadline must match with the provided parameter."
    --                        (dl == deadline)
    --                   && traceIfFalse
    --                        "Deadline UTxO must carry the required Lovelaces."
    --                        (utxoHasLovelaces governanceLovelaces utxo)
    --               in
    --               go rest (cond, rpcFound)
    --               -- }}}
    --             _                ->
    --               -- {{{
    --               traceError "Deadline UTxO must be produced first."
    --               -- }}}
    --           -- }}}
    --         -- }}}
    --       else
    --         -- {{{
    --         go rest acc
    --         -- }}}
    --   in
    --   go (txInfoOutputs info) (False, False)
    --   -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && traceIfFalse "Deadline has passed." deadlineIsValid
  && checkMintedAmount
  && validOutputsPresent
  -- }}}


qvfPolicy :: TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy oref deadline =
  -- {{{
  let
    wrap :: (Integer -> ScriptContext -> Bool) -> PSU.V2.UntypedMintingPolicy
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


