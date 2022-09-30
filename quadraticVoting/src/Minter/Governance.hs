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
import           Ledger                               ( scriptCurrencySymbol )
import           Ledger.Value as Value                ( flattenValue )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            ( ownCurrencySymbol )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.Datum
import           Utils
-- }}}


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = TokenName "QVF"


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef
            -> POSIXTime
            -> TokenName
            -> ()
            -> ScriptContext
            -> Bool
mkQVFPolicy oref deadline tn () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx

    hasUTxO :: Bool
    hasUTxO = utxoIsGettingSpent (txInfoInputs info) oref

    checkMintedAmount :: Bool
    checkMintedAmount =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->
          -- {{{
          if tn' == tn then
            traceIfFalse "Exactly 2 tokens must be minted" (amt == 2)
          else
            traceError "Bad token name."
          -- }}}
        _              ->
          -- {{{
          traceError "Exactly 1 type of asset must be minted."
          -- }}}
      -- }}}

    validOutputsPresent :: Bool
    validOutputsPresent =
      -- {{{
      case filter (utxoHasX ownSym (Just tn)) (txInfoOutputs info) of
        -- TODO: Is expecting a specific order possible?
        [o0, o1] ->
          -- {{{
          case (getInlineDatum o0, getInlineDatum o1) of
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
          -- }}}
        _        ->
          -- {{{
          traceError "The 2 minted tokens must be split among 2 UTxOs."
          -- }}}
      -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && checkMintedAmount
  && validOutputsPresent
  -- }}}


qvfPolicy :: TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy oref deadline =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' deadline' tn' -> PSU.V2.mkUntypedMintingPolicy $ mkQVFPolicy oref' deadline' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    `PlutusTx.applyCode`
    PlutusTx.liftCode qvfTokenName
  -- }}}


qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline
