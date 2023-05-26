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
import           Ledger.Value as Value                ( valueOf )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Interval            as Interval
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            ( ownCurrencySymbol
                                                      , txSignedBy )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.Datum
import           Data.Redeemer.Governance
import           Utils
-- }}}


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = emptyTokenName


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

    outputs :: [TxOut]
    outputs = txInfoOutputs info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in
  case r of
    Initiate deadline     ->
      -- {{{
      let
        checkMintedAmount :: Bool
        checkMintedAmount =
          -- {{{
          traceIfFalse
            "E000"
            (valueOf (txInfoMint info) ownSym qvfTokenName == 2)
          -- }}}

        -- | Allows change output(s) to be produced at the first index(es) of
        --   the transaction. The rest should be carrying the minted assets
        --   along with proper datums.
        validOutputsPresent :: Bool
        validOutputsPresent =
          -- {{{
          case filter (utxoHasOnlyX ownSym qvfTokenName) outputs of
            [o0, o1] ->
              -- {{{
                 traceIfFalse
                   "E005"
                   ( utxoHasOnlyXWithLovelaces
                       ownSym
                       qvfTokenName
                       deadlineLovelaces
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
              -- }}}
            _        -> traceError "E010"
          -- }}}
      in
         checkMintedAmount
      && validOutputsPresent
      && traceIfFalse "E011" (utxoIsGettingSpent inputs oref)
      && traceIfFalse
           "E012"
           ( Interval.contains
               (Interval.to (deadline - minDeadlineThreshold))
               (txInfoValidRange info)
           )
      -- }}}
    Conclude govRef dlRef ->
      -- {{{
      let
        foundTuple =
          findMap2
            (resolveIfRefEquals govRef)
            (resolveIfRefEquals dlRef)
            inputs
      in
      case foundTuple of
        (Just govUTxO, Just dlUTxO) ->
          -- {{{
          case (getInlineDatum govUTxO, getInlineDatum dlUTxO) of
            (DistributionProgress _ remPrizes _, DeadlineDatum _) ->
              -- {{{
              traceIfFalse "E003" (remPrizes == 0)
              -- }}}
            _                                                     ->
              -- {{{
              traceError "E096"
              -- }}}
          -- }}}
        _                           ->
          -- {{{
          traceError "E095"
          -- }}}
      -- }}}
    -- For development. TODO: REMOVE.
    Dev               ->
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

