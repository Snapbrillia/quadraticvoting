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


module Minter.Donation where

import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap                    as Map
import qualified PlutusTx
import           PlutusTx.Prelude

import           Data.Datum
import           Data.DonationInfo
import           Utils


-- REDEEMER
-- {{{
data DonationRedeemer
  = DonateToProject DonationInfo
  | Consolidate     BuiltinByteString -- ^ Project's identifier
  | Dev

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject ,0)
  , ('Consolidate     ,1)
  , ('Dev             ,20)
  ]
-- }}}


{-# INLINABLE mkDonationPolicy #-}
mkDonationPolicy :: CurrencySymbol
                 -> DonationRedeemer
                 -> ScriptContext
                 -> Bool
mkDonationPolicy sym action ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputs  = txInfoInputs info
    outputs = txInfoOutputs info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in
  case action of
    DonateToProject DonationInfo{..} ->
      -- {{{
      let
        tn :: TokenName
        tn = TokenName diProjectId

        -- Raises exception upon failure.
        inputProjUTxO :: TxOut
        inputProjUTxO = getInputGovernanceUTxOFrom sym tn inputs

        -- | Checks the datum of the input project UTxO, and in case the datum
        --   has a proper constructor, the number of donations received so far
        --   is retrieved.
        --
        --   Raises exception upon failure.
        currDCount :: Integer
        currDCount =
          -- {{{
          case getInlineDatum inputProjUTxO of 
            ReceivedDonationsCount soFar ->
              -- {{{
              soFar
              -- }}}
            _                            ->
              -- {{{
              traceError "Invalid datum for donation count."
              -- }}}
          -- }}}

        outputSAndVAreValid :: TxOut -> TxOut -> Bool
        outputSAndVAreValid s v =
          -- {{{
             traceIfFalse
               "The first UTxO produced at the script address must be the updated project UTxO."
               ( validateGovUTxO
                   (txOutValue inputProjUTxO)
                   (txOutAddress inputProjUTxO)
                   (ReceivedDonationsCount $ currDCount + 1)
                   s
               )
          && traceIfFalse
               "Invalid value for the donation UTxO."
               (utxoHasOnlyXWithLovelaces ownSym tn 1 diAmount v)
          && traceIfFalse
               "Produced donation UTxO must carry donor's public key hash as an inlinde datum."
               (utxosDatumMatchesWith (Donation diDonor) v)
          -- }}}
      in 
         traceIfFalse
           "Donation amount is too small"
           (diAmount >= minDonationAmount)
      && traceIfFalse
           "This project has reached the maximum number of donations."
           (currDCount < maxTotalDonationCount)
      && traceIfFalse
           "Donor's signature is required."
           (txSignedBy info diDonor)
      && ( case outputs of
             [s, v]       ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             [_, s, v]    ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             [_, _, s, v] ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             _            ->
               -- {{{
               traceError
                 "There should be exactly 1 project, and 1 donation UTxOs produced."
               -- }}}
         )
      -- }}}
    Consolidate projectId            ->
      -- {{{
      let
        tn                       :: TokenName
        tn                       = TokenName projectId

        -- Raises exception upon failure.
        inputProjUTxO            :: TxOut
        inputProjUTxO            = getInputGovernanceUTxOFrom sym tn inputs

        projAddr                 :: Address
        projAddr                 = txOutAddress inputProjUTxO

        -- Raises exception upon failure.
        foldVerifiedDonations    :: TxInInfo
                                 -> (Integer, Integer, Integer)
                                 -> (Integer, Integer, Integer)
        foldVerifiedDonations
          TxInInfo{txInInfoResolved = utxo@TxOut{txOutValue = val}}
          acc@(totTs, totLs, ws) =
            -- {{{
            case flattenValue val of
              [(sym', tn', amt'), (_, _, lovelaces)] ->
                -- {{{
                if sym' == ownSym && tn' == tn then
                  -- {{{
                  case getInlineDatum utxo of
                    ValidatedFoldedDonations dsMap ->
                      -- {{{
                      let
                        (toBurn, w) = sumSquareRoots dsMap
                      in
                      -- TODO: Is this redundant?
                      if txOutAddress utxo == projAddr then
                        -- TODO: This one is very likely redundant. Leaving it
                        --       here for better error messages during testing.
                        if amt' == toBurn then
                          (totTs + amt', totLs + lovelaces, ws + w)
                        else
                          traceError "This shouldn't've happened happened."
                      else
                        traceError "The validated UTxO must come from the same address as the project UTxO."
                      -- }}}
                    _                              ->
                      -- {{{
                      traceError "Invalid UTxO."
                      -- }}}
                  -- }}}
                else
                  -- {{{
                  acc
                  -- }}}
                -- }}}
              _                                      ->
                -- {{{
                acc
                -- }}}
            -- }}}

        (ds, ls, inputsSumW)     = foldr foldVerifiedDonations (0, 0, 0) inputs

        -- Raises exception upon failure.
        (updatedDatum, outputLs) =
          -- {{{
          case getInlineDatum inputProjUTxO of
            ReceivedDonationsCount dSoFar                      ->
              -- {{{
              if dSoFar == ds then
                ( PrizeWeight (inputsSumW * inputsSumW) False
                , ls
                )
              else
                traceError "There must be exactly an equal number of input donations for consolidation."
              -- }}}
            DonationFoldingProgress totDs fSoFar assigned last ->
              -- {{{
              if fSoFar == totDs && assigned == last then
                if totDs == ds then
                  ( PrizeWeight (inputsSumW * inputsSumW) False
                  , ls
                  )
                else
                  ( ConsolidationProgress totDs toBurn ls inputsSumW
                  , ls
                  )
              else
                traceError "All donations must be folded for consolidation."
              -- }}}
            ConsolidationProgress totDs cSoFar lSoFar wsSoFar  ->
              -- {{{
              let
                newCSoFar = cSoFar + ds
                finalLs   = lSoFar + ls
                finalSum  = wsSoFar + inputsSumW
              in
              if newCSoFar == totDs then
                -- {{{
                ( PrizeWeight (finalSum * finalSum) False
                , finalLs
                )
                -- }}}
              else
                -- {{{
                ( ConsolidationProgress totDs newCSoFar finalLs finalSum
                , finalLs
                )
                -- }}}
              -- }}}
            _                                                  ->
              -- {{{
              traceError "Invalid datum attached to the project UTxO for consolidation."
              -- }}}
          -- }}}

        -- Raises exception upon failure.
        outputIsValid o          =
          -- {{{
             traceIfFalse
               "Invalid updated value for the project UTxO."
               ( utxoHasOnlyXWithLovelaces
                   sym
                   tn
                   1
                   (outputLs + halfOfTheRegistrationFee)
                   o
               )
          && traceIfFalse
               "Invalid updated value for the project UTxO."
               (utxosDatumMatchesWith updatedDatum o)
          && traceIfFalse
               "Consolidated UTxO must be produced at its originating address."
               (txOutAddress o == projAddr)
          && traceIfFalse
               "All donations must be included in the final folding transaction."
               (ds == requiredDonationCount)
          -- }}}
      in
      case outputs of
        [o]       ->
          -- {{{
          consolidatedOutputIsValid o
          -- }}}
        [_, o]    ->
          -- {{{
          consolidatedOutputIsValid o
          -- }}}
        [_, _, o] ->
          -- {{{
          consolidatedOutputIsValid o
          -- }}}
        _         ->
          -- {{{
          traceError
            "Only one, two, or three outputs are allowed for consolidation."
          -- }}}
      -- }}}
    -- TODO: REMOVE.
    Dev                              ->
      True
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
donationPolicy :: CurrencySymbol -> MintingPolicy
donationPolicy sym =
  -- {{{
  let
    wrap :: (DonationRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap . mkDonationPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
  -- }}}


-- donationSymbol :: CurrencySymbol -> CurrencySymbol
-- donationSymbol = scriptCurrencySymbol . donationPolicy
-- }}}


-- UTILS
-- {{{
{-# INLINABLE sumSquareRoots #-}
sumSquareRoots :: Map PubKeyHash (Integer, Integer) -> (Integer, Integer)
sumSquareRoots dsMap =
  -- {{{
  let
    ds                          = Map.elems dsMap
    foldFn (tCount, ls) (ts, w) = (tCount + ts, takeSqrt ls + w)
  in
  foldr foldFn 0 ds
  -- }}}
-- }}}
