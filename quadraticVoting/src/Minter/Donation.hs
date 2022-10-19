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
  | FoldDonations   BuiltinByteString -- ^ Project's identifier
  | Dev

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject ,0)
  , ('FoldDonations   ,1)
  , ('Dev             ,10)
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
               (utxoHasOnlyXWithLovelaces ownSym tn diAmount v)
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
             [s, v]    ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             [_, s, v] ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             _              ->
               -- {{{
               traceError
                 "There should be exactly 1 project, and 1 donation UTxOs produced."
               -- }}}
         )
      -- }}}
    FoldDonations projectId          ->
      -- {{{
      let
        tn :: TokenName
        tn = TokenName projectId

        -- Raises exception upon failure.
        inputProjUTxO :: TxOut
        inputProjUTxO = getInputGovernanceUTxOFrom sym tn inputs

        foldDonationsPhaseTwo :: Integer -> Bool
        foldDonationsPhaseTwo requiredDonationCount =
          -- {{{
          let
            (ds, total, finalMap) = foldDonationInputs ownSym tn inputs
            updatedDatum          =
              PrizeWeight (foldDonationsMap finalMap) False
            foldedOutputIsValid o =
              -- {{{
                 traceIfFalse
                   "Invalid updated value for the project UTxO."
                   ( utxoHasOnlyXWithLovelaces
                       sym
                       tn
                       (total + halfOfTheRegistrationFee)
                       o
                   )
              && traceIfFalse
                   "Invalid updated value for the project UTxO."
                   (utxosDatumMatchesWith updatedDatum o)
              && traceIfFalse
                   "Folded UTxO must be produced at its originating address."
                   (txOutAddress o == txOutAddress inputProjUTxO)
              && traceIfFalse
                   "All donations must be included in the final folding transaction."
                   (ds == requiredDonationCount)
              -- }}}
          in
          case outputs of
            [o]    ->
              -- {{{
              foldedOutputIsValid o
              -- }}}
            [_, o] ->
              -- {{{
              foldedOutputIsValid o
              -- }}}
            _      ->
              -- {{{
              traceError "Invalid outputs pattern."
              -- }}}
          -- }}}
      in
      case getInlineDatum inputProjUTxO of
        ReceivedDonationsCount tot        ->
          -- {{{
          if tot <= maxDonationInputsForPhaseTwo then
            foldDonationsPhaseTwo tot
          else
            traceError "Donation count is too large for direct burning."
          -- }}}
        DonationFoldingProgress tot soFar ->
          -- {{{
          if tot == soFar then
            foldDonationsPhaseTwo tot
          else
            traceError "All donation tokens must be folded before burning."
          -- }}}
        _                                 ->
          -- {{{
          traceError
            "Project UTxO must carry the proper datum to allow burning of its donation tokens."
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
{-# INLINABLE foldDonationsMap #-}
-- | Notating Lovelace contributions to each project as \(v\), this is the
--   quadratic formula to represent individual prize weights (\(w_p\)):
--   \[
--       w_p = (\sum{\sqrt{v}})^2
--   \]
foldDonationsMap :: Map PubKeyHash Integer -> Integer
foldDonationsMap dsMap =
  -- {{{
  let
    ds                      = Map.toList dsMap
    foldFn (_, lovelaces) w = takeSqrt lovelaces + w
    initW                   = foldr foldFn 0 ds
  in
  initW * initW
  -- }}}
-- }}}
