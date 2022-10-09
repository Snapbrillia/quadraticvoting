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

import           Ledger                               ( scriptCurrencySymbol )
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

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject ,0)
  , ('FoldDonations   ,1)
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

        -- | Looks for the singular project input UTxO to find its origin
        --   address, and the number of donations received so far.
        --
        --   Raises exception upon failure.
        originAddr :: Address
        currDCount :: Integer
        (originAddr, currDCount) =
          -- {{{
          case getInlineDatum inputProjUTxO of 
            ReceivedDonationsCount soFar ->
              -- {{{
              (txOutAddress inputProjUTxO, soFar)
              -- }}}
            _                           ->
              -- {{{
              traceError "Invalid datum for donation count."
              -- }}}
          -- }}}

        -- | Raises exception upon failure (full description at the `Utils`
        --   module).
        filterVAndValidateP :: TxOut -> Bool
        filterVAndValidateP =
          -- {{{
          utxoHasXOrValidGov
            ownSym
            tn
            sym
            tn
            originAddr
            (ReceivedDonationsCount $ currDCount + 1)
          -- }}}
  
        outputSAndVArePresent :: Bool
        outputSAndVArePresent =
          -- {{{
          case filter filterVAndValidateP outputs of
            -- TODO: Verify the enforcement of specific order.
            [_, v] ->
              -- {{{
                 traceIfFalse
                   "Produced donation UTxO must carry donor's public key hash as an inlinde datum."
                   (utxosDatumMatchesWith (Donation diDonor) v)
              && traceIfFalse
                   "Donation UTxO must carry exactly the same Lovelace count as specified."
                   (utxoHasLovelaces diAmount v)
              -- }}}
            _        ->
              -- {{{
              traceError "There should be exactly 1 governance UTxO, and 1 donation UTxO produced."
              -- }}}
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
      && outputSAndVArePresent
      -- }}}
    FoldDonations projectId          ->
      -- {{{
      let
        tn :: TokenName
        tn = TokenName projectId

        -- Raises exception upon failure.
        inputProjUTxO :: TxOut
        inputProjUTxO = getInputGovernanceUTxOFrom sym tn inputs

        originAddr :: Address
        originAddr = txOutAddress inputProjUTxO

        foldDonationsPhaseTwo :: Integer -> Bool
        foldDonationsPhaseTwo requiredDonationCount =
          -- {{{
          let
            (ds, total, finalMap) = foldDonationInputs ownSym tn inputs
            updatedDatum          =
              PrizeWeight (foldDonationsMap finalMap) False
          in
          case filter (validateGovUTxO sym tn originAddr updatedDatum) outputs of
            [o] ->
              -- {{{
                 traceIfFalse
                   "All donations must be included in the final folding transaction."
                   (ds == requiredDonationCount)
              && traceIfFalse
                   "All donations Lovelaces must be included within the project UTxO."
                   (utxoHasLovelaces (total + halfOfTheRegistrationFee) o)
              -- }}}
            _   ->
              -- {{{
              traceError "Missing output project UTxO with prize weight."
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


donationSymbol :: CurrencySymbol -> CurrencySymbol
donationSymbol = scriptCurrencySymbol . donationPolicy
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
