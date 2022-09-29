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
import qualified PlutusTx
import           PlutusTx.Prelude

import           Datum
import           DonationInfo
import           Utils


-- REDEEMER
-- {{{
data DonationRedeemer
  = DonateToProject DonationInfo
  | FoldDonations

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

    inputs = txInfoInputs info

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
          filterXAndValidateGov
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
          case filter filterVAndValidateP (txInfoOutputs info) of
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
      && outputSAndVArePresent
      -- }}}
    FoldDonations                    ->
      -- {{{
      traceError "TODO."
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
