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


module Minter.Registration where


import           Ledger                               ( scriptCurrencySymbol )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import           PlutusTx.Prelude

import           Datum
import qualified Minter.Governance                    as Gov
import           RegistrationInfo
import           Utils


-- REDEEMER
-- {{{
data RegistrationRedeemer
  = RegisterProject RegistrationInfo
  | ConcludeAndRefund

PlutusTx.makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject  , 0)
  , ('ConcludeAndRefund, 1)
  ]
-- }}}


-- POLICY SCRIPT
-- {{{
{-# INLINABLE mkRegistrationPolicy #-}
mkRegistrationPolicy :: CurrencySymbol
                     -> TokenName
                     -> RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy sym tn action ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputs = txInfoInputs info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in 
  case action of
    RegisterProject RegistrationInfo{..} ->
      -- {{{
      let
        -- | The resulting token name based on the specified UTxO being spent.
        currTN :: TokenName
        currTN = orefToTokenName riTxOutRef

        inputGovUTxO :: TxOut
        inputGovUTxO =
          -- {{{
          case filter (utxoHasX ownSym (Just tn) . txInInfoResolved) inputs of
            [txIn] ->
              -- {{{
              txInInfoResolved txIn
              -- }}}
            _      ->
              -- {{{
              traceError
                "Expecting exactly one funding round authentication token to be spent."
              -- }}}
          -- }}}

        -- | Looks for the singular authenticated governance input UTxO to find
        --   the origin address of the governance asset, and the number of
        --   projects registrered so far.
        --
        --   Raises exception upon failure.
        originAddr          :: Address
        currentProjectCount :: Integer
        (originAddr, currentProjectCount) =
          -- {{{
          case getInlineDatum inputGovUTxO of 
            RegisteredProjectsCount soFar ->
              -- {{{
              (txOutAddress inputGovUTxO, soFar)
              -- }}}
            _                           ->
              -- {{{
              traceError "Invalid datum for project registration."
              -- }}}
          -- }}}
  
        -- | Raises exception if it finds an `S` token which is getting sent to
        --   a different address than where it's coming from, or doesn't have
        --   a properly updated datum.
        filterPsAndValidateS :: TxOut -> Bool
        filterPsAndValidateS  utxo =
          -- {{{
          -- Is the UTxO carrying a project token?
          if utxoHasX ownSym (Just currTN) utxo then
            -- {{{
            True
            -- }}}
          -- Or is it a UTxO carrying the governance asset?
          else if utxoHasX sym (Just tn) utxo then
            -- {{{
            if txOutAddress utxo == originAddr then
              if utxosDatumMatchesWith (RegisteredProjectsCount $ currentProjectCount + 1) utxo then
                True
              else
                traceError
                  "Output governance UTxO must increment the project count by 1."
            else
              traceError
                "Governance token must be sent back to the same address from which it's getting consumed."
            -- }}}
          else
            -- {{{
            False
            -- }}}
          -- }}}

        -- | Validates the presence of 2 output project UTxOs: one for storing
        --   project's static information, and the other to keep a record of
        --   the number of donations the project will receive.
        --
        --   Output governance UTxO gets validated by the filtering function,
        --   but its presence is expected at the head of the filtered list.
        --
        --   Raises exception on @False@.
        outputSAndPsArePresent :: Bool
        outputSAndPsArePresent =
          -- {{{
          case filter filterPsAndValidateS (txInfoOutputs info) of
            -- TODO: Is it OK to expect a certain order for these outputs?
            --       This is desired to prevent higher transaction fees.
            [_, p0, p1] ->
              -- {{{
                 traceIfFalse
                   "First project output must carry its static info."
                   ( utxosDatumMatchesWith
                       (ProjectInfo riProjectDetails)
                       p0
                   )
              && traceIfFalse
                   "Second project output must carry its record of donations."
                   ( utxosDatumMatchesWith
                       (ReceivedDonationsCount 0)
                       p1
                   )
              && traceIfFalse
                   "Half of the registration fee should be stored in project's reference UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee p0)
              && traceIfFalse
                   "Half of the registration fee should be stored in project's main UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee p1)
              -- }}}
            _           ->
              -- {{{
              traceError
                "There should be exactly 1 governance, and 2 project UTxOs produced."
              -- }}}
        -- }}}
      in
         outputSAndPsArePresent
      && traceIfFalse
           "Specified UTxO must be consumed."
           (utxoIsGettingSpent inputs riTxOutRef)
      -- }}}
    ConcludeAndRefund                    ->
      -- {{{
      traceError "TODO."
      -- }}}
  -- }}}

-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
registrationPolicy :: CurrencySymbol -> MintingPolicy
registrationPolicy sym =
  -- {{{
  let
    wrap :: (RegistrationRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \sym' tn' -> wrap $ mkRegistrationPolicy sym' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
    `PlutusTx.applyCode`
    PlutusTx.liftCode Gov.qvfTokenName
  -- }}}


registrationSymbol :: CurrencySymbol -> CurrencySymbol
registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}
