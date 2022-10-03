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

import           Data.Datum
import           Data.RegistrationInfo
import qualified Minter.Governance                    as Gov
import           Utils


-- REDEEMER
-- {{{
data RegistrationRedeemer
  = RegisterProject RegistrationInfo
  | ConcludeAndRefund BuiltinByteString

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
        inputGovUTxO = getInputGovernanceUTxOFrom sym tn inputs

        -- | Looks for the singular authenticated governance input UTxO to find
        --   the origin address of the governance asset, and the number of
        --   projects registrered so far.
        --
        --   Raises exception upon failure.
        originAddr :: Address
        currPCount :: Integer
        (originAddr, currPCount) =
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

        -- | Raises exception upon failure (full description at the `Utils`
        --   module).
        filterPsAndValidateS :: TxOut -> Bool
        filterPsAndValidateS =
          -- {{{
          filterXAndValidateGov
            ownSym
            currTN
            sym
            tn
            originAddr
            (RegisteredProjectsCount $ currPCount + 1)
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
      && traceIfFalse
           "Project owner's signature is required."
           (txSignedBy info $ pdPubKeyHash riProjectDetails)
      -- }}}
    ConcludeAndRefund projectId          ->
      -- {{{
      let
        currTN :: TokenName
        currTN = TokenName projectId

        inputPsAreValid =
          case filter (utxoHasX ownSym (Just currTN) . txInInfoResolved) inputs of
            [TxInInfo{txInInfoResolved = p0}, TxInInfo{txInInfoResolved = p1}] ->
              -- {{{
              case getInlineDatum p0 of
                ProjectInfo ProjectDetails{..} ->
                  -- {{{
                  case getInlineDatum p1 of
                    Escrow _                                 ->
                      -- {{{
                         traceIfFalse
                           "Escrow must be depleted before refunding the registration fee."
                           ( lovelaceFromValue (txOutValue p1)
                             == halfOfTheRegistrationFee
                           )
                      && traceIfFalse
                           "Transaction must be signed by the project owner."
                           (txSignedBy info pdPubKeyHash)
                      -- }}}
                    _                                        ->
                      -- {{{
                      traceError "Invalid datum for the second project input."
                      -- }}}
                  -- }}}
                _                              ->
                  -- {{{
                  traceError "First project input must be the info UTxO."
                  -- }}}
              -- }}}
            _        ->
              -- {{{
              traceError "Exactly 2 project inputs are expected."
              -- }}}
      in
      inputPsAreValid
      -- traceError "TODO."
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
