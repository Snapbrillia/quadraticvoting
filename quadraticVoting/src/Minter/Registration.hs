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

        -- | Checks if the given UTxOs carry project's assets, and makes sure
        --   the first one has a `ProjectInfo` datum, while the second one has
        --   a `ReceivedDonationsCount` attached. Also expects each of them to
        --   carry exactly half of the registration fee Lovelaces.
        --
        --   Raises exception on @False@.
        outputPsAreValid :: TxOut -> TxOut -> Bool
        outputPsAreValid p0 p1 =
          -- {{{
             traceIfFalse
               "Missing project asset in its static info UTxO."
               (utxoHasX ownSym currTN p0)
          && traceIfFalse
               "Missing project asset in its state UTxO."
               (utxoHasX ownSym currTN p1)
          && traceIfFalse
               "First project output must carry its static info."
               (utxosDatumMatchesWith (ProjectInfo riProjectDetails) p0)
          && traceIfFalse
               "Second project output must carry its record of donations."
               (utxosDatumMatchesWith (ReceivedDonationsCount 0) p1)
          && traceIfFalse
               "Half of the registration fee should be stored in project's reference UTxO."
               (utxoHasLovelaces halfOfTheRegistrationFee p0)
          && traceIfFalse
               "Half of the registration fee should be stored in project's main UTxO."
               (utxoHasLovelaces halfOfTheRegistrationFee p1)
          -- }}}

        -- | Aside from validating the project outputs, it also validates that
        --   the produced governance UTxO is sent back to its origin, and that
        --   it has a properly updated datum attached.
        --
        --   Raises exception on @False@.
        outputSAndPsAreValid :: TxOut -> TxOut -> TxOut -> Bool
        outputSAndPsAreValid s p0 p1 =
          -- {{{
             outputPsAreValid p0 p1
          && traceIfFalse
               "The first UTxO produced at the script address must be the governance UTxO."
               ( validateGovUTxO
                   sym
                   tn
                   originAddr
                   (RegisteredProjectsCount $ currPCount + 1)
                   s
               )
          -- }}}
      in
         traceIfFalse
           "Specified UTxO must be consumed."
           (utxoIsGettingSpent inputs riTxOutRef)
      && traceIfFalse
           "Project owner's signature is required."
           (txSignedBy info $ pdPubKeyHash riProjectDetails)
      && ( case outputs of
             [s, p0, p1]     ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             [ch, s, p0, p1] ->
               -- {{{
               utxoHasOnlyAda' ch && outputSAndPsAreValid s p0 p1
               -- }}}
             _               ->
               -- {{{
               traceError
                 "There should be exactly 1 governance, and 2 project UTxOs produced."
               -- }}}
         )
      -- }}}
    ConcludeAndRefund projectId          ->
      -- {{{
      let
        currTN :: TokenName
        currTN = TokenName projectId

        validateInputs TxInInfo{txInInfoResolved = p0} TxInInfo{txInInfoResolved = p1} =
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
      in
      case inputs of
        [i0, i1]                                  ->
          -- {{{
          validateInputs i0 i1
          -- }}}
        [TxInInfo{txInInfoResolved = ch}, i0, i1] ->
          -- {{{
          utxoHasOnlyAda' ch && validateInputs i0 i1
          -- }}}
        _                                         ->
          -- {{{
          traceError "Exactly 2 project inputs are expected."
          -- }}}
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
