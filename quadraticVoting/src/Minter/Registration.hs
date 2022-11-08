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
  | Dev

PlutusTx.makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject  , 0)
  , ('ConcludeAndRefund, 1)
  , ('Dev              , 10)
  ]
-- }}}


-- POLICY SCRIPT
-- {{{
{-# INLINABLE mkRegistrationPolicy #-}
mkRegistrationPolicy :: PubKeyHash
                     -> CurrencySymbol
                     -> TokenName
                     -> RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy pkh sym tn action ctx =
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
    RegisterProject RegistrationInfo{..} ->
      -- {{{
      let
        -- | The resulting token name based on the specified UTxO being spent.
        currTN :: TokenName
        currTN = orefToTokenName riTxOutRef

        inputGovUTxO :: TxOut
        inputGovUTxO = getInputGovernanceUTxOFrom sym tn inputs

        -- | Looks for the singular authenticated governance input UTxO to find
        --   the origin address of the governance asset, its assets, and the
        --   number of projects registrered so far.
        --
        --   Raises exception upon failure.
        currPCount :: Integer
        currPCount =
          -- {{{
          case getInlineDatum inputGovUTxO of 
            RegisteredProjectsCount soFar ->
              -- {{{
              soFar
              -- }}}
            _                             ->
              -- {{{
              traceError "E013"
              -- }}}
          -- }}}

        -- | Checks if the given UTxOs carry project's assets, and makes sure
        --   the first one has a `ProjectInfo` datum, while the second one has
        --   a `ReceivedDonationsCount` attached. Also expects each of them to
        --   carry exactly half of the registration fee Lovelaces.
        --
        --   It also validates that the produced governance UTxO is sent back
        --   to its origin, and that it has a properly updated datum attached.
        --
        --   Raises exception on @False@.
        outputSAndPsAreValid :: TxOut -> TxOut -> TxOut -> Bool
        outputSAndPsAreValid s p0 p1 =
          -- {{{
             traceIfFalse
               "E014"
               ( validateGovUTxO
                   (txOutValue inputGovUTxO)
                   (txOutAddress inputGovUTxO)
                   (RegisteredProjectsCount $ currPCount + 1)
                   s
               )
          && traceIfFalse
               "E015"
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   p0
               )
          && traceIfFalse
               "E016"
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   p1
               )
          && traceIfFalse
               "E017"
               (utxosDatumMatchesWith (ProjectInfo riProjectDetails) p0)
          && traceIfFalse
               "E018"
               (utxosDatumMatchesWith (ReceivedDonationsCount 0) p1)
          -- }}}
      in
         traceIfFalse
           "E019"
           (utxoIsGettingSpent inputs riTxOutRef)
      && traceIfFalse
           "E020"
           (txSignedBy info $ pdPubKeyHash riProjectDetails)
      && ( case outputs of
             [s, p0, p1]    ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             [_, s, p0, p1] ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             _              ->
               -- {{{
               traceError "E021"
               -- }}}
         )
      -- }}}
    ConcludeAndRefund projectId          ->
      -- {{{
      let
        currTN :: TokenName
        currTN = TokenName projectId

        validateInputs :: TxInInfo -> TxInInfo -> Bool
        validateInputs TxInInfo{txInInfoResolved = p0} TxInInfo{txInInfoResolved = p1} =
          -- {{{
          case getInlineDatum p0 of
            ProjectInfo ProjectDetails{..} ->
              -- {{{
              case getInlineDatum p1 of
                Escrow _                                 ->
                  -- {{{
                     traceIfFalse
                       "E022"
                       ( utxoHasOnlyXWithLovelaces
                           ownSym
                           currTN
                           halfOfTheRegistrationFee
                           p0
                       )
                  && traceIfFalse
                       "E023"
                       ( utxoHasOnlyXWithLovelaces
                           ownSym
                           currTN
                           halfOfTheRegistrationFee
                           p1
                       )
                  && traceIfFalse
                       "E024"
                       (txSignedBy info pdPubKeyHash)
                  -- }}}
                _                                        ->
                  -- {{{
                  traceError "E025"
                  -- }}}
              -- }}}
            _                              ->
              -- {{{
              traceError "E026"
              -- }}}
          -- }}}
      in
      case inputs of
        [i0, i1]    ->
          -- {{{
          validateInputs i0 i1
          -- }}}
        [_, i0, i1] ->
          -- {{{
          validateInputs i0 i1
          -- }}}
        _                                         ->
          -- {{{
          traceError "E027"
          -- }}}
      -- }}}
    -- TODO: REMOVE.
    Dev                                  ->
      traceIfFalse "E028" $ txSignedBy info pkh
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
registrationPolicy :: PubKeyHash -> CurrencySymbol -> MintingPolicy
registrationPolicy pkh sym =
  -- {{{
  let
    wrap :: (RegistrationRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' sym' tn' -> wrap $ mkRegistrationPolicy pkh' sym' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
    `PlutusTx.applyCode`
    PlutusTx.liftCode Gov.qvfTokenName
  -- }}}


-- registrationSymbol :: CurrencySymbol -> CurrencySymbol
-- registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}
