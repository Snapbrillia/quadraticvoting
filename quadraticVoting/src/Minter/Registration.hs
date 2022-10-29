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
  , ('Dev              , 20)
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
              traceError "Invalid datum for project registration."
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
               "The produced governance UTxO must have untouched value."
               ( validateGovUTxO
                   (txOutValue inputGovUTxO)
                   (txOutAddress inputGovUTxO)
                   (RegisteredProjectsCount $ currPCount + 1)
                   s
               )
          && traceIfFalse
               "Invalid value for project's reference UTxO."
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   1
                   halfOfTheRegistrationFee
                   p0
               )
          && traceIfFalse
               "Invalid value for project's main UTxO."
               ( utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   1
                   halfOfTheRegistrationFee
                   p1
               )
          && traceIfFalse
               "First project output must carry its static info."
               (utxosDatumMatchesWith (ProjectInfo riProjectDetails) p0)
          && traceIfFalse
               "Second project output must carry its record of donations."
               (utxosDatumMatchesWith (ReceivedDonationsCount 0) p1)
          -- }}}
      in
         traceIfFalse
           "Specified UTxO must be consumed."
           (utxoIsGettingSpent inputs riTxOutRef)
      && traceIfFalse
           "Project owner's signature is required."
           (txSignedBy info $ pdPubKeyHash riProjectDetails)
      && ( case outputs of
             [s, p0, p1]       ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             [_, s, p0, p1]    ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             [_, _, s, p0, p1] ->
               -- {{{
               outputSAndPsAreValid s p0 p1
               -- }}}
             _                 ->
               -- {{{
               traceError
                 "There should be exactly 1 governance, and 2 project UTxOs produced."
               -- }}}
         )
      -- }}}
    ConcludeAndRefund projectId          ->
      -- {{{
      case inputs of
        TxInInfo{txInInfoResolved = p0} : TxInInfo{txInInfoResolved = p1} : rest ->
          -- {{{
          case getInlineDatum p0 of
            ProjectInfo ProjectDetails{..} ->
              -- {{{
              case getInlineDatum p1 of
                Escrow _                                 ->
                  -- {{{
                  let
                    currTN :: TokenName
                    currTN = TokenName projectId

                    addr0 :: Address
                    addr0 = txOutAddress p0

                    addr1 :: Address
                    addr1 = txOutAddress p1
                  in
                     traceIfFalse
                       "Invalid project info UTxO provided."
                       ( utxoHasOnlyXWithLovelaces
                           ownSym
                           currTN
                           1
                           halfOfTheRegistrationFee
                           p0
                       )
                  && traceIfFalse
                       "Escrow must be depleted before refunding the registration fee."
                       ( utxoHasOnlyXWithLovelaces
                           ownSym
                           currTN
                           1
                           halfOfTheRegistrationFee
                           p1
                       )
                  && traceIfFalse
                       "Transaction must be signed by the project owner."
                       (txSignedBy info pdPubKeyHash)
                  && traceIfFalse
                       "Mismatch of input project UTxO addresses."
                       (addr0 == addr1)
                  && ( if any ((== addr0) . txOutAddress . txInInfoResolved) rest then
                         traceError "No other UTxOs from the contract can be spent."
                       else
                         True
                     )
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
        _                                                                      ->
          -- {{{
          traceError "Exactly 2 project inputs are expected."
          -- }}}
      -- }}}
    -- TODO: REMOVE.
    Dev                                  ->
      True
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


-- registrationSymbol :: CurrencySymbol -> CurrencySymbol
-- registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}
