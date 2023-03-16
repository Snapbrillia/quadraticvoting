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
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE NamedFieldPuns        #-}


module Minter.Registration where


import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import           PlutusTx.Prelude

import           Data.Datum
import qualified Minter.Governance                    as Gov
import           Utils


-- REDEEMER
-- {{{
data RegistrationRedeemer
  = RegisterProject   ProjectDetails
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
mkRegistrationPolicy :: PubKeyHash
                     -> CurrencySymbol
                     -> RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy pkh sym action ctx =
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
    RegisterProject pd          ->
      -- {{{
      let
        inputGovUTxO :: TxOut
        inputGovUTxO = getInputGovernanceUTxOFrom sym Gov.qvfTokenName inputs

        govAddr :: Address
        govAddr      = txOutAddress inputGovUTxO

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

        -- | The resulting token name based on the specified UTxO being spent.
        currTN :: TokenName
        currTN = indexToTokenName currPCount

        -- | Checks if the given UTxOs carry project's assets, and makes sure
        --   the first one has a `ProjectInfo` datum, while the second one has
        --   a `ProjectDonations` attached. Also expects each of them to carry
        --   exactly half of the registration fee Lovelaces.
        --
        --   It also validates that the produced governance UTxO is sent back
        --   to its origin, and that it has a properly updated datum attached.
        --
        --   Raises exception on @False@.
        outputSAndPsAreValid :: TxOut -> TxOut -> TxOut -> Bool
        outputSAndPsAreValid s p0 p1 =
          -- {{{
          let
            addr0 = txOutAddress p0
          in
             traceIfFalse
               "E2"
               (pdRequested pd >= minRequestable)
          && traceIfFalse
               "E014"
               ( validateGovUTxO
                   (txOutValue inputGovUTxO)
                   govAddr
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
               (utxosDatumMatchesWith (ProjectInfo pd) p0)
          && traceIfFalse
               "E018"
               (utxosDatumMatchesWith (ProjectDonations Nothing) p1)
          && traceIfFalse "E145" (addr0 == govAddr)
          && traceIfFalse "E146" (addr0 == txOutAddress p1)
          -- }}}
        
        -- | Attempts to extract owner's public key hash from the given
        --   address.
        --
        --   Raises exception if the address is that of a script.
        projOwnerPKH :: PubKeyHash
        projOwnerPKH =
          -- {{{
          case addressToPubKeyHash (pdPubKeyHash pd) of
            Just pkh -> pkh
            Nothing  -> traceError "E147"
          -- }}}
      in
         traceIfFalse
           "E020"
           (txSignedBy info projOwnerPKH)
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
               traceError "E021"
               -- }}}
         )
      -- }}}
    ConcludeAndRefund projectID ->
      -- {{{
      let
        currTN                                  = TokenName projectID
        filterFn TxInInfo{txInInfoResolved = o} =
          -- {{{
          utxoHasOnlyXWithLovelaces ownSym currTN halfOfTheRegistrationFee o
          -- }}}
        validateRemoval    ProjectDetails{..}   =
          -- {{{
          let
            inputGovUTxO :: TxOut
            inputGovUTxO =
              getInputGovernanceUTxOFrom sym Gov.qvfTokenName inputs

            updatedDatum :: QVFDatum
            updatedDatum =
              -- {{{
              case getInlineDatum inputGovUTxO of
                RegisteredProjectsCount ps         ->
                  RegisteredProjectsCount (ps - 1)
                PrizeWeightAccumulation ps elimMap ->
                  PrizeWeightAccumulation (ps - 1) elimMap
                _                                  ->
                  traceError "E097"
              -- }}}

            -- | Checks the output governance UTxO to have unchanged
            --   address and value, and that it also has the proper
            --   datum attached.
            --
            --   Raises exception on @False@.
            outputSIsValid :: TxOut -> Bool
            outputSIsValid s =
              -- {{{
              traceIfFalse
                "E098"
                ( validateGovUTxO
                    (txOutValue inputGovUTxO)
                    (txOutAddress inputGovUTxO)
                    updatedDatum
                    s
                )
              -- }}}
          in
          case outputs of
            [TxOut{txOutAddress = ownerAddr, txOutValue}, s] ->
              -- {{{
              let
                txFee = lovelaceFromValue (txInfoFee info)
                outL  = lovelaceFromValue txOutValue
              in
                 outputSIsValid s
              && traceIfFalse "E130" (pdAddress == ownerAddr)
              && traceIfFalse "E131" (outL == registrationFee - txFee)
              && traceIfFalse "E132" (txFee < 1_700_000)
              -- This last validation is put in place to prevent a
              -- possible attack where the attacker sets the
              -- transaction fee as high as possible to serve the
              -- network at the expense of the project owner.
              --
              -- However, this magic number (1700000) is based on the
              -- protocol paramaters at the time of writing, and
              -- *can* change. TODO.
              -- }}}
            _                                                ->
              -- {{{
              traceError "E099"
              -- }}}
          -- }}}
        validateConclusion ProjectDetails{..}   =
          -- {{{
          traceIfFalse "E024" (txSignedBy info $ addressToPubKeyHash pdAddress)
          -- }}}
      in
      case filter filterFn inputs of
        [TxInInfo{txInInfoResolved = p0}, TxInInfo{txInInfoResolved = p1}] ->
          -- {{{
          case (getInlineDatum p0, getInlineDatum p1) of
            (ProjectInfo pd          , ProjectDonations Nothing) ->
              -- {{{
              validateRemoval pd
              -- }}}
            (ProjectInfo pd          , Escrow _                ) ->
              -- {{{
              validateConclusion pd
              -- }}}
            (ProjectDonations Nothing, ProjectInfo pd          ) ->
              -- {{{
              validateRemoval pd
              -- }}}
            (Escrow _                , ProjectInfo pd          ) ->
              -- {{{
              validateConclusion pd
              -- }}}
            _                                                    ->
              -- {{{
              traceError "E026"
              -- }}}
          -- }}}
        _                                                                  ->
          -- {{{
          traceError "E027"
          -- }}}
      -- }}}
    -- TODO: REMOVE.
    Dev                         ->
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
    $$(PlutusTx.compile [|| \pkh' sym' -> wrap $ mkRegistrationPolicy pkh' sym' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
  -- }}}


-- registrationSymbol :: CurrencySymbol -> CurrencySymbol
-- registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}
