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
import           Data.Redeemers                       ( RegistrationRedeemer(..) )
import qualified Minter.Governance                    as Gov
import           Utils


-- POLICY SCRIPT
-- {{{
{-# INLINABLE mkRegistrationPolicy #-}
mkRegistrationPolicy :: PubKeyHash
                     -> CurrencySymbol
                     -> Integer
                     -> RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy pkh sym maxRemovalTxFee action ctx =
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
        inputGovUTxO@TxOut{txOutAddress = govAddr, txOutValue = govVal} =
          getInputGovernanceUTxOFrom sym Gov.qvfTokenName inputs
        
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

        currProjectCount :: Integer
        currProjectCount =
          case getInlineDatum inputGovUTxO of
            RegisteredProjectsCount p -> p
            _                         -> traceError "E013"

        validOutputs :: [TxOut]
        validOutputs =
          registrationOutputs ownSym pd govAddr govVal currProjectCount

        -- | Filter function to only keep outputs which share the same address
        --   as the input governance UTxO.
        filterFn :: TxOut -> Bool
        filterFn TxOut{txOutAddress = utxoAddr} = utxoAddr == govAddr
      in
         traceIfFalse
           "E020"
           (txSignedBy info projOwnerPKH)
      && traceIfFalse
           "E2"
           (pdRequested pd >= minRequestable)
      && ( case filter filterFn outputs of
             outs@[_, _, _] ->
               traceIfFalse "E004" (outs == validOutputs)
             _              ->
               traceError "E021"
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

            -- | Checks the output governance UTxO to have unchanged address
            --   and value, and that it also has the proper datum attached.
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
            -- By allowing only two outputs, this validator forces the
            -- transaction fee to be covered by the registration fee itself.
            [TxOut{txOutAddress = ownerAddr, txOutValue}, s] ->
              -- {{{
              let
                txFee = lovelaceFromValue (txInfoFee info)
                outL  = lovelaceFromValue txOutValue
              in
                 outputSIsValid s
              && traceIfFalse "E130" (pdAddress == ownerAddr)
              && traceIfFalse "E131" (outL == registrationFee - txFee)
              && traceIfFalse "E132" (txFee < maxRemovalTxFee)
              -- This last validation is put in place to prevent a possible
              -- attack where the attacker sets the transaction fee as high as
              -- possible to serve the network at the expense of the project
              -- owner. @maxRemovalTxFee@ should be provided based on the
              -- protocol parameters at the time of generating the currency
              -- symbold of this minter.
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
registrationPolicy :: PubKeyHash -> CurrencySymbol -> Integer -> MintingPolicy
registrationPolicy pkh sym maxFee =
  -- {{{
  let
    wrap :: (RegistrationRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' sym' maxFee' -> wrap $ mkRegistrationPolicy pkh' sym' maxFee' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
    `PlutusTx.applyCode`
    PlutusTx.liftCode maxFee
  -- }}}


-- registrationSymbol :: CurrencySymbol -> CurrencySymbol
-- registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}


-- UTILS
-- {{{
{-# INLINABLE registrationOutputs #-}
registrationOutputs :: CurrencySymbol
                    -> ProjectDetails
                    -> Address
                    -> Value
                    -> Integer
                    -> [TxOut]
registrationOutputs projSym pd qvfAddr govInVal currProjCount =
  -- {{{
  let
    outputGov =
      TxOut
        { txOutAddress = qvfAddr
        , txOutValue   = govInVal
        , txOutDatum   =
              qvfDatumToInlineDatum
            $ RegisteredProjectsCount
            $ currProjCount + 1
        , txOutReferenceScript = Nothing
        }
    projVal =
      makeAuthenticValue
        halfOfTheRegistrationFee
        projSym
        (indexToTokenName currProjCount)
        1
  in
  [ outputGov
  , outputGov
      { txOutDatum = qvfDatumToInlineDatum $ ProjectInfo pd
      , txOutValue = projVal
      }
  , outputGov
      { txOutDatum = qvfDatumToInlineDatum $ ProjectDonations Nothing
      , txOutValue = projVal
      }
  ]
  -- }}}
-- }}}
