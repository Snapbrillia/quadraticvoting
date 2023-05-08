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
import qualified Data.Redeemer.QVF                    as QVF
import           Data.Redeemer.Registration           ( RegistrationRedeemer(..) )
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

    inputs      = txInfoInputs info
    outputs     = txInfoOutputs info
    txRedeemers = txInfoRedeemers info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx
  in 
  case action of
    RegisterProject govRef pd                                         ->
      -- {{{
      let
        inputGovUTxO@TxOut{txOutAddress = govAddr, txOutValue = govVal} =
          getGovernanceUTxOFrom govRef sym Gov.qvfTokenName inputs

        -- | Using @govRef@, this logic looks up the corresponding redeemer
        --   to make sure the right endpoint of the main spending script is
        --   being invoked.
        --
        --   Raises exception on @False@.
        qvfRedeemerIsValid :: Bool
        qvfRedeemerIsValid = 
          -- {{{
          case getRedeemerOf @QVF.QVFRedeemer (Spending govRef) txRedeemers of
            Just QVF.RegisterProject -> True
            _                        -> traceError "E036"
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
      && qvfRedeemerIsValid
      -- }}}
    RemoveAndRefund govRef infoRef projRef projectID govProducedFirst ->
      -- {{{
      let
        currTN       = TokenName projectID
        foundTriplet =
          findMap3
            (resolveIfRefEquals govRef)
            (resolveIfRefEquals infoRef)
            (resolveIfRefEquals projRef)
            inputs
      in
      case foundTriplet of
        (Just govUTxO, Just infoUTxO, Just projUTxO) ->
          -- {{{
          let
            cond =
                 utxoHasOnlyX sym qvfTokenName govUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   infoUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   projUTxO

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
              && traceIfFalse
                   "TODO"
                   ( case getRedeemerOf @QVF.QVFRedeemer (Spending govRef) txRedeemers of
                       Just QVF.ConcludeProject -> True
                       _                        -> traceError "E027"
                   )
              -- }}}
          in
          if cond then
            -- {{{
            case (getInlineDatum infoUTxO, getInlineDatum projUTxO) of
              (ProjectInfo pd, ProjectDonations Nothing) ->
                -- {{{
                case outputs of
                  -- By allowing only two outputs, this validator forces the
                  -- transaction fee to be covered by the registration fee
                  -- itself.
                  [utxo0, utxo1] ->
                    -- {{{
                    let
                      (TxOut{txOutAddress = ownerAddr, txOutValue}, s) =
                        if govProducedFirst then
                          (utxo1, utxo0)
                        else
                          (utxo0, utxo1)
                      txFee = lovelaceFromValue (txInfoFee info)
                      outL  = lovelaceFromValue txOutValue
                    in
                       outputSIsValid s
                    && traceIfFalse "E130" (pdAddress == ownerAddr)
                    && traceIfFalse "E131" (outL == registrationFee - txFee)
                    && traceIfFalse "E132" (txFee < maxRemovalTxFee)
                    -- This last validation is put in place to prevent a
                    -- possible attack where the attacker sets the transaction
                    -- fee as high as possible to serve the network at the
                    -- expense of the project owner. @maxRemovalTxFee@ should
                    -- be provided based on the protocol parameters at the time
                    -- of generating the currency symbol of this minter.
                    -- }}}
                  _              ->
                    -- {{{
                    traceError "E099"
                    -- }}}
                -- }}}
              _                                          ->
                traceError "E014"
            -- }}}
          else
            traceError "E015"
          -- }}}
        _                                            ->
          -- {{{
          traceError "E016"
          -- }}}
      -- }}}
    ConcludeAndRefund infoRef projRef projectID                       ->
      -- {{{
      let
        currTN     = TokenName projectID
        foundTuple =
          findMap2
            (resolveIfRefEquals infoRef)
            (resolveIfRefEquals projRef)
            inputs
      in
      case foundTuple of
        (Just infoUTxO, Just projUTxO) ->
          -- {{{
          let
            cond =
                 utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   infoUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   projUTxO
          in
          if cond then
            -- {{{
            case (getInlineDatum infoUTxO, getInlineDatum projUTxO) of
              (ProjectInfo ProjectDetails{..}, Escrow _) ->
                -- {{{
                traceIfFalse
                  "E024"
                  (txSignedBy info $ addressToPubKeyHash pdAddress)
                -- }}}
              _                                          ->
                -- {{{
                traceError "E026"
                -- }}}
            -- }}}
          else
            traceError "E017"
          -- }}}
        _                              ->
          traceError "E018"
      -- }}}
    -- TODO: REMOVE.
    Dev                                                               ->
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
