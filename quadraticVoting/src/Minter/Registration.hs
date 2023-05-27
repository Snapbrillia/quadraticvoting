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
          case addressToPubKeyHash (pdAddress pd) of
            Just pkh' -> pkh'
            Nothing   -> traceError "E147"
          -- }}}

        currProjectCount :: Integer
        currProjectCount =
          case getInlineDatum inputGovUTxO of
            RegisteredProjectsCount p -> p
            _                         -> traceError "E013"

        projTokenName :: TokenName
        projTokenName = indexToTokenName currProjectCount

        validOutputs :: [TxOut]
        validOutputs =
          registrationOutputs
            ownSym
            projTokenName
            pd
            govAddr
            govVal
            (currProjectCount + 1)

        -- | Filter function to only keep outputs which share the same address
        --   as the input governance UTxO.
        filterFn :: TxOut -> Bool
        filterFn TxOut{txOutAddress = utxoAddr} = utxoAddr == govAddr
      in
         traceIfFalse "E020" (txSignedBy info projOwnerPKH)
      && traceIfFalse "E2"   (pdRequested pd >= minRequestable)
      && ( case filter filterFn outputs of
             outs@[_, _, _] ->
               traceIfFalse "E004" (outs == validOutputs)
             _              ->
               traceError "E021"
         )
      && qvfRedeemerIsValid
      -- }}}
    RemoveAndRefund govRef projRef infoRef projectID govProducedFirst ->
      -- {{{
      let
        currTN       = TokenName projectID
        foundTriplet =
          findMap3
            (resolveIfRefEquals govRef)
            (resolveIfRefEquals projRef)
            (resolveIfRefEquals infoRef)
            inputs
      in
      case foundTriplet of
        (Just govUTxO, Just projUTxO, Just infoUTxO) ->
          -- {{{
          let
            cond =
                 utxoHasOnlyX sym Gov.qvfTokenName govUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   projUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   infoUTxO

            qvfRedeemerIsValid :: Bool
            qvfRedeemerIsValid = 
              -- {{{
              case getRedeemerOf @QVF.QVFRedeemer (Spending govRef) txRedeemers of
                Just (QVF.ConcludeProject govRef') -> govRef == govRef'
                _                                  -> traceError "E059"
              -- }}}

            updatedDatum :: QVFDatum
            updatedDatum =
              -- {{{
              case getInlineDatum govUTxO of
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
                       (txOutValue govUTxO)
                       (txOutAddress govUTxO)
                       updatedDatum
                       s
                   )
              && ( case getRedeemerOf @QVF.QVFRedeemer (Spending govRef) txRedeemers of
                     Just (QVF.ConcludeProject _) -> True
                     _                            -> traceError "E027"
                 )
              -- }}}
          in
          if cond then
            -- {{{
            case (getInlineDatum projUTxO, getInlineDatum infoUTxO) of
              (ProjectDonations Nothing, ProjectInfo pd) ->
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
                    && qvfRedeemerIsValid
                    && traceIfFalse "E130" (pdAddress pd == ownerAddr)
                    && traceIfFalse "E131" (outL == registrationFee - txFee)
                    && traceIfFalse "E132" (txFee < maxRemovalTxFee)
                    -- This last validation is put in place to prevent a
                    -- possible attack where the attacker sets the transaction
                    -- fee as high as possible to serve the network at the
                    -- expense of the project owner. @maxRemovalTxFee@ should
                    -- be provided based on the protocol parameters at the time
                    -- of generating the currency symbol of this minter.
                    --
                    -- TODO: This may not be the best approach as there is no
                    -- validation for correctness of the `maxRemovalTxFee`
                    -- parameter (e.g. this can put the contract at a stand-
                    -- still if it's set too low).
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
    ConcludeAndRefund govRef projRef infoRef projectID                ->
      -- {{{
      let
        currTN     = TokenName projectID
        foundTuple =
          findMap2
            (resolveIfRefEquals projRef)
            (resolveIfRefEquals infoRef)
            inputs
      in
      case foundTuple of
        (Just projUTxO, Just infoUTxO) ->
          -- {{{
          let
            cond =
                 utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   projUTxO
              && utxoHasOnlyXWithLovelaces
                   ownSym
                   currTN
                   halfOfTheRegistrationFee
                   infoUTxO

            qvfRedeemerIsValid :: Bool
            qvfRedeemerIsValid = 
              -- {{{
              case getRedeemerOf @QVF.QVFRedeemer (Spending govRef) txRedeemers of
                Just (QVF.ConcludeProject govRef') -> govRef == govRef'
                _                                  -> traceError "E120"
              -- }}}
          in
          if cond then
            -- {{{
            case (getInlineDatum projUTxO, getInlineDatum infoUTxO) of
              (Escrow _, ProjectInfo ProjectDetails{..}) ->
                -- {{{
                   traceIfFalse
                     "E024"
                     ( case addressToPubKeyHash pdAddress of
                         Just pkh' -> txSignedBy info pkh'
                         _         -> traceError "E081"
                     )
                && qvfRedeemerIsValid
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
                    -> TokenName
                    -> ProjectDetails
                    -> Address
                    -> Value
                    -> Integer
                    -> [TxOut]
registrationOutputs projSym projTN pd qvfAddr govInVal nextProjCount =
  -- {{{
  let
    outputGov =
      TxOut
        { txOutAddress = qvfAddr
        , txOutValue   = govInVal
        , txOutDatum   =
            qvfDatumToInlineDatum $ RegisteredProjectsCount nextProjCount
        , txOutReferenceScript = Nothing
        }
    projVal = makeAuthenticValue halfOfTheRegistrationFee projSym projTN 1
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
