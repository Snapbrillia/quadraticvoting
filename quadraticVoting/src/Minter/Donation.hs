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

import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap                    as Map
import qualified PlutusTx
import           PlutusTx.Prelude

import           Data.Datum
import           Data.DonationInfo
import           Utils


-- REDEEMER
-- {{{
data DonationRedeemer
  = DonateToProject DonationInfo
  | FoldDonations   BuiltinByteString -- ^ Project's identifier
  | Dev

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject ,0)
  , ('FoldDonations   ,1)
  , ('Dev             ,20)
  ]
-- }}}


{-# INLINABLE mkDonationPolicy #-}
mkDonationPolicy :: PubKeyHash
                 -> CurrencySymbol
                 -> DonationRedeemer
                 -> ScriptContext
                 -> Bool
mkDonationPolicy pkh sym action ctx =
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
    DonateToProject DonationInfo{..} ->
      -- {{{
      let
        tn :: TokenName
        tn = TokenName diProjectID

        -- Raises exception upon failure.
        inputProjUTxO :: TxOut
        inputProjUTxO = getInputGovernanceUTxOFrom sym tn inputs

        -- | Checks the datum of the input project UTxO, and in case the datum
        --   has a proper constructor, the number of donations received so far
        --   is retrieved.
        --
        --   Raises exception upon failure.
        currDCount :: Integer
        currDCount =
          -- {{{
          case getInlineDatum inputProjUTxO of 
            ReceivedDonationsCount soFar ->
              -- {{{
              soFar
              -- }}}
            _                            ->
              -- {{{
              traceError "E029"
              -- }}}
          -- }}}

        outputSAndVAreValid :: TxOut -> TxOut -> Bool
        outputSAndVAreValid s v =
          -- {{{
             traceIfFalse
               "E030"
               ( validateGovUTxO
                   (txOutValue inputProjUTxO)
                   (txOutAddress inputProjUTxO)
                   (ReceivedDonationsCount $ currDCount + 1)
                   s
               )
          && traceIfFalse
               "E031"
               (utxoHasOnlyXWithLovelaces ownSym tn diAmount v)
          && traceIfFalse
               "E032"
               (utxosDatumMatchesWith (Donation diDonor) v)
          -- }}}
      in 
         traceIfFalse
           "E033"
           (diAmount >= minDonationAmount)
      && traceIfFalse
           "E034"
           (currDCount < maxTotalDonationCount)
      && traceIfFalse
           "E035"
           (txSignedBy info diDonor)
      && ( case outputs of
             [s, v]         ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             [_, s, v]      ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             [_, _, s, v]   ->
               -- {{{
               outputSAndVAreValid s v
               -- }}}
             _              ->
               -- {{{
               traceError "E036"
               -- }}}
         )
      -- }}}
    FoldDonations projectID          ->
      -- {{{
      let
        tn :: TokenName
        tn = TokenName projectID

        -- Raises exception upon failure.
        inputProjUTxO :: TxOut
        inputProjUTxO = getInputGovernanceUTxOFrom sym tn inputs

        foldDonationsPhaseTwo :: Integer -> Bool
        foldDonationsPhaseTwo requiredDonationCount =
          -- {{{
          let
            (ds, total, finalMap) = foldDonationInputs ownSym tn inputs
            updatedDatum          =
              PrizeWeight (foldDonationsMap finalMap) False
            foldedOutputIsValid o =
              -- {{{
                 traceIfFalse
                   "E037"
                   ( utxoHasOnlyXWithLovelaces
                       sym
                       tn
                       (total + halfOfTheRegistrationFee)
                       o
                   )
              && traceIfFalse
                   "E038"
                   (utxosDatumMatchesWith updatedDatum o)
              && traceIfFalse
                   "E039"
                   (txOutAddress o == txOutAddress inputProjUTxO)
              && traceIfFalse
                   "E040"
                   (ds == requiredDonationCount)
              -- }}}
          in
          case outputs of
            [o]       ->
              -- {{{
              foldedOutputIsValid o
              -- }}}
            [_, o]    ->
              -- {{{
              foldedOutputIsValid o
              -- }}}
            [_, _, o] ->
              -- {{{
              foldedOutputIsValid o
              -- }}}
            _         ->
              -- {{{
              traceError "E041"
              -- }}}
          -- }}}
      in
      case getInlineDatum inputProjUTxO of
        ReceivedDonationsCount tot        ->
          -- {{{
          if tot <= maxDonationInputsForPhaseTwo then
            foldDonationsPhaseTwo tot
          else
            traceError "E042"
          -- }}}
        DonationFoldingProgress tot soFar ->
          -- {{{
          if tot == soFar then
            foldDonationsPhaseTwo tot
          else
            traceError "E043"
          -- }}}
        _                                 ->
          -- {{{
          traceError "E044"
          -- }}}
      -- }}}
    -- TODO: REMOVE.
    Dev                              ->
      traceIfFalse "E045" $ txSignedBy info pkh
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
donationPolicy :: PubKeyHash -> CurrencySymbol -> MintingPolicy
donationPolicy pkh sym =
  -- {{{
  let
    wrap :: (DonationRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' sym' -> wrap $ mkDonationPolicy pkh' sym' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
  -- }}}
-- }}}


-- UTILS
-- {{{
{-# INLINABLE sumSquareRoots #-}
sumSquareRoots :: Map PubKeyHash Integer -> Integer
sumSquareRoots dsMap =
  -- {{{
  let
    ds          = Map.elems dsMap
    foldFn ls w = takeSqrt ls + w
  in
  foldr foldFn 0 ds

  -- }}}


{-# INLINABLE foldDonationsMap #-}
-- | Notating Lovelace contributions to each project as \(v\), this is the
--   quadratic formula to represent individual prize weights (\(w_p\)):
--   \[
--       w_p = (\sum{\sqrt{v}})^2
--   \]
foldDonationsMap :: Map PubKeyHash Integer -> Integer
foldDonationsMap dsMap =
  -- {{{
  let
    initW = sumSquareRoots dsMap
  in
  initW * initW
  -- }}}
-- }}}
