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
  = DonateToProject  Bool DonationInfo
    -- ^ The `Bool` value indicates whether the new donation should go at the
    --   head of the list or not.
  | FoldDonations    BuiltinByteString
    -- ^ Project's identifier (token names).
  | Dev
    -- ^ For development. TODO: Remove.

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject , 0 )
  , ('FoldDonations   , 1 )
  , ('Dev             , 20)
  ]
-- }}}


{-# INLINABLE mkDonationPolicy #-}
mkDonationPolicy :: PubKeyHash
                 -> CurrencySymbol
                 -> DonationRedeemer
                 -> ScriptContext
                 -> Bool
mkDonationPolicy pkh projSym action ctx =
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
    DonateToProject prepend di ->
      -- {{{
      let
        expectedOutputs = donationOutputs projSym ownSym prepend di inputs
      in
         traceIfFalse
           "E033"
           (diAmount >= minDonationAmount)
      && traceIfFalse
           "E035"
           (txSignedBy info diDonor)
      && ( case outputs of
             _ : rest     ->
               -- {{{
               traceIfFalse "E141" $ rest == expectedOutputs
               -- }}}
             _ : _ : rest ->
               -- {{{
               traceIfFalse "E142" $ rest == expectedOutputs
               -- }}}
             _            ->
               -- {{{
               traceIfFalse "E143" $ outputs == expectedOutputs
               -- }}}
         )
      -- }}}
    FoldDonations projectID    ->
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
    Dev                        ->
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


-- | Depending on the given `ListPlacement`, this function traverses the inputs
--   expecting different sets of input UTxOs. If the governance symbol is
--   provided, resolution of a free donation is implied.
--
--   Fully validates the inputs.
{-# INLINABLE listPlacementToOutputs #-}
donationOutputs :: CurrencySymbol
                -> CurrencySymbol
                -> Bool
                -> DonationInfo
                -> [TxInInfo]
                -> [TxOut]
donationOutputs projSym donSym prepend DonationInfo{..} inputs =
  -- {{{
  let
    tn :: TokenName
    tn                     = TokenName diProjectID

    makeDonUTxO :: Address -> Maybe PubKeyHash -> TxOut
    makeDonUTxO addr mNext =
      -- {{{
      TxOut
        { txOutAddress         = addr
        , txOutValue           = makeAuthenticValue diAmount donSym tn 1
        , txOutDatum           = Donation diDonor mNext
        , txOutReferenceScript = Nothing
        }
      -- }}}
  in
  if prepend then
    -- {{{
    case filter (utxoHasOnlyX projSym tn . txInInfoResolved) inputs of
      [TxInInfo{txInInfoResolved = projUTxO}] ->
        -- {{{
        let
          updateProj :: TxOut -> TxOut
          updateProj p =
            p { txOutDatum =
                  qvfDatumToInlineDatum $ ProjectDonations $ Just diDonor
              }
        in
        case getInlineDatum projUTxO of
          ProjectDonations Nothing            ->
            [ updateProj projUTxO
            , makeDonUTxO (txOutAddress projUTxO) Nothing
            ]
          ProjectDonations m@(Just nextDonor) ->
            if diDonor < nextDonor then
              [ updateProj projUTxO
              , makeDonUTxO (txOutAddress projUTxO) m
              ]
            else
              traceError "E150"
          _                                   ->
            traceError "E134"
        -- }}}
      _                                       ->
        -- {{{
        traceError "E135"
        -- }}}
    -- }}}
  else
    -- {{{
    case filter (utxoHasOnlyX donSym tn . txInInfoResolved) inputs of
      [TxInInfo{txInInfoResolved = donUTxO}] ->
        -- {{{
        let
          updateDonWith :: QVFDatum -> TxOut -> TxOut
          updateDonWith newDatum d =
            d {txOutDatum = qvfDatumToInlineDatum newDatum}
        in
        case getInlineDatum donUTxO of
          Donation pkh Nothing            ->
            if diDonor > pkh then
              [ updateDonWith (Donation pkh $ Just diDonor) donUTxO
              , makeDonUTxO (txOutAddress donUTxO) Nothing
              ]
            else
              traceError "E137"
          Donation pkh m@(Just nextDonor) ->
            if diDonor > pkh && diDonor < nextDonor then
              [ updateDonWith (Donation pkh $ Just diDonor) donUTxO
              , makeDonUTxO (txOutAddress donUTxO) m
              ]
            else
              traceError "E138"
          _                               ->
            traceError "E139"
        -- }}}
      _                                      ->
        -- {{{
        traceError "E140"
        -- }}}
    -- }}}
  -- }}}
-- }}}
