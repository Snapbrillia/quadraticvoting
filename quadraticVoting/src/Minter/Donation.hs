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
import           Plutus.V1.Ledger.Value               ( valueOf )
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap                    as Map
import qualified PlutusTx
import           PlutusTx.List                        ( sortBy )
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


-- | Depending on the prepend flag, this function filters the inputs to find
--   either a single project UTxO or donation UTxO such than the new donation
--   can sit in front of it.
--
--   Fully validates the input.
{-# INLINABLE donationOutputs #-}
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
        , txOutDatum           = qvfDatumToInlineDatum $ Donation diDonor mNext
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


-- | Helper function for sorting donation UTxOs.
{-# INLINABLE compareDonationDatums #-}
compareDonationDatums :: QVFDatum -> QVFDatum -> Ordering
compareDonationDatums (Donation pkh0 _) (Donation pkh1 _) = compare pkh0 pkh1
compareDonationDatums _                 _                 = traceError "E025"


{-# INLINABLE compareDonationUTxOs #-}
compareDonationUTxOs :: TxOut -> TxOut -> Ordering
compareDonationUTxOs o0 o1 =
  compareDonationDatums (getInlineDatum o0) (getInlineDatum o1)


-- | Abstracted function for getting the proper output for the folding endpoint
--   of the donation minter. The second value is the number of donation assets
--   that must be burnt.
--
--   Note that there is no need to validate addresses of the donation UTxOs as
--   it's not possible to mint donations to an address other than the one where
--   its corresponding project UTxO comes from.
--
--   Raises exception if no proper project UTxOs are found in the inputs.
{-# INLINABLE foldingOutputs #-}
foldDonationsOutput :: CurrencySymbol
                    -> CurrencySymbol
                    -> TokenName
                    -> [TxInInfo]
                    -> (TxOut, Integer)
foldDonationsOutput projSym donSym tn inputs =
  -- {{{
  let
    -- | Function for finding and "destructuring" the input project UTxO.
    projPluckFn :: TxInInfo -> Maybe (Address, Value, Integer, PubKeyHash)
    projPluckFn TxInInfo{txInInfoResolved = i@TxOut{..}} =
      -- {{{
      if utxoHasOnlyX projSym tn i then
        case getInlineDatum i of
          ProjectDonations (Just pkh)   ->
            Just (txOutAddress, txOutValue, 0, pkh)
          DonationFoldingProgress w pkh ->
            Just (txOutAddress, txOutValue, w, pkh)
          _                             ->
            Nothing
      else
        Nothing
      -- }}}

    -- | Helper function for constructing the final project UTxO, and donation
    --   asset burn count. The `Value` argument is presumed to have all the
    --   provided input donations accumulated.
    makeOutputAndBurnCount :: Address -> Value -> QVFDatum -> (TxOut, Integer)
    makeProjutAndBurnCount a v d =
      -- {{{
      ( TxOut
          { txOutAddress         = a
          , txOutDatum           = qvfDatumToInlineDatum d
          , txOutReferenceScript = Nothing
          , txOutValue           =
              makeAuthenticValue (lovelaceFromValue v) projSym tn 1
          }
      , valueOf v donSym tn
      )
      -- }}}

    -- | Recursive function for collecting all the provided donation UTxOs,
    --   constructing the proper updated project UTxO, and also returning the
    --   valid burn count for donation assets.
    donsToProjectOutput :: Address
                        -> Value
                        -> Integer
                        -> Maybe PubKeyHash
                        -> [TxInInfo]
                        -> (TxOut, Integer)
    donsToProjectOutput a v w Nothing           _        =
      -- {{{
      makeProjutAndBurnCount a v $ PrizeWeight (w * w) False
      -- }}}
    donsToProjectOutput a v w mP@(Just headPKH) donIns   =
      -- {{{
      let
        pluckFn :: TxInInfo -> Maybe (Value, Maybe PubKeyHash)
        pluckFn TxInInfo{txInInfoResolved = o@TxOut{txOutValue = v}} =
          -- {{{
          case getInlineDatum o of
            Donation pkh mNext ->
              if pkh == headPKH then Just (v, mNext) else Nothing
            _                  ->
              Nothing
          -- }}}
      in
      case pluck pluckFn dons of
        Just ((donVal, mNext), restOfIns) ->
          -- {{{
          donsToProjectOutput
            a
            (v <> donVal) -- ^ Note that all the donation assets are getting accumulated.
            (w + takeSqrt (lovelaceFromValue donVal))
            mNext
            restOfIns
          -- }}}
        Nothing                           ->
          -- {{{
          -- No more proper donation UTxOs provided.
          makeProjutAndBurnCount a v $ DonationFoldingProgress w mP
          -- }}}
      -- }}}
  in
  case pluck projPluckFn inputs of
    Just ((pA, pV, w, hPKH), restOfInputs) ->
      donsToProjectOutput pA pV w (Just hPKH) restOfInputs
    Nothing                                ->
      traceError "E009"
  -- }}}
-- }}}
