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
import qualified Plutus.V1.Ledger.Value               as Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap                    as Map
import qualified PlutusTx
import           PlutusTx.Prelude

import           Data.Datum
import           Data.DonationInfo
import           Data.Redeemer.Donation
import           Utils


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
    DonateToProject prepend ref di ->
      -- {{{
      let
        expectedOutputs :: [TxOut]
        expectedOutputs = donationOutputs projSym ownSym prepend ref di inputs
        -- | Since `donationOutputs` can only return 2 outputs, the partial
        --   `head` function has been used cautiously here to pick a "sample"
        --   for the source address.
        scriptAddr :: Address
        scriptAddr = txOutAddress $ head expectedOutputs
      in
         traceIfFalse "E033" (diAmount di >= minDonationAmount)
      && traceIfFalse "E035" (txSignedBy info $ diDonor di)
      && traceIfFalse
           "E127"
           (keepOutputsFrom scriptAddr outputs == expectedOutputs)
      -- }}}
    FoldDonations projectRef       ->
      -- {{{
      let
        (projOutput@TxOut{txOutAddress = projAddr}, burnValue) =
          foldDonationsOutput projSym ownSym projectRef inputs
      in
         traceIfFalse "E022" (txInfoMint info == burnValue)
      && traceIfFalse
           "E023"
           ( case keepOutputsFrom projAddr outputs of
               [o] -> o == projOutput
               _   -> traceError "E034"
           )
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
-- | Depending on the prepend flag, this function filters the inputs to find
--   either a single project UTxO or donation UTxO such than the new donation
--   can sit in front of it.
--
--   Fully validates the input, raises exception if invalid or not found.
{-# INLINABLE donationOutputs #-}
donationOutputs :: CurrencySymbol
                -> CurrencySymbol
                -> Bool
                -> TxOutRef
                -> DonationInfo
                -> [TxInInfo]
                -> [TxOut]
donationOutputs projSym donSym prepend ref DonationInfo{..} inputs =
  -- {{{
  let
    tn :: TokenName
    tn = TokenName diProjectID

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
  case find ((==) ref . txInInfoOutRef) inputs of
    Just (TxInInfo{txInInfoResolved = targetUTxO}) ->
      -- {{{
      if prepend && utxoHasOnlyX projSym tn targetUTxO then
        -- {{{
        let
          updateProj :: TxOut -> TxOut
          updateProj p =
            p { txOutDatum =
                  qvfDatumToInlineDatum $ ProjectDonations $ Just diDonor
              }
        in
        case getInlineDatum targetUTxO of
          ProjectDonations Nothing            ->
            [ updateProj targetUTxO
            , makeDonUTxO (txOutAddress targetUTxO) Nothing
            ]
          ProjectDonations m@(Just nextDonor) ->
            if diDonor < nextDonor then
              [ updateProj targetUTxO
              , makeDonUTxO (txOutAddress targetUTxO) m
              ]
            else
              traceError "E150"
          _                                   ->
            traceError "E134"
        -- }}}
      else if utxoHasOnlyX donSym tn targetUTxO then
        -- {{{
        let
          updateDonWith :: QVFDatum -> TxOut -> TxOut
          updateDonWith newDatum d =
            d {txOutDatum = qvfDatumToInlineDatum newDatum}
        in
        case getInlineDatum targetUTxO of
          Donation pkh Nothing            ->
            if diDonor > pkh then
              [ updateDonWith (Donation pkh $ Just diDonor) targetUTxO
              , makeDonUTxO (txOutAddress targetUTxO) Nothing
              ]
            else
              traceError "E137"
          Donation pkh m@(Just nextDonor) ->
            if diDonor > pkh && diDonor < nextDonor then
              [ updateDonWith (Donation pkh $ Just diDonor) targetUTxO
              , makeDonUTxO (txOutAddress targetUTxO) m
              ]
            else
              traceError "E138"
          _                               ->
            traceError "E139"
        -- }}}
      else
        traceError "E126"
      -- }}}
    _                                              ->
      -- {{{
      traceError "E135"
      -- }}}
  -- }}}


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


-- | Abstracted function for getting the proper output for the folding endpoint
--   of the donation minter. The second value is the number of donation assets
--   that must be burnt.
--
--   Note that there is no need to validate addresses of the donation UTxOs as
--   it's not possible to mint donations to an address other than the one where
--   its corresponding project UTxO comes from.
--
--   Raises exception if no proper project UTxOs are found in the inputs.
{-# INLINABLE foldDonationsOutput #-}
foldDonationsOutput :: CurrencySymbol
                    -> CurrencySymbol
                    -> TxOutRef
                    -> [TxInInfo]
                    -> (TxOut, Value)
foldDonationsOutput projSym donSym projRef inputs =
  -- {{{
  case pluckMap (resolveIfRefEquals projRef) inputs of
    Just (pUTxO@TxOut{txOutAddress = pA, txOutValue = pV}, restOfInputs) ->
      -- {{{
      case getTokenNameOfUTxO projSym pUTxO of
        Just tn ->
          -- {{{
          let
            initW :: Integer
            initPKH :: PubKeyHash
            (initW, initPKH) =
              -- {{{
              case getInlineDatum pUTxO of
                ProjectDonations (Just pkh)   -> (0, pkh)
                DonationFoldingProgress w pkh -> (w, pkh)
                _                             -> traceError "E143"
              -- }}}

            donInputs :: [TxInInfo]
            donInputs = keepInputsFrom pA restOfInputs

            -- | Helper function for constructing the final project UTxO, and
            --   donation asset burn value. The `Value` argument is presumed to
            --   have all the provided input donations accumulated.
            makeOutputAndBurnCount :: Address
                                   -> Value
                                   -> QVFDatum
                                   -> (TxOut, Value)
            makeOutputAndBurnCount a v d =
              -- {{{
              ( TxOut
                  { txOutAddress         = a
                  , txOutDatum           = qvfDatumToInlineDatum d
                  , txOutReferenceScript = Nothing
                  , txOutValue           =
                      makeAuthenticValue (lovelaceFromValue v) projSym tn 1
                  }
              , Value.singleton donSym tn $ Value.valueOf v donSym tn
              )
              -- }}}

            -- | Recursive function for collecting all the provided donation
            --   UTxOs, constructing the proper updated project UTxO, and also
            --   returning the valid burn count for donation assets.
            donsToProjectOutput :: Address
                                -> Value
                                -> Integer
                                -> Maybe PubKeyHash
                                -> [TxInInfo]
                                -> (TxOut, Value)
            donsToProjectOutput a v w Nothing        _      =
              -- {{{
              makeOutputAndBurnCount a v $ PrizeWeight (w * w) False
              -- }}}
            donsToProjectOutput a v w (Just headPKH) donIns =
              -- {{{
              let
                pluckFn :: TxInInfo -> Maybe (Value, Maybe PubKeyHash)
                pluckFn TxInInfo{txInInfoResolved = o@TxOut{txOutValue = v'}} =
                  -- {{{
                  case getInlineDatum o of
                    Donation pkh mNext ->
                      if pkh == headPKH then Just (v', mNext) else Nothing
                    _                  ->
                      Nothing
                  -- }}}
              in
              case pluckMap pluckFn donIns of
                Just ((donVal, mNext), restOfIns) ->
                  -- {{{
                  if valueHasOnlyX donSym tn donVal then
                    donsToProjectOutput
                      a
                      (v <> donVal) -- ^ Note that all the donation assets are getting accumulated.
                      (w + takeSqrt (lovelaceFromValue donVal))
                      mNext
                      restOfIns
                  else
                    traceError "E136"
                  -- }}}
                Nothing                           ->
                  -- {{{
                  -- No more proper donation UTxOs provided.
                  makeOutputAndBurnCount a v $ DonationFoldingProgress w headPKH
                  -- }}}
              -- }}}
          in
          donsToProjectOutput pA pV initW (Just initPKH) donInputs
          -- }}}
        Nothing -> traceError "E144"
      -- }}}
    Nothing                                                              ->
      traceError "E009"
  -- }}}
-- }}}
