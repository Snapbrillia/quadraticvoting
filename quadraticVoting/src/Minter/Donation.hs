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
import           Data.ListPlacement
import           Utils


-- REDEEMER
-- {{{
data DonationRedeemer
  = DonateToProject  ListPlacement DonationInfo
    -- ^ Using `ListPlacement` to help the minter decide on the proper
    --   validation logic.
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
    DonateToProject lp di ->
      -- {{{
      let
        expectedOutputs = listPlacementToOutputs projSym ownSym lp di inputs
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


-- | Depending on the given `ListPlacement`, this function traverses the inputs
--   expecting different sets of input UTxOs. If the governance symbol is
--   provided, resolution of a free donation is implied.
--
--   Fully validates the inputs.
{-# INLINABLE listPlacementToOutputs #-}
listPlacementToOutputs :: CurrencySymbol
                       -> CurrencySymbol
                       -> ListPlacement
                       -> DonationInfo
                       -> [TxInInfo]
                       -> [TxOut]
listPlacementToOutputs projSym donSym lp DonationInfo{..} inputs =
  -- {{{
  let
    projTN :: TokenName
    projTN                                 = TokenName diProjectID

    isDon :: TxInInfo -> Maybe TxOut
    isDon TxInInfo{txInInfoResolved = o}   =
      -- {{{
      if utxoHasOnlyX donSym projTN o then
        Just o
      else
        Nothing
      -- }}}

    lookForTwo :: (TxInInfo -> Maybe TxOut)
               -> (TxInInfo -> Maybe TxOut)
               -> (QVFDatum -> QVFDatum -> Ordering) -- ^ A comparison function to help with sorting of the found UTxOs. `EQ` is equivalent to failure.
               -> Maybe (TxOut, QVFDatum, TxOut, QVFDatum)
    lookForTwo lCheck rCheck compareDatums =
      -- {{{
      let
        go (i : is) acc =
          case acc of
            (Nothing, Nothing) -> go is (lCheck i, rCheck i)
            (l      , Nothing) -> go is (l       , rCheck i)
            (Nothing, r      ) -> go is (lCheck i, r       )
            (Just l , Just r ) ->
              -- {{{
              let
                lO = txInInfoResolved l
                rO = txInInfoResolved r
                lD = getInlineDatum lO
                rD = getInlineDatum rO
              in
              case compareDatums lD rD of
                LT -> Just (lO, lD, rO, rD)
                GT -> Just (rO, rD, lO, lD)
                EQ -> Nothing -- ^ Reverting back to @Nothing@ to signal failure.
              -- }}}
      in
      go inputs (Nothing, Nothing)
      -- }}}

    mintedDonVal :: Value
    mintedDonVal                           =
      makeAuthenticValue diAmount donSym projTN 1

    projForMintedDon :: TxOut -> TxOut
    projForMintedDon p                     =
      p {txOutDatum = qvfDatumToInlineDatum $ ProjectDonations $ Just diDonor}
  in
  case lp of
    First   ->
      -- {{{
      case filter (utxoHasOnlyX projSym projTN . txInInfoResolved) inputs of
        [TxInInfo{txInInfoResolved = projO}] ->
          -- {{{
          case getInlineDatum projO of
            ProjectDonations Nothing ->
              -- {{{
              [ projForMintedDon projO
              , projO
                  { txOutValue = mintedDonVal
                  , txOutDatum =
                      qvfDatumToInlineDatum $ Donation diDonor Nothing
                  }
              ]
              -- }}}
            _                        ->
              -- {{{
              traceError "E134"
              -- }}}
          -- }}}
        _                                    ->
          -- {{{
          traceError "E135"
          -- }}}
      -- }}}
    Prepend ->
      -- {{{
      let
        isProj :: TxInInfo -> Maybe TxOut
        isProj TxInInfo{txInInfoResolved = o} =
          -- {{{
          if utxoHasOnlyX projSym projTN o then
            Just o
          else
            Nothing
          -- }}}

        compProjAndDon :: QVFDatum -> QVFDatum -> Ordering
        -- {{{
        compProjAndDon (ProjectDonations (Just pkh)) (Donation pkh' _) =
          if pkh == pkh' && diDonor < pkh then LT else EQ
        compProjAndDon _                             _                 = EQ
        -- }}}
      in
      case lookForTwo isProj isDon compProjAndDon of
        Just (projO, _, donO, Donation pkh) ->
          -- {{{
          if diDonor < pkh then
            [ projForMintedDon projO
            , donO
                { txOutValue = mintedDonVal
                , txOutDatum =
                    qvfDatumToInlineDatum $ Donation diDonor (Just pkh)
                }
            , donO
            ]
          else
            traceError "E150"
          -- }}}
        _                                   ->
          -- {{{
          traceError "E136"
          -- }}}
      -- }}}
    Insert  ->
      -- {{{
      let
        compTwoDons :: QVFDatum -> QVFDatum -> Ordering
        -- {{{
        compTwoDons
          (Donation pkh0  (Just pkh1 ))
          (Donation pkh0' (Just pkh1')) =
          | pkh1  == pkh0' && diDonor > pkh0  && diDonor < pkh0' = LT
          | pkh1' == pkh0  && diDonor > pkh0' && diDonor < pkh0  = GT
          | otherwise                                            = EQ
        compTwoDons
          (Donation pkh0  (Just pkh1 ))
          (Donation pkh0' Nothing     ) =
          if pkh1  == pkh0' && diDonor > pkh0  && diDonor < pkh0' then
            LT
          else
            EQ
        compTwoDons
          (Donation pkh0  Nothing     )
          (Donation pkh0' (Just pkh1')) =
          if pkh1' == pkh0  && diDonor > pkh0' && diDonor < pkh0  then
            GT
          else
            EQ
        compTwoDons _ _                 =
          EQ
        -- }}}
      in
      case lookForTwo isDon isDon compTwoDons of
        Just (prevDonO, Donation pkh0 _, nextDonO, Donation pkh1 _) ->
          -- {{{
          [ prevDonO
              { txOutDatum =
                  qvfDatumToInlineDatum $ Donation pkh0 (Just diDonor)
              }
          , prevDonO
              { txOutValue = mintedDonVal
              , txOutDatum =
                  qvfDatumToInlineDatum $ Donation diDonor (Just pkh1)
              }
          , nextDonO
          ]
          -- }}}
        _                                                           ->
          -- {{{
          traceError "E137"
          -- }}}
      -- }}}
    Append  ->
      -- {{{
      case filter (utxoHasOnlyX donSym projTN . txInInfoResolved) inputs of
        [TxInInfo{txInInfoResolved = lastDonO}] ->
          -- {{{
          case getInlineDatum lastDonO of
            Donation pkh Nothing ->
              -- {{{
              if diDonor > pkh then
                [ lastDonO
                    { txOutDatum =
                        qvfDatumToInlineDatum $ Donation pkh (Just diDonor)
                    }
                , lastDonO
                    { txOutValue = mintedDonVal
                    , txOutDatum =
                        qvfDatumToInlineDatum $ Donation diDonor Nothing
                    }
                ]
              else
                traceError "E138"
              -- }}}
            _                    ->
              -- {{{
              traceError "E139"
              -- }}}
          -- }}}
        _                                       ->
          -- {{{
          traceError "E140"
          -- }}}
      -- }}}
  -- }}}
-- }}}
