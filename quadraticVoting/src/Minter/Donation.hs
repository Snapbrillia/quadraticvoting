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
    DonateToProject lp DonationInfo{..} ->
      let
        projTN       = TokenName diProjectID

        -- | Note the inner helper function validates the inputs' datums.
        sortedInputs =
          sortedInputsFromListPlacement projSym ownSym projTN lp diDonor inputs
      in
      case sortedInputs of
        SortedForFirst   projO             ->
        SortedForPrepend projO donO        ->
        SortedForInsert  prevDonO nextDonO ->
        SortedForAppend  lastDonO          ->
        SortedFailed                       ->
          -- {{{
          traceError "E148"
          -- }}}
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


{-# INLINABLE outputsFromListPlacement #-}
outputsFromListPlacement :: CurrencySymbol
                         -> TokenName
                         -> DonationInfo
                         -> ListPlacement
                         -> [TxOut]
outputsFromListPlacement donSym projTN DonationInfo{..} lp =
  let
    mintedDonVal       =
      makeAuthenticValue diAmount donSym projTN 1
    projForMintedDon p =
      p {txOutDatum = qvfDatumToInlineDatum $ ProjectDonations $ Just diDonor}
  in
  case lp of
    SortedForFirst   Nothing projO             ->
      -- {{{
      [ projForMintedDon projO
      , projO
          { txOutValue = mintedDonVal
          , txOutDatum =
              qvfDatumToInlineDatum $ LinkedDonation diDonor Nothing
          }
      ]
      -- }}}
    SortedForPrepend Nothing projO donO        ->
      -- {{{
      case getInlineDatum donO of
        LinkedDonation pkh _ ->
          -- {{{
          if diDonor < pkh then
            [ projForMintedDon projO
            , projO
                { txOutValue = mintedDonVal
                , txOutDatum =
                    qvfDatumToInlineDatum $ LinkedDonation diDonor (Just pkh)
                }
            , donO
            ]
          else
            traceError "E150"
          -- }}}
        _                    ->
          -- {{{
          -- This should never happen. Tweaking the intermediate `SortedInputs`
          -- to prevent this impossible branch (i.e. including the datum of the
          -- sorted UTxO within the datatype) would lead to double sources of
          -- truth. TODO: Is there a better design?
          traceError "E149"
          -- }}}
      -- }}}
    SortedForInsert  Nothing prevDonO nextDonO ->
      -- {{{
      case (getInlineDatum prevDonO, getInlineDatum nextDonO) of
        (LinkedDonation pkh0 _, LinkedDonation pkh1 _) ->
          -- {{{
          if diDonor > pkh0 && diDonor < pkh1 then
            [ prevDonO
                { txOutDatum =
                    qvfDatumToInlineDatum $ LinkedDonation pkh0 (Just diDonor)
                }
            , prevDonO
                { txOutValue = mintedDonVal
                , txOutDatum =
                    qvfDatumToInlineDatum $ LinkedDonation diDonor (Just pkh1)
                }
            , nextDonO
            ]
          else
          -- }}}
        _                                              ->
          -- {{{
          traceError "E151"
          -- }}}
      -- }}}
    SortedForAppend  Nothing lastDonO          ->
      -- {{{
      case getInlineDatum lastDonO of
        LinkedDonation pkh Nothing ->
          -- {{{
          if diDonor > pkh then
            [ lastDonO
                { txOutDatum =
                    qvfDatumToInlineDatum $ LinkedDonation pkh (Just diDonor)
                }
            , lastDonO
                { txOutValue = mintedDonVal
                , txOutDatum =
                    qvfDatumToInlineDatum $ LinkedDonation diDonor Nothing
                }
            ]
          else
            traceError "E152"
          -- }}}
        _                                              ->
          -- {{{
          traceError "E153"
          -- }}}
      -- }}}
    _                                          ->
-- }}}
