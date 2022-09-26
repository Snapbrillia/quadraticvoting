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

import Datum
import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import Utils


-- REDEEMER
-- {{{
data DonationInfo = DonationInfo
  { diProjectId :: !BuiltinByteString
  , diDonor     :: !BuiltinByteString
  , diAmount    :: !Integer
  }

PlutusTx.unstableMakeIsData ''DonationInfo


data DonationRedeemer
  = DonateToProject DonationInfo
  | FoldDonations

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject ,0)
  , ('FoldDonations   ,1)
  ]
-- }}}


{-# INLINABLE mkDonationPolicy #-}
mkDonationPolicy :: CurrencySymbol
                 -> DonationRedeemer
                 -> ScriptContext
                 -> Bool
mkDonationPolicy sym action ctx =
  -- {{{
  case action of DonateToProject di ->
    let
    tn = TokenName $ diProjectId donInfo

    info :: TxInfo
    info = scriptContextTxInfo ctx

    currDonationAmount :: Integer
    currDonationAmount =  ReceivedDonationsCount $ getInlineDatum $ txInfoOutputs info 

    utxosDatumMatchesWith :: QVFDatum -> TxOut -> Bool
    utxosDatumMatchesWith newDatum =
      -- {{{
      (newDatum ==) . getInlineDatum
      -- }}}
  
    xIsPresent :: CurrencySymbol -- ^ X's currency symbol
               -> TokenName      -- ^ X's token name
               -> Integer        -- ^ Increase in output's Lovelace count
               -> QVFDatum       -- ^ Updated datum
               -> Bool
    xIsPresent sym tn increaseInLovelace newDatum =
      -- {{{
      case filter (utxoHasX sym $ Just tn) (txInfoOutputs info) of
        [txOut] ->
          -- {{{
          let
            inUTxO        = getXInputUTxO sym tn
            inVal         = txOutValue inUTxO
            outVal        = txOutValue txOut
            desiredOutVal =
              inVal <> lovelaceValueOf increaseInLovelace
          in
             traceIfFalse
               "Authenticated output doesn't have enough Lovelaces"
               (outVal == desiredOutVal)
          && traceIfFalse
               "Invalid datum attached to the authenticated output."
               (utxosDatumMatchesWith newDatum txOut)
          -- }}}
        _       ->
          -- {{{
          traceError "There must be exactly 1 authentication asset produced."
          -- }}}
      -- }}}

    utxoHasX :: CurrencySymbol -> Maybe TokenName -> TxOut -> Bool
    utxoHasX sym mTN utxo =
      -- {{{
        isJust
      $ find
          ( \(sym', tn', amt') ->
                 sym' == sym
              && ( case mTN of
                     Just tn -> tn' == tn
                     Nothing -> True
                 )
              && amt' == 1
          )
      $ flattenValue
      $ txOutValue utxo
      -- }}}

    outputVIsPresent :: Bool
    outputVIsPresent =
      -- {{{
      case filter (utxoHasX sym $ Just tn) (txInfoOutputs info) of
        [o] ->
          -- {{{
              traceIfFalse
                "Produced donation UTxO must carry donor's public key hash as an inlinde datum."
                  ( utxosDatumMatchesWith
                      (Donation % diDonor donInfo)
                      o
                  )
          && traceIfFalse
                "Donation UTxO must carry exactly the same Lovelace count as specified."
                ((lovelaceValueOf (diAmount donInfo)) == (txOutValue o))
          -- }}}
        _        ->
          -- {{{
          traceError "There should be exactly 1 donation UTxO produced."
          -- }}}
      -- }}}

    in 
       traceIfFalse
         "Donation amount is too small"
         (diAmount di >= minDonationAmount)
    && traceIfFalse
         "This project has reached the maximum number of donations."
         (currDonationAmount < maxTotalDonationCount)
    && outputVIsPresent
    && xIsPresent sym tn 0 (ReceivedDonationsCount $ currDonationAmount + 1)

  -- }}}

minDonationAmount :: Integer
minDonationAmount = 2_000_000

maxTotalDonationCount :: Integer
maxTotalDonationCount = 30_000

-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
donationPolicy :: CurrencySymbol -> Scripts.MintingPolicy
donationPolicy sym =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkDonationPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
  -- }}}


donationSymbol :: CurrencySymbol -> CurrencySymbol
donationSymbol = scriptCurrencySymbol . donationPolicy
-- }}}
