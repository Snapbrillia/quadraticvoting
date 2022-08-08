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


module Genesis where


import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import Utils


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkQVFPolicy oref tn () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->
          -- {{{
          tn' == tn && amt == 1
          -- }}}
        _              ->
          -- {{{
          False
          -- }}}
      -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && traceIfFalse "Exactly one auth token must be minted." checkMintedAmount
  -- }}}


qvfPolicy :: TxOutRef -> Scripts.MintingPolicy
qvfPolicy oref =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkQVFPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode emptyTokenName
  -- }}}


qvfSymbol :: TxOutRef -> CurrencySymbol
qvfSymbol = scriptCurrencySymbol . qvfPolicy
