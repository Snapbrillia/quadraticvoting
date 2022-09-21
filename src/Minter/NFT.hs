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


module Minter.NFT where


import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Api                 ( TxOutRef(..)
                                                      , ScriptContext(..)
                                                      , TxInfo(..)
                                                      , TxInInfo(..)
                                                      )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                               ( scriptCurrencySymbol
                                                      )
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value

import Utils


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = TokenName "QVF"


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
          if tn' == tn then
            amt == 1
          else
            traceError "Bad token name."
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
  Plutonomy.optimizeUPLC $ PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> PSU.V2.mkUntypedMintingPolicy $ mkQVFPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode qvfTokenName
  -- }}}


qvfSymbol :: TxOutRef -> CurrencySymbol
qvfSymbol = scriptCurrencySymbol . qvfPolicy
