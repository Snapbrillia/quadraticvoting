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


module Token where


import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkQVFPolicy oref tn () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkToken :: Bool
    checkToken =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->
          -- {{{
             traceIfFalse "Minted token has invalid name." (tn' == tn)
          && traceIfFalse "Exaclty 1 token must be minted." (amt == 1)
          -- }}}
        _               ->
          -- {{{
          traceError "Exactly 1 asset must be minted."
          -- }}}
      -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && checkToken
  -- }}}


qvfPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
qvfPolicy oref tn =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkQVFPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
  -- }}}


qvfSymbol :: TxOutRef -> TokenName -> CurrencySymbol
qvfSymbol oref tn = scriptCurrencySymbol $ qvfPolicy oref tn
