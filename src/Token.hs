{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Token where


import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value


-- POLICY PARAMETERS
-- {{{
data PolicyParams = PolicyParams
  { ppORef   :: !TxOutRef
  , ppToken  :: !TokenName
  , ppAmount :: !Integer
  }

PlutusTx.makeLift ''PolicyParams
-- }}}


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: PolicyParams -> () -> ScriptContext -> Bool
mkQVFPolicy PolicyParams {..} () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    go :: [TxInInfo] -> Bool
    go []       = False
    go (i : is) =
      -- {{{
      if txInInfoOutRef i == ppORef then
        True
      else
        go is
      -- }}}

    hasUTxO :: Bool
    hasUTxO = go $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn, amt)] ->
          -- {{{
          tn  == ppToken && amt == ppAmount
          -- }}}
        _              ->
          -- {{{
          False
          -- }}}
      -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && traceIfFalse "Amount of minted token is not valid." checkMintedAmount
  -- }}}


qvfPolicy :: PolicyParams -> Scripts.MintingPolicy
qvfPolicy pParams =
  -- {{{
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkQVFPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pParams
  -- }}}


qvfSymbol :: PolicyParams -> CurrencySymbol
qvfSymbol = scriptCurrencySymbol . qvfPolicy
