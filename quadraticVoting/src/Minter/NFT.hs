-- EXTENSIONS
-- {{{
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
-- }}}


-- MODULE
-- {{{
module Minter.NFT where
-- }}}


-- IMPORTS
-- {{{
import           Ledger                               ( scriptCurrencySymbol )
import Ledger.Value as Value                          ( CurrencySymbol
                                                      , TokenName(TokenName)
                                                      , flattenValue )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Api                 ( MintingPolicy
                                                      , POSIXTime
                                                      , ScriptContext(..)
                                                      , TxInfo(..)
                                                      , TxInInfo(..)
                                                      , TxOutRef(..)
                                                      )
import qualified PlutusTx
import           PlutusTx.Prelude                     ( Bool(False)
                                                      , Eq((==))
                                                      , ($)
                                                      , (&&)
                                                      , any
                                                      , traceError
                                                      , traceIfFalse )
import           Utils
-- }}}


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = TokenName "QVF"


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef -> POSIXTime -> TokenName -> () -> ScriptContext -> Bool
mkQVFPolicy oref deadline tn () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = utxoIsGettingSpent (txInfoInputs info) oref

    checkMintedAmount :: Bool
    checkMintedAmount =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->
          -- {{{
          if tn' == tn then
            amt == 2
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
  && traceIfFalse "Exactly two auth tokens must be minted." checkMintedAmount
  -- }}}


qvfPolicy :: TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy oref deadline =
  -- {{{
  Plutonomy.optimizeUPLC $ PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' deadline' tn' -> PSU.V2.mkUntypedMintingPolicy $ mkQVFPolicy oref' deadline' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    `PlutusTx.applyCode`
    PlutusTx.liftCode qvfTokenName
  -- }}}


qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline
