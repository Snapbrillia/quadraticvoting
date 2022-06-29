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


module NFT where


import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value


nftTokenName :: TokenName
nftTokenName = TokenName emptyByteString


{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    go :: [TxInInfo] -> Bool
    go []       = False
    go (i : is) =
      -- {{{
      if txInInfoOutRef i == oref then
        True
      else
        go is
      -- }}}

    hasUTxO :: Bool
    hasUTxO = go $ txInfoInputs info

    checkMintedNFT :: Bool
    checkMintedNFT =
      case flattenValue (txInfoMint info) of
        [(_, tn, amt)] -> tn == nftTokenName && amt == 1
        _              -> False
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && traceIfFalse "There should be exactly one NFT minted." checkMintedNFT
  -- }}}

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref =
  -- {{{
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
  -- }}}

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
