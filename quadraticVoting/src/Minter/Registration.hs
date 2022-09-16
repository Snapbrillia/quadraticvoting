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


module Minter.Registration where


import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import           Utils
import qualified Minter.NFT           as NFT


-- REDEEMER
-- {{{
data RegistrationInfo = RegistrationInfo
  { riTxOutRef   :: !TxOutRef
  , riPubKeyHash :: !BuiltinByteString
  , riLabel      :: !BuiltinByteString
  , riRequested  :: !Integer
  }

PlutusTx.unstableMakeIsData ''RegistrationInfo


data RegistrationRedeemer
  = RegisterProject RegistrationInfo
  | DistributePrize BuiltinByteString -- ProjectID : hashTxOutRef (riTxOutRef RegistrationInfo)

PlutusTx.makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject, 0)
  , ('DistributePrize, 1)
  ]
-- }}}


-- POLICY SCRIPT
-- {{{
{-# INLINABLE mkRegistrationPolicy #-}
mkRegistrationPolicy :: CurrencySymbol
                     -> TokenName
                     -> RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy sym tn action ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasInUTxO :: Bool
    hasInUTxO = any (\i -> isSame (txOutValue $ txInfoResolved i)) $ txInfoInputs info

    hasOutUTxO :: Bool
    hasOutUTxO = any (\i -> isSame (txOutValue i)) $ txInfoOutputs info

    isSame :: Value -> Bool
    isSame value = case flattenValue value of
        [_,(symY', tnY', amtY)] -> symY' == sym && tnY' == tn && amtY == 1
        _                       -> False

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx

  in
  case action of
    RegisterProject ri ->
      let
        signedByRegistrator :: Bool
        signedByRegistrator = txSignedBy info $ riPubKeyHash ri

        checkTokenName :: TokenName ->  Bool
        checkTokenName tn = unTokenName tn  == hashTxOutRef (riTxOutRef ri)

        tnIsCorrect :: Bool
        tnIsCorrect = case flattenValue (txInfoMint info) of
            [(sym', tn', amt)] -> ownSym == sym' && checkTokenName tn' && amt  == 1 -- ownSym check may be redundant
            _               -> False


        hasRed :: Bool
        hasRed = any (\i -> hasPTokenandFees (txOutValue i)) $ txInfoOutputs info
        -- Check red UTXO is present in Output, and that it has P token
        -- and  min 2 Ada --ref to constant in Utilities Module  is available 

        hasPTokenandFees :: Value -> Bool
        hasPTokenandFees value = case flattenValue value of
            [(sym', tn', amt),(symY', tnY', amtY)] -> amt  == minLovelace ||
                                                      symY' == ownSym && checkTokenName tnY' && amtY == 1
            _               -> False

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == riTxOutRef ri) $ txInfoInputs info


        {- ---------------------------------------- -}

        -- Go through list of inputs and find "yellow" utxo;
        -- The yellow one is S (in readme file)
        -- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
        -- Plutus Tx version of find.
        -- Predicate logic involves finding the "yellow"

      in
        traceIfFalse
          "Registrator's signature missing"
          signedByRegistrator                         &&
        traceIfFalse
          "Token name is not hash of specified UTXO"
          tnIsCorrect                                 &&
        traceIfFalse
          "Not all tokens present"
          (hasInUTxO && hasOutUTxO)                   &&
        traceIfFalse
          "Missing fees or incorrect token"
          hasRed                                      &&
        traceIfFalse
          "Utxo does not match redeemer"
          hasUTxO

    DistributePrize  projectID  ->
      let
          checkTokenName :: TokenName ->  Bool
          checkTokenName tn = unTokenName tn  == projectID

          tokenIsCorrect :: Bool
          tokenIsCorrect = case flattenValue (txInfoMint info) of
              [(sym', tn', amt)] -> ownSym == sym' && checkTokenName tn' && amt  == (-1) -- ownSym check may be redundant
              _               -> False
      in
          traceIfFalse
            "Not all tokens present"
            (hasInUTxO && hasOutUTxO)         &&
          traceIfFalse
            "Missing fees or incorrect token"
            tokenIsCorrect


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
registrationPolicy :: CurrencySymbol -> Scripts.MintingPolicy
registrationPolicy sym =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \sym' tn' -> Scripts.wrapMintingPolicy $ mkRegistrationPolicy sym' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
    `PlutusTx.applyCode`
    PlutusTx.liftCode NFT.qvfTokenName
  -- }}}


registrationSymbol :: CurrencySymbol -> CurrencySymbol
registrationSymbol = scriptCurrencySymbol . registrationPolicy
-- }}}
-- }}}
