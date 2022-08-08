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


module ProjectRegistrant where


import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import qualified Genesis


-- REGISTRANT PARAMETERS 
-- {{{
data RegistrantParams = RegistrantParams
  { rpQVFSymbol     :: !CurrencySymbol
  , rpQVFScript     :: !ValidatorHash
  , rpProjectScript :: !ValidatorHash
  }

PlutusTx.makeLift ''RegistrantParams
-- }}}



{-# INLINABLE mkProjectRegistrant #-}
mkProjectRegistrant :: RegistrantParams -> () -> ScriptContext -> Bool
mkProjectRegistrant RegistrantParams {..} () ctx =
  let
    info                = scriptContextTxInfo ctx
    ownSymbol           = ownCurrencySymbol ctx
  in
  undefined



{-# INLINABLE checkScriptOutput #-}
checkScriptOutput :: FromData a
                  => TxInfo
                  -> CurrencySymbol
                  -> (DatumHash -> Maybe a)
                  -> ValidatorHash
                  -> Bool
checkScriptOutput txInfo currSymbol fromDatumHash scriptValHash =
  case scriptOutputsAt scriptValHash info of
    [(outputDatumHash, outputValue)] ->
      let
        mOutputDatum :: Maybe Integer
        mOutputDatum = do
          Datum d <- findDatum outputDatumHash info
          PlutusTx.fromBuiltinData d
        oneTokenOut =
          case flattenValue outputValue of
            [(symbol, tn, amt)] ->
                 symbol == ownSymbol
              && tn     == emptyTokenName
              && amt    == 1
      in
      undefined
    _                                ->
      traceError
        "There should be exactly one output to the project script address."
