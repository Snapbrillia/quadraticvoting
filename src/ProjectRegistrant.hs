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



-- | Checks if exactly one asset is being sent to a specific script address,
--   and also that this output UTxO has a retreivable datum attached. Returns
--   the datum.
{-# INLINABLE checkOutputToScriptAndGetDatum #-}
checkOutputToScriptAndGetDatum :: FromData a
                               => TxInfo
                               -> CurrencySymbol
                               -> TokenName
                               -> (DatumHash -> Maybe a)
                               -> ValidatorHash
                               -> a
checkOutputToScriptAndGetDatum txInfo
                               currencySymbol
                               tokenName
                               fromDatumHash
                               validatorHash =
  -- {{{
  case scriptOutputsAt validatorHash info of
    [(outputDatumHash, outputValue)] ->
      -- {{{
      let
        mOutputDatum = fromDatumHash outputDatumHash
        -- mOutputDatum :: Maybe Integer
        -- mOutputDatum = do
        --   Datum d <- findDatum outputDatumHash info
        --   PlutusTx.fromBuiltinData d
        oneTokenOut =
          -- {{{
          case flattenValue outputValue of
            [(symbol, tn, amt)] ->
              -- {{{
                 symbol == ownSymbol
              && tn     == tokenName
              && amt    == 1
              -- }}}
            _                   ->
              -- {{{
              False
              -- }}}
          -- }}}
      in
      case mOutputDatum of
        Nothing ->
          -- {{{
          traceError "Datum not found."
          -- }}}
        Just d  ->
          -- {{{
          d
          -- }}}
      -- }}}
    _                                ->
      -- {{{
      traceError
        "There should be exactly one output to the project script address."
      -- }}}
  -- }}}
