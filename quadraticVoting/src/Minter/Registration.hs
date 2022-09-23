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


import qualified Ledger.Ada                  as Ada
import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import           Datum
import           Utils
import qualified Minter.NFT           as NFT


-- REDEEMER
-- {{{
data RegistrationInfo = RegistrationInfo
  { riTxOutRef                :: !TxOutRef
  , riPubKeyHash              :: !BuiltinByteString
  , riLabel                   :: !BuiltinByteString
  , riRequested               :: !Integer
  , riRegisteredProjectsCount :: !Integer
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
{-
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

-}
{-# INLINABLE mkRegistrationPolicy #-}
mkRegistrationPolicy :: RegistrationRedeemer
                     -> ScriptContext
                     -> Bool
mkRegistrationPolicy action ctx =
    -- (RegisteredProjectsCount soFar                , RegisterProject regInfo) ->
      -- Project Registration
      -- {{{
      RegisterProject regInfo ->

      let

        soFar = riRegisteredProjectsCount regInfo
        tn    = orefToTokenName $ riTxOutRef regInfo

        -- | Raises exception on @False@.
        outputPsArePresent :: Bool
        outputPsArePresent =
          -- {{{
          case filter (utxoHasX qvfProjectSymbol $ Just tn) (getContinuingOutputs ctx) of
            -- TODO: Is it OK to expect a certain order for these outputs?
            --       This is desired to avoid higher transaction fees.
            [o0, o1] ->
              -- {{{
                 traceIfFalse
                   "First continuing output must carry project's static info."
                   ( utxosDatumMatchesWith
                       (ProjectInfo $ registrationInfoToProjectDetials regInfo)
                       o0
                   )
              && traceIfFalse
                   "Second continuing output must carry record of donations."
                   ( utxosDatumMatchesWith
                       (ReceivedDonationsCount 0)
                       o1
                   )
              && traceIfFalse
                   "Half of the registration fee should be stored in project's reference UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee o0)
              && traceIfFalse
                   "Half of the registration fee should be stored in project's main UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee o1)
              -- }}}
            _        ->
              -- {{{
              traceError "There should be exactly 2 project UTxOs produced."
              -- }}}
          -- }}}


          -- | Helper function to see if the given UTxO carries the given amount of
          --   Lovelaces.
          utxoHasLovelaces :: Integer -> TxOut -> Bool
          utxoHasLovelaces lovelaces txOut =
            -- {{{
            txOutValue txOut == Ada.lovelaceValueOf lovelaces
            -- }}}


          -- | Checks if a UTxO carries a specific inline datum.
          --
          --   Raises exception upon failure of getting the inline datum.
          utxosDatumMatchesWith :: QVFDatum -> TxOut -> Bool
          utxosDatumMatchesWith newDatum =
            -- {{{
            (newDatum ==) . getInlineDatum
            -- }}}

          --   Raises exception upon failure.
          getInlineDatum :: TxOut -> QVFDatum
          getInlineDatum utxo =
            -- {{{
            case txOutDatum utxo of
              OutputDatum (Datum d) ->
                d
              _                     ->
                traceError "Bad inline datum."
            -- }}}



          -- | Checks 1 X comes from the script, and 1 X goes back to the script,
          --   with enough added Lovelaces and properly updated datum.
          --
          --   Raises exception on @False@.
          xIsPresent :: CurrencySymbol -- ^ X's currency symbol
                     -> TokenName      -- ^ X's token name
                     -> Integer        -- ^ Increase in output's Lovelace count
                     -> QVFDatum       -- ^ Updated datum
                     -> Bool
          xIsPresent sym tn increaseInLovelace newDatum =
            -- {{{
            case filter (utxoHasX sym $ Just tn) (getContinuingOutputs ctx) of
              [txOut] ->
                -- {{{
                let
                  inUTxO        = getXInputUTxO sym tn
                  inVal         = txOutValue inUTxO
                  outVal        = txOutValue txOut
                  desiredOutVal =
                    inVal <> Ada.lovelaceValueOf increaseInLovelace
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


          -- | Checks if a given UTxO has exactly 1 of asset X.
          utxoHasX :: CurrencySymbol -> Maybe TokenName -> TxOut -> Bool
          utxoHasX sym mTN utxo =
            -- {{{
              txOutValue utxo
            & flattenValue
            & find
                ( \(sym', tn', amt') ->
                       sym' == sym
                    && ( case mTN of
                           Just tn -> tn' == tn
                           Nothing -> True
                       )
                    && amt' == 1
                )
            & isJust
            -- }}}


      in
         xIsPresent
           qvfSymbol
           qvfTokenName
           0
           (RegisteredProjectsCount $ soFar + 1)
      && outputPsArePresent
      -- }}}

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
