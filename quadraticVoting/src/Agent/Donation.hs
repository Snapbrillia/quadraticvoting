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
module Agent.Donation where
-- }}}


-- IMPORTS
-- {{{
import           Ledger.Value as Value                ( flattenValue )
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Interval            as Interval
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            ( ownCurrencySymbol )
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.Datum
import           Utils
-- }}}


data AgentRedeemer
  = Fold     BuiltinByteString -- ^ Project's ID (i.e. token name)
  | Traverse BuiltinByteString -- ^ Project's ID
  | Dev

PlutusTx.makeIsDataIndexed ''AgentRedeemer
  [ ('Fold    , 0)
  , ('Traverse, 1)
  , ('Dev     , 20)
  ]


mkAgent :: CurrencySymbol -> AgentRedeemer -> ScriptContext -> Bool
mkAgent donSym action ctx =
  let
    info    :: TxInfo
    info    = scriptContextTxInfo ctx

    inputs  :: [TxInInfo]
    inputs  = txInfoInputs info

    outputs :: [TxOut]
    outputs = txInfoOutputs info
  in
  case action of
    Fold projID     ->
      -- {{{
      let
        tn                         = TokenName projID
        (mOrigin, ds, tot, donMap) = foldDonationInputs donSym tn inputs
        validateFold foldedUTxO    =
          -- {{{
             traceIfFalse
               "Incorrect output value after folding specified."
               (utxoHasOnlyXWithLovelaces donSym tn ds tot foldedUTxO)
          && traceIfFalse
               "Donations output must carry the folded donations."
               (utxosDatumMatchesWith (Donations donMap) foldedUTxO)
          && traceIfFalse
               "Folded UTxO should be produced at the same address as the incoming donation UTxOs."
               (fromJust mOrigin == txOutAddress foldedUTxO)
               -- ^^^^^^ WARNING! Used with caution.
          -- }}}
      in
      case outputs of
        [o]       ->
          -- {{{
          validateFold o
          -- }}}
        [_, o]    ->
          -- {{{
          validateFold o
          -- }}}
        [_, _, o] ->
          -- {{{
          validateFold o
          -- }}}
        _         ->
          -- {{{
          traceError
            "Folding transaction must produce the folded result as the first output after the change output(s)."
          -- }}}
      -- }}}
    Traverse projID ->
      -- {{{
      let
        tn                                  :: TokenName
        tn                                  = TokenName projID

        validateInputsOutputs               :: TxInInfo
                                            -> TxInInfo
                                            -> TxOut
                                            -> TxOut
                                            -> Bool
        validateInputsOutputs
          TxInInfo{txInInfoResolved = in0@TxOut{txOutAddress = iAddr0}}
          TxInInfo{txInInfoResolved = in1@TxOut{txOutAddress = iAddr1}}
          out0@TxOut{txOutAddress = oAddr0}
          out1@TxOut{txOutAddress = oAddr1} =
            -- {{{
            case (getInlineDatum in0, getInlineDatum in1) of
              (UnvalidatedFoldedDonations ix0 iMap0 traversedSoFar0 lastIx0, UnvalidatedFoldedDonations ix1 iMap1 traversedSoFar1 lastIx1) ->
                -- {{{
                if ix1 == traversedSoFar0 + 1 then
                  -- {{{
                  let
                    pairs0                               = Map.toList iMap0
                    go lst acc                           =
                      -- {{{
                      case (lst, acc) of
                        ([], _)                                         ->
                          -- {{{
                          acc
                          -- }}}
                        ((k, (t, l)) : ps, (m0, m1, ts, ls, initialTs)) ->
                          -- {{{
                          -- The resulting product is:
                          --   - `m0` is the updated map for the traversing
                          --     UTxO,
                          --   - `m1` is the updated map for the traversed
                          --     UTxO,
                          --   - `ts` is the number of tokens to be
                          --     transferred,
                          --   - `ls` is the number of Lovelaces to be
                          --     transferred,
                          --   - `initialTs` is the total token count of the
                          --     input traversing UTxO.
                          case Map.lookup k m1 of
                            Just v@(tokens, lovelaces) ->
                              go
                                ps
                                ( Map.unionWith addIntegerTuples m0 $ Map.singleton k v
                                , Map.delete k m1
                                , ts + tokens
                                , ls + lovelaces
                                , initialTs + t
                                )
                            Nothing                    ->
                              go ps (m0, m1, ls, ts, count + t)
                          -- }}}
                      -- }}}
                    (oMap0', oMap1', ts0to1, ls0to1, c0) =
                      go pairs0 (iMap0, iMap1, 0, 0, 0)
                    inVal0                               = txOutValue in0
                    inVal1                               = txOutValue in1
                    outVal0'                             =
                         addLovelacesTo ls0to1 inVal0
                      <> Value.singleton donSym tn ts0to1
                    outVal1'                             =
                         addLovelacesTo (negate ls0to1) inVal1
                      <> Value.singleton donSym tn (negate ts0to1)
                    out0DatumIsValid                     =
                      -- {{{
                      if ix1 == lastIx0 then
                        -- {{{
                        case getInlineDatum out0 of
                          ValidatedFoldedDonations oMap0 ->
                            -- {{{
                            traceIfFalse
                              "Traversing map is not updated properly."
                              (oMap0 == oMap0')
                            -- }}}
                          _                              ->
                            -- {{{
                            traceError
                              "Invalid datum attached to the produced traversing UTxO."
                            -- }}}
                        -- }}}
                      else
                        -- {{{
                        case getInlineDatum out0 of
                          UnvalidatedFoldedDonations ix0' oMap0 traversedSoFar0' lastIx0' ->
                            -- {{{
                            traceIfFalse
                              "Traversing map is not updated properly."
                              (    ix0     == ix0'
                                && oMap0   == oMap0'
                                && ix1     == traversedSoFar0'
                                && lastIx0 == lastIx0'
                              )
                            -- }}}
                          _                                                             ->
                            -- {{{
                            traceError
                              "Invalid datum attached to the produced traversing UTxO."
                            -- }}}
                        -- }}}
                      -- }}}
                    out1DatumIsValid                     =
                      -- {{{
                      traceIfFalse
                        "Invalid datum attached to the produced traversed UTxO."
                        ( utxosDatumMatchesWith
                            ( UnvalidatedFoldedDonations
                                ix1
                                oMap1'
                                traversedSoFar1
                                lastIx1
                            )
                            out1
                        )
                      -- }}}
                  in
                  -- {{{
                     traceIfFalse
                       "Invalid addresses."
                       (    iAddr0 == iAddr1
                         && iAddr0 == oAddr0
                         && oAddr0 == oAddr1
                       )
                  && traceIfFalse
                       "Traversing UTxO has an invalid asset."
                       ( case flattenValue inVal0 of
                           [(sym', tn', _), _] ->
                             sym' == donSym && tn' == tn
                           _                   ->
                             False
                       )
                  && traceIfFalse
                       "Traversed UTxO has an invalid asset."
                       ( case flattenValue inVal1 of
                           [(sym', tn', _), _] ->
                             sym' == donSym && tn' == tn
                           _                   ->
                             False
                       )
                  && traceIfFalse
                       "Invalid value in the produced traversing UTxO."
                       (txOutValue out0 == outVal0')
                  && traceIfFalse
                       "Invalid value in the produced traversed UTxO."
                       (txOutValue out1 == outVal1')
                  && out0DatumIsValid
                  && out1DatumIsValid
                  -- }}}
                  -- }}}
                else
                  -- {{{
                  traceError "First folded UTxO is not traversing the correct UTxO."
                  -- }}}
                -- }}}
              _                                                                                                                            ->
                -- {{{
                traceError "Invalid input datums for the traversing stage."
                -- }}}
            -- }}}
      in
      case (inputs, outputs) of
        (i0 : i1 : _, [o0, o1])       ->
          -- {{{
          validateInputsOutputs i0 i1 o0 o1
          -- }}}
        (i0 : i1 : _, [_, o0, o1])    ->
          -- {{{
          validateInputsOutputs i0 i1 o0 o1
          -- }}}
        (i0 : i1 : _, [_, _, o0, o1]) ->
          -- {{{
          validateInputsOutputs i0 i1 o0 o1
          -- }}}
        _                             ->
          -- {{{
          traceError "Invalid inputs/outputs pattern."
          -- }}}
      -- }}}
    Dev             ->
      -- {{{
      True
      -- }}}


agentPolicy :: CurrencySymbol -> MintingPolicy
agentPolicy donSym =
  -- {{{
  let
    wrap :: (AgentRedeemer -> ScriptContext -> Bool)
         -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap . mkQVFPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode donSym
  -- }}}


