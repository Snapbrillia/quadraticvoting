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
module Minter.Governance where
-- }}}


-- IMPORTS
-- {{{
import           Ledger                               ( scriptCurrencySymbol )
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


{-# INLINABLE qvfTokenName #-}
qvfTokenName :: TokenName
qvfTokenName = TokenName "QVF"


{-# INLINABLE mkQVFPolicy #-}
mkQVFPolicy :: TxOutRef
            -> POSIXTime
            -> TokenName
            -> BuiltinData
            -> ScriptContext
            -> Bool
mkQVFPolicy oref deadline tn _ ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx

    hasUTxO :: Bool
    hasUTxO =
      -- {{{
      utxoIsGettingSpent (txInfoInputs info) oref
      -- }}}

    deadlineIsValid :: Bool
    deadlineIsValid =
      -- {{{
      Interval.to deadline `Interval.contains` txInfoValidRange info
      -- }}}

    checkMintedAmount :: Bool
    checkMintedAmount =
      -- {{{
      case flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->
          -- {{{
          if tn' == tn then
            traceIfFalse "Exactly 2 tokens must be minted" (amt == 2)
          else
            traceError "Bad token name."
          -- }}}
        _              ->
          -- {{{
          traceError "Exactly 1 type of asset must be minted."
          -- }}}
      -- }}}

    validateTwoOutputs o0 o1 =
         traceIfFalse
           "Missing governance token in the deadline UTxO."
           (utxoHasX ownSym (Just tn) o0)
      && traceIfFalse
           "Missing governance token in the main UTxO."
           (utxoHasX ownSym (Just tn) o1)
      && ( case (getInlineDatum o0, getInlineDatum o1) of
             (DeadlineDatum dl, RegisteredProjectsCount count) ->
               -- {{{
                  traceIfFalse
                    "Deadline must match with the provided parameter."
                    (dl == deadline)
               && traceIfFalse
                    "Funding round must start with 0 registered projects."
                    (count == 0)
               && traceIfFalse
                    "Deadline UTxO must carry the required Lovelaces."
                    (utxoHasLovelaces governanceLovelaces o0)
               && traceIfFalse
                    "Governance UTxO must carry the required Lovelaces."
                    (utxoHasLovelaces governanceLovelaces o1)
               -- }}}
             _                                                 ->
               -- {{{
               traceError
                 "Either invalid datums produced, or produced in wrong order."
               -- }}}
         )

    validOutputsPresent :: Bool
    validOutputsPresent =
      -- {{{
      case txInfoOutputs info of
        [o0, o1]    ->
          -- {{{
          validateTwoOutputs o0 o1
          -- }}}
        [_, o0, o1] ->
          -- {{{
          validateTwoOutputs o0 o1
          -- }}}
        _           ->
          -- {{{
          traceError "The 2 minted tokens must be split among 2 UTxOs."
          -- }}}
      -- }}}

    -- validDeadlineOutput :: Bool
    -- validMainOutput     :: Bool
    -- (validDeadlineOutput, validMainOutput) =
    --   -- {{{
    --   let
    --     go []            acc                     = acc
    --     go (utxo : rest) acc@(dlFound, rpcFound) =
    --       if utxoHasX ownSym (Just tn) utxo then
    --         -- {{{
    --         if dlFound then
    --           -- {{{
    --           case getInlineDatum utxo of
    --             RegisteredProjectsCount count ->
    --               -- {{{
    --               ( dlFound
    --               ,    traceIfFalse
    --                      "Funding round must start with 0 registered projects."
    --                      (count == 0)
    --                 && traceIfFalse
    --                      "Governance UTxO must carry the required Lovelaces."
    --                      (utxoHasLovelaces governanceLovelaces utxo)
    --               )
    --               -- }}}
    --             _                             ->
    --               -- {{{
    --               traceError "Invalid datum for the second UTxO."
    --               -- }}}
    --           -- }}}
    --         else
    --           -- {{{
    --           case getInlineDatum utxo of
    --             DeadlineDatum dl ->
    --               -- {{{
    --               let
    --                 cond =
    --                      traceIfFalse
    --                        "Deadline must match with the provided parameter."
    --                        (dl == deadline)
    --                   && traceIfFalse
    --                        "Deadline UTxO must carry the required Lovelaces."
    --                        (utxoHasLovelaces governanceLovelaces utxo)
    --               in
    --               go rest (cond, rpcFound)
    --               -- }}}
    --             _                ->
    --               -- {{{
    --               traceError "Deadline UTxO must be produced first."
    --               -- }}}
    --           -- }}}
    --         -- }}}
    --       else
    --         -- {{{
    --         go rest acc
    --         -- }}}
    --   in
    --   go (txInfoOutputs info) (False, False)
    --   -- }}}
  in
     traceIfFalse "UTxO not consumed." hasUTxO
  && traceIfFalse "Deadline has passed." deadlineIsValid
  && checkMintedAmount
  && validOutputsPresent
  -- }}}


qvfPolicy :: TxOutRef -> POSIXTime -> MintingPolicy
qvfPolicy oref deadline =
  -- {{{
  let
    wrap :: (BuiltinData -> ScriptContext -> Bool) -> PSU.V2.UntypedMintingPolicy
    wrap = PSU.V2.mkUntypedMintingPolicy
  in
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' deadline' tn' -> wrap $ mkQVFPolicy oref' deadline' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    `PlutusTx.applyCode`
    PlutusTx.liftCode qvfTokenName
  -- }}}


qvfSymbol :: TxOutRef -> POSIXTime -> CurrencySymbol
qvfSymbol oref deadline = scriptCurrencySymbol $ qvfPolicy oref deadline


x :: Data
x =
  Constr 0 -- ScriptContext
    [ Constr 0 -- scriptContextTxInfo
        -- {{{
        [ List     -- txInfoInputs
            -- {{{
            [ Constr 0 -- TxInInfo
                [ Constr 0 -- txInInfoOutRef
                    [ Constr 0
                        [ B "\140O\142\210\150\DC4\148\191a\170\163\227\174\r\245\SUB4~\217\220\186>L\236i\183\160:\230o\154\213"
                        ]
                    , I 0
                    ]
                , Constr 0 -- txInInfoResolved
                    [ Constr 0 -- txOutAddress
                        [ Constr 0
                            [ B "n\188\b\178\DC1\190{ <\188T\238\252\202\150\EM\ETB5V\ETX&\211\145\154\188\162B["
                            ]
                        , Constr 1 []
                        ]
                    , Map [(B "", Map [(B "", I 9757062140)])] -- txOutValue
                    , Constr 0 [] -- txOutDatum
                    , Constr 1 [] -- txOutReferenceScript
                    ]
                ]
            ]
            -- }}}
        , List     -- txInfoReferenceInputs
            -- {{{
            []
            -- }}}
        , List     -- txInfoOutputs
            -- {{{
            [ Constr 0 -- TxOut
                -- {{{
                [ Constr 0 -- txOutAddress
                    [ Constr 0
                        [ B "n\188\b\178\DC1\190{ <\188T\238\252\202\150\EM\ETB5V\ETX&\211\145\154\188\162B["
                        ]
                    , Constr 1 []
                    ]
                , Map [(B "", Map [(B "", I 9753617579)])] -- txOutValue
                , Constr 0 [] -- txOutDatum
                , Constr 1 [] -- txOutReferenceScript
                ]
                -- }}}
            , Constr 0 -- TxOut
                -- {{{
                [ Constr 0 -- txOutAddress
                    [ Constr 1
                        [ B "E\132,\173\238\170\174\220t\237\249\250\185\194\138\198\v+\229\152<\169[a\241\138\SIp"
                        ]
                    , Constr 1 []
                    ]
                , Map -- txOutValue
                    [ (B "", Map [(B "", I 1500000)])
                    , ( B "\178\RS\144\156\215\SOH\165W%\\}4\138E\233M\NUL#sM\234+p\DC3\246\202\&4\174"
                      , Map [(B "QVF", I 1)]
                      )
                    ]
                , Constr 2 [Constr 0 [I 1667642400000]] -- txOutDatum
                , Constr 1 [] -- txOutReferenceScript
                ]
                -- }}}
            , Constr 0 -- TxOut
                -- {{{
                [ Constr 0 -- txOutAddress
                    [ Constr 1
                        [ B "E\132,\173\238\170\174\220t\237\249\250\185\194\138\198\v+\229\152<\169[a\241\138\SIp"
                        ]
                    , Constr 1 []
                    ]
                , Map -- txOutValue
                    [ (B "", Map [(B "", I 1500000)])
                    , ( B "\178\RS\144\156\215\SOH\165W%\\}4\138E\233M\NUL#sM\234+p\DC3\246\202\&4\174"
                      , Map [(B "QVF" , I 1)]
                      )
                    ]
                , Constr 2 [Constr 1 [I 0]] -- txOutDatum
                , Constr 1 [] -- txOutReferenceScript
                ]
                -- }}}
            ]
            -- }}}
        , Map      -- txInfoFee
            -- {{{
            [(B "", Map [(B "", I 444561)])]
            -- }}}
        , Map      -- txInfoMint
            -- {{{
            [ (B "", Map [(B "" , I 0)])
            , ( B "\178\RS\144\156\215\SOH\165W%\\}4\138E\233M\NUL#sM\234+p\DC3\246\202\&4\174"
              , Map [(B "QVF", I 2)]
              )
            ]
            -- }}}
        , List     -- txInfoDCert
            -- {{{
            []
            -- }}}
        , Map      -- txInfoWdrl
            -- {{{
            []
            -- }}}
        , Constr 0 -- txInfoValidRange
            -- {{{
            [ Constr 0 [Constr 0 [], Constr 1 []]
            , Constr 0 [Constr 1 [I 1665115486000] ,Constr 1 []]
            ]
            -- }}}
        , List     -- txInfoSignatories
            -- {{{
            []
            -- }}}
        , Map      -- txInfoRedeemers
            -- {{{
            [ ( Constr 0 -- Minting :: ScriptPurpose
                  [ B "\178\RS\144\156\215\SOH\165W%\\}4\138E\233M\NUL#sM\234+p\DC3\246\202\&4\174"
                  ]
              , I 0      -- Redeemer
              )
            ]
            -- }}}
        , Map      -- txInfoData
            -- {{{
            []
            -- }}}
        , Constr 0 -- txInfoId
            -- {{{
            [ B "Fg%\147\195c\172\155z~\229\251\248\STX\t\214\ETX\184\166\232q\236\165m\242\DLEpk\143\251i\207"
            ]
            -- }}}
        ]
        -- }}}
    , Constr 0 -- Minting :: ScriptPurpose
        -- {{{
        [ B "\178\RS\144\156\215\SOH\165W%\\}4\138E\233M\NUL#sM\234+p\DC3\246\202\&4\174"
        ]
        -- }}}
    ]
