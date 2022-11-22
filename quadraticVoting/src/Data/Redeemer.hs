-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- }}}


-- MODULE
-- {{{
module Data.Redeemer where
-- }}}


-- IMPORTS
-- {{{
import           Data.Aeson             ( FromJSON
                                        , ToJSON )
import           GHC.Generics           ( Generic )
import           Ledger
import           Plutus.V2.Ledger.Api   ( PubKeyHash
                                        , POSIXTime
                                        , Map
                                        , BuiltinByteString )
import qualified PlutusTx.AssocMap      as Map
import           PlutusTx.AssocMap      ( Map )
import           Prelude                ( Show
                                        , show )
import qualified PlutusTx
import           PlutusTx.Prelude       ( Bool(False)
                                        , Integer
                                        , BuiltinByteString
                                        , Eq(..)
                                        , (&&) )
import           Data.DonationInfo
-- }}}


-- QVF ACTION
-- {{{
data QVFRedeemer
  = UpdateDeadline  POSIXTime
  | RegisterProject
  | Contribute      Integer
  | DonateToProject
  | FoldDonations
  | AccumulatePrizeWeights
  | EliminateProject
  | DistributePrize BuiltinByteString
  | UnlockEscrowFor PubKeyHash Integer
  | WithdrawBounty  PubKeyHash
  | ConcludeProject
  | ConcludeFundingRound
  | Dev

PlutusTx.makeIsDataIndexed ''QVFRedeemer
  [ ('UpdateDeadline        , 0 )
  , ('RegisterProject       , 1 )
  , ('Contribute            , 2 )
  , ('DonateToProject       , 3 )
  , ('FoldDonations         , 4 )
  , ('AccumulatePrizeWeights, 5 )
  , ('EliminateProject      , 6 )
  , ('DistributePrize       , 7 )
  , ('UnlockEscrowFor       , 8 )
  , ('WithdrawBounty        , 9 )
  , ('ConcludeProject       , 10)
  , ('ConcludeFundingRound  , 11)
  , ('Dev                   , 20)
  ]
-- }}}



