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
data QVFAction
  = UpdateDeadline        POSIXTime
  | RegisterProject
  | DonateToProject       BuiltinByteString
  | FoldDonations
  | AbsorbDuplicateDonors
  | ConsolidateDonations
  | AccumulateDonations
  | PayKeyHolderFee
  | DistributePrizes
  | UnlockEscrowFor       PubKeyHash Integer
  | WithdrawBounty        PubKeyHash
  | ConcludeProject
  | Dev

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('UpdateDeadline       , 0)
  , ('RegisterProject      , 1)
  , ('DonateToProject      , 2)
  , ('FoldDonations        , 3)
  , ('AbsorbDuplicateDonors, 4)
  , ('ConsolidateDonations , 5)
  , ('AccumulateDonations  , 6)
  , ('PayKeyHolderFee      , 7)
  , ('DistributePrizes     , 8)
  , ('UnlockEscrowFor      , 9)
  , ('WithdrawBounty       , 10)
  , ('ConcludeProject      , 11)
  , ('Dev                  , 20)
  ]
-- }}}



