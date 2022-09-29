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
module Datum where
-- }}}


-- IMPORTS
-- {{{
import Data.Aeson                       ( FromJSON
                                        , ToJSON )
import GHC.Generics                     ( Generic )
import Ledger
import Plutus.V2.Ledger.Api             ( PubKeyHash
                                        , POSIXTime
                                        , Map
                                        , BuiltinByteString )
import qualified PlutusTx.AssocMap      as Map
import           PlutusTx.AssocMap      ( Map )
import           Prelude                ( Show
                                        , show
                                        )
import qualified PlutusTx
import PlutusTx.Prelude                 ( Bool(False)
                                        , Integer
                                        , BuiltinByteString
                                        , Eq(..)
                                        , (&&) )
-- }}}


-- PROJECT DETAILS
-- {{{
data ProjectDetails = ProjectDetails
  { pdPubKeyHash :: PubKeyHash
  , pdName       :: BuiltinByteString
  , pdRequested  :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)
instance Eq ProjectDetails where
  {-# INLINABLE (==) #-}
  ProjectDetails p0 n0 r0 == ProjectDetails p1 n1 r1 =
    -- {{{
    p0 == p1 && n0 == n1 && r0 == r1
    -- }}}

PlutusTx.unstableMakeIsData ''ProjectDetails
-- }}}


-- DATUM
-- {{{
data QVFDatum
  -- {{{
  = DeadlineDatum
    -- ^ The datum attached to a reference UTxO for reading the deadline of the fund.
      -- {{{
      POSIXTime
      -- }}}

  | RegisteredProjectsCount 
    -- ^ The "main" datum, keeping a record of the number of registered projects.
      -- {{{
      Integer
      -- }}}

  | DonationAccumulationProgress
    -- ^ For keeping track of the folded projects traversed.
      -- {{{
      Integer -- ^ Total project count.
      Integer -- ^ Projects traversed so far.
      Integer -- ^ Total Lovelaces so far.
      Integer -- ^ Sum of prize weights so far.
      -- }}}

  | DonationAccumulationConcluded
    -- ^ Datum after collecting all donations.
      -- {{{
      Integer -- ^ Total project count.
      Integer -- ^ Total Lovelaces.
      Integer -- ^ Sum of prize weights.
      Bool    -- ^ Key holder fee collected or not.
      -- }}}

  | ProjectInfo
    -- ^ To store static info in a reference UTxO.
      -- {{{
      ProjectDetails
      -- }}}

  | ReceivedDonationsCount
    -- ^ Datum for a project UTxO. Tracks number of donations, not amount.
      -- {{{
      Integer
      -- }}}
  
  | DonationFoldingProgress
    -- ^ Project UTxO during the first phase of folding the donations.
      -- {{{
      Integer -- ^ Total donation count.
      Integer -- ^ Folded so far.
      -- }}}

  | PrizeWeight
    -- ^ Result of folding all donations.
      -- {{{
      Integer -- ^ Prize weight.
      Bool    -- ^ Whether depleted or not.
      -- }}}

  | Donation
    -- ^ For a single donation UTxO.
      -- {{{
      PubKeyHash -- ^ Donor's public key hash.
      -- }}}

  | Donations
    -- ^ For output donation UTxO of the first phase of folding donations.
      -- {{{
      (Map PubKeyHash Integer)
      -- }}}

  | Escrow
    -- ^ For UTxOs that store the excess reward won by projects.
      -- {{{
      (Map PubKeyHash Integer)
      -- }}}

  deriving (Show, Generic, FromJSON, ToJSON)
  -- }}}

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  -- {{{
  DeadlineDatum pt0 == DeadlineDatum pt1 = pt0 == pt1
  RegisteredProjectsCount c0 == RegisteredProjectsCount c1 = c0  == c1
  DonationAccumulationProgress t0 s0 d0 w0 == DonationAccumulationProgress t1 s1 d1 w1 = t0 == t1 && s0 == s1 && d0 == d1 && w0 == w1
  DonationAccumulationConcluded t0 d0 w0 k0 == DonationAccumulationConcluded t1 d1 w1 k1 = t0 == t1 && d0 == d1 && w0 == w1 && k0 == k1
  ProjectInfo dets0 == ProjectInfo dets1 = dets0 == dets1
  ReceivedDonationsCount c0 == ReceivedDonationsCount c1 = c0 == c1
  DonationFoldingProgress t0 s0 == DonationFoldingProgress t1 s1 = t0 == t1 && s0 == s1
  PrizeWeight w0 d0 == PrizeWeight w1 d1 = w0 == w1 && d0 == d1
  Donation p0 == Donation p1 = p0 == p1
  Donations m0 == Donations m1 = m0 == m1
  Escrow m0 == Escrow m1 = m0 == m1
  _ == _ = False
  -- }}}

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('DeadlineDatum                , 0)
  , ('RegisteredProjectsCount      , 1)
  , ('DonationAccumulationProgress , 2)
  , ('DonationAccumulationConcluded, 3)
  , ('ProjectInfo                  , 4)
  , ('ReceivedDonationsCount       , 5)
  , ('DonationFoldingProgress      , 6)
  , ('PrizeWeight                  , 7)
  , ('Donation                     , 8)
  , ('Donations                    , 9)
  , ('Escrow                       , 10)
  ]
-- }}}
