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
module Data.Datum where
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
import           PlutusTx.Prelude       ( Bool(..)
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


-- QVF DATUM
-- {{{
data QVFDatum
  -- {{{
  -- {{{ GOVERNANCE UTXOs (YELLOW) 
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

  | PrizeWeightAccumulation
    -- ^ Progress of forming the complete map of prize weights.
      -- {{{
      Integer                                             -- ^ Total donation count.
      (Map BuiltinByteString (Integer, Integer, Integer)) -- ^ Requested funds, raised donations, and prize weights of each project.
      -- }}}

  | ProjectEliminationProgress
    -- ^ Progress of eliminating non-eligible projects.
    -- The match pool is needed to be stored because the key holder fee will
    -- remain inside this UTxO after elimination of each project, and therefore
    -- the Lovelace count won't continue to represent the match pool.
      -- {{{
      Integer                                             -- ^ Match pool.
      (Map BuiltinByteString (Integer, Integer, Integer)) -- ^ Requested funds, raised donations, and prize weights of each project.
      -- }}}

  | DistributionProgress
    -- ^ All non-eligible projects are eliminated at this point.
      -- {{{
      Integer -- ^ Starting match pool.
      Integer -- ^ Remaining projects to collect their prizes.
      Integer -- ^ Sum of the prize weights (i.e. the denominator).
      -- }}}
  -- }}}

  -- {{{ PROJECT UTXOs (RED) 
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

  | ConsolidationProgress
    -- ^ Progress of summing the donation square roots.
      -- {{{
      Integer -- ^ Remaining donations to be consolidated.
      Integer -- ^ Sum of square roots so far.
      -- }}}

  | PrizeWeight
    -- ^ Result of folding all donations. Carries the donations.
      -- {{{
      Integer -- ^ Prize weight.
      Bool    -- ^ Whether processed or not.
      -- }}}

  | Escrow
    -- ^ For UTxOs that store the excess reward won by projects.
      -- {{{
      (Map PubKeyHash Integer)
      -- }}}
  -- }}}

  -- {{{ DONATION UTXOs (GREEN) 
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
  -- }}}
  -- }}}
  deriving (Show, Generic, FromJSON, ToJSON)

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  -- {{{
  DeadlineDatum pt0 == DeadlineDatum pt1 = pt0 == pt1
  RegisteredProjectsCount c0 == RegisteredProjectsCount c1 = c0  == c1
  PrizeWeightAccumulation t0 w0 == PrizeWeightAccumulation t1 w1 = t0 == t1 && w0 == w1
  ProjectEliminationProgress m0 w0 == ProjectEliminationProgress m1 w1 = m0 == m1 && w0 == w1
  DistributionProgress m0 p0 w0 == DistributionProgress m1 p1 w1 = m0 == m1 && p0 == p1 && w0 == w1
  --
  ProjectInfo dets0 == ProjectInfo dets1 = dets0 == dets1
  ReceivedDonationsCount c0 == ReceivedDonationsCount c1 = c0 == c1
  DonationFoldingProgress t0 s0 == DonationFoldingProgress t1 s1 = t0 == t1 && s0 == s1
  ConsolidationProgress r0 w0 == ConsolidationProgress r1 w1 = r0 == r1 && w0 == w1
  PrizeWeight w0 b0 == PrizeWeight w1 b1 = w0 == w1 && b0 == b1
  Escrow m0 == Escrow m1 = m0 == m1
  --
  Donation p0 == Donation p1 = p0 == p1
  Donations m0 == Donations m1 = m0 == m1
  --
  _ == _ = False
  -- }}}

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('DeadlineDatum             , 0 )
  , ('RegisteredProjectsCount   , 1 )
  , ('PrizeWeightAccumulation   , 2 )
  , ('ProjectEliminationProgress, 3 )
  , ('DistributionProgress      , 4 )
  --
  , ('ProjectInfo               , 5 )
  , ('ReceivedDonationsCount    , 6 )
  , ('DonationFoldingProgress   , 7 )
  , ('ConsolidationProgress     , 8 )
  , ('PrizeWeight               , 9 )
  , ('Escrow                    , 10)
  --
  , ('Donation                  , 11)
  , ('Donations                 , 12)
  ]
-- }}}
