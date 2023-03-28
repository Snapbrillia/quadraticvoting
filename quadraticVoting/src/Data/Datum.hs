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
import qualified Prelude                as P
import           Prelude                ( Show
                                        , show )
import qualified PlutusTx
import           PlutusTx.Prelude       ( Bool(..)
                                        , Integer
                                        , BuiltinByteString
                                        , Eq(..)
                                        , (&&)
                                        , Maybe
                                        )
-- }}}


-- PROJECT DETAILS
-- {{{
data ProjectDetails = ProjectDetails
  { pdAddress   :: Address
  , pdName      :: BuiltinByteString
  , pdRequested :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON, P.Eq)

instance Eq ProjectDetails where
  {-# INLINABLE (==) #-}
  ProjectDetails a0 n0 r0 == ProjectDetails a1 n1 r1 =
    -- {{{
    a0 == a1 && n0 == n1 && r0 == r1
    -- }}}

PlutusTx.unstableMakeIsData ''ProjectDetails
-- }}}


-- PROJECT ELIMINATION INFO
-- {{{
data EliminationInfo = EliminationInfo
  { eiRequested :: Integer
  , eiRaised    :: Integer
  , eiWeight    :: Integer -- TODO: remove?
  } deriving (Show, Generic, FromJSON, ToJSON, P.Eq)

instance Eq EliminationInfo where
  {-# INLINABLE (==) #-}
  EliminationInfo p0 n0 r0 == EliminationInfo p1 n1 r1 =
    -- {{{
    p0 == p1 && n0 == n1 && r0 == r1
    -- }}}

PlutusTx.unstableMakeIsData ''EliminationInfo
-- }}}


-- QVF DATUM
-- {{{
data QVFDatum
  -- {{{
  -- {{{ GOVERNANCE UTXOs (YELLOW) 
  = DeadlineDatum
    -- ^ The datum attached to a reference UTxO for reading the deadline of the
    --   fund.
      -- {{{
      POSIXTime -- The deadline in milliseconds.
      -- }}}

  | RegisteredProjectsCount 
    -- ^ The "main" datum, keeping a record of the number of registered projects.
      -- {{{
      Integer
      -- }}}

  | PrizeWeightAccumulation
    -- ^ Progress of forming the complete map of prize weights.
      -- {{{
      Integer                                 -- ^ Total registered projects count.
      (Map BuiltinByteString EliminationInfo) -- ^ Requested funds, raised donations, and prize weights of each project.
      -- }}}

  | ProjectEliminationProgress
    -- ^ Progress of eliminating non-eligible projects.
    -- The match pool is needed to be stored because the key holder fee will
    -- remain inside this UTxO after elimination of each project, and therefore
    -- the Lovelace count won't continue to represent the match pool.
      -- {{{
      Integer                                 -- ^ Match pool.
      (Map BuiltinByteString EliminationInfo) -- ^ Requested funds, raised donations, and prize weights of each project.
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

  | ProjectDonations
    -- ^ Datum for a project UTxO. Points to the (possible) first donor's
    --   donation UTxO (more precisely, points to the public key hash contained
    --   within the datum of donor'r donation UTxO).
      -- {{{
      (Maybe PubKeyHash)
      -- }}}
  
  | DonationFoldingProgress
    -- ^ Project UTxO during folding phase. Accumulates the donations.
      -- {{{
      Integer    -- ^ Sum of the square roots of donations folded so far.
      PubKeyHash -- ^ Next donor's public key hash (this is not `Maybe` as it
                 --   wouldn't make sense to be in progress if there are no
                 --   more donations to fold).
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
    -- ^ A donation UTxO (an element of the on-chain map).
      -- {{{
      PubKeyHash         -- Donor's public key hash.
      (Maybe PubKeyHash) -- Possible next donor's public key hash.
      -- }}}
  -- }}}
  -- }}}
  deriving (Show, Generic, FromJSON, ToJSON, P.Eq)

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  -- {{{
  DeadlineDatum d0                 == DeadlineDatum d1                 = d0 == d1
  RegisteredProjectsCount c0       == RegisteredProjectsCount c1       = c0 == c1
  PrizeWeightAccumulation t0 w0    == PrizeWeightAccumulation t1 w1    = t0 == t1 && w0 == w1
  ProjectEliminationProgress m0 w0 == ProjectEliminationProgress m1 w1 = m0 == m1 && w0 == w1
  DistributionProgress m0 p0 w0    == DistributionProgress m1 p1 w1    = m0 == m1 && p0 == p1 && w0 == w1
  --
  ProjectInfo dets0                == ProjectInfo dets1                = dets0 == dets1
  ProjectDonations mP0             == ProjectDonations mP1             = mP0 == mP1
  DonationFoldingProgress w0 p0    == DonationFoldingProgress w1 p1    = w0 == w1 && p0 == p1
  PrizeWeight w0 b0                == PrizeWeight w1 b1                = w0 == w1 && b0 == b1
  Escrow m0                        == Escrow m1                        = m0 == m1
  --
  Donation p0 mP0                  == Donation p1 mP1                  = p0 == p1 && mP0 == mP1
  --
  _                                == _                                = False
  -- }}}

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('DeadlineDatum             , 0 )
  , ('RegisteredProjectsCount   , 1 )
  , ('PrizeWeightAccumulation   , 2 )
  , ('ProjectEliminationProgress, 3 )
  , ('DistributionProgress      , 4 )
  --
  , ('ProjectInfo               , 5 )
  , ('ProjectDonations          , 6 )
  , ('DonationFoldingProgress   , 7 )
  , ('PrizeWeight               , 8 )
  , ('Escrow                    , 9 )
  --
  , ('Donation                  , 10)
  ]
-- }}}
