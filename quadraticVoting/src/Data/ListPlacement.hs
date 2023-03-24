-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- }}}


-- MODULE
-- {{{
module Data.ListPlacement
  ( ListPlacement (..)
  , SortedInputs
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Data.Aeson             ( FromJSON
                                        , ToJSON )
import           GHC.Generics           ( Generic )
import           Prelude                ( Show )
import           Plutus.V2.Ledger.Api   ( TxOut )
import qualified PlutusTx
import           PlutusTx.Prelude       ( Maybe )
-- }}}

data ListPlacement
  = First
  | Prepend
  | Insert
  | Append
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ListPlacement
  [ ('First  , 0 )
  , ('Prepend, 1 )
  , ('Insert , 2 )
  , ('Append , 3 )
  ]


-- | Intermediate datatype for interacting with the on-chain linked list. As
--   this datatype is meant to be used for both direct donations and resolution
--   of free donations, the shared @Maybe (TxOut, TxOut)@ argument is
--   meant for housing the multi-donation input and the free donation input.
--   Functions that return this datatype should make sure that for prepend and
--   insert variants, the two @TxOut@ arguments are ordered.
data SortedInputs
  = SortedForFirst   (Maybe (TxOut, TxOut)) TxOut
  | SortedForPrepend (Maybe (TxOut, TxOut)) TxOut TxOut
  | SortedForInsert  (Maybe (TxOut, TxOut)) TxOut TxOut
  | SortedForAppend  (Maybe (TxOut, TxOut)) TxOut
  | SortedFailed

PlutusTx.makeIsDataIndexed ''SortedInputs
  [ ('SortedForFirst  , 0 )
  , ('SortedForPrepend, 1 )
  , ('SortedForInsert , 2 )
  , ('SortedForAppend , 3 )
  , ('SortedFailed    , 4 )
  ]
