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
module Data.ListPlacement where
-- }}}


-- IMPORTS
-- {{{
import           Data.Aeson             ( FromJSON
                                        , ToJSON )
import           GHC.Generics           ( Generic )
import           Prelude                ( Show )
import qualified PlutusTx
-- }}}

data ListPlacement
  = Prepend
  | Insert
  | Append
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('Prepend, 0 )
  , ('Insert , 1 )
  , ('Append , 2 )
  ]

