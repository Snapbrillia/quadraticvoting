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
module Data.DonationInfo where
-- }}}


-- IMPORTS
-- {{{
import           Data.Aeson             ( FromJSON
                                        , ToJSON )
import           GHC.Generics           ( Generic )
import           Ledger
import           Plutus.V2.Ledger.Api
import           Prelude                ( Show )
import qualified PlutusTx
import           PlutusTx.Prelude
-- }}}


data DonationInfo = DonationInfo
  { diProjectID :: BuiltinByteString
  , diDonor     :: PubKeyHash
  , diAmount    :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''DonationInfo

