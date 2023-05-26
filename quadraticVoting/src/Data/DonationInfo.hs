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
import Data.Aeson           ( FromJSON
                            , ToJSON )
import GHC.Generics         ( Generic )
import Plutus.V2.Ledger.Api ( BuiltinByteString
                            , PubKeyHash )
import Prelude              ( Show )
import PlutusTx             ( makeIsDataIndexed )
import PlutusTx.Prelude
-- }}}


data DonationInfo = DonationInfo
  { diProjectID :: BuiltinByteString
  , diDonor     :: PubKeyHash
  , diAmount    :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''DonationInfo

