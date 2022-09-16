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


module Minter.Donation where


import qualified Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value

import Utils


-- REDEEMER
-- {{{
data DonationInfo = DonationInfo
  { diProjectId :: !BuiltinByteString
  , diDonor     :: !BuiltinByteString
  , diAmount    :: !Integer
  }

PlutusTx.unstableMakeIsData ''DonationInfo


data DonationRedeemer
  = DonateToProject DonationInfo
  | FoldDonations

PlutusTx.makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject, 0)
  , ('FoldDonations  , 1)
  ]
-- }}}


{-# INLINABLE mkDonationPolicy #-}
mkDonationPolicy :: CurrencySymbol
                 -> DonationRedeemer
                 -> ScriptContext
                 -> Bool
mkDonationPolicy sym action ctx =
  -- {{{
  False -- TODO.
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
donationPolicy :: CurrencySymbol -> Scripts.MintingPolicy
donationPolicy sym =
  -- {{{
  Plutonomy.optimizeUPLC $ mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkDonationPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sym
  -- }}}


donationSymbol :: CurrencySymbol -> CurrencySymbol
donationSymbol = scriptCurrencySymbol . donationPolicy
-- }}}
