{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.Donation where


import Plutus.V2.Ledger.Api ( PubKeyHash
                            , POSIXTime
                            , BuiltinByteString )
import PlutusTx             ( makeIsDataIndexed )
import PlutusTx.Prelude     ( Bool )

import Data.DonationInfo    ( DonationInfo )


data DonationRedeemer
  = DonateToProject  Bool DonationInfo
    -- ^ The `Bool` value indicates whether the new donation should go at the
    --   head of the list or not.
  | FoldDonations    BuiltinByteString
    -- ^ Project's identifier (token names).
  | Dev
    -- ^ For development. TODO: Remove.

makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject , 0 )
  , ('FoldDonations   , 1 )
  , ('Dev             , 20)
  ]
