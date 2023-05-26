{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.Donation where


import Plutus.V2.Ledger.Api ( TxOutRef )
import PlutusTx             ( makeIsDataIndexed )

import Data.DonationInfo    ( DonationInfo )


data DonationRedeemer
  = DonateToProject  Bool TxOutRef DonationInfo
    -- ^ The `Bool` value indicates whether the new donation should go at the
    --   head of the list or not. In other words, if `prepend` is `True`, it's
    --   implied that `TxOutRef` points to a project UTxO, and a donation UTxO
    --   if `False`.
  | FoldDonations    TxOutRef
    -- ^ Pointer to project's head UTxO.
  | Dev
    -- ^ For development. TODO: Remove.

makeIsDataIndexed ''DonationRedeemer
  [ ('DonateToProject , 0 )
  , ('FoldDonations   , 1 )
  , ('Dev             , 20)
  ]
