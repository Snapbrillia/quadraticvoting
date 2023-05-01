{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.Registration where


import Plutus.V2.Ledger.Api ( BuiltinByteString
                            , TxOutRef )
import PlutusTx             ( makeIsDataIndexed )

import Data.Datum           ( ProjectDetails )


data RegistrationRedeemer
  = RegisterProject   TxOutRef ProjectDetails
  --                  ^------^
  --  Helper from off-chain for finding the governance UTxO
  | RemoveAndRefund   TxOutRef TxOutRef TxOutRef BuiltinByteString
  --                  ^------^ ^------^ ^------^
  --                    Gov.     Info     State
  | ConcludeAndRefund TxOutRef TxOutRef BuiltinByteString
  --                  ^------^ ^------^
  --                    Info     State
  | Dev

makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject  , 0 )
  , ('ConcludeAndRefund, 1 )
  , ('Dev              , 20)
  ]
