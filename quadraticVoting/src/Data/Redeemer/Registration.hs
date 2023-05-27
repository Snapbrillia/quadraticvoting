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
  | RemoveAndRefund   TxOutRef TxOutRef TxOutRef BuiltinByteString Bool
  --                  ^------^ ^------^ ^------^                   ^--^
  --                    Gov.     State    Info     indicates order of ouputs (gov. first or not)
  | ConcludeAndRefund TxOutRef TxOutRef TxOutRef BuiltinByteString
  --                  ^------^ ^------^ ^------^
  --                    Gov.     State    Info  
  | Dev

makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject  , 0 )
  , ('RemoveAndRefund  , 1 )
  , ('ConcludeAndRefund, 2 )
  , ('Dev              , 20)
  ]
