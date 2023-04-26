{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.Registration where


import Plutus.V2.Ledger.Api ( BuiltinByteString )
import PlutusTx             ( makeIsDataIndexed )

import Data.Datum           ( ProjectDetails )


data RegistrationRedeemer
  = RegisterProject   ProjectDetails
  | ConcludeAndRefund BuiltinByteString
  | RegDev

makeIsDataIndexed ''RegistrationRedeemer
  [ ('RegisterProject  , 0 )
  , ('ConcludeAndRefund, 1 )
  , ('Dev              , 20)
  ]
