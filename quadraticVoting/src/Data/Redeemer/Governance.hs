{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.Governance where


import Plutus.V2.Ledger.Api ( POSIXTime
                            , TxOutRef )
import PlutusTx             ( makeIsDataIndexed )


data GovernanceRedeemer
  = Initiate POSIXTime
  | Conclude TxOutRef TxOutRef
  --         ^------^ ^------^
  --           Gov.   Deadline
  | Dev

makeIsDataIndexed ''GovernanceRedeemer
  [ ('Initiate, 0 )
  , ('Conclude, 1 )
  , ('Dev     , 20)
  ]
