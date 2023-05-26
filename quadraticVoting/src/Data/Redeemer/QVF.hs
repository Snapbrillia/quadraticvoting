{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.Redeemer.QVF where


import Plutus.V2.Ledger.Api ( PubKeyHash
                            , POSIXTime
                            , TxOutRef
                            , BuiltinByteString )
import PlutusTx             ( makeIsDataIndexed )
import PlutusTx.Prelude     ( Integer )


data QVFRedeemer
  = UpdateDeadline         POSIXTime
  | RegisterProject
  | Contribute             Integer
  | DonateToProject
  | IncreaseDonation       BuiltinByteString Integer
  | FoldDonations          TxOutRef -- <- ProjRef
  | AccumulatePrizeWeights TxOutRef -- <- GovRef
  | EliminateProject                         TxOutRef TxOutRef TxOutRef
    --                                       ^------^ ^------^ ^------^
    --                                       GovRef  ProjectRef InfoRef
    --                                       v------v v------v v------v
  | DistributePrize        BuiltinByteString TxOutRef TxOutRef TxOutRef
  | UnlockEscrowFor        PubKeyHash Integer
  | WithdrawBounty         PubKeyHash
  | ConcludeProject        TxOutRef
  | ConcludeFundingRound   TxOutRef -- <- GovRef
  | Dev

makeIsDataIndexed ''QVFRedeemer
  [ ('UpdateDeadline        , 0 )
  , ('RegisterProject       , 1 )
  , ('Contribute            , 2 )
  , ('DonateToProject       , 3 )
  , ('IncreaseDonation      , 4 )
  , ('FoldDonations         , 5 )
  , ('AccumulatePrizeWeights, 6 )
  , ('EliminateProject      , 7 )
  , ('DistributePrize       , 8 )
  , ('UnlockEscrowFor       , 9 )
  , ('WithdrawBounty        , 10)
  , ('ConcludeProject       , 11)
  , ('ConcludeFundingRound  , 12)
  , ('Dev                   , 20)
  ]
