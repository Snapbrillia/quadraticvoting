#!/bin/bash

drain_from_wallets_to 1 20 $keyHolder
distribute_from_to_wallets $keyHolder 1 20

. scripts/initiation.sh

initiate_fund

. scripts/register-project.sh 1 ProjectA 10000000000

for i in $(seq 2 20); do
  . scripts/donate-to-project.sh $i $(tail -n 1 $registeredProjectsFile) "$i"0000000
done

. scripts/fold-donations.sh 1

. scripts/accumulate-donations.sh

. scripts/collect-key-holder-fee.sh

. scripts/distribute-prize.sh 1
