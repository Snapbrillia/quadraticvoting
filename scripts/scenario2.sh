#!/bin/bash

. scripts/initiation.sh

initiate_fund

. scripts/register-project.sh 1 ProjectA 10000000000

for i in $(seq 2 100); do
  . scripts/donate-to-project.sh $i $(tail -n 1 $registeredProjectsFile) 10000000
done

. scripts/fold-donations.sh 1

. scripts/accumulate-donations.sh

. scripts/collect-key-holder-fee.sh

. scripts/distribute-prize.sh 1
