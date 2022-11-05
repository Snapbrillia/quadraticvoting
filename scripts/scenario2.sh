#!/bin/bash

. scripts/initiation.sh

initiate_fund

. scripts/register-project.sh 1 ProjectA 10000000000

. scripts/register-project.sh 2 ProjectA 10000000000

for i in $(seq 3 800); do
  . scripts/donate-to-project.sh $i $(sed '1q;d' $registeredProjectsFile) 10000000
done
for i in $(seq 801 1000); do
  . scripts/donate-to-project.sh $i $(sed '2q;d' $registeredProjectsFile) 10000000
done

. scripts/fold-donations.sh 1

. scripts/fold-donations.sh 2

. scripts/accumulate-donations.sh

. scripts/collect-key-holder-fee.sh

. scripts/distribute-prize.sh 1

. scripts/distribute-prize.sh 2
