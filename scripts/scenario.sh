#!/bin/bash

. scripts/initiation.sh

initiate_fund

. scripts/contribute.sh $keyHolder 100000000

. scripts/register-project.sh 1 ProjectA 300000000

. scripts/register-project.sh 2 ProjectB 300000000

. scripts/register-project.sh 3 ProjectC 300000000

. scripts/register-project.sh 4 ProjectD 300000000

. scripts/register-project.sh 5 ProjectE 300000000

donor=6
for i in $(seq 0 4); do
  proj=$(project_index_to_token_name $i)
  . scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 3); do
  proj=$(project_index_to_token_name $i)
  . scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 2); do
  proj=$(project_index_to_token_name $i)
  . scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 1); do
  proj=$(project_index_to_token_name $i)
  . scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

proj=$(project_index_to_token_name 0)
. scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
donor=$(expr $donor + 1)

for i in $(seq 0 4); do
  . scripts/fold-donations.sh $i 1
done

. scripts/accumulate-prize-weights.sh
 
for i in $(seq 0 4); do
  . scripts/eliminate-one-project.sh
done


for i in $(seq 0 1); do
  . scripts/distribute-prize.sh $i
done
