#!/bin/bash

. scripts/initiation.sh

# initiate_fund

# . scripts/register-project.sh 1 ProjectA 10000000000
# 
# . scripts/register-project.sh 2 ProjectB 10000000000
# 
# . scripts/register-project.sh 3 ProjectC 10000000000
# 
# . scripts/register-project.sh 4 ProjectD 10000000000
# 
# . scripts/register-project.sh 5 ProjectE 10000000000

# donor=6
# for i in $(tail -n 5 $registeredProjectsFile); do
#   . scripts/donate-to-project.sh $donor $i "$(expr $donor - 5)0000000"
#   donor=$(expr $donor + 1)
# done
# 
# for i in $(tail -n 4 $registeredProjectsFile); do
#   . scripts/donate-to-project.sh $donor $i "$(expr $donor - 5)0000000"
#   donor=$(expr $donor + 1)
# done
# 
# for i in $(tail -n 3 $registeredProjectsFile); do
#   . scripts/donate-to-project.sh $donor $i "$(expr $donor - 5)0000000"
#   donor=$(expr $donor + 1)
# done
# 
# for i in $(tail -n 2 $registeredProjectsFile); do
#   . scripts/donate-to-project.sh $donor $i "$(expr $donor - 5)0000000"
#   donor=$(expr $donor + 1)
# done
# 
# for i in $(tail -n 1 $registeredProjectsFile); do
#   . scripts/donate-to-project.sh $donor $i "$(expr $donor - 5)0000000"
#   donor=$(expr $donor + 1)
# done

for i in $(seq 1 5); do
  . scripts/fold-donations.sh $i
done

. scripts/accumulate-donations.sh

. scripts/collect-key-holder-fee.sh
