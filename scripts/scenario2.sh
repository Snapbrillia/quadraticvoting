#!/bin/bash

. scripts/initiation.sh

initiate_fund

. scripts/register-project.sh 1 ProjectA 10000000000

# . scripts/register-project.sh 2 ProjectB 10000000000
# 
# . scripts/register-project.sh 3 ProjectC 10000000000
# 
# . scripts/register-project.sh 4 ProjectD 10000000000

. scripts/donate-to-project.sh dev 1 6 1005 50

proj1=$(project_number_to_token_name 1)
. scripts/donate-to-project.sh 6   $proj1 10000000
. scripts/donate-to-project.sh 20  $proj1 10000000
. scripts/donate-to-project.sh 50  $proj1 10000000
. scripts/donate-to-project.sh 99  $proj1 10000000
. scripts/donate-to-project.sh 124 $proj1 10000000
. scripts/donate-to-project.sh 151 $proj1 10000000

# . scripts/donate-to-project.sh dev 2 606 805 50
# 
# . scripts/donate-to-project.sh dev 3 806 955 50
# 
# . scripts/donate-to-project.sh dev 4 956 1005 50

. scripts/fold-donations.sh 1 1

# . scripts/fold-donations.sh 2 1
# 
# . scripts/fold-donations.sh 3 1
# 
# . scripts/fold-donations.sh 4 1

. scripts/accumulate-donations.sh

. scripts/collect-key-holder-fee.sh

. scripts/distribute-prize.sh 1

# . scripts/distribute-prize.sh 2
# 
# . scripts/distribute-prize.sh 3
# 
# . scripts/distribute-prize.sh 4
