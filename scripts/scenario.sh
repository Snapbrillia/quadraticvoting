#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

. $REPO/scripts/initiation.sh

#initiate_fund dev

. $REPO/scripts/contribute.sh $keyHolder 100000000

. $REPO/scripts/env.sh

. $REPO/scripts/register-project.sh 1 ProjectA 400000000

. $REPO/scripts/register-project.sh 2 ProjectB 350000000

. $REPO/scripts/register-project.sh 3 ProjectC 300000000

. $REPO/scripts/register-project.sh 4 ProjectD 300000000

. $REPO/scripts/register-project.sh 5 ProjectE 300000000

#. $REPO/scripts/register-project.sh 6 ProjectF 300000000

#. $REPO/scripts/register-project.sh 7 ProjectG 300000000

#. $REPO/scripts/register-project.sh 8 ProjectH 300000000

#. $REPO/scripts/register-project.sh 9 ProjectI 300000000

donor=6
for i in $(seq 0 4); do
  proj=$(project_index_to_token_name $i)
  . $REPO/scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 3); do
  proj=$(project_index_to_token_name $i)
  . $REPO/scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 2); do
  proj=$(project_index_to_token_name $i)
  . $REPO/scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

for i in $(seq 0 1); do
  proj=$(project_index_to_token_name $i)
  . $REPO/scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
  donor=$(expr $donor + 1)
done

proj=$(project_index_to_token_name 0)
. $REPO/scripts/donate-to-project.sh $donor $proj "$(expr $donor - 5)0000000"
donor=$(expr $donor + 1)

# for tn in $(get_all_token_names | jq -r '.[]'); do
#   . $REPO/scripts/fold-donations.sh $tn 1
# done
# 
# . $REPO/scripts/accumulate-prize-weights.sh
#  
# for i in $(seq 0 4); do
#   . $REPO/scripts/eliminate-one-project.sh
# done
# 
# 
# if [ "$ENV" == "dev" ]; then
#   for i in $(seq 0 1); do
#     . $REPO/scripts/distribute-prize.sh $i
#   done
# else
#   echo "TERMINATED: The distribution phase of this scenario only works in the \"dev\""
#   echo "environment (i.e. \"\$ENV\" == \"dev\")."
#   return 1
# fi
