#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

. $REPO/scripts/env.sh

projectWalletAddress=$1

projectTokenName=$(cat $registeredProjectsFile | jq -r ". [] | select(.address == \"$projectWalletAddress\") | .tn")

if [ -z "$projectTokenName" ]; then
  echo "0"
else 
  projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
  projectLovelaceAmount=$(echo $projectUTxOObj | jq -r .lovelace)
  bountyCredit=$(expr $projectLovelaceAmount - $halfOfTheRegistrationFee)
  echo $bountyCredit
fi

