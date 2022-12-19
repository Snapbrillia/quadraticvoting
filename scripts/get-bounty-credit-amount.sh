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

projectWalletAddress=$1

projectTokenName=$(remove_quotes $(cat $registeredProjectsFile | jq ". [] | select(.address == \"$projectWalletAddress\") | .tn"))
projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
projectLovelaceAmount=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))


echo $projectLovelaceAmount

