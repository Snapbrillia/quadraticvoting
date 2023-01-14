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

walletLabel=$1
UTxOPresent=$(get_first_utxo_of $custodialWalletLabel/$walletLabel)
if [ -z "$UTxOPresent" ]; then
  echo "True"
else 
  echo "False"
fi
