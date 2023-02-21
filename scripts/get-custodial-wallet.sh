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

walletLabel=$1
# Check if wallet exists, or if not create it.
if [ -f $custodialWalletsDir/$walletLabel.addr ]; then
  cat $custodialWalletsDir/$walletLabel.addr
else 
  generate_wallet $custodialWalletsLabel/$walletLabel
  cat $custodialWalletsDir/$walletLabel.addr
fi
