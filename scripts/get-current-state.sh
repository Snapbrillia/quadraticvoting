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

policySymbols=$(get_symbols_array)
scriptUTxOs=$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))

if [ "$scriptUTxOs" == "$(cat $scriptUTxOsFile)" ]; then
    echo "NoChange"
else 
    $qvf current-state "$policySymbols" "$scriptUTxOs"
    echo $scriptUTxOs > $scriptUTxOsFile
fi 