if [ -z $REPO ]; then
  echo
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

. $REPO/scripts/env.sh

scriptUTxOs=$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))

if [ "$scriptUTxOs" == "$(cat $scriptUTxOsFile)" ]; then
  return 0
fi

echo $scriptUTxOs > $scriptUTxOsFile


if [ "$1" == "--pretty" ]; then
  $qvf pretty-leaderboard "$scriptUTxOs"
else
  $qvf emulate-outcome "$scriptUTxOs"
fi
