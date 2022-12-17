. $REPO/scripts/initiation.sh

if [ "$1" == "--pretty" ]; then
  $qvf pretty-leaderboard "$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))"
else
  $qvf emulate-outcome "$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))"
fi
