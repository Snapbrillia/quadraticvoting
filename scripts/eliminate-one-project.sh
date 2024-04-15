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

keyHoldersAddress=$(cat $preDir/$keyHolder.addr)
keyHoldersPubKeyHash=$(cat $preDir/$keyHolder.pkh)
qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)

qvfRefUTxO=$(cat $qvfRefUTxOFile)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
noMoreLeftToEliminate=$($qvf datum-is DistributionProgress "$govCurrDatum")
if [ $noMoreLeftToEliminate == "True" ]; then
  echo "All non-eligible projects are eliminated."
  return 1
fi
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

allProjectInfoUTxOs="$(get_all_projects_info_utxos_datums_values)"
allProjectStateUTxOs="$(get_all_projects_state_utxos_datums_values)"

result=$($qvf eliminate-one-project \
  "$govUTxOObj"                     \
  "$allProjectInfoUTxOs"            \
  "$allProjectStateUTxOs"           \
  "$(cat $registeredProjectsFile)"  \
  "$(cat $fileNamesJSONFile)"
  )

txInConstant="--spending-tx-in-reference $qvfRefUTxO    \
  --spending-plutus-script-v2                           \
  --spending-reference-tx-in-inline-datum-present       \
  --spending-reference-tx-in-redeemer-file $devRedeemer
  "

echo $result

inputs=$(echo "$result" | jq -c .inputs)
refs=$(echo "$result" | jq -c .refs)
outputs=$(echo "$result" | jq -c .outputs)

echo
echo "================================================"
echo "$result" | jq -c .extra
echo "================================================"
echo

refArgs=$(qvf_output_to_tx_ins "--read-only-tx-in-reference" "" "$refs")
inputArgs=$(qvf_output_to_tx_ins "--tx-in" "$txInConstant" "$inputs")
outputArgs=$(qvf_output_to_tx_outs "$outputs")

keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)

generate_protocol_params

buildTx="$cli $BUILD_TX_CONST_ARGS
  --required-signer-hash $keyHoldersPubKeyHash
  $refArgs $inputArgs $outputArgs
  --tx-in $keyHoldersInUTxO
  --tx-in-collateral $keyHoldersInUTxO
  --change-address $keyHoldersAddress
  "

rm -f $tempBashFile
touch $tempBashFile
echo $buildTx > $tempBashFile
. $tempBashFile

sign_and_submit_tx $preDir/$keyHolder.skey
store_current_slot_3 $keyHolder $scriptLabel $projectTokenName
wait_for_new_slot $keyHolder
store_current_slot_3 $keyHolder $scriptLabel $projectTokenName
wait_for_new_slot $keyHolder
