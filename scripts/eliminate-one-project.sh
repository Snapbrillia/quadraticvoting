#!/bin/bash

. $REPO/scripts/initiation.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)

qvfRefUTxO=$(cat $qvfRefUTxOFile)
regRefUTxO=$(cat $regRefUTxOFile)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

allProjectInfoUTxOs="$(get_all_projects_info_utxos_datums_values)"
allProjectStateUTxOs="$(get_all_projects_state_utxos_datums_values)"



result=$($qvf eliminate-one-project    \
  "$govUTxOObj"                        \
  "$allProjectInfoUTxOs"               \
  "$allProjectStateUTxOs"              \
  "$(cat $fileNamesJSONFile)"
  )

txInConstant="--spending-tx-in-reference $qvfRefUTxO    \
  --spending-plutus-script-v2                           \
  --spending-reference-tx-in-inline-datum-present       \
  --spending-reference-tx-in-redeemer-file $devRedeemer
  "

inputs=$(echo "$result" | jq -c .inputs)
refs=$(echo "$result" | jq -c .refs)
outputs=$(echo "$result" | jq -c .outputs)

refArgs=$(qvf_output_to_tx_ins "--read-only-tx-in-reference" "" "$refs")
inputArgs=$(qvf_output_to_tx_ins "--tx-in" "$txInConstant" "$inputs")
outputArgs=$(qvf_output_to_tx_outs "$outputs")

keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)

generate_protocol_params
