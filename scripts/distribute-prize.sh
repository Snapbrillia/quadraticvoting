#!/bin/bash

. $REPO/scripts/initiation.sh

if [ "$ENV" == "dev" ]; then
  projectTokenName=$(project_index_to_token_name "$1")
else
  projectTokenName=$1
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)

qvfRefUTxO=$(cat $qvfRefUTxOFile)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(echo $govUTxOObj | jq -r .utxo)
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(echo $govUTxOObj | jq -r .lovelace)

projectsInfoUTxOObj="$(get_projects_info_utxo $projectTokenName)"
projectsStateUTxOObj="$(get_projects_state_utxo $projectTokenName)"
ownerAddrStr=$(get_projects_owner_address "$projectTokenName")

# ownerAddrStr : govInputStr : infoInputStr: projInputStr : fileNamesJSON
result=$($qvf distribute-prize  \
  $ownerAddrStr                 \
  "$govUTxOObj"                 \
  "$projectsInfoUTxOObj"        \
  "$projectsStateUTxOObj"       \
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
