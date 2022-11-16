#!/bin/bash

. scripts/initiation.sh

projectTokenName=$(project_number_to_token_name "$1")

qvfAddress=$(cat $scriptAddressFile)
regSym=$(cat $regSymFile)
projectAsset="$regSym.$projectTokenName"
qvfRefUTxO=$(cat $qvfRefUTxOFile)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

projectUTxOs="$(get_script_utxos_datums_values $qvfAddress $projectAsset)"
infoUTxOObj=""
prizeUTxOObj=""
temp0="$(echo "$projectUTxOs" | jq -c 'map(.datum) | .[0]')"
isInfo=$($qvf datum-is ProjectInfo "$temp0")
if [ $isInfo == "True" ]; then
  infoUTxOObj="$(echo "$projectUTxOs" | jq -c '.[0]')"
  prizeUTxOObj="$(echo "$projectUTxOs" | jq -c '.[1]')"
else
  infoUTxOObj="$(echo "$projectUTxOs" | jq -c '.[1]')"
  prizeUTxOObj="$(echo "$projectUTxOs" | jq -c '.[0]')"
fi
infoDatum="$(echo "$infoUTxOObj" | jq -c .datum)"
prizeDatum="$(echo "$prizeUTxOObj" | jq -c .datum)"
infoUTxO="$(echo "$infoUTxOObj" | jq .utxo)"
prizeUTxO="$(echo "$prizeUTxOObj" | jq .utxo)"

qvfResult="$($qvf distribute-prize "$infoDatum" "$prizeDatum" "$(cat $fileNamesJSONFile)")"
ownerLovelaces=$(echo "$qvfResult" | jq '.owner | tonumber')
escrowLovelaces=$(echo "$qvfResult" | jq '.escrow | tonumber')

ownersInUTxO=$(get_first_utxo_of $1)
txInConstant="--spending-tx-in-reference $qvfRefUTxO --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"
govOut="$qvfAddress + $(expr $govLovelaces - $ownerLovelaces) lovelace + 1 $govAsset"
escrowOut="$qvfAddress + $escrowLovelaces lovelace + 1 $projectAsset"
ownerAddr=$(cat $preDir/$1.addr)
ownerOut="$ownerAddr + $ownerLovelaces lovelace"

generate_protocol_params

buildTx="$cli $BUILD_TX_CONST_ARGS
  --required-signer-hash $keyHoldersPubKeyHash
  --read-only-tx-in-reference $infoUTxO
  --tx-in $ownersInUTxO
  --tx-in-collateral $ownersInUTxO
  --tx-in $govUTxO $txInConstant
  --tx-in $prizeUTxO $txInConstant
  --tx-out \"$govOut\"
  --tx-out-inline-datum-file $updatedDatumFile
  --tx-out \"$escrowOut\"
  --tx-out-inline-datum-file $newDatumFile
  --tx-out \"$ownerOut\"
  --change-address $ownerAddr
  "

echo $buildTx > $tempBashFile
. $tempBashFile

sign_and_submit_tx $preDir/$1.skey $preDir/$keyHolder.skey
wait_for_new_slot
store_current_slot
wait_for_new_slot
