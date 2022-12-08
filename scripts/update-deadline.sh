#!/bin/bash

. $REPO/scripts/initiation.sh

export deadline=$1

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
inUTxO=$(get_first_utxo_of $keyHolder)

dlUTxOObj="$(get_deadline_utxo)"
dlUTxO=$(echo "$dlUTxOObj" | jq -r .utxo)
dlLovelaces=$(echo "$dlUTxOObj" | jq -r .lovelaces)
dlOutput="$qvfAddress + $dlLovelaces lovelace + 1 $govAsset"
dlDatum=$(getFileName ocfnDeadlineDatum)

# Using `qvf-cli` to write the proper datum and redeemer to disk:
$qvf update-deadline          \
  $deadline                   \
  "$(cat $fileNamesJSONFile)"

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $keyHoldersPubKeyHash              \
  --tx-in $dlUTxO                                           \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  --tx-in $inUTxO --tx-in-collateral $inUTxO                \
  --tx-out "$dlOutput"                                      \
  --tx-out-inline-datum-file $dlDatum                       \
  --change-address $keyHoldersAddress   

sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
store_current_slot
wait_for_new_slot
