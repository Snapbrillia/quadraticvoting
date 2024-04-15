#!/bin/bash

. $REPO/scripts/env.sh

export deadline=$1

keyHoldersAddress=$(cat $preDir/$keyHolder.addr)
keyHoldersPubKeyHash=$(cat $preDir/$keyHolder.pkh)
qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
inUTxO=$(get_first_utxo_of $keyHolder)

deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

dlUTxOObj="$(get_deadline_utxo)"
dlUTxO=$(echo "$dlUTxOObj" | jq -r .utxo)
dlLovelaces=$(echo "$dlUTxOObj" | jq -r .lovelace)
dlOutput="$qvfAddress + $dlLovelaces lovelace + 1 $govAsset"
dlDatum=$(getFileName ocfnDeadlineDatum)

echo_for_dev "Getting the current slot..."
currentSlot=$(get_newest_slot)

# Using `qvf-cli` to write the proper datum and redeemer to disk, along with
# the new slot number.
qvfResult=$($qvf update-deadline \
  $currentSlot                   \
  $deadline                      \
  "$(cat $fileNamesJSONFile)"
)

echo_for_dev "$qvfResult"

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
  --invalid-hereafter $cappedSlot                           \
  --change-address $keyHoldersAddress   

sign_and_submit_tx $preDir/$keyHolder.skey
store_current_slot_2 "deadline" $keyHolder
wait_for_new_slot "deadline"
store_current_slot_2 "deadline" $keyHolder
wait_for_new_slot "deadline"
