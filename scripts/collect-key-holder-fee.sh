#!/bin/bash

. scripts/initiation.sh


qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
govUTxOObj="$(get_script_utxos_datums_values $qvfAddress $govAsset | jq -c '.[0]')"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

keyHolderFee=$($qvf collect-key-holder-fee "$(cat $fileNamesJSONFile)")
echo $keyHolderFee

govOutput="$qvfAddress + $(expr $govLovelaces - $keyHolderFee) lovelace + 1 $govAsset"
qvfRefUTxO=$(cat $qvfRefUTxOFile)
keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                               \
  --required-signer-hash $keyHoldersPubKeyHash          \
  --tx-in-collateral $keyHoldersInUTxO                  \
  --tx-in $govUTxO                                      \
  --spending-tx-in-reference $qvfRefUTxO                \
  --spending-plutus-script-v2                           \
  --spending-reference-tx-in-inline-datum-present       \
  --spending-reference-tx-in-redeemer-file $devRedeemer \
  --tx-out "$govOutput"                                 \
  --tx-out-inline-datum-file $updatedDatumFile          \
  --change-address $keyHoldersAddress  

sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
store_current_slot
wait_for_new_slot
