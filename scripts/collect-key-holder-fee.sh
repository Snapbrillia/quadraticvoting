#!/bin/bash

. scripts/env.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
qvfUTxOObj="$(get_script_utxos_datums_values $qvfAddress $govAsset | jq -c '.[0]')"
qvfUTxO=$(remove_quotes $(echo $qvfUTxOObj | jq -c .utxo))

qvfCurrDatum="$(echo $qvfUTxOObj | jq -c .datum)"
echo "$qvfCurrDatum" > $currentDatumFile

fee=$(qvf-cli collect-key-holder-fee "$(cat $fileNamesJSONFile)")

# TODO more string processing of fee is likely
# Should be easy and straightforward to implement, below
fee=$fee

qvfLovelaces=$(remove_quotes $(echo $qvfUTxOObj | jq -c .lovelace))
qvfLovelacesMinusFee=$(expr $qvfLovelaces - $fee)
firstUTxO="$qvfAddress + $qvfLovelacesMinusFee lovelace + 1 $govAsset"

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                               \
  --tx-in $qvfInUTxO                                    \
  --spending-tx-in-reference $(cat $qvfRefUTxOFile)     \
  --spending-plutus-script-v2                           \
  --spending-reference-tx-in-inline-datum-present       \
  --spending-reference-tx-in-redeemer-file $devRedeemer \ 
  --tx-in-collateral $(get_first_utxo_of $keyHolder)    \
  --tx-out "$keyHoldersAddress" +"$fee"                 \
  --tx-out "$firstUTxO"                                 \
  --tx-out-inline-datum-file $updatedDatumFile          \
  --change-address $keyHoldersAddress
sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
# }}}
