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

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                               \
  --tx-in $qvfInUTxO                                    \
  --tx-in-collateral $(get_first_utxo_of $keyHolder)    \
  --tx-out "$keyHolder"+"$fee"                          \
  --tx-out-inline-datum-file $updatedDatumFile          \
  --change-address $qvfAddress
sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
# }}}
