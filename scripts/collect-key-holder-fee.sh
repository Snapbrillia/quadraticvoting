#!/bin/bash

. scripts/env.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
fee=$(qvf-cli collect-key-holder-fee "$(cat $fileNamesJSONFile)")

# TODO more string processing of fee is likely
# Should be easy and straightforward to implement, below
fee=$fee

#give_lovelace $qvfAddress $keyHolder $fee

qvfUTxOObj="$(get_script_utxos_datums_values $qvfAddress $govAsset | jq -c '.[0]')"
qvfUTxO=$(remove_quotes $(echo $qvfUTxOObj | jq -c .utxo))

# WARNINGL are the following two lines needed?
qvfCurrDatum="$(echo $qvfUTxOObj | jq -c .datum)"
echo "$qvfCurrDatum" > $currentDatumFile

generate_protocol_params

# WARNING:Unsure about where to get .skey in sign_and_submit_tx below.

$cli $BUILD_TX_CONST_ARGS                                   \
  --tx-in $qvfInUTxO                                      \
  --tx-in-collateral $qvfInUTxO                           \
  --tx-out "$keyHolder"+"$fee"                             \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out "$donationUTxO"                                  \
  --tx-out-inline-datum-file $newDatumFile                  \
  --change-address $qvfAddress
sign_and_submit_tx $preDir/$referenceWallet.skey
wait_for_new_slot
# }}}
