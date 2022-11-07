#!/bin/bash

. $HOME/quadraticVoting/scripts/initiation.sh


projectTokenName=$1
donationAmount=$2
donorPKH=$3
donorAddress=$4
txInUTxO=$5
txInCollateralUTxO=$6
txOutUTxO=$7

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineAsset="$govAsset.$(cat $deadlineTokenNameHexFile)"
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)
projectAsset="$regSym.$projectTokenName"
donAsset="$donSym.$projectTokenName"

# Get the project UTxO. Since the first project UTxO produced at the
# registration phase is the one carrying the static UTxO, index 1 is used to
# get the UTxO with the "state" datum attached.
projectUTxOObj="$(get_script_utxos_datums_values $qvfAddress $projectAsset | jq -c '.[1]')"
projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
echo "$projectCurrDatum" > $currentDatumFile
projectLovelaces=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))

deadlineUTxO=$(remove_quotes $(get_script_utxos_datums_values $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))


$qvf donate-to-project        \
  $donorPKH                   \
  $projectTokenName           \
	$donationAmount             \
  "$(cat $fileNamesJSONFile)"

outputProjectUTxO="$qvfAddress + $projectLovelaces lovelace + 1 $projectAsset"
donationUTxO="$qvfAddress + $donationAmount lovelace + 1 $donAsset"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $donorPKH                          \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $projectUTxO                                      \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  $txInUTxO                                                 \
  $txInCollateralUTxO                                       \
  $txOutUTxO                                                \
  --tx-out "$outputProjectUTxO"                             \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out "$donationUTxO"                                  \
  --tx-out-inline-datum-file $newDatumFile                  \
  --invalid-hereafter $cappedSlot                           \
  --mint "1 $donAsset"                                      \
  --mint-tx-in-reference $donRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $donSym                                       \
  --change-address $donorAddress                            \
  --cddl-format

store_current_slot

JSON_STRING=$( jq -n \
                  --arg bn "$(cat $txBody | jq -r .cborHex)" \
                  '{transaction: $bn }' )

echo "---$JSON_STRING"
# }}}
