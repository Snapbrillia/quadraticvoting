#!/bin/bash

. $HOME/quadraticVoting/scripts/initiation.sh


projectName=$1
projectRequestedFund=$2
projectOwnerAddress=$3
projectOwnerPKH=$4
projectIdUTxO=$5
txInUTxO=$6
txInCollateralUTxO=$7
txOutUTxO=$8

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineAsset="$govAsset.$(cat $deadlineTokenNameHexFile)"
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

govUTxOObj="$(get_script_utxos_datums_values $qvfAddress $govAsset | jq -c '.[0]')"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))
deadlineUTxO=$(remove_quotes $(get_script_utxos_datums_values $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))


$qvf register-project         \
  $projectIdUTxO              \
  $projectOwnerPKH            \
	$projectName                \
	$projectRequestedFund       \
  "$(cat $fileNamesJSONFile)"

# {{{
projectTokenName=$(cat $projectTokenNameFile)
echo $projectTokenName >> $registeredProjectsFile
projectAsset="$regSym.$projectTokenName"
projectDatumFile="$newDatumFile"
projectInfoDatumFile="$preDir/$projectTokenName"

regFeeLovelaces=1500000 # 1.5 ADA, half of the registration fee.
firstUTxO="$qvfAddress + $govLovelaces lovelace + 1 $govAsset"
projUTxO="$qvfAddress + $regFeeLovelaces lovelace + 1 $projectAsset"


qvfRefUTxO=$(cat $qvfRefUTxOFile)
regRefUTxO=$(cat $regRefUTxOFile)

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $projectOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $govUTxO                                          \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  $txInUTxO                                                 \
  $txInCollateralUTxO                                       \
  $txOutUTxO                                                \
  --tx-out "$firstUTxO"                                     \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out "$projUTxO"                                      \
  --tx-out-inline-datum-file $projectInfoDatumFile          \
  --tx-out "$projUTxO"                                      \
  --tx-out-inline-datum-file $projectDatumFile              \
  --invalid-hereafter $cappedSlot                           \
  --mint "2 $projectAsset"                                  \
  --mint-tx-in-reference $regRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $regSym                                       \
  --change-address $projectOwnerAddress                     \
  --cddl-format
  
store_current_slot

JSON_STRING=$( jq -n \
                  --arg bn "$(cat $txBody | jq -r .cborHex)" \
                  --arg on "$(cat $preDir/project-token-name.hex)" \
                  '{transaction: $bn, projectTokenName: $on }' )

echo "---$JSON_STRING"