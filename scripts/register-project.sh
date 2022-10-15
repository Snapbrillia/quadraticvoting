#!/bin/bash

. scripts/env.sh
. scripts/initiation.sh
. scripts/blockfrost.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineAsset="$govAsset$(cat $deadlineTokenNameHexFile)"
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

govUTxOObj="$(bf_get_utxos_datums_lovelaces $qvfAddress $govAsset | jq -c '.[0]')"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))
deadlineUTxO=$(remove_quotes $(bf_get_utxos_datums_lovelaces $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))

projectOwnerWalletLabel=$1
projectName=$2
projectRequestedFund=$3
projectOwnerAddress=$(cat $preDir/$projectOwnerWalletLabel.addr)

projectIdUTxO=$(get_first_utxo_of $projectOwnerWalletLabel)
projectOwnerPKH=$(cat $preDir/$projectOwnerWalletLabel.pkh)

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

args="
  --required-signer-hash $projectOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $govUTxO                                          \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  --tx-in $projectIdUTxO                                    \
  --tx-in-collateral $projectIdUTxO                         \
  --tx-out \"$firstUTxO\"                                   \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out \"$projUTxO\"                                    \
  --tx-out-inline-datum-file $projectInfoDatumFile          \
  --tx-out \"$projUTxO\"                                    \
  --tx-out-inline-datum-file $projectDatumFile              \
  --invalid-hereafter $cappedSlot                           \
  --mint \"2 $projectAsset\"                                \
  --mint-tx-in-reference $regRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $regSym                                       \
  --change-address $projectOwnerAddress"

echo -e $args

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $projectOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $govUTxO                                          \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  --tx-in $projectIdUTxO                                    \
  --tx-in-collateral $projectIdUTxO                         \
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
  --change-address $projectOwnerAddress

sign_and_submit_tx $keyHoldersSigningKeyFile $preDir/$1.skey
wait_for_new_slot
# }}}
