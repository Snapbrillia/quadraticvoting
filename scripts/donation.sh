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
projectAsset="$regSym.$projectTokenName"

# get project utxo
projectUtxoObj="$(bf_get_utxos_datums_lovelaces $qvfAddress $projectAsset | jq -c '.[0]')"
projectUtxo=$(remove_quotes $(echo $projectUtxoObj | jq -c .utxo))
projectLovelaces=$(remove_quotes $(echo $projectUtxoObj | jq -c .lovelace))
deadlineUTxO=$(remove_quotes $(bf_get_utxos_datums_lovelaces $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))

donatorWalletLabel=$1
donateAmount=$2

donatorIdUTxO=$(get_first_utxo_of $donatorWalletLabel)
donatorOwnerPKH=$(cat $preDir/$donatorWalletLabel.pkh)

donateTokenName="$projectAsset"
donateAsset="$donSym.$donateTokenName"

# get projectUtxo assets
projUTxO="$qvfAddress + $projectLovelaces lovelace + 1 $projectAsset"
donateUTxO="$qvfAddress + $donateAmount lovelace + 1 $donateAsset"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

generate_protocol_params

args="
  --required-signer-hash $donatorOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $projectUtxo                                      \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  --tx-in $donatorIdUTxO                                    \
  --tx-in-collateral $donatorIdUTxO                         \
  --tx-out \"$projUTxO\"                                   \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out \"$donateUTxO\"                                    \
  --tx-out-inline-datum-file $newDatumFile                  \
  --invalid-hereafter $cappedSlot                           \
  --mint \"2 $donateAsset\"                                \
  --mint-tx-in-reference $donRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $donSym                                       \
  --change-address $projectOwnerAddress"

echo -e $args

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $donatorOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $projectUtxo                                      \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  --tx-in $donatorIdUTxO                                    \
  --tx-in-collateral $donatorIdUTxO                         \
  --tx-out "$projUTxO"                                     \
  --tx-out-inline-datum-file $updatedDatumFile                  \
  --tx-out "$donateUTxO"                                      \
  --tx-out-inline-datum-file $newDatumFile                  \           
  --invalid-hereafter $cappedSlot                           \
  --mint "2 $donateAsset"                                  \
  --mint-tx-in-reference $donRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $donSym                                       \
  --change-address $projectOwnerAddress

sign_and_submit_tx $keyHoldersSigningKeyFile $preDir/$1.skey
wait_for_new_slot
# }}}
