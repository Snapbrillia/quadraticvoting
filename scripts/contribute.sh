#!/bin/bash


if [ "$ENV" == "dev" ]; then
. $REPO/scripts/initiation.sh
  sponsorWalletLabel=$1
  contributionAmount=$2
  sponsorInputUTxO=$(get_first_utxo_of $sponsorWalletLabel)
  sponsorAddress=$(cat $preDir/$sponsorWalletLabel.addr)
  txInUTxO="--tx-in $sponsorInputUTxO"
  txInCollateralUTxO="--tx-in-collateral $sponsorInputUTxO"
  txOutUTxO=""
else
. $HOME/quadraticvoting/scripts/initiation.sh
  sponsorAddress=$1
  contributionAmount=$2
  txInUTxO=$3
  txInCollateralUTxO=$4
  txOutUTxO=$5
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))
deadlineUTxO=$(remove_quotes $(get_deadline_utxo | jq -c '.utxo'))
qvfRefUTxO=$(cat $qvfRefUTxOFile)
newGovLovelaces=$(expr $govLovelaces + $contributionAmount)
govOutput="$qvfAddress + $newGovLovelaces lovelace + 1 $govAsset"

# Using `qvf-cli` to write the proper redeemer to disk:
$qvf contribute               \
  $contributionAmount         \
  "$(cat $fileNamesJSONFile)"

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                       \
  --read-only-tx-in-reference $deadlineUTxO                     \
  --tx-in $govUTxO                                              \
  --spending-tx-in-reference $qvfRefUTxO                        \
  --spending-plutus-script-v2                                   \
  --spending-reference-tx-in-inline-datum-present               \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile     \
  $txInUTxO $txInCollateralUTxO $txOutUTxO                      \
  --tx-out "$govOutput"                                         \
  --tx-out-inline-datum-file $currentDatumFile                  \
  --invalid-hereafter $cappedSlot                               \
  --change-address $sponsorAddress      

if [ "$ENV" == "dev" ]; then
  sign_and_submit_tx $preDir/$sponsorWalletLabel.skey
  wait_for_new_slot
else
  store_current_slot
  JSON_STRING=$( jq -n                         \
    --arg bn "$(cat $txBody | jq -r .cborHex)" \
    '{transaction: $bn}' )
  echo "---$JSON_STRING"
fi
