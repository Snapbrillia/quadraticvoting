#!/bin/bash

. scripts/initiation.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)

deadlineAsset="$govAsset.$(cat $deadlineTokenNameHexFile)"
deadlineUTxO=$(remove_quotes $(get_script_utxos_datums_values $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

mkdir -p $projsPreDir

govUTxOObj="$(get_script_utxos_datums_values $qvfAddress $govAsset | jq -c '.[0]')"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

allProjectStateUTxOs="$(get_all_projects_state_utxos_datums_values | jq -c '.[0:8]')"

txInConstant="--spending-tx-in-reference $qvfRefUTxO --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"
txInArg="$(echo "$allProjectStateUTxOs" | jq --arg consts "$txInConstant" 'map("--tx-in " + .utxo + " " + $consts) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
txInArg=$(remove_quotes "$txInArg")

elemCount=$(echo "$allProjectStateUTxOs" | jq length)
initialProjectsArg="$(echo "$allProjectStateUTxOs" | jq -c 'map((.asset | split(".") | .[1]) + " " + (.lovelace|tostring) + " " + (.datum|tostring)) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
projectsArg="$(jq_to_bash_3 "$initialProjectsArg" "$elemCount")"

accumulatedLovelaces=$($qvf accumulate-donations $projectsArg "$(cat $fileNamesJSONFile)")
govOutput="$qvfAddress + $accumulatedLovelaces lovelace + 1 $govAsset"

commonProjOutPrefix="--tx-out \"$qvfAddress + 1500000 lovelace + 1 $regSym"
projsOuts=""
for tn in $(cat $registeredProjectsFile); do
  projsOuts="$projsOuts $commonProjOutPrefix.$tn\" --tx-out-inline-datum-file $projsPreDir/$tn.datum"
done

keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)

generate_protocol_params

buildTx="$cli $BUILD_TX_CONST_ARGS               \
  --required-signer-hash $keyHoldersPubKeyHash   \
  --read-only-tx-in-reference $deadlineUTxO      \
  --tx-in $govUTxO $txInConstant $txInArg        \
  --tx-in $keyHoldersInUTxO                      \
  --tx-in-collateral $keyHoldersInUTxO           \
  --tx-out \"$govOutput\"                        \
  --tx-out-inline-datum-file $updatedDatumFile   \
  $projsOuts --change-address $keyHoldersAddress
  "

tempSh="$preDir/temp_buildTx.sh"
touch $tempSh
echo $buildTx > $tempSh
. $tempSh
rm -f $tempSh

sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot

finished="$($qvf datum-is DonationAccumulationConcluded "$(cat $updatedDatumFile)")"

if [ $finished == "False" ]; then
  . scripts/accumulate-donations.sh
fi
