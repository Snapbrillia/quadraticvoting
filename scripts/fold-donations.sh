#!/bin/bash

. scripts/initiation.sh

projectTokenName=$(project_number_to_token_name $1)

qvfAddress=$(cat $scriptAddressFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineAsset="$govAsset.$(cat $deadlineTokenNameHexFile)"
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)
projectAsset="$regSym.$projectTokenName"
donAsset="$donSym.$projectTokenName"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

projectUTxOObj="$(get_script_utxos_datums_values $qvfAddress $projectAsset | jq -c '.[1]')"
projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
echo "$projectCurrDatum" > $currentDatumFile
projectLovelaces=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))

deadlineUTxO=$(remove_quotes $(get_script_utxos_datums_values $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))

allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"

txInConstant="--spending-tx-in-reference $qvfRefUTxO --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"
txInArg="$(echo "$allDonations" | jq --arg consts "$txInConstant" 'map("--tx-in " + .utxo + " " + $consts) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
txInArg=$(remove_quotes "$txInArg")

initialDonationsArg="$(echo "$allDonations" | jq 'map((.lovelace|tostring) + " " + (.assetCount|tostring) + " " + (.datum | tostring)) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
count=1
last=$(echo "$allDonations" | jq '(3 * length)')
if [ $last -eq 0 ]; then
  return 1
fi
donationsArg=""
for i in $initialDonationsArg; do
  if [ $count -eq 1 ]; then
    donationsArg="$(remove_quotes $i)"
  elif [ $count -eq $last ]; then
    lastCharRemoved=${i::-1}
    donationsArg="$donationsArg $(remove_back_slashes $lastCharRemoved)"
  else
    donationsArg="$donationsArg $(remove_back_slashes $i)"
  fi
  count=$(expr $count + 1)
done

resultJSON="$($qvf fold-donations $donationsArg "$(cat $fileNamesJSONFile)")"

mintCount=$(echo "$resultJSON" | jq '(.mint|tonumber)')
mintArg=""
if [ $mintCount -lt 0 ]; then
  mintArg="--mint \"$mintCount $donAsset\""
  mintArg="$mintArg --mint-tx-in-reference $donRefUTxO"
  mintArg="$mintArg --mint-plutus-script-v2"
  mintArg="$mintArg --mint-reference-tx-in-redeemer-file $devRedeemer"
  mintArg="$mintArg --policy-id $donSym"
fi

lovelaceCount=$(echo "$resultJSON" | jq '(.lovelace|tonumber)')

keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)
outputProjectUTxO="$qvfAddress + $lovelaceCount lovelace + 1 $projectAsset"

generate_protocol_params

buildTx="$cli $BUILD_TX_CONST_ARGS             \
  --required-signer-hash $keyHoldersPubKeyHash \
  --read-only-tx-in-reference $deadlineUTxO    \
  --tx-in $projectUTxO $txInConstant $txInArg  \
  --tx-in $keyHoldersInUTxO                    \
  --tx-in-collateral $keyHoldersInUTxO         \
  --tx-out \"$outputProjectUTxO\"              \
  --tx-out-inline-datum-file $updatedDatumFile \
  $mintArg                                     \
  --change-address $keyHoldersAddress"

tempSh="$preDir/temp_buildTx.sh"
touch $tempSh
echo $buildTx > $tempSh
. $tempSh
rm -f $tempSh

sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
