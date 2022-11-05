#!/bin/bash

. scripts/initiation.sh

projectTokenName=$(project_number_to_token_name "$1")

qvfAddress=$(cat $scriptAddressFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineAsset="$govAsset.$(cat $deadlineTokenNameHexFile)"
projectAsset="$regSym.$projectTokenName"
donAsset="$donSym.$projectTokenName"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

deadlineUTxO=$(remove_quotes $(get_script_utxos_datums_values $qvfAddress $deadlineAsset | jq -c '.[0] | .utxo'))

txInConstant="--spending-tx-in-reference $qvfRefUTxO --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"

# Takes 4 arguments:
#   1. Project's state input,
#   2. Donation(s) inputs,
#   3. Project's state output,
#   4. Extra argument.
build_submit_wait() {
  # {{{
  keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)
  
  generate_protocol_params
  
  buildTx="$cli $BUILD_TX_CONST_ARGS
    --required-signer-hash $keyHoldersPubKeyHash
    ""$1"" ""$2""
    --tx-in $keyHoldersInUTxO
    --tx-in-collateral $keyHoldersInUTxO
    ""$3"" ""$4""
    --change-address $keyHoldersAddress"
  
  tempSh="$preDir/temp_buildTx.sh"
  touch $tempSh
  echo $buildTx > $tempSh
  . $tempSh
  rm -f $tempSh
  
  sign_and_submit_tx $preDir/$keyHolder.skey
  wait_for_new_slot
  store_current_slot
  wait_for_new_slot
  # }}}
}


finished="False"
phase=1
while [ $phase -lt 4 ]; do
  b=8
  if [ $phase -eq 3 ]; then
    b=2
  fi
  constr=9
  if [ $phase -gt 1 ]; then
    constr=10
  fi
  allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
  donUTxOCount=$(echo "$allDonations" | jq length)
  txsNeeded=$(echo $donUTxOCount | jq --arg b "$b" '(. / ($b|tonumber)) | ceil')
  txsDone=0
  while [ $txsDone -lt $txsNeeded ]; do
    projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
    projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
    projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
    echo "$projectCurrDatum" > $currentDatumFile
    projectLovelaces=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))
  
    allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
    donations="$(echo "$allDonations" | jq -c --arg constr "$constr" --arg b "$b" 'map(select((.datum .constructor) == ($constr|tonumber))) | .[0:($b|tonumber)]')"
    totalInAsset="$(echo "$donations" | jq 'map(.assetCount) | reduce .[] as $l (0; . + $l)')"
    elemCount=$(echo "$donations" | jq length)
    initialDonationsArg="$(echo "$donations" | jq -c 'map((.lovelace|tostring) + " " + (.assetCount|tostring) + " " + (.datum | tostring)) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
    donationsArg="$(jq_to_bash_3 "$initialDonationsArg" "$elemCount")"
    resultJSON="$($qvf fold-donations $donationsArg "$(cat $fileNamesJSONFile)")"
    lovelaceCount=$(echo "$resultJSON" | jq '(.lovelace|tonumber)')
    mintCount=$(echo "$resultJSON" | jq '(.mint|tonumber)')
  
    txInArg="$(echo "$donations" | jq --arg consts "$txInConstant" 'map("--tx-in " + .utxo + " " + $consts) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
    txInArg=$(remove_quotes "$txInArg")
    extraArg=""
    projectInput=""
    outputProjectUTxO=""
    if [ $mintCount -lt 0 ]; then
      # In this case, the $lovelaceCount included half the registration fee.
      extraArg="
        --mint \"$mintCount $donAsset\"
        --mint-tx-in-reference $donRefUTxO
        --mint-plutus-script-v2
        --mint-reference-tx-in-redeemer-file $devRedeemer
        --policy-id $donSym
      "
      finished="True"
      projectInput="--tx-in $projectUTxO $txInConstant"
      outputProjectUTxO="--tx-out \"$qvfAddress + $lovelaceCount lovelace + 1 $projectAsset\" --tx-out-inline-datum-file $updatedDatumFile"
    else
      extraArg="
        --tx-out \"$qvfAddress + $lovelaceCount lovelace + $totalInAsset $donAsset\"
        --tx-out-inline-datum-file $newDatumFile
      "
      finished="False"
    fi
    build_submit_wait "$projectInput" "$txInArg" "$outputProjectUTxO" "$extraArg"
    if [ $finished == "True" ]; then
      break 2
    fi
    txsDone=$(expr $txsDone + 1)
  done
  phase=$(expr $phase + 1)
done


# If, at this point, $finished is "True", it means that all the donations have
# consolidated already.

if [ $finished == "False" ]; then
  b=8
  constr=10
  allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
  donUTxOCount=$(echo "$allDonations" | jq length)
  txsNeeded=$(echo $donUTxOCount | jq --arg b "$b" '(. / ($b|tonumber)) | ceil')
  txsDone=0
  while [ $txsDone -lt $txsNeeded ]; do
    projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
    projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
    projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
    echo "$projectCurrDatum" > $currentDatumFile
    projectLovelaces=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))
  
    allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
    donations="$(echo "$allDonations" | jq -c --arg constr "$constr" --arg b "$b" 'map(select((.datum .constructor) == ($constr|tonumber))) | .[0:($b|tonumber)]')"
    totalInAsset="$(echo "$donations" | jq 'map(.assetCount) | reduce .[] as $l (0; . + $l)')"
    elemCount=$(echo "$donations" | jq length)
    initialDonationsArg="$(echo "$donations" | jq -c 'map((.lovelace|tostring) + " " + (.assetCount|tostring) + " " + (.datum | tostring)) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
    donationsArg="$(jq_to_bash_3 "$initialDonationsArg" "$elemCount")"
    resultJSON="$($qvf consolidate-donations $donationsArg "$(cat $fileNamesJSONFile)")"
    lovelaceCount=$(echo "$resultJSON" | jq '(.lovelace|tonumber)')
    mintCount=$(echo "$resultJSON" | jq '(.mint|tonumber)')
    txInArg="$(echo "$donations" | jq --arg consts "$txInConstant" 'map("--tx-in " + .utxo + " " + $consts) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
    txInArg=$(remove_quotes "$txInArg")
    mintArg="
      --mint \"$mintCount $donAsset\"
      --mint-tx-in-reference $donRefUTxO
      --mint-plutus-script-v2
      --mint-reference-tx-in-redeemer-file $devRedeemer
      --policy-id $donSym
    "
    projectInput="--tx-in $projectUTxO $txInConstant"
    outputProjectUTxO="--tx-out \"$qvfAddress + $lovelaceCount lovelace + 1 $projectAsset\" --tx-out-inline-datum-file $updatedDatumFile"

    build_submit_wait "$projectInput" "$txInArg" "$outputProjectUTxO" "$extraArg"

    txsDone=$(expr $txsDone + 1)
  done
fi



