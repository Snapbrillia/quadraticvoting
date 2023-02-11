#!/bin/bash

MAX_SPENDABLE_UTXOS=8

. $REPO/scripts/initiation.sh

projectTokenName=$(project_index_to_token_name "$1")
startingPhase=$2

qvfAddress=$(cat $scriptAddressFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
projectAsset="$regSym.$projectTokenName"
donAsset="$donSym.$projectTokenName"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)

deadlineUTxO=$(get_deadline_utxo | jq -r '.utxo')

txInConstant="--spending-tx-in-reference $qvfRefUTxO --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"

allDonations=""
donations=""
totalInAsset=0
projectUTxO=""
projectInLovelace=0
resultJSON=""
lovelaceCount=0
mintCount=0
txInArg=""
mintArg=""


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
  
  echo $buildTx > $tempBashFile
  . $tempBashFile
  
  sign_and_submit_tx $preDir/$keyHolder.skey
  store_current_slot_2 $scriptLabel $keyHolder
  wait_for_new_slot $scriptLabel
  store_current_slot_2 $scriptLabel $keyHolder
  wait_for_new_slot $scriptLabel
  # }}}
}


# Takes no arguments.
mkMintArg() {
  # {{{
  mintArg="
    --mint \"$mintCount $donAsset\"
    --mint-tx-in-reference $donRefUTxO
    --mint-plutus-script-v2
    --mint-reference-tx-in-redeemer-file $devRedeemer
    --policy-id $donSym
  "
  # }}}
}


# Takes 3 arguments:
#   1. Input count,
#   2. Constructor index,
#   3. `qvf-cli` endpoint.
iteration_helper() {
  # {{{
  projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
  projectUTxO=$(echo $projectUTxOObj | jq -r .utxo)
  projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
  projectInLovelace="$(echo $projectUTxOObj | jq -c .lovelace)"
  echo "$projectCurrDatum" > $currentDatumFile
  
  donations="$(echo "$allDonations" | jq -c --arg b "$1" --arg constr "$2" 'map(select((.datum .constructor) == ($constr|tonumber))) | .[0:($b|tonumber)]')"
  allDonations="$(echo "$allDonations" | jq -c --arg b "$1" --arg constr "$2" 'map(select((.datum .constructor) == ($constr|tonumber))) | .[($b|tonumber):]')"
  totalInAsset="$(echo "$donations" | jq 'map(.assetCount) | reduce .[] as $l (0; . + $l)')"
  elemCount=$(echo "$donations" | jq length)
  initialDonationsArg="$(echo "$donations" | jq -c 'map((.lovelace|tostring) + " " + (.assetCount|tostring) + " " + (.datum | tostring)) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
  donationsArg="$(jq_to_bash_3 "$initialDonationsArg" "$elemCount")"
  resultJSON="$($qvf "$3" $donationsArg "$(cat $fileNamesJSONFile)")"
  echo $resultJSON
  lovelaceCount=$(echo "$resultJSON" | jq '(.lovelace|tonumber)')
  mintCount=$(echo "$resultJSON" | jq '(.mint|tonumber)')
  txInArg="$(echo "$donations" | jq -r --arg consts "$txInConstant" 'map("--tx-in " + .utxo + " " + $consts) | reduce .[] as $l (""; if . == "" then $l else . + " " + $l end)')"
  # txInArg=$(remove_quotes "$txInArg")
  # }}}
}


finished="False"
phase="$startingPhase"
while [ $phase -lt 4 ]; do
  # {{{
  b=$MAX_SPENDABLE_UTXOS
  if [ $phase -eq 3 ]; then
    b=2
  fi
  constr=$($qvf get-constr-index Donation)
  if [ $phase -gt 1 ]; then
    constr=$($qvf get-constr-index Donations)
  fi
  allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
  donUTxOCount=$(echo "$allDonations" | jq length)
  if [ $donUTxOCount -eq 0 ]; then
    if [ $phase -gt 1 ]; then
      echo "No donations found."
      return 1
    else
      govUTxOObj="$(get_governance_utxo)"
      projectsInfoUTxOObj="$(get_projects_info_utxo $projectTokenName)"
      projectsStateUTxOObj="$(get_projects_state_utxo $projectTokenName)"

      govUTxO=$(echo $govUTxOObj | jq -r .utxo)
      govLovelaces=$(echo $govUTxOObj | jq -r .lovelace)
      projectsInfoUTxO=$(echo $projectsInfoUTxOObj | jq -r .utxo)
      projectsStateUTxO=$(echo $projectsStateUTxOObj | jq -r .utxo)

      ownerAddrStr=$(get_projects_owner_address "$projectTokenName")
      qvfRes=$($qvf remove-donationless-project \
        "$govUTxOObj"                           \
        "$projectsInfoUTxOObj"                  \
        "$projectsStateUTxOObj"                 \
        "$(cat $fileNamesJSONFile)"
      )
      if [ "$ENV" == "dev" ]; then
        echo $qvfRes
      fi
      mintArg="
        --mint \"-2 $projectAsset\"
        --mint-tx-in-reference $(cat $regRefUTxOFile)
        --mint-plutus-script-v2
        --mint-reference-tx-in-redeemer-file $minterRedeemerFile
        --policy-id $regSym
      "
      collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)
      generate_protocol_params
      buildTx="$cli $BUILD_TX_CONST_ARGS
        --tx-in $govUTxO           $txInConstant
        --tx-in $projectsInfoUTxO  $txInConstant
        --tx-in $projectsStateUTxO $txInConstant
        --tx-in-collateral $collateralUTxO
        $mintArg
        --tx-out \"$qvfAddress + $govLovelaces lovelace + 1 $govAsset\"
        --tx-out-inline-datum-file $updatedDatumFile
        --change-address $ownerAddrStr
      "
      echo $buildTx > $tempBashFile
      . $tempBashFile
      sign_and_submit_tx $preDir/$collateralKeyHolder.skey
      store_current_slot_2 $scriptLabel $collateralKeyHolder
      wait_for_new_slot $scriptLabel
      store_current_slot_2 $scriptLabel $collateralKeyHolder
      wait_for_new_slot $scriptLabel
    fi
  fi
  txsNeeded=$(echo $donUTxOCount | jq --arg b "$b" '(. / ($b|tonumber)) | ceil')
  txsDone=0
  while [ $txsDone -lt $txsNeeded ]; do
    # {{{
    iteration_helper "$b" "$constr" "fold-donations"
    extraArg=""
    projectInput=""
    outputProjectUTxO=""
    if [ $mintCount -lt 0 ]; then
      # In this case, the $lovelaceCount included half the registration fee.
      mkMintArg
      extraArg="$mintArg"
      # extraArg="
      #   --mint \"$mintCount $donAsset\"
      #   --mint-tx-in-reference $donRefUTxO
      #   --mint-plutus-script-v2
      #   --mint-reference-tx-in-redeemer-file $devRedeemer
      #   --policy-id $donSym
      # "
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
    # }}}
  done
  phase=$(expr $phase + 1)
  # }}}
done


# If, at this point, $finished is "True", it means that all the donations have
# consolidated already.
#
# Otherwise, the folded donations need to be "traversed" and "consolidated."

b=$(expr $MAX_SPENDABLE_UTXOS - 2)
if [ $finished == "False" ]; then
  # {{{
  # The first step is to go over all the folded donations, and check if there
  # are any duplicates. If a pair is found, a transaction is submitted to merge
  # the duplicate into the first donation map.
  allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
  allDonationsCount=$(echo "$allDonations" | jq length)
  if [ $allDonationsCount -le $b ]; then
    echo "No need for traversal. Re-folding to trigger the consolidation."
    . scripts/fold-donations.sh $1 2
  fi
  i=0
  echo "Traversing folded donations..."
  while [ $i -lt $allDonationsCount ]; do
    # {{{
    i=$(expr $i + 1)
    donationPairs="$(echo "$allDonations" | jq -c '.[0] as $l | .[1:] | map([$l,.])')"
    allDonations="$(echo "$allDonations" | jq -c '.[1:]')"
    pairCount="$(echo "$donationPairs" | jq length)"
    for j in $(seq 0 $(expr $pairCount - 1)); do
      # {{{
      thePair="$(echo "$donationPairs" | jq -c --arg j "$j" '.[($j|tonumber)]')"
      utxo0="$(echo "$thePair" | jq -c '.[0]')"
      utxo1="$(echo "$thePair" | jq -c '.[1]')"
      l0="$(echo "$utxo0" | jq .lovelace)"
      v0="$(echo "$utxo0" | jq .assetCount)"
      d0="$(echo "$utxo0" | jq .datum)"
      u0="$(echo "$utxo0" | jq .utxo)"
      l1="$(echo "$utxo1" | jq .lovelace)"
      v1="$(echo "$utxo1" | jq .assetCount)"
      d1="$(echo "$utxo1" | jq .datum)"
      u1="$(echo "$utxo1" | jq .utxo)"
      result="$($qvf traverse-donations "$l0" "$d0" "$l1" "$d1" "$(cat $fileNamesJSONFile)")"
      if [ "$result" == "Nothing" ]; then
        # {{{
        echo "No duplicates found between $u0 and $u1"
        continue
        # }}}
      else
        # {{{
        echo "Duplicate(s) found between $u0 and $u1"
        echo $result
        outL0="$(echo "$result" | jq .lovelace0)"
        outL1="$(echo "$result" | jq .lovelace1)"
        in0="--tx-in $u0 $txInConstant"
        in1="--tx-in $u1 $txInConstant"
        out0="
          --tx-out \"$qvfAddress + $outL0 lovelace + $v0 $donAsset\"
          --tx-out-inline-datum-file $updatedDatumFile
        "
        out1="
          --tx-out \"$qvfAddress + $outL1 lovelace + $v1 $donAsset\"
          --tx-out-inline-datum-file $newDatumFile
        "

        keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)
        
        generate_protocol_params
        
        buildTx="$cli $BUILD_TX_CONST_ARGS
          --required-signer-hash $keyHoldersPubKeyHash
          --tx-in $keyHoldersInUTxO
          --tx-in-collateral $keyHoldersInUTxO
          $in0 $in1 $out0 $out1
          --change-address $keyHoldersAddress"
        
        echo $buildTx > $tempBashFile
        . $tempBashFile
        
        sign_and_submit_tx $preDir/$keyHolder.skey
        store_current_slot_2 $scriptLabel $keyHolder
        wait_for_new_slot $scriptLabel
        store_current_slot_2 $scriptLabel $keyHolder
        wait_for_new_slot $scriptLabel
        allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
        allDonationsCount=$(echo "$allDonations" | jq length)
        i=0
        break
        # }}}
      fi
      # }}}
    done
    # }}}
  done
  # At this point, there shouldn't be any duplicate donations. Therefore, the
  # consolidation stage can commence.
  constr=$($qvf get-constr-index Donations)
  allDonations="$(get_script_utxos_datums_values $qvfAddress $donAsset)"
  donUTxOCount=$(echo "$allDonations" | jq length)
  txsNeeded=$(echo $donUTxOCount | jq --arg b "$(expr $b - 1)" '(. / ($b|tonumber)) | ceil')
  echo "Transactions needed to consolidate donations: $txsNeeded"
  txsDone=0
  while [ $txsDone -lt $txsNeeded ]; do
    # {{{
    iteration_helper "$b" "$constr" "consolidate-donations"
    mkMintArg
    projectInput="--tx-in $projectUTxO $txInConstant"
    outputProjectUTxO="
      --tx-out \"$qvfAddress + $(expr $projectInLovelace + $lovelaceCount) lovelace + 1 $projectAsset\"
      --tx-out-inline-datum-file $updatedDatumFile
    "

    build_submit_wait "$projectInput" "$txInArg" "$outputProjectUTxO" "$mintArg"

    txsDone=$(expr $txsDone + 1)
    # }}}
  done
  # }}}
fi

