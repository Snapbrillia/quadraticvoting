#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

. $REPO/scripts/env.sh


keyHoldersAddress=$(cat $preDir/$keyHolder.addr)
keyHoldersPubKeyHash=$(cat $preDir/$keyHolder.pkh)
qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)
estimate="False"
silent="True"
queue="False"


if [ "$ENV" == "dev" ]; then
  # {{{
  projectOwnerWalletLabel=$1
  projectName=$2
  projectRequestedFund=$3
  projectOwnerAddress=$(cat $preDir/$projectOwnerWalletLabel.addr)
  projectOwnerPKH=$(cat $preDir/$projectOwnerWalletLabel.pkh)
  ownerInputUTxO=$(get_first_utxo_of $projectOwnerWalletLabel)
  txInUTxO="--tx-in $ownerInputUTxO"
  txOutUTxO=""
  changeAddress=$projectOwnerAddress
  silence="False"
  # }}}
elif [ "$3" == "--estimate-fee" ]; then
  # {{{
  projectName=$1
  projectRequestedFund=$2
  estimate="True"
  # }}}
else 
  # {{{
  projectName=$1
  projectRequestedFund=$2
  if [ "$4" == "--queue" ]; then
    queue="True"
    walletLabel=$3
    projectOwnerAddress=$(cat $custodialWalletsDir/$walletLabel.addr)
    projectOwnerPKH=$(cat $custodialWalletsDir/$walletLabel.pkh)
    txInUTxO="--tx-in $(get_first_utxo_of $custodialWalletLabel/$walletLabel) --tx-in $(get_first_utxo_of $keyHolder)"
    changeAddress=$keyHoldersAddress
  else
    projectOwnerAddress=$3
    projectOwnerPKH=$4
    txInUTxO=$5
    txOutUTxO=$6
    changeAddress=$projectOwnerAddress
  fi
  # }}}
fi


# Takes 5 arguments:
#   1. Constant arguments,
#   2. Project owner's public key hash,
#   3. Inputs,
#   4. Outputs,
#   5. Change address.
build_tx_with() {
  # {{{
  govUTxOObj="$(get_governance_utxo)"
  govUTxO=$(echo $govUTxOObj | jq -r .utxo)
  govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
  echo "$govCurrDatum" > $currentDatumFile
  govLovelaces=$(echo $govUTxOObj | jq -r .lovelace)
  deadlineUTxO=$(get_deadline_utxo | jq -r .utxo)

  qvfRes=$($qvf register-project \
    $2                           \
    "$projectName"               \
    $projectRequestedFund        \
    "$(cat $fileNamesJSONFile)"
  )

  projectTokenName=$(cat $projectTokenNameFile)
  projectAsset="$regSym.$projectTokenName"
  projectDatumFile="$newDatumFile"
  projectInfoDatumFile="$preDir/$projectTokenName"

  firstUTxO="$qvfAddress + $govLovelaces lovelace + 1 $govAsset"
  projUTxO="$qvfAddress + $halfOfTheRegistrationFee lovelace + 1 $projectAsset"

  qvfRefUTxO=$(cat $qvfRefUTxOFile)
  regRefUTxO=$(cat $regRefUTxOFile)

  collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)

  generate_protocol_params

  cliRes=$($cli $1                                                 \
    --required-signer-hash $2                                      \
    --read-only-tx-in-reference $deadlineUTxO                      \
    --tx-in $govUTxO                                               \
    --spending-tx-in-reference $qvfRefUTxO                         \
    --spending-plutus-script-v2                                    \
    --spending-reference-tx-in-inline-datum-present                \
    --spending-reference-tx-in-redeemer-file $qvfRedeemerFile      \
    $3                                                             \
    --tx-in-collateral "$collateralUTxO"                           \
    $4                                                             \
    --tx-out "$firstUTxO"                                          \
    --tx-out-inline-datum-file $updatedDatumFile                   \
    --tx-out "$projUTxO"                                           \
    --tx-out-inline-datum-file $projectInfoDatumFile               \
    --tx-out "$projUTxO"                                           \
    --tx-out-inline-datum-file $projectDatumFile                   \
    --invalid-hereafter $cappedSlot                                \
    --mint "2 $projectAsset"                                       \
    --mint-tx-in-reference $regRefUTxO                             \
    --mint-plutus-script-v2                                        \
    --mint-reference-tx-in-redeemer-file $minterRedeemerFile       \
    --policy-id $regSym                                            \
    --change-address $5
  )
  if [ "$estimate" == "True" ]; then
    echo $cliRes | tr -d -c 0-9
  elif [ "$silent" == "False" ]; then
    echo "qvf-cli log:"
    echo $qvfRes
    echo "cardano-cli log:"
    echo $cliRes
  fi
  # }}}
}


# Checks if project's UTxO is free for interaction (i.e. making sure it hasn't
# been consumed recently). Echos "NetworkBusy" if it's not.
if [ "$queue" == "True" ]; then
  differenceBetweenSlots=$(get_slot_difference_2 $keyHolder $scriptLabel)
else
  differenceBetweenSlots=$(get_slot_difference $scriptLabel)
fi

if [ $differenceBetweenSlots -lt 100 ] && [ "$ENV" != "dev" ]; then
  # {{{
  echo "NetworkBusy"
  # }}}
else 
  # {{{
  if [ "$estimate" == "True" ]; then
    # {{{
    khsUTxO=$(get_first_utxo_of $keyHolder)
    ckhsUTxO=$(get_first_utxo_of $collateralKeyHolder)
    build_tx_with                                            \
      "$BUILD_TX_CONST_ARGS_NO_OUT_FILE --out-file $dummyTx" \
      $keyHoldersPubKeyHash                                  \
      "--tx-in $khsUTxO --tx-in $ckhsUTxO"                   \
      ""                                                     \
      $keyHoldersAddress
    # }}}
  else
    # {{{
    build_tx_with            \
      "$BUILD_TX_CONST_ARGS" \
      $projectOwnerPKH       \
      "$txInUTxO"            \
      "$txOutUTxO"           \
      $changeAddress
    # }}}
  fi

  if [ "$estimate" == "True" ]; then
    # {{{
    :
    # }}}
  else
    # {{{
    # Write new project to disk:
    projectTokenName=$(cat $projectTokenNameFile)
    newProjJSON="{\"pkh\":\"$projectOwnerPKH\",\"address\":\"$projectOwnerAddress\",\"tn\":\"$projectTokenName\"}"
    newRegisteredProjects=$(cat $registeredProjectsFile \
      | jq -c --argjson obj "$newProjJSON"              \
              --arg     tn  "$projectTokenName"         \
              'if (map(select(.tn == $tn)) == []) then . += [$obj] else (map(select(.tn != $tn) | . += [$obj])) end'
      )
    echo $newRegisteredProjects > $registeredProjectsFile
    # --------------------------
    if [ "$ENV" == "dev" ]; then
      # {{{
      sign_and_submit_tx $preDir/$projectOwnerWalletLabel.skey $preDir/$collateralKeyHolder.skey
      store_current_slot_2 $projectTokenName $scriptLabel
      wait_for_new_slot $projectTokenName
      store_current_slot_2 $projectTokenName $scriptLabel
      wait_for_new_slot $projectTokenName
      # }}}
    elif [ "$queue" == "True" ]; then
      # {{{
      sign_and_submit_tx $custodialWalletsDir/$walletLabel.skey $preDir/$collateralKeyHolder.skey $preDir/$keyHolder.skey
      JSON_STRING=$( jq -n                            \
        --arg on "$(cat $projectTokenNameFile)"       \
        '{projectTokenName: $on }' )
      echo "$JSON_STRING"
      store_current_slot_3 $projectTokenName $scriptLabel $keyHolder
      # }}}
    else
      # {{{
      JSON_STRING=$( jq -n                            \
        --arg tu "$(cat $txBody | jq -r .cborHex)"    \
        --arg on "$(cat $projectTokenNameFile)"       \
        '{unsignedTx: $tu, projectTokenName: $on }' )
      echo "$JSON_STRING"
      # }}}
    fi
    # }}}
  fi
  # }}}
fi


