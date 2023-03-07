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

if [ "$ENV" == "dev" ]; then
  sponsorWalletLabel=$1
  contributionAmount=$2
  sponsorInputUTxO=$(get_first_utxo_of $sponsorWalletLabel)
  sponsorAddress=$(cat $preDir/$sponsorWalletLabel.addr)
  txInUTxO="--tx-in $sponsorInputUTxO"
  txInCollateralUTxO="--tx-in-collateral $sponsorInputUTxO"
  txOutUTxO=""
  changeAddress=$($preDir/$sponsorWalletLabel.addr)
else
  contributionAmount=$1
  collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)
  txInCollateralUTxO="--tx-in-collateral $collateralUTxO"
  if [ "$3" == "--queue" ]; then
    queue="True"
    walletLabel=$2
    projectOwnerAddress=$(cat $custodialWalletsDir/$walletLabel.addr)
    projectOwnerPKH=$(cat $custodialWalletsDir/$walletLabel.pkh)
    txInUTxO="--tx-in $(get_first_utxo_of $custodialWalletLabel/$walletLabel) --tx-in $(get_first_utxo_of $keyHolder)"
    changeAddress=$keyHoldersAddress
  else
    changeAddress=$2
    txInUTxO=$3
    txOutUTxO=$4
  fi
fi

if [ "$queue" == "True" ]; then
  differenceBetweenSlots=$(get_slot_difference_2 $keyHolder $scriptLabel)
else
  differenceBetweenSlots=$(get_slot_difference $scriptLabel)
fi

if [ $differenceBetweenSlots -lt 100 ]; then
  echo "NetworkBusy"
else 
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
    --change-address $changeAddress      

  if [ "$ENV" == "dev" ]; then
    sign_and_submit_tx $preDir/$sponsorWalletLabel.skey
    store_current_slot $scriptLabel
    wait_for_new_slot $scriptLabel
    store_current_slot $scriptLabel
    wait_for_new_slot $scriptLabel
  elif [ "$queue" == "True" ]; then
      # {{{
    sign_and_submit_tx $custodialWalletsDir/$walletLabel.skey $preDir/$collateralKeyHolder.skey $preDir/$keyHolder.skey
    transactionHash=$($cli transaction txid --tx-file $txSigned)
    echo "$transactionHash"
    store_current_slot_2 $scriptLabel $keyHolder
     # }}}
  else
    JSON_STRING=$( jq -n                         \
      --arg bn "$(cat $txBody | jq -r .cborHex)" \
      '{transaction: $bn}' )
    echo "--json--$JSON_STRING"
  fi
fi
