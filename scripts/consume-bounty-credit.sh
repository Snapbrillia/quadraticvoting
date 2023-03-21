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

consumeAmount=$1
projectWalletAddress=$2

projectTokenName=$(cat $registeredProjectsFile | jq -r ". [] | select(.address == \"$projectWalletAddress\") | .tn")
differenceBetweenSlots=$(get_slot_difference_2 $keyHolder $projectTokenName)

if [ $differenceBetweenSlots -lt 100 ]; then
  # {{{
  echo "NetworkBusy"
  # }}}
else
  keyHoldersPubKeyHash=$(cat $preDir/$keyHolder.pkh)
  keyHoldersAddress=$(cat $preDir/$keyHolder.addr)

  deadlineSlot=$(cat $deadlineSlotFile)

  projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
  projectUTxO=$(echo $projectUTxOObj | jq -r .utxo)
  projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
  projectAsset=$(echo $projectUTxOObj | jq -r .asset)
  echo "$projectCurrDatum" > $currentDatumFile

  projectLovelaceAmount=$(echo $projectUTxOObj | jq -r .lovelace)
  returnLovelaceAmount=$(expr $projectLovelaceAmount - $consumeAmount)
  returnedUTxO="$qvfAddress + $returnLovelaceAmount lovelace + 1 $projectAsset"
  txInUTxO=$(get_first_utxo_of $keyHolder)
  collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)
  qvfRefUTxO=$(cat $qvfRefUTxOFile)

  escrowWalletUTxO="$bountyEscrowWalletAddress+$consumeAmount" 

  generate_protocol_params
  
  cliRes=$($cli $BUILD_TX_CONST_ARGS                                     \
      --required-signer-hash $keyHoldersPubKeyHash                       \
      --tx-in $projectUTxO                                               \
      --spending-tx-in-reference $qvfRefUTxO                             \
      --spending-plutus-script-v2                                        \
      --spending-reference-tx-in-inline-datum-present                    \
      --spending-reference-tx-in-redeemer-file $devRedeemer              \
      --tx-in "$txInUTxO"                                                \
      --tx-in-collateral "$collateralUTxO"                               \
      --tx-out "$returnedUTxO"                                           \
      --tx-out-inline-datum-file $currentDatumFile                       \
      --tx-out "$escrowWalletUTxO"                                       \
      --change-address $keyHoldersAddress       
  )
                            
  sign_and_submit_tx $preDir/$keyHolder.skey $preDir/$collateralKeyHolder.skey
  transactionHash=$($cli transaction txid --tx-file $txSigned)
  echo "$transactionHash"
  store_current_slot_2 $keyHolder $projectTokenName
  
fi
