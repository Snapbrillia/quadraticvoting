#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
. $REPO/scripts/local-env.sh
fi

. $REPO/scripts/initiation.sh

consumeAmount=$1
projectWalletAddress=$2
txInUTxO=$3
txOutUTxO=$4

projectTokenName=$(remove_quotes $(cat $registeredProjectsFile | jq ". [] | select(.address == \"$projectWalletAddress\") | .tn"))
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
echo `$projectUTxOObj projectUTxOObj`
projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
projectAsset=$(remove_quotes $(echo $projectUTxOObj | jq -c .asset))
echo "$projectCurrDatum" > $currentDatumFile

projectLovelaceAmount=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))
returnLovelaceAmount=$(($projectLovelaceAmount - $consumeAmount + $halfOfTheRegistrationFee))
returnedUTxO=""$qvfAddress" + $returnLovelaceAmount lovelace + 1 $projectAsset"
collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)
qvfRefUTxO=$(cat $qvfRefUTxOFile)


escrowWalletUTxO="$bountyEscrowWalletAddress+$consumeAmount"

# what are the parameters that needs to be passed to qvf-cli to generate 
# the bounty credit redeemer 

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                            \
  --required-signer-hash $keyHoldersPubKeyHash                       \
  --tx-in $projectUTxO                                               \
  --spending-tx-in-reference $qvfRefUTxO                             \
  --spending-plutus-script-v2                                        \
  --spending-reference-tx-in-inline-datum-present                    \
  --spending-reference-tx-in-redeemer-file $devRedeemer              \
  $txInUTxO                                                          \
  --tx-in-collateral "$collateralUTxO"                               \
  $txOutUTxO                                                         \
  --tx-out "$returnedUTxO"                                           \
  --tx-out-inline-datum-file $currentDatumFile                       \
  --tx-out "$escrowWalletUTxO"                                       \
  --invalid-hereafter $cappedSlot                                    \
  --change-address $projectWalletAddress                             


JSON_STRING=$( jq -n \
                  --arg bn "$(cat $txBody | jq -r .cborHex)" \
                  '{transaction: $bn }' )

echo "---$JSON_STRING"

store_current_slot
