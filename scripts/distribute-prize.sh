#!/bin/bash

. $REPO/scripts/initiation.sh

if [ "$ENV" == "dev" ]; then
  projectTokenName=$(project_index_to_token_name "$1")
else
  projectTokenName=$1
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)

qvfRefUTxO=$(cat $qvfRefUTxOFile)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))

projectsInfoUTxOObj="$(get_projects_info_utxo $projectTokenName)"
projectsStateUTxOObj="$(get_projects_state_utxo $projectTokenName)"
ownerAddrStr=$(cat $registeredProjectsFile \
  | jq --arg tn "$projectTokenName"        \
  'map(select(.tn == $tn)) | .[0]'
  )

# govInputStr : infoInputStr : projInputStr : ownerAddrStr 
outputsStr=$($qvf distribute-prize \
  "$govUTxOObj"                    \
  "$projectsInfoUTxOObj"           \
  "$projectsStateUTxOObj"          \
  $ownerAddrStr                    \
  "$(cat $fileNamesJSONFile)"
  )

outputArgs=$(qvf_output_to_tx_outs "$outputsStr")

keyHoldersInUTxO=$(get_first_utxo_of $keyHolder)

generate_protocol_params

buildTx="$cli $BUILD_TX_CONST_ARGS
  --required-signer-hash $keyHoldersPubKeyHash
  --tx-in $keyHoldersInUTxO
  --tx-in-collateral $keyHoldersInUTxO
  $refArgs $inputArgs $outputArgs
  --change-address $keyHoldersAddress
  "
echo $buildTx > $tempBashFile
. $tempBashFile

sign_and_submit_tx $preDir/$keyHolder.skey
wait_for_new_slot
store_current_slot
wait_for_new_slot
