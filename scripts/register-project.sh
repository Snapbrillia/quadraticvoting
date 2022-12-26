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

wait_for_new_slot

if [ "$ENV" == "dev" ]; then
  projectOwnerWalletLabel=$1
  projectName=$2
  projectRequestedFund=$3
  projectOwnerAddress=$(cat $preDir/$projectOwnerWalletLabel.addr)
  projectOwnerPKH=$(cat $preDir/$projectOwnerWalletLabel.pkh)
  ownerInputUTxO=$(get_first_utxo_of $projectOwnerWalletLabel)
  txInUTxO="--tx-in $ownerInputUTxO"
  # txInCollateralUTxO="--tx-in-collateral $ownerInputUTxO"
  txInCollateralUTxO="--tx-in-collateral $(get_first_utxo_of $collateralKeyHolder)"
  txOutUTxO=""
else
  projectName=$1
  projectRequestedFund=$2
  projectOwnerAddress=$3
  projectOwnerPKH=$4
  txInUTxO=$5
  txOutUTxO=$6
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(echo $govUTxOObj | jq -r .utxo)
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(echo $govUTxOObj | jq -r .lovelace)
deadlineUTxO=$(get_deadline_utxo | jq -r .utxo)

$qvf register-project         \
  $projectOwnerPKH            \
	$projectName                \
	$projectRequestedFund       \
  "$(cat $fileNamesJSONFile)"

# {{{
projectTokenName=$(cat $projectTokenNameFile)
newProjJSON="{\"pkh\":\"$projectOwnerPKH\",\"address\":\"$projectOwnerAddress\",\"tn\":\"$projectTokenName\"}"
newRegisteredProjects=$(cat $registeredProjectsFile \
  | jq -c --argjson obj "$newProjJSON"              \
  'if (map(select(. == $obj)) == []) then . += [$obj] else . end'
  )
echo $newRegisteredProjects > $registeredProjectsFile
projectAsset="$regSym.$projectTokenName"
projectDatumFile="$newDatumFile"
projectInfoDatumFile="$preDir/$projectTokenName"

firstUTxO="$qvfAddress + $govLovelaces lovelace + 1 $govAsset"
projUTxO="$qvfAddress + $halfOfTheRegistrationFee lovelace + 1 $projectAsset"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
regRefUTxO=$(cat $regRefUTxOFile)

collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                        \
  --required-signer-hash $projectOwnerPKH                        \
  --read-only-tx-in-reference $deadlineUTxO                      \
  --tx-in $govUTxO                                               \
  --spending-tx-in-reference $qvfRefUTxO                         \
  --spending-plutus-script-v2                                    \
  --spending-reference-tx-in-inline-datum-present                \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile      \
  $txInUTxO                                                      \
  --tx-in-collateral "$collateralUTxO"                           \
  $txOutUTxO                                                     \
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
  --change-address $projectOwnerAddress

if [ "$ENV" == "dev" ]; then
  sign_and_submit_tx $preDir/$projectOwnerWalletLabel.skey $preDir/$collateralKeyHolder.skey
  wait_for_new_slot
  store_current_slot
  wait_for_new_slot
else
  JSON_STRING=$( jq -n                                                \
    --arg tu "$(cat $txBody | jq -r .cborHex)"                        \
    --arg on "$(cat $projectTokenNameFile)"                           \
    --arg ts "$(cat $txSigned)"                                       \
    '{unsignedTx: $tu, projectTokenName: $on ,signedTx: $ts }' )
  echo "---$JSON_STRING"
  store_current_slot
fi
# }}}
