#!/bin/bash

. $REPO/scripts/initiation.sh


if [ "$ENV" == "dev" ]; then
  projectOwnerWalletLabel=$1
  projectName=$2
  projectRequestedFund=$3
  projectOwnerAddress=$(cat $preDir/$projectOwnerWalletLabel.addr)
  projectOwnerPKH=$(cat $preDir/$projectOwnerWalletLabel.pkh)
  ownerInputUTxO=$(get_first_utxo_of $projectOwnerWalletLabel)
  txInUTxO="--tx-in $ownerInputUTxO"
  txInCollateralUTxO="--tx-in-collateral $ownerInputUTxO"
  txOutUTxO=""
else
  projectName=$1
  projectRequestedFund=$2
  projectOwnerAddress=$3
  projectOwnerPKH=$4
  txInUTxO=$5
  txInCollateralUTxO=$6
  txOutUTxO=$7
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

govUTxOObj="$(get_governance_utxo)"
govUTxO=$(remove_quotes $(echo $govUTxOObj | jq -c .utxo))
govCurrDatum="$(echo $govUTxOObj | jq -c .datum)"
echo "$govCurrDatum" > $currentDatumFile
govLovelaces=$(remove_quotes $(echo $govUTxOObj | jq -c .lovelace))
deadlineUTxO=$(remove_quotes $(get_deadline_utxo | jq -c '.utxo'))

$qvf register-project         \
  $projectOwnerPKH            \
	$projectName                \
	$projectRequestedFund       \
  "$(cat $fileNamesJSONFile)"

# {{{
projectTokenName=$(cat $projectTokenNameFile)
newProjJSON="{\"pkh\":\"$projectOwnerPKH\",\"address\":\"$projectOwnerAddress\",\"tn\":\"$projectTokenName\"}"
newRegisteredProjects=$(cat $registeredProjectsFile \
  | jq --argjson obj "$newProjJSON"                 \
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

generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                   \
  --required-signer-hash $projectOwnerPKH                   \
  --read-only-tx-in-reference $deadlineUTxO                 \
  --tx-in $govUTxO                                          \
  --spending-tx-in-reference $qvfRefUTxO                    \
  --spending-plutus-script-v2                               \
  --spending-reference-tx-in-inline-datum-present           \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile \
  $txInUTxO $txInCollateralUTxO $txOutUTxO                  \
  --tx-out "$firstUTxO"                                     \
  --tx-out-inline-datum-file $updatedDatumFile              \
  --tx-out "$projUTxO"                                      \
  --tx-out-inline-datum-file $projectInfoDatumFile          \
  --tx-out "$projUTxO"                                      \
  --tx-out-inline-datum-file $projectDatumFile              \
  --invalid-hereafter $cappedSlot                           \
  --mint "2 $projectAsset"                                  \
  --mint-tx-in-reference $regRefUTxO                        \
  --mint-plutus-script-v2                                   \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile  \
  --policy-id $regSym                                       \
  --change-address $projectOwnerAddress

if [ "$ENV" == "dev" ]; then
  sign_and_submit_tx $preDir/$projectOwnerWalletLabel.skey
  wait_for_new_slot
else
  store_current_slot
  JSON_STRING=$( jq -n                         \
    --arg bn "$(cat $txBody | jq -r .cborHex)" \
    --arg on "$(cat $projectTokenNameFile)"    \
    '{transaction: $bn, projectTokenName: $on}' )
  echo "---$JSON_STRING"
fi
# }}}
