#!/bin/bash

if [ "$ENV" == "dev" ]; then
. $REPO/scripts/initiation.sh
else
. $HOME/quadraticvoting/scripts/initiation.sh
fi

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)
regSym=$(cat $regSymFile)
donSym=$(cat $donSymFile)
deadlineSlot=$(cat $deadlineSlotFile)
cappedSlot=$(cap_deadline_slot $deadlineSlot)

# Takes 1 argument:
#   1. Donation count.
mkProjectDatum() {
  constr=$($qvf get-constr-index ReceivedDonationsCount)
  echo "{\"constructor\":$constr,\"fields\":[{\"int\":$1}]}"
}

# Takes 1 argument:
#   1. Donor's public key hash.
mkDonationDatum() {
  constr=$($qvf get-constr-index Donation)
  echo "{\"constructor\":$constr,\"fields\":[{\"bytes\":\"$1\"}]}"
}


# Takes 4 arguments:
#   1. Target project's number,
#   2. Starting donor wallet number,
#   3. Ending donor wallet number,
#   4. Max donor count per transaction.
dev() {
  # {{{
  devDonDir="$preDir/dev_donations"
  mkdir -p $devDonDir
  projectTokenName=$(project_number_to_token_name $1)
  projectAsset="$regSym.$projectTokenName"
  donAsset="$donSym.$projectTokenName"

  # Addition of 1 is because of inclusivity of the `seq` command.
  givenDonorCount=$(expr $3 - $2 + 1)
  lastTxIndex=$(echo "$givenDonorCount" | jq --arg den "$4" 'tonumber | (. / ($den|tonumber)) | ceil | (. - 1)')

  listFile="$preDir/donors.json"
  rm -f $listFile
  touch $listFile
  echo -n "[" > $listFile
  donorCount=1
  for i in $(seq $2 $3); do
    pkh=$(cat $preDir/$i.pkh)
    if [ $donorCount -eq 1 ]; then
      echo -n   $(mkDonationDatum $pkh)  >> $listFile
    else
      echo -n ",$(mkDonationDatum $pkh)" >> $listFile
    fi
    donorCount=$(expr $donorCount + 1)
  done
  donorCount=$(expr $donorCount - 1)
  echo -n "]" >> $listFile
  allDatums=$(cat $listFile | jq -c)

  for i in $(seq 0 $lastTxIndex); do
    donationDatums=$(echo "$allDatums" | jq -c --arg i "$i" --arg d "$4" '(($i|tonumber) * ($d|tonumber)) as $j | .[$j:($j + ($d|tonumber))]')
    donAssetCount=$(echo "$donationDatums" | jq length)
    txOutArg=""
    for j in $(seq 0 $(expr $donAssetCount - 1)); do
      donationUTxO="$qvfAddress + 10000000 lovelace + 1 $donAsset"
      echo "$donationDatums" | jq --arg j "$j" '.[($j|tonumber)]' > $devDonDir/$j.datum
      txOutArg="$txOutArg --tx-out \"$donationUTxO\" --tx-out-inline-datum-file $devDonDir/$j.datum"
    done
    inUTxO="$(get_first_utxo_of $keyHolder)"
    txInArg="--tx-in $inUTxO --tx-in-collateral $inUTxO"
    if [ $i -eq $lastTxIndex ]; then
      projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
      projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
      projectUpdatedDatum=$(mkProjectDatum $donorCount)
      echo "$projectUpdatedDatum" > $updatedDatumFile
      projectOutput="$qvfAddress + 1500000 lovelace + 1 $projectAsset"
      txInArg="$txInArg --tx-in $projectUTxO
        --spending-tx-in-reference $qvfRefUTxO
        --spending-plutus-script-v2
        --spending-reference-tx-in-inline-datum-present
        --spending-reference-tx-in-redeemer-file $devRedeemer
      "
      txOutArg="$txOutArg --tx-out \"$projectOutput\" --tx-out-inline-datum-file $updatedDatumFile"
    fi
    buildTx="$cli $BUILD_TX_CONST_ARGS
      --required-signer-hash $keyHoldersPubKeyHash $txInArg $txOutArg
      --mint \"$donAssetCount $donAsset\"
      --mint-tx-in-reference $donRefUTxO
      --mint-plutus-script-v2
      --mint-reference-tx-in-redeemer-file $devRedeemer
      --policy-id $donSym
      --change-address $keyHoldersAddress
    "
    echo $buildTx > $tempBashFile
    . $tempBashFile
    
    sign_and_submit_tx $preDir/$keyHolder.skey
    store_current_slot
    wait_for_new_slot
    store_current_slot
    wait_for_new_slot
  done
  # }}}
}


if [ "$1" == "dev" ]; then
  dev $2 $3 $4 $5
  return 0
fi

if [ "$ENV" == "dev" ]; then
  donorWalletLabel=$1
  projectTokenName=$2
  donationAmount=$3
  donorPKH=$(cat $preDir/$donorWalletLabel.pkh)
  donorAddress=$(cat $preDir/$donorWalletLabel.addr)
  donorInUTxO=$(get_first_utxo_of $donorWalletLabel)
  txInUTxO="--tx-in $donorInUTxO"
  txInCollateralUTxO="--tx-in-collateral $donorInUTxO"
  txOutUTxO=""
else
  projectTokenName=$1
  donationAmount=$2
  donorPKH=$3
  donorAddress=$4
  txInUTxO=$5
  txOutUTxO=$6
fi

projectAsset="$regSym.$projectTokenName"
donAsset="$donSym.$projectTokenName"

projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
projectUTxO=$(remove_quotes $(echo $projectUTxOObj | jq -c .utxo))
projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
echo "$projectCurrDatum" > $currentDatumFile
projectLovelaces=$(remove_quotes $(echo $projectUTxOObj | jq -c .lovelace))

deadlineUTxO=$(remove_quotes $(get_deadline_utxo | jq -c '.utxo'))

$qvf donate-to-project        \
  $donorPKH                   \
  $projectTokenName           \
	$donationAmount             \
  "$(cat $fileNamesJSONFile)"

outputProjectUTxO="$qvfAddress + $projectLovelaces lovelace + 1 $projectAsset"
donationUTxO="$qvfAddress + $donationAmount lovelace + 1 $donAsset"

qvfRefUTxO=$(cat $qvfRefUTxOFile)
donRefUTxO=$(cat $donRefUTxOFile)
collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)


generate_protocol_params

$cli $BUILD_TX_CONST_ARGS                                       \
  --required-signer-hash $donorPKH                              \
  --read-only-tx-in-reference $deadlineUTxO                     \
  --tx-in $projectUTxO                                          \
  --spending-tx-in-reference $qvfRefUTxO                        \
  --spending-plutus-script-v2                                   \
  --spending-reference-tx-in-inline-datum-present               \
  --spending-reference-tx-in-redeemer-file $qvfRedeemerFile     \
  $txInUTxO                                                     \
  --tx-in-collateral "$collateralUTxO"                          \
  $txOutUTxO                                                    \
  --tx-out "$outputProjectUTxO"                                 \
  --tx-out-inline-datum-file $updatedDatumFile                  \
  --tx-out "$donationUTxO"                                      \
  --tx-out-inline-datum-file $newDatumFile                      \
  --invalid-hereafter $cappedSlot                               \
  --mint "1 $donAsset"                                          \
  --mint-tx-in-reference $donRefUTxO                            \
  --mint-plutus-script-v2                                       \
  --mint-reference-tx-in-redeemer-file $minterRedeemerFile      \
  --policy-id $donSym                                           \
  --change-address $donorAddress

if [ "$ENV" == "dev" ]; then
  sign_and_submit_tx $preDir/$donorWalletLabel.skey
  wait_for_new_slot
  store_current_slot
  wait_for_new_slot
else
  store_current_slot
  JSON_STRING=$( jq -n                         \
    --arg tu "$(cat $txBody | jq -r .cborHex)" \
    '{unsignedTx: $tu }' )
  echo "---$JSON_STRING"
fi
# }}}

