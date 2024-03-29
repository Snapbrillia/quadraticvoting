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


# For development:
# {{{
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
      projectUTxO=$(echo $projectUTxOObj | jq -r .utxo)
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
    store_current_slot $projectTokenName
    wait_for_new_slot $projectTokenName
    store_current_slot $projectTokenName
    wait_for_new_slot $projectTokenName
  done
  # }}}
}


if [ "$1" == "dev" ] && [ "$ENV" == "dev" ]; then
  dev $2 $3 $4 $5
  return 0
fi
# }}}


# Handling arguments:
if [ "$ENV" == "dev" ]; then
  # {{{
  donorWalletLabel=$1
  projectTokenName=$2
  donationAmount=$3
  donorPKH=$(cat $preDir/$donorWalletLabel.pkh)
  donorAddress=$(cat $preDir/$donorWalletLabel.addr)
  donorInUTxO=$(get_first_utxo_of $donorWalletLabel)
  txInUTxO="--tx-in $donorInUTxO"
  txOutUTxO=""
  changeAddress=$donorAddress
  silence="False"
  # }}}
elif [ "$2" == "--estimate-fee" ]; then
  # {{{
  projectTokenName=$1
  estimate="True"
  # }}}
else
  # {{{
  projectTokenName=$1
  donationAmount=$2
  if [ "$4" == "--queue" ]; then
    queue="True"
    walletLabel=$3
    donorPKH=$(cat $custodialWalletsDir/$walletLabel.pkh)
    donorAddress=$(cat $custodialWalletsDir/$walletLabel.addr)
    txInUTxO="--tx-in $(get_first_utxo_of $custodialWalletLabel/$walletLabel) --tx-in $(get_first_utxo_of $keyHolder)"
    changeAddress=$keyHoldersAddress
  else
    donorPKH=$3
    donorAddress=$4
    txInUTxO=$5
    txOutUTxO=$6
    changeAddress=$donorAddress
  fi
  # }}}
fi


# Takes 6 arguments:
#   1. Constant arguments,
#   2. Donation amount,
#   3. Donor's public key hash,
#   4. Inputs,
#   5. Outputs,
#   6. Change address.
build_tx_with() {
  # {{{
  donAmount=$2
  projectAsset="$regSym.$projectTokenName"
  donAsset="$donSym.$projectTokenName"

  projectUTxOObj="$(get_projects_state_utxo $projectTokenName)"
  projectUTxO=$(echo $projectUTxOObj | jq -r .utxo)
  projectCurrDatum="$(echo $projectUTxOObj | jq -c .datum)"
  echo "$projectCurrDatum" > $currentDatumFile
  projectLovelaces=$(echo $projectUTxOObj | jq -r .lovelace)

  deadlineUTxO=$(get_deadline_utxo | jq -r '.utxo')

  qvfRes=$($qvf donate-to-project \
    $3                            \
    $projectTokenName             \
    $donAmount                    \
    "$(cat $fileNamesJSONFile)"
  )

  outputProjectUTxO="$qvfAddress + $projectLovelaces lovelace + 1 $projectAsset"
  donationUTxO="$qvfAddress + $donAmount lovelace + 1 $donAsset"

  qvfRefUTxO=$(cat $qvfRefUTxOFile)
  donRefUTxO=$(cat $donRefUTxOFile)
  collateralUTxO=$(get_first_utxo_of $collateralKeyHolder)

  generate_protocol_params

  cliRes=$($cli $1                                                \
    --required-signer-hash $3                                     \
    --read-only-tx-in-reference $deadlineUTxO                     \
    --tx-in $projectUTxO                                          \
    --spending-tx-in-reference $qvfRefUTxO                        \
    --spending-plutus-script-v2                                   \
    --spending-reference-tx-in-inline-datum-present               \
    --spending-reference-tx-in-redeemer-file $qvfRedeemerFile     \
    $4                                                            \
    --tx-in-collateral "$collateralUTxO"                          \
    $5                                                            \
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
    --change-address $6
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
  differenceBetweenSlots=$(get_slot_difference_2 $keyHolder $projectTokenName)
else
  differenceBetweenSlots=$(get_slot_difference $projectTokenName)
fi

if [ $differenceBetweenSlots -lt 100 ] && [ "$ENV" != "dev" ]; then
  # {{{
  echo "NetworkBusy"
  # }}}
else
  # {{{
  # Build the transaction:
  if [ "$estimate" == "True" ]; then
    # {{{
    khsLovelaces=$(get_first_lovelace_count_of $preDir/$keyHolder.addr)
    khsUTxO=$(get_first_utxo_of $keyHolder)
    ckhsUTxO=$(get_first_utxo_of $collateralKeyHolder)
    build_tx_with                                            \
      "$BUILD_TX_CONST_ARGS_NO_OUT_FILE --out-file $dummyTx" \
      $khsLovelaces                                          \
      $keyHoldersPubKeyHash                                  \
      "--tx-in $khsUTxO --tx-in $ckhsUTxO"                   \
      ""                                                     \
      $keyHoldersAddress
    # }}}
  else
    # {{{
    build_tx_with            \
      "$BUILD_TX_CONST_ARGS" \
      $donationAmount        \
      $donorPKH              \
      "$txInUTxO"            \
      "$txOutUTxO"           \
      $changeAddress
    # }}}
  fi

  # Sign and submit (if applicable):
  if [ "$estimate" == "True" ]; then
    # {{{
    :
    # }}}
  elif [ "$ENV" == "dev" ]; then
    # {{{
    sign_and_submit_tx $preDir/$donorWalletLabel.skey $preDir/$collateralKeyHolder.skey
    store_current_slot $projectTokenName
    wait_for_new_slot $projectTokenName
    store_current_slot $projectTokenName
    wait_for_new_slot $projectTokenName
    # }}}
  elif [ "$queue" == "True" ]; then
    # {{{
    sign_and_submit_tx $custodialWalletsDir/$walletLabel.skey $preDir/$collateralKeyHolder.skey $preDir/$keyHolder.skey
    transactionHash=$($cli transaction txid --tx-file $txSigned)
    echo "$transactionHash"
    store_current_slot_2 $projectTokenName $keyHolder 
    # }}}
  else
    # {{{
    JSON_STRING=$( jq -n                         \
      --arg tu "$(cat $txBody | jq -r .cborHex)" \
      '{unsignedTx: $tu }' )
    echo "$JSON_STRING"
    # }}}
  fi
  # }}}
fi

