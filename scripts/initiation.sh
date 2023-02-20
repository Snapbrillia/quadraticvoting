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


startingWallet=1
endingWallet=20
totalLovelaceToDistribute=4000000000 # 200 ADA per wallet.

govSym=""

generate_wallets_and_distribute() {
  # Generating multiple wallets:
  generate_wallets_from_to $startingWallet $endingWallet
  
  # Distributing the given amount of Lovelaces equally between the generated
  # wallets:
  distribute_from_to_wallets   \
    $keyHolder                 \
    $startingWallet            \
    $endingWallet              \
    $totalLovelaceToDistribute
}

# ----------------------------------------------------------

get_script_address() {
  plutus_script_to_address
  cat $scriptAddressFile
}

get_gov_symbol() {
  $cli transaction policyid --script-file $govScriptFile
}

store_gov_symbol() {
  govSym=$($cli transaction policyid --script-file $govScriptFile)
  echo $govSym > $govSymFile
}

get_gov_asset() {
  store_gov_symbol
  echo $govSym
}

get_script_data_hash() {
  $cli transaction hash-script-data --script-data-file $1
}


# Takes no arguments.
deploy_scripts() {
  # {{{
  spendingUTxO=$(get_first_utxo_of $keyHolder)
  scriptLovelaces=62000000 # 62.0 ADA
  scriptUTxO="$referenceWalletAddress+$scriptLovelaces"

  generate_protocol_params

  # Transaction to submit the main script to chain:
  $cli $BUILD_TX_CONST_ARGS                        \
    --tx-in $spendingUTxO                          \
    --tx-out "$scriptUTxO"                         \
    --tx-out-reference-script-file $mainScriptFile \
    --change-address $keyHoldersAddress

  sign_and_submit_tx $keyHoldersSigningKeyFile
  store_current_slot_2 $keyHolder $referenceWallet
  wait_for_new_slot $keyHolder
  store_current_slot_2 $keyHolder $referenceWallet
  wait_for_new_slot $keyHolder

  # At this point there is only one UTxO sitting at the reference wallet:
  qvfRefUTxO=$(get_first_utxo_of $referenceWallet)

  # So should be the case with key holder's wallet:
  spendingUTxO=$(get_first_utxo_of $keyHolder)

  scriptLovelaces=42000000 # 42.0 ADA
  scriptUTxO="$referenceWalletAddress+$scriptLovelaces"

  generate_protocol_params

  # Transaction to submit the minting scripts to chain:
  $cli $BUILD_TX_CONST_ARGS                       \
    --tx-in $spendingUTxO                         \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $regScriptFile \
    --tx-out-inline-datum-value 0                 \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $donScriptFile \
    --tx-out-inline-datum-value 1                 \
    --change-address $keyHoldersAddress

  sign_and_submit_tx $keyHoldersSigningKeyFile
  store_current_slot_3 $keyHolder $referenceWallet
  wait_for_new_slot $keyHolder
  store_current_slot_3 $keyHolder $referenceWallet
  wait_for_new_slot $keyHolder

  regRefUTxO=$(get_first_utxo_of $referenceWallet)
  if [ $regRefUTxO == $qvfRefUTxO ]; then
    regRefUTxO=$(get_nth_utxo_of $referenceWallet 2)
    donRefUTxO=$(get_nth_utxo_of $referenceWallet 3)
  else
    donRefUTxO=$(get_nth_utxo_of $referenceWallet 2)
  fi

  echo $qvfRefUTxO > $qvfRefUTxOFile
  echo $regRefUTxO > $regRefUTxOFile
  echo $donRefUTxO > $donRefUTxOFile
  # }}}
}


initiate_fund() {
  # {{{
  deadline=$1
  if [ "$1" == "dev" ]; then
    deadline=$(posix_one_month_from_now)
  fi
  for proj in $(cat $registeredProjectsFile | jq 'map(.tn) | .[]'); do
    rm -f $preDir/$proj
  done
  rm -f $registeredProjectsFile
  touch $registeredProjectsFile
  rm -rf $projsPreDir
  mkdir -p $projsPreDir
  echo "Getting the current slot..."
  currentSlot=$(get_newest_slot)

  # GENERATING SCRIPTS
  # {{{
  echo
  echo "Getting the UTxO to be used for finding the currency symbols..."
  genesisUTxO=$(get_first_utxo_of $keyHolder)
  echo $genesisUTxO > $govUTxOFile
  echo
  echo -e "The UTxO is:$WHITE"
  echo "$genesisUTxO"
  echo -e "$NO_COLOR"

  # Generating the validation script, minting script, and some other files:
  echo "Finding the scripts and writing them to disk..."
  $qvf generate scripts                   \
    $keyHoldersPubKeyHash                 \
    $genesisUTxO                          \
    $currentSlot                          \
    $deadline                             \
    "$(cat $fileNamesJSONFile | jq -c .)"
  # }}}

  # SUBMITING THE INITIAL TRANSACTION
  # {{{
  scriptAddr=$(get_script_address)

  deadlineSlot=$(cat $deadlineSlotFile) 
  cappedSlot=$(cap_deadline_slot $deadlineSlot)

  deadlineDatum=$(getFileName ocfnDeadlineDatum)
  govDatumFile=$(getFileName ocfnInitialGovDatum)
  # NOTE: The order of these 2 assignments matters.
  govAsset=$(get_gov_asset)
  # -----------------------------------------------
  regSym=$($cli transaction policyid --script-file $regScriptFile)
  echo $regSym > $regSymFile
  donSym=$($cli transaction policyid --script-file $donScriptFile)
  echo $donSym > $donSymFile

  dlUTxO="$scriptAddr + $deadlineLovelaces lovelace + 1 $govAsset"
  outUTxO="$scriptAddr + $governanceLovelaces lovelace + 1 $govAsset"

  generate_protocol_params

  # Transaction to mint 2 governance tokens, and include 1 in two UTxOs
  # produced at the script address. Minter expects the deadline UTxO
  # to be produced first.
  $cli $BUILD_TX_CONST_ARGS                   \
    --tx-in $genesisUTxO                      \
    --tx-in-collateral $genesisUTxO           \
    --tx-out "$dlUTxO"                        \
    --tx-out-inline-datum-file $deadlineDatum \
    --tx-out "$outUTxO"                       \
    --tx-out-inline-datum-file $govDatumFile  \
    --invalid-hereafter $cappedSlot           \
    --mint "2 $govAsset"                      \
    --mint-script-file $govScriptFile         \
    --mint-redeemer-file $minterRedeemerFile  \
    --change-address $keyHoldersAddress
  
  sign_and_submit_tx $keyHoldersSigningKeyFile
  store_current_slot_2 $scriptLabel $keyHolder
  wait_for_new_slot $scriptLabel
  # }}}

  store_current_slot_2 $scriptLabel $keyHolder
  wait_for_new_slot $scriptLabel

  # STORING SCRIPTS ON-CHAIN #
  # {{{
  deploy_scripts
  # }}}

  # }}}

    # Update Funding Round
  if [ $ENV = "prod" ]; then 
    jq '.currentFundingRound |= .+1' $fundingRoundFile > temp.json && mv temp.json $fundingRoundFile
  fi
}


# Takes 1 argument:
#   1. The number of the target project.
dev_depletion() {
  # {{{
  collateral=$(get_first_utxo_of $keyHolder)
  govAsset=$(get_gov_asset)
  regRefUTxO=$(cat $regRefUTxOFile)
  regSym=$(cat $regSymFile)
  donRefUTxO=$(cat $donRefUTxOFile)
  donSym=$(cat $donSymFile)
  scriptAddr=$(cat $scriptAddressFile)
  # allUTxOs=$(get_nth_projects_donation_utxos $1 | jq '.[0:9]')
  # allUTxOs=$(get_all_projects_utxos_datums_values | jq '.[0:9]')
  allUTxOs=$(get_all_script_utxos_datums_values $scriptAddr | jq '.[0:8]')
  echo "$allUTxOs" | jq 'map(.asset)'
  const="--spending-tx-in-reference $(cat $qvfRefUTxOFile) --spending-plutus-script-v2 --spending-reference-tx-in-inline-datum-present --spending-reference-tx-in-redeemer-file $devRedeemer"
  txInArg=$(echo "$allUTxOs" | jq --arg const "$const" 'map("--tx-in " + .utxo + " " + $const) | reduce .[] as $a (""; if . == "" then $a else (. + " " + $a) end)')
  txInArg=$(remove_quotes "$txInArg")
  mintArg=$(echo "$allUTxOs" | jq 'map("-" + (.assetCount | tostring) + " " + .asset) | reduce .[] as $a (""; if . == "" then $a else (. + " + " + $a) end)')
  mintArg=$(remove_quotes "$mintArg")

  buildTx="$cli $BUILD_TX_CONST_ARGS
    --required-signer-hash $(cat $preDir/$keyHolder.pkh)
    --tx-in-collateral $collateral
    $txInArg
    --mint \"$mintArg\"
    --mint-script-file $govScriptFile
    --mint-redeemer-file $devRedeemer
    --mint-tx-in-reference $regRefUTxO
    --mint-plutus-script-v2
    --mint-reference-tx-in-redeemer-file $devRedeemer
    --policy-id $(cat $regSymFile)
    --change-address $(cat $preDir/$keyHolder.addr)
  "
  #   --mint-tx-in-reference $donRefUTxO
  #   --mint-plutus-script-v2
  #   --mint-reference-tx-in-redeemer-file $devRedeemer
  #   --policy-id $(cat $donSymFile)
  # buildTx="$cli $BUILD_TX_CONST_ARGS
  #   --required-signer-hash $(cat $preDir/$keyHolder.pkh)
  #   --tx-in-collateral $collateral
  #   $txInArg
  #   --mint \"$mintArg\"
  #   --mint-script-file $govScriptFile
  #   --mint-redeemer-file $devRedeemer
  #   --mint-tx-in-reference $donRefUTxO
  #   --mint-plutus-script-v2
  #   --mint-reference-tx-in-redeemer-file $devRedeemer
  #   --policy-id $(cat $donSymFile)
  #   --change-address $(cat testnet/$keyHolder.addr)
  # "
  echo $buildTx > $tempBashFile
  . $tempBashFile
  sign_and_submit_tx $preDir/$keyHolder.skey
  store_current_slot $keyHolder
  wait_for_new_slot $keyHolder
  show_utxo_tables $scriptLabel
  # }}}
}

dev_reset() {
  dev_depletion
  deplete_reference_wallet "DEV RESET."
  tidy_up_wallet $keyHolder "DEV RESET."
}

dev_restart() {
  dev_reset
  cabal install qvf-cli --overwrite-policy=always
  initiate_fund
}
