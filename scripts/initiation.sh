#!/bin/bash

. scripts/env.sh

startingWallet=1
endingWallet=20
totalLovelaceToDistribute=4000000000 # 200 ADA per wallet.

deadline=1667642400000

tokenNameHex=""
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
  echo $govSym.$tokenNameHex
}

get_script_data_hash() {
  $cli transaction hash-script-data --script-data-file $1
}

initiate_fund() {
  # {{{
  currentSlot=$(get_newest_slot)

  # GENERATING SCRIPTS
  # {{{
  genesisUTxO=$(get_first_utxo_of $keyHolder)
  echo $genesisUTxO > $govUTxOFile

  # Generating the validation script, minting script, and some other files:
  $qvf generate scripts                   \
    $keyHoldersPubKeyHash                 \
    $genesisUTxO                          \
    $currentSlot                          \
    $deadline                             \
    "$(cat $fileNamesJSONFile | jq -c .)"
  # }}}

  # SUBMITING THE INITIAL TRANSACTION
  # {{{
  tokenNameHex=$(cat $tokenNameHexFile)
  # 
  scriptAddr=$(get_script_address)

  deadlineSlot=$(cat $deadlineSlotFile) 
  cappedSlot=$(cap_deadline_slot $deadlineSlot)

  deadlineDatum=$(getFileName ocfnDeadlineDatum)
  govDatumFile=$(getFileName ocfnInitialGovDatum)
  govAsset=$(get_gov_asset)
  govLovelaces=1500000 #  1.5 ADA
  firstUTxO="$scriptAddr + $govLovelaces lovelace + 1 $govAsset"

  generate_protocol_params

  # Transaction to mint 2 governance tokens, and include 1 in two UTxOs
  # produced at the script address. Minter expects the deadline UTxO
  # to be produced first.
  $cli $BUILD_TX_CONST_ARGS                   \
    --tx-in $genesisUTxO                      \
    --tx-in-collateral $genesisUTxO           \
    --tx-out "$firstUTxO"                     \
    --tx-out-inline-datum-file $deadlineDatum \
    --tx-out "$firstUTxO"                     \
    --tx-out-inline-datum-file $govDatumFile  \
    --invalid-hereafter $cappedSlot           \
    --mint "2 $govAsset"                      \
    --mint-script-file $govScriptFile         \
    --mint-redeemer-value 0                   \
    --change-address $keyHoldersAddress
  
  sign_tx_by $keyHoldersSigningKeyFile
  submit_tx
  # }}}

  store_current_slot
  wait_for_new_slot

  # STORING SCRIPTS ON-CHAIN #
  # {{{
  spendingUTxO=$(get_first_utxo_of $keyHolder)
  scriptLovelaces=60000000 # 60.0 ADA
  scriptUTxO="$referenceWalletAddress+$scriptLovelaces"

  generate_protocol_params

  # Transaction to submit the main script to chain:
  $cli $BUILD_TX_CONST_ARGS                        \
    --tx-in $spendingUTxO                          \
    --tx-in-collateral $spendingUTxO               \
    --tx-out "$scriptUTxO"                         \
    --tx-out-reference-script-file $mainScriptFile \
    --change-address $keyHoldersAddress

  sign_tx_by $keyHoldersSigningKeyFile
  submit_tx

  store_current_slot
  wait_for_new_slot

  # At this point there is only one UTxO sitting at the reference wallet:
  qvfRefUTxO=$(get_first_utxo_of $referenceWallet)

  # So should be the case with key holder's wallet:
  spendingUTxO=$(get_first_utxo_of $keyHolder)

  scriptLovelaces=30000000 # 60.0 ADA
  scriptUTxO="$referenceWalletAddress+$scriptLovelaces"

  generate_protocol_params

  # Transaction to submit the minting scripts to chain:
  $cli $BUILD_TX_CONST_ARGS                       \
    --tx-in $spendingUTxO                         \
    --tx-in-collateral $spendingUTxO              \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $regScriptFile \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $donScriptFile \
    --change-address $keyHoldersAddress

  sign_tx_by $keyHoldersSigningKeyFile
  submit_tx

  store_current_slot
  wait_for_new_slot

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

  store_current_slot

  # }}}
}
