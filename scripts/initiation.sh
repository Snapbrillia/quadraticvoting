#!/bin/bash

. scripts/env.sh

startingWallet=1
endingWallet=20
totalLovelaceToDistribute=4000000000 # 200 ADA per wallet.

export deadline=1667642400000

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

get_deadline_asset() {
  echo $(cat $govSymFile).$(cat $deadlineTokenNameHexFile)
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
  scriptAddr=$(get_script_address)

  deadlineSlot=$(cat $deadlineSlotFile) 
  cappedSlot=$(cap_deadline_slot $deadlineSlot)

  deadlineDatum=$(getFileName ocfnDeadlineDatum)
  govDatumFile=$(getFileName ocfnInitialGovDatum)
  # NOTE: The order of these 2 assignments matters.
  govAsset=$(get_gov_asset)
  deadlineAsset=$(get_deadline_asset)
  # -----------------------------------------------
  govLovelaces=1500000 #  1.5 ADA
  firstUTxO="$scriptAddr + $govLovelaces lovelace + 1 $deadlineAsset"
  secondUTxO="$scriptAddr + $govLovelaces lovelace + 1 $govAsset"

  generate_protocol_params

  # Transaction to mint 2 governance tokens, and include 1 in two UTxOs
  # produced at the script address. Minter expects the deadline UTxO
  # to be produced first.
  $cli $BUILD_TX_CONST_ARGS                   \
    --tx-in $genesisUTxO                      \
    --tx-in-collateral $genesisUTxO           \
    --tx-out "$firstUTxO"                     \
    --tx-out-inline-datum-file $deadlineDatum \
    --tx-out "$secondUTxO"                    \
    --tx-out-inline-datum-file $govDatumFile  \
    --invalid-hereafter $cappedSlot           \
    --mint "1 $deadlineAsset + 1 $govAsset"   \
    --mint-script-file $govScriptFile         \
    --mint-redeemer-value 0                   \
    --change-address $keyHoldersAddress
  
  sign_and_submit_tx $keyHoldersSigningKeyFile
  wait_for_new_slot
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

  sign_and_submit_tx $keyHoldersSigningKeyFile
  wait_for_new_slot

  store_current_slot
  wait_for_new_slot

  # At this point there is only one UTxO sitting at the reference wallet:
  qvfRefUTxO=$(get_first_utxo_of $referenceWallet)

  # So should be the case with key holder's wallet:
  spendingUTxO=$(get_first_utxo_of $keyHolder)

  scriptLovelaces=32000000 # 32.0 ADA
  scriptUTxO="$referenceWalletAddress+$scriptLovelaces"

  generate_protocol_params

  # Transaction to submit the minting scripts to chain:
  $cli $BUILD_TX_CONST_ARGS                       \
    --tx-in $spendingUTxO                         \
    --tx-in-collateral $spendingUTxO              \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $regScriptFile \
    --tx-out-inline-datum-value 0                 \
    --tx-out "$scriptUTxO"                        \
    --tx-out-reference-script-file $donScriptFile \
    --tx-out-inline-datum-value 1                 \
    --change-address $keyHoldersAddress

  sign_and_submit_tx $keyHoldersSigningKeyFile
  wait_for_new_slot

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


dev_depletion() {
  # {{{
  collateral=$(get_first_utxo_of $keyHolder)
  deadlineAsset=$(get_deadline_asset)
  govAsset=$(get_gov_asset)
  $cli $BUILD_TX_CONST_ARGS                                        \
    --tx-in-collateral $collateral                                 \
    --tx-in $(get_first_utxo_of $scriptLabel)                      \
    --spending-tx-in-reference $(cat qvfRefUTxOFile)               \
    --spending-plutus-script-v2                                    \
    --spending-reference-tx-in-inline-datum-present                \
    --spending-reference-tx-in-redeemer-file $preDir/dev.redeemer  \
    --tx-in $(get_nth_utxo_of $scriptLabel 2)                      \
    --spending-tx-in-reference $(cat qvfRefUTxOFile)               \
    --spending-plutus-script-v2                                    \
    --spending-reference-tx-in-inline-datum-present                \
    --spending-reference-tx-in-redeemer-file $preDir/dev.redeemer  \
    --mint "-1 $deadlineAsset + -1 $govAsset"                      \
    --mint-script-file $govScriptFile                              \
    --mint-redeemer-value 1                                        \
    --change-address $(cat testnet/$keyHolder.addr)
  sign_and_submit_tx $preDir/$keyHolder.skey
  wait_for_new_slot
  show_utxo_tables $scriptLabel
  # }}}
}

deplete_reference_wallet() {
  # {{{
  $cli $BUILD_TX_CONST_ARGS                    \
    $(get_all_input_utxos_at $referenceWallet) \
    --change-address $keyHoldersAddress
  sign_and_submit_tx $preDir/$referenceWallet.skey
  wait_for_new_slot
  show_utxo_tables $referenceWallet
  # }}}
}
