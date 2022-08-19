#!/bin/bash

. scripts/env.sh

startingWallet=1
endingWallet=20
totalLovelaceToDistribute=4000000000

tokenName="QVF"
deadline=1660914000000

# Storing the hex format of the token name:
$qvf string-to-hex $tokenName $tokenNameHexFile

tokenNameHex=$(cat $tokenNameHexFile)

unitRedeemer="$preDir/unit.redeemer"
initialDatum="$preDir/initial.datum"


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

get_policy_id() {
  $cli transaction policyid --script-file $policyScriptFile
}

store_policy_id() {
  policyId=$($cli transaction policyid --script-file $policyScriptFile)
  echo $policyId > $policyIdFile
}

get_auth_asset() {
  store_policy_id
  echo $policyId.$tokenNameHex
}

get_script_data_hash() {
  $cli transaction hash-script-data --script-data-file $1
}

initiate_fund() {
  # Since the key holder wallet is going to hold a single UTxO after the
  # distribution, we can get the first UTxO of the wallet address and use
  # it as the genesis UTxO:
  genesisUTxO=$(get_first_utxo_of $keyHolder)

  # Storing the UTxO:
  echo $genesisUTxO > $authAssetUTxOFile

  # Generating the validation script, minting script, and some other files:
  $qvf generate scripts   \
    $keyHoldersPubKeyHash \
    $genesisUTxO          \
    $tokenName            \
    $deadline             \
    $policyScriptFile     \
    $scriptPlutusFile     \
    $unitRedeemer         \
    $initialDatum
  
  scriptAddr=$(get_script_address)
  authAsset=$(get_auth_asset)

  # COMMENTED OUT FOR TESTING - TODO: UNCOMMENT
  deadlineSlot=$(get_deadline_slot $initialDatum)
  echo $deadlineSlot > $deadlineSlotFile
  # deadlineSlot=$(cat $deadlineSlotFile) 
  # Structure of the first UTxO to be sent to the script address. The 2 ADA
  # here is to compensate both the required minimum of ~1.2 ADA, and the limit
  # imposed by the distribution endpoint of the validator.
  firstUTxO="$scriptAddr + 2000000 lovelace + 1 $authAsset"

  generate_protocol_params

  $cli transaction build                    \
    --babbage-era                           \
    $MAGIC                                  \
    --tx-in $genesisUTxO                    \
    --tx-in-collateral $genesisUTxO         \
    --tx-out "$firstUTxO"                   \
    --tx-out-datum-embed-file $initialDatum \
    --mint "1 $authAsset"                   \
    --mint-script-file $policyScriptFile    \
    --mint-redeemer-file $unitRedeemer      \
    --change-address $keyHoldersAddress     \
    --protocol-params-file $protocolsFile   \
    --out-file $txBody     
  
  # Signing of the transaction:
  sign_tx_by $keyHoldersSigningKeyFile

  # Submission of the transaction:
  submit_tx

  # Store current slot number for future interactions:
  store_current_slot
}
