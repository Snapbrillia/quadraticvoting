#!/bin/bash

. env.sh

startingWallet=1
endingWallet=20
totalLovelaceToDistribute=4000000000

tokenName="QVF"
deadline=1660608000000

# Storing the hex format of the token name:
$qvf string-to-hex $tokenName $tokenNameHexFile

tokenNameHex=$(cat $tokenNameHexFile)

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
  $policyId=$($cli transaction policyid --script-file $policyScriptFile)
}

get_auth_asset() {
  echo $(get_policy_id).$tokenNameHex
}

get_script_data_hash() {
  $cli transaction hash-script-data --script-data-file $1
}

initiate_fund() {
  # Since the key holder wallet is going to hold a single UTxO after the
  # distribution, we can get the first UTxO of the wallet address and use
  # it as the genesis UTxO:
  $genesisUTxO=$(get_first_utxo_of $keyHolder)

  # Storing the UTxO:
  echo $genesisUTxO > $authAssetUTxOFile

  # Generating the validation script, minting script, and some other files:
  $qvf generate scripts   \
    # key holder's pub key hash:
    $keyHoldersPubKeyHash \
    # auth token identifier utxo:
    $genesisUTxO          \
    # auth token name:
    $tokenName            \
    # deadline:
    $deadline             \
    # output minitng script file:
    $policyScriptFile     \
    # output validation script file:
    $scriptPlutusFile     \
    # output file for unit data:
    $unitRedeemer         \
    # output file for initial datum:
    $initialDatum
  
  scriptAddr=$(get_script_address)
  authAsset=$(get_auth_asset)
  initialDatumHash=$(get_script_data_hash $initialDatum)
  
  # Structure of the first UTxO to be sent to the script address. The 2 ADA
  # here is to compensate both the required minimum of ~1.2 ADA, and the limit
  # imposed by the distribution endpoint of the validator.
  firstUTxO="$scriptAddr + 2000000 lovelace + 1 $authAsset"

  generate_protocol_params

  $cli transaction build                       \
    --babbage-era                              \
    $MAGIC                                     \
    # consuming the utxo from key holder's wallet:
    --tx-in $genesisUTxO                       \
    # using that same utxo as collateral:
    --tx-in-collateral $genesisUTxO            \
    # sending the tokens to the script address:
    --tx-out "$firstUTxO"                      \
    # attaching the hash of the initial datum to the utxo being produced:
    --tx-out-datum-hash $initialDatumHash      \
    # minting an authentication token:
    --mint "1 $authAsset"                      \
    # providing the serialized minting script:
    --mint-script-file $policyScriptFile       \
    # providing the required redeemer (the value is not important):
    --mint-redeemer-file $unitRedeemer \
    # returning the excess ADA back to the key holder:
    --change-address $keyHolderAddress         \
    # this file can be generated using the `cardano-cli` application:
    --protocol-params-file $protocolsFile      \
    --out-file $txBody     
  
  # Signing of the transaction:
  sign_tx_by $keyHoldersSigningKeyFile

  # Submission of the transaction:
  submit_tx
}
