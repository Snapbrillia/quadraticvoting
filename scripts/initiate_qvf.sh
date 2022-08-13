#!/bin/bash

# Make sure you have edited `env.sh` to have CARDANO_NODE_SOCKET_PATH properly
# assigned.
. env.sh

keyHolder="keyHolder"
keyHolderAddressFile="$preDir/$keyHolder.addr"
keyHolderPubKeyHashFile="$preDir/$keyHolder.pkh"
keyHolderSigningKeyFile="$preDir/$keyHolder.skey"

startingWallet=1
endingWallet=10
totalLovelaceToDistribute=2000000000

tokenName="QuadraToken"
tokenCount=10

# Storing the hex format of the token name:
$qvf string-to-hex $tokenName $tokenNameHexFile

tokenNameHex=$(cat $tokenNameHexFile)

notStartedDatum="NotStarted.datum"
initiateFundRedeemer="InitiateFund.redeemer"
memptyDatum="InProgress-mempty.datum"


generate_wallets_and_distribute() {
    # Generating multiple wallets:
    generate_wallets_from_to $startingWallet $endingWallet
    
    # Distributing the given amount of Lovelaces equally between the generated
    # wallets:
    distribute_from_to_wallets     \
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

construct_and_submit_genesis_tx() {
    # Since the key holder wallet is going to hold a single UTxO after the
    # distribution, we can get the first UTxO of the wallet address and use
    # it as the genesis UTxO:
    $genesisUTxO=$(get_first_utxo_of $keyHolder)
    
    # Generating the validation script, minting script,
    $qvf generate scripts     \
        # key holder's pub key hash:
        $keyHolderPubKeyHash  \
        # auth token identifier utxo:
        $genesisUTxO          \
        # auth token name:
        $tokenName            \
        # auth token count:
        $tokenCount           \
        # output minitng script file:
        $policyScript         \
        # output validation script file:
        $scriptPlutusFile     \
        # output file for `NotStarted` datum:
        $notStartedDatum      \
        # output file for `InitiateFund` redeemer:
        $initiateFundRedeemer \
        # output file for `InProgress mempty` datum:
        $memptyDatum
    
    scriptAddr=$(get_script_address)
    authAsset=$(get_auth_asset)
    notStartedDatumHash=$(get_script_data_hash $notStartedDatum)
    memptyDatumHash=$(get_script_data_hash $memptyDatum)
    
    # Structure of the first UTxO to be sent to the script
    # address. The 2 ADA here is to compensate the required
    # minimum of ~1.2 ADA.
    firstUTxO="$scriptAddr + 2000000 lovelace + $tokenCount $authAsset"

    $cli transaction build                         \
        --babbage-era                              \
        $MAGIC                                     \
        # consuming the utxo from key holder's wallet:
        --tx-in $genesisUTxO                       \
        # using that same utxo as collateral:
        --tx-in-collateral $genesisUTxO            \
        # sending the tokens to the script address:
        --tx-out "$firstUTxO"                      \
        # attaching the hash of the `NotStarted` datum to the utxo being produced:
        --tx-out-datum-hash $notStartedDatumHash   \
        # minting of 10 authentication tokens:
        --mint "$tokenCount $authAsset"            \
        # providing the serialized minting script:
        --mint-script-file $policyScript           \
        # providing the required redeemer (the value is not important):
        --mint-redeemer-file $initiateFundRedeemer \
        # returning the excess ADA back to the key holder:
        --change-address $keyHolderAddress         \
        # this file can be generated using the `cardano-cli` application:
        --protocol-params-file protocol.json       \
        --out-file tx.unsigned 
    
    # Signing of the transaction:
    $cli transaction sign                           \
        --tx-body-file tx.unsigned                  \
        --signing-key-file $keyHolderSigningKeyFile \
        $MAGIC                                      \
        --out-file tx.signed
    
    # Submission of the transaction:
    $cli transaction submit \
        $MAGIC              \
        --tx-file tx.signed
}


# ----------------------------------------------------------

# Constructing Transaction with $tokenCount outputs, 1 for each authentication
# token.
initiate_fund() {
    authAsset=$(get_auth_asset)
    utxoWithAllAuths=$(get_first_utxo_of $scriptAddr)
    oneAuthUTxO="--tx-out $scriptAddr + 10000000 lovelace + 1 $authAsset --tx-out-datum-embed-file $memptyDatum "
    utxoFromKeyHolder=$(get_first_utxo_of $keyHolderAddressFile)
    txOut=""
    for i in $(seq 1 $tokenCount)
    do
        txOut=$txOut$oneAuthUTxO
    done
    
    $cli transaction build                            \
        --babbage-era                                 \
        $MAGIC                                        \
        --tx-in $utxoFromKeyHolder                    \
        --tx-in-collateral $utxoFromKeyHolder         \
        --tx-in $utxoWithAllAuths                     \
        --tx-in-datum-file $notStartedDatum           \
        --tx-in-script-file $scriptPlutusFile         \
        --tx-in-redeemer-file $initiateFundRedeemer   \
        --required-signer $keyHolderSigningKeyFile    \
        $txOut                                        \
        --change-address $(cat $keyHolderAddressFile) \
        --protocol-params-file protocol.json          \
        --out-file tx.unsigned 
    
    # Sign the transaction:
    $cli transaction sign                           \
        --tx-body-file tx.unsigned                  \
        --signing-key-file $keyHolderSigningKeyFile \
        $MAGIC                                      \
        --out-file tx.signed
    
    # Submit the transaction:
    $cli transaction submit \
        $MAGIC              \
        --tx-file tx.signed
}
# ----------------------------------------------------------
