#!/bin/bash


# Make sure you have edited `env.sh` to have CARDANO_NODE_SOCKET_PATH properly
# assigned.
. env.sh

$cli="cardano-cli"
$qvf="cabal run qvf-cli --"

$keyHolderAddressFile="wallets/keyHolder.addr"
$keyHolderPubKeyHashFile="wallets/keyHolder.pkh"
$keyHolderSigningKeyFile="wallets/keyHolder.skey"

$startingWallet=1
$endingWallet=10
$totalLovelaceToDistribute=2000000000

$tokenName="QuadraToken"
$tokenCount=10

$policyFile="minting.plutus"
$validatorFile="qvf.plutus"
$scriptAddressFile="qvf.addr"
$tokenNameHexFile="token.hex"
$qvf string-to-hex $tokenName $tokenNameHexFile
$tokenNameHex=$(cat $tokenNameHexFile)

$notStartedDatum="notStarted-datum.json"
$initiateFundRedeemer="initiate-fund.json"
$memptyDatum="mempty-datum.json"


generate_wallets_and_distribute() {
    # Generating multiple wallets:
    generate_wallets_from_to $startingWallet $endingWallet
    
    # Distributing the given amount of Lovelaces equally between the generated
    # wallets:
    distribute_from_to_wallets     \
        $keyHolderAddressFile      \
        $startingWallet            \
        $endingWallet              \
        $totalLovelaceToDistribute \
        $keyHolderSigningKeyFile
}

# ----------------------------------------------------------

get_script_address() {
    plutus_script_to_address $validatorFile $scriptAddressFile
    cat $scriptAddressFile
}

get_policy_id() {
    $cli transaction policyid --script-file $policyFile
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
    $genesisUTxO=$(get_first_utxo_of $keyHolderAddressFile)
    
    # Generating the validation script, minting script,
    $qvf generate scripts     \ # the command
        $keyHolderPubKeyHash  \ # key holder's pub key hash.
        $genesisUTxO          \ # auth token identifier utxo.
        $tokenName            \ # auth token name.
        $tokenCount           \ # auth token count.
        $policyFile           \ # output minitng script file.
        $validatorFile        \ # output validation script file.
        $notStartedDatum      \ # output file for `NotStarted` datum.
        $initiateFundRedeemer \ # output file for `InitiateFund` redeemer.
        $memptyDatum            # output file for `InProgress mempty` datum.
    
    $scriptAddr=$(get_script_address)
    $authAsset=$(get_auth_asset)
    $notStartedDatumHash=$(get_script_data_hash $notStartedDatum)
    $memptyDatumHash=$(get_script_data_hash $memptyDatum)
    
    # Structure of the first UTxO to be sent to the script
    # address. The 2 ADA here is to compensate the required
    # minimum of ~1.2 ADA.
    $firstUTxO="$scriptAddr + 2000000 lovelace + $tokenCount $authAsset"

    $cli transaction build                         \
        --babbage-era                              \
        $MAGIC                                     \
        --tx-in $genesisUTxO                       \ # consuming the utxo from key holder's wallet.
        --tx-in-collateral $genesisUTxO            \ # using that same utxo as collateral.
        --tx-out "$firstUTxO"                      \ # sending the tokens to the script address.
        --tx-out-datum-hash $notStartedDatumHash   \ # attaching the hash of the `NotStarted` datum to the utxo being produced.
        --mint "$tokenCount $authAsset"            \ # minting of 10 authentication tokens.
        --mint-script-file $policyFile             \ # providing the serialized minting script.
        --mint-redeemer-file $initiateFundRedeemer \ # providing the required redeemer (the value is not important).
        --change-address $keyHolderAddress         \ # returning the excess ADA back to the key holder.
        --protocol-params-file protocol.json       \ # this file can be generated using the `cardano-cli` application.
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
    $authAsset=$(get_auth_asset)
    $utxoWithAllAuths=$(get_first_utxo_of $scriptAddr)
    $oneAuthUTxO="--tx-out $scriptAddr + 10000000 lovelace + 1 $authAsset --tx-out-datum-embed-file $memptyDatum "
    $utxoFromKeyHolder=$(get_first_utxo_of $keyHolderAddressFile)
    $txOut=""
    for i in $(seq 1 $tokenCount)
    do
        txOut=$txOut$txOut
    done
    
    $cli transaction build                            \
        --babbage-era                                 \
        $MAGIC                                        \
        --tx-in $utxoFromKeyHolder                    \
        --tx-in-collateral $utxoFromKeyHolder         \
        --tx-in $utxoWithAllAuths                     \
        --tx-in-datum-file $notStartedDatum           \
        --tx-in-script-file $validatorFile            \
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
