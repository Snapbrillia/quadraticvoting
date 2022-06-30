#!/bin/bash

export CARDANO_NODE_SOCKET_PATH=testnet/node.sock
export MAGIC='--testnet-magic 1097911063'

# Generates a key pair.  Needs 2 args: 2 names for the generated vkey and skey files respectively.
key_gen () {
    cardano-cli address key-gen \
        --verification-key-file $1 \
        --signing-key-file $2
}

# Builds an address. Needs 2 args: name of the vkey file and name for the generated address file.
address_build () {
    cardano-cli address build \
        --payment-verification-key-file $1 \
        --testnet-magic 1097911063 \
        --out-file $2
}

# Get tx-in hash address, given an address FILE name (not the address itself)
get_hash_addr () {

    return `cardano-cli query utxo \
        --address $(cat $filename3) \
        --testnet-magic 1097911063 \
    | awk 'FNR == 3 {print $1}'`

}

# Send lovelace from Arg1 addr to Ar2 addr with amt of Arg3 lovelace using signing key Arg4 (Which should be Arg1's signing key--We could find this automatically without this parameter perhaps)
# Needs testing, esp. for the fn call below
send_lovelace () {

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat $1) \
    --tx-in `get_hash_addr $1`#0 \ # Will this work ????
    --tx-out "$(cat $2) $3" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $4 \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
}

