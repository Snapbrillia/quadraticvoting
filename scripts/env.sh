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


