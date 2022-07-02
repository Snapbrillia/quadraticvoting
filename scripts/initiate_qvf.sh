#!/bin/bash


# Make sure you have edited `env.sh` to have CARDANO_NODE_SOCKET_PATH properly
# assigned.
sh env.sh


# This script assumes that you have *installed* the `qvf-cli` application, and
# have its binary listed in your PATH environment variable. To allow utilizing
# symlinks however, the binary is dispatched via a variable here:
app=qvf-cli


key_holder="01"
key_holder_pkh="$key_holder.pkh"
key_holder_addr="$key_holder.addr"
key_holder_skey="$key_holder.skey"
num_wallets=20

policy_file="qvf.token"
validator_file="qvf.plutus"
script_addr="qvf.addr"


# Creates $num_wallets, starting from 1:
generate_wallets_from_to 1 $num_wallets


# Distributing 100 ADA among 20 wallets:
distribute_from_to_wallets $key_holder_addr \
                           1                \
                           $num_wallets     \
                           2000000000       \
                           $key_holder_skey


# Generate the minting policy script (`qvf.token`)
# and the validator script (`qvf.plutus`).
$app generate                               \
     scripts                                \
     $(cat $key_holder_pkh)                 \
     $(get_first_utxo_of $key_holder_addr)  \
     5445535431                             \ # == TEST1
     1                                      \
     $policy_file                           \
     $validator_file


# Get the currency symbol of the minting script.
policy_id=$(cardano-cli transaction policyid --script-file $policy_file)


plutus_script_to_address $validator_file $script_addr


# # Declutter the working directory:
# mkdir wallets
# mv *.skey wallets
# mv *.vkey wallets
# mv *.addr wallets
# mv *.pkh  wallets
