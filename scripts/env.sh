#!/bin/bash

# Change for your environment
#export CARDANO_NODE_SOCKET_PATH=~/Plutus/plutus/plutus-pioneer-program/code/week06/testnet/node.socket
#export CARDANO_NODE_SOCKET_PATH=~/plutus-pioneer-program/code/week03/testnet/node.socket
export CARDANO_NODE_SOCKET_PATH=node.socket
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
        $MAGIC \
        --out-file $2
}

# Generates the public key hash. Needs 2 args: name of the vkey file and name for the generated file.
key_hash () {
    cardano-cli address key-hash \
        --payment-verification-key-file $1 \
        --out-file $2
}


# Generates keys and builds addresses for those keys and then public key hash files from numerical ranges Arg1 to Arg2 (inclusive). This follows the style of the PPP lectures, e.g.: 01.vkey, 01.skey, 01.addr, 01.pkh
# Please be careful. Let's set a limit on the amount for now.
generate_keys_addr_hash_range () {

    max_amt=100

    if [ `expr $2 - $1` -ge $max_amt ]
    then
    echo "That's over 100 wallets generated. Please reconsider. Edit fn if you really want to."
    else

    # Important part
    for i in $(seq $1 $2)
    do
    key_gen $i.vkey $i.skey
    address_build $i.vkey $i.addr
    key_hash $i.vkey $i.pkh
    done

    fi
}

# Send lovelace from Arg1 addr to Ar2 addr with amt of Arg3 lovelace using signing key Arg4 (Which should be Arg1's signing key--We could find this automatically without this parameter perhaps)
send_lovelace () {

tx_in=`cardano-cli query utxo \
        --address $(cat $1) \
        $MAGIC \
        | awk 'FNR == 3 {print $1}'`'#0'

cardano-cli transaction build \
    --alonzo-era \
    $MAGIC \
    --change-address $(cat $1) \
    --tx-in $tx_in \
    --tx-out "$(cat $2) $3 lovelace" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $4 \
    $MAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    $MAGIC \
    --tx-file tx.signed
}

# Get all utxos from address Arg1
get_utxos () {
    echo `cardano-cli query utxo \
        --address $(cat $1) \
        $MAGIC \
        | sed 1,2d | awk '{ print $1"#"$2}' \
        | sed 's/^/--tx-in /' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g'`
}


# Send lovelace from Arg1 addr (including all of its utxos) to a range of Arg2-Arg3 of addresses (generated from generate_keys_addr_hash_range--should be a contigous range if generated separately) of an equal amts of lovelace of Arg4 using signing key Arg5 (Which should be Arg1's signing key--We could find this automatically without this parameter perhaps)
send_lovelace_from_one_wallet_with_multiple_utxos_to_multiple_wallets () {

    tx_in_str=$(get_utxos $1)
    tx_out_str=''
    signing_keys_str=''
    num_of_wallets=`expr $3 - $2`
    num_of_wallets=`expr $num_of_wallets + 1` # +1 for spending wallet and +1 due to inclusive range
    lovelace_amt=`expr $4 / $num_of_wallets`
    echo $lovelace_amt $num_of_wallets

    # Potential change: we could query the total amount of lovelace at all utxos of spending wallet instead of relying on Arg4; but the current way provides flexibility of limiting the amount to spend

    # Build the string of --tx-out's
    for i in $(seq $2 $3)
    do
        cat=$(cat $i.addr)
        tx_out_str=$tx_out_str' --tx-out '$cat'+'$lovelace_amt
    done
  
    # Build the string of signing key files
    for i in $(seq $2 $3)
    do
        signing_keys_str=$signing_keys_str' --signing-key-file '$i'.skey'
    done
    echo $tx_in_str $tx_out_str $signing_keys_str
    # Transaction
    cardano-cli transaction build \
        --alonzo-era \
        $MAGIC \
        $tx_in_str \
        --change-address $(cat $1) \
        $tx_out_str \
        --out-file multiTx.body

    cardano-cli transaction sign \
        --tx-body-file multiTx.body \
        --signing-key-file $5 \
        $MAGIC \
        --out-file multiTx.signed

    cardano-cli transaction submit \
        $MAGIC \
        --tx-file multiTx.signed
}


# Send all lovelace from a range of Arg1-Arg2 of addresses (generated from generate_keys_addr_hash_range--should be a contigous range if generated separately) to Arg3 address.
send_lovelace_from_many_wallet_with_multiple_utxos_to_one_wallet () {

    tx_in_str=''
    signing_keys_str=''

    # Build the string of --tx-in's
    for i in $(seq $1 $2)
    do
        tx_in_str=$tx_in_str$(get_utxos $i.addr)' '
    done

    # Build the string of signing key files
    for i in $(seq $1 $2)
    do
        signing_keys_str=$signing_keys_str' --signing-key-file '$i'.skey'
    done

    # Transaction
    cardano-cli transaction build \
        --alonzo-era \
        $MAGIC \
        $tx_in_str \
        --change-address $(cat $3) \
        --tx-out $(cat $3)'+1000000' \
        --out-file multiTx.body

    cardano-cli transaction sign \
        --tx-body-file multiTx.body \
        $signing_keys_str \
        $MAGIC \
        --out-file multiTx.signed

    cardano-cli transaction submit \
        $MAGIC \
        --tx-file multiTx.signed
}

