#!/bin/bash

# ==== CHANGE THIS VARIABLE ACCORDINGLY ==== #
#export CARDANO_NODE_SOCKET_PATH=~/Plutus/plutus/plutus-pioneer-program/code/week06/testnet/node.socket
#export CARDANO_NODE_SOCKET_PATH=~/plutus-pioneer-program/code/week03/testnet/node.socket
export CARDANO_NODE_SOCKET_PATH=node.socket
# ========================================== #
export MAGIC='--testnet-magic 1097911063'

# Generates a key pair.
#
# Takes 2 arguments:
#   1. Verification key output file,
#   2. Signing key output file.
generate_skey_and_vkey () {
    cardano-cli address key-gen    \
        --verification-key-file $1 \
        --signing-key-file $2
}


# Creates an address file from a verification file.
# 
# Takes 2 arguments:
#   1. Verification key file,
#   2. Address output file.
vkey_to_address () {
    cardano-cli address build              \
        --payment-verification-key-file $1 \
        $MAGIC                             \
        --out-file $2
}


# Creates a public key hash file from a verification file.
# 
# Takes 2 arguments:
#   1. Verification key file,
#   2. Public key hash output file.
vkey_to_public_key_hash () {
    cardano-cli address key-hash           \
        --payment-verification-key-file $1 \
        --out-file $2
}


# Creates the associated address from a Plutus script File.
# 
# Takes 2 arguments:
#   1. Compiled Plutus script file,
#   2. Address output file.
plutus_script_to_address () {
    cardano-cli address build-script \
        --script-file $1             \
        $MAGIC                       \
        --out-file $2
}


# Given a numeric range (inclusive), generates all four files of a wallet
# (.vkey, .skey, .addr, .pkh) for each number.
#
# This function has builtin safety to prevent rewrites and wallet loss.
#
# For now, the maximum number of generated wallets is capped at 100.
# 
# Takes 2 arguments:
#   1. Starting number,
#   2. Ending number.
generate_wallets_from_to () {

    max_amt=100

    if [ `expr $2 - $1` -ge $max_amt ]
    then
    echo "That's over 100 wallets generated. Please reconsider. Edit fn if you really want to."
    else

    # Important part
    for i in $(seq $1 $2)
    do
    if [ -f $i.vkey ] || [ -f $i.skey ] || [ -f $i.addr ] || [ -f $i.pkh ]
    then

    echo "Error! $i.vkey, $i.skey, $i.addr, or $i.pkh already exist. Move/rename/remove them first and run again."
    break

    else

    generate_skey_and_vkey $i.vkey $i.skey
    vkey_to_address $i.vkey $i.addr
    vkey_to_public_key_hash $i.vkey $i.pkh

    fi
    done
    fi
}

# Displays the utxo information table of one or multiple addresses.
#
# Takes at least 1 argument:
#   1. Wallet address file.
#   2 - infinity. Any additional wallet address files.
show_utxo_tables () {
    for i in $@
    do
        echo "$i.addr utxos: "
        cardano-cli query utxo  \
            --address $(cat $i) \
            $MAGIC
    done
}
# TODO: refactor code that makes the same query as the above function.

# Given a numeric range (inclusive), displays the utxo information table from
# the address of the .addr file of each number, and displays any addresses
# provided after the numeric range.
# Takes at least 2 arguments:
#
#   1.            Starting number,
#   2.            Ending number.
#   3 - infinity. Any additional wallet address files.
show_utxo_tables_from_to () {

    for i in $(seq $1 $2)
    do
        echo "$i.addr utxos: "
        show_utxo_tables $i.addr
    done

    shift 2
    if [ -n $1 ]
    then
        show_utxo_tables $@
    fi

}

# Returns the "first" UTxO from a wallet (the first in the table returned by
# the `cardano-cli` application).
#
# Takes 1 argument:
#   1. Wallet address file.
get_first_utxo_of () {
    echo `cardano-cli query utxo                      \
        --address $(cat $1)                           \
        $MAGIC                                        \
        | sed 1,2d                                    \
        | awk 'FNR == 3 {print $1"#"$2}'`
}


# Returns a list of all UTxO's available at the given wallet address file, each
# prefixed with "--tx-in" for convenient use while constructing a transaction.
# 
# Takes 1 argument:
#   1. Wallet address file.
get_all_input_utxos_at () {
    echo `cardano-cli query utxo                      \
        --address $(cat $1)                           \
        $MAGIC                                        \
        | sed 1,2d                                    \
        | awk '{print $1"#"$2}'                       \
        | sed 's/^/--tx-in /'                         \
        | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g'`
}


# Equally distributes a given total Lovelace count from a wallet, between a
# number of wallets designated with a numeric range.
#
# Consumes all the available UTxO's, and returns the change back to the same
# wallet.
#
# TODO: Might require fixing as it can't handle UTxO's carrying tokens. It's
#       not a harmful limitation, it just fails if the given wallet has tokens
#       stored inside.
#
# Takes 5 arguments:
#   1. The spending wallet,
#   2. Starting number of the receiving wallets,
#   3. Ending number of the receiving wallets,
#   4. Total amount of Lovelace to be distributed equally,
#   5. Signing key file of the spending wallet. # TODO: Can this be removed?
distribute_from_to_wallets () {

    tx_in_str=$(get_all_input_utxos_at $1)
    tx_out_str=''
    num_of_wallets=`expr $3 - $2`
    num_of_wallets=`expr $num_of_wallets + 1` # +1 to compensate range inclusivity.
    lovelace_amt=`expr $4 / $num_of_wallets`

    # Potential change: we could query the total amount of lovelace at all
    # UTxO's of spending wallet instead of relying on Arg4; but the current
    # way provides flexibility of limiting the amount to spend

    # Build the string of --tx-out's
    for i in $(seq $2 $3)
    do
        cat=$(cat $i.addr)
        tx_out_str=$tx_out_str' --tx-out '$cat'+'$lovelace_amt
    done
  
    # Helper logs:
    echo "Starting to distribute a total of $4 Lovelaces between $num_of_wallets number of wallets."
    echo "(Each wallet will receive $lovelace_amt Lovelaces)."
    echo
    echo "Input UTxO's are:"
    echo $tx_in_str
    echo
    echo "Output addresses are:"
    echo $tx_out_str

    # Transaction
    cardano-cli transaction build   \
        --alonzo-era                \
        $MAGIC                      \
        $tx_in_str                  \
        --change-address $(cat $1)  \
        $tx_out_str                 \
        --out-file multiTx.body

    cardano-cli transaction sign    \
        --tx-body-file multiTx.body \
        --signing-key-file $5       \
        $MAGIC                      \
        --out-file multiTx.signed

    cardano-cli transaction submit  \
        $MAGIC                      \
        --tx-file multiTx.signed
}


# Drains a range of wallets into a single wallet. The receiving wallet will
# end up with 2 UTxO's: One holding 1 ADA, while the other holds the rest of
# the spent Lovelaces.
#
# TODO: Might require fixing as it can't handle UTxO's carrying tokens. It's
#       not a harmful limitation, it just fails if the given wallet has tokens
#       stored inside.
#
# Takes 3 arguments:
#   1. Starting number of the spending wallets,
#   2. Ending number of the spending wallets,
#   3. Address file of the receiving wallet.
drain_from_wallets_to () {

    tx_in_str=''
    signing_keys_str=''

    # Build the string of --tx-in's
    for i in $(seq $1 $2)
    do
        tx_in_str=$tx_in_str$(get_all_input_utxos_at $i.addr)' '
    done

    # Build the string of signing key files
    for i in $(seq $1 $2)
    do
        signing_keys_str=$signing_keys_str' --signing-key-file '$i'.skey'
    done

    # Transaction
    cardano-cli transaction build    \
        --alonzo-era                 \
        $MAGIC                       \
        $tx_in_str                   \
        --change-address $(cat $3)   \
        --tx-out $(cat $3)'+1000000' \
        --out-file multiTx.body

    cardano-cli transaction sign     \
        --tx-body-file multiTx.body  \
        $signing_keys_str            \
        $MAGIC                       \
        --out-file multiTx.signed

    cardano-cli transaction submit   \
        $MAGIC                       \
        --tx-file multiTx.signed
}


# Helper function that returns how much Lovelace is held in the first UTxO of
# the given wallet address.
#
# Takes 1 argument:
#   1. User's wallet address file.
get_first_lovelace_count_of () {
    echo `cardano-cli query utxo                      \
        --address $(cat $1)                           \
        $MAGIC                                        \
        | sed 1,2d                                    \
        | awk 'FNR == 1 {print $3}'`
}


# Given a wallet, a script, and other arguments, this function constructs,
# signs and submits a transaction for interacting with a smart contract.
#
# Takes 7 arguments:
#   1. User's wallet address file,
#   2. User's wallet signing key file,
#   3. The script file,
#   4. Script's current datum JSON file,
#   5. Redeemer's JSON file for the intended endpoint,
#   6. Amount that should be added to script's holding,
#   7. Updated datum of the script after the transaction.
interact_with_smart_contract () {
    users_utxo=$(get_first_utxo_of $1)
    script_addr=$(plutus_script_to_address $3)
    script_holding=$(get_first_lovelace_count_of $script_addr)
    extra_output="" # TODO: Should add $6 with $script_holding.

    cardano-cli transaction build                 \
        --tx-in $users_utxo                       \
        --tx-in $(get_first_utxo_of $script_addr) \
        --tx-in-script-file $3                    \
        --tx-in-datum-file $4                     \
        --tx-in-redeemer-file $5                  \
        --tx-in-collateral $users_utxo            \
        --tx-out $(cat $1)+$extra_output          \
        --tx-out-datum-embed-file  $7             \
        --change-address $(cat $1)                \
        --protocol-params-file protocol.json      \
        --out-file tx.raw                         \
        $MAGIC

    cardano-cli transaction sign                  \
        --tx-body-file tx.raw                     \
        --signing-key-file $2                     \
        $MAGIC                                    \
        --out-file tx.signed

    cardano-cli transaction submit                \
        $MAGIC                                    \
        --tx-file tx.signed


#     cardano-cli transaction build \
#         --tx-in e32e72881fc632efe5f8f387fffdd2c0dcf8871e2bebf55dc371eb8a3340b5e4#0 \
#         --tx-in e32e72881fc632efe5f8f387fffdd2c0dcf8871e2bebf55dc371eb8a3340b5e4#1 \
#         --tx-in-script-file testnet/demoScript.plutus \
#         --tx-in-datum-file initial.json \
#         --tx-in-redeemer-file contr-redeemer.json \
#         --tx-in-collateral e32e72881fc632efe5f8f387fffdd2c0dcf8871e2bebf55dc371eb8a3340b5e4#0 \
#         --tx-out $(cat script.addr)+86900000 \
#         --tx-out-datum-embed-file contr-datum.json \
#         --change-address $(cat testnet/01.addr) \
#         --protocol-params-file protocol.json \
#         --out-file tx.raw \
#         --testnet-magic 1097911063
#         
#         
#         cardano-cli transaction sign \
#         --tx-body-file tx.raw \
#         --signing-key-file testnet/01.skey \
#         --testnet-magic 1097911063 \
#         --out-file tx.signed
#         
#         cardano-cli transaction submit \
#         --testnet-magic 1097911063 \
#         --tx-file tx.signed
}
