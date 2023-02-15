export WHITE="\033[97m"
export NO_COLOR="\033[0m"

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

# Removes the single quotes.
#
# Takes 1 argument:
#   1. Target string.
remove_single_quotes() {
  # {{{
  echo $1           \
  | sed 's|['"\'"',]||g'
  # }}}
}

# Removes the double quotes.
#
# Takes 1 argument:
#   1. Target string.
remove_quotes() {
  # {{{
  echo $1           \
  | sed 's|[",]||g'
  # }}}
}

# Removes the backslashes.
#
# Takes 1 argument:
#   1. Target string.
remove_back_slashes() {
  # {{{
  echo $1           \
  | sed 's|['"\\"']||g'
  # }}}
}


# Returns the POSIX time in milliseconds a week from now.
#
# Takes no arguments.
posix_one_week_from_now() {
  now=$(date +%s)
  init=$(expr $now + 604800)
  echo "$init"000
}


# Returns the POSIX time in milliseconds a month from now.
#
# Takes no arguments.
posix_one_month_from_now() {
  now=$(date +%s)
  init=$(expr $now + 2628000)
  echo "$init"000
}

export scriptLabel="qvf"
export fileNamesJSONFile="$preDir/fileNames.json"
# Creating the $fileNamesJSONFile:
# {{{
touch $fileNamesJSONFile
if [ ! "$(cat $fileNamesJSONFile)" ]; then
  echo "{ \"ocfnPreDir\"              : \"$preDir\""                      > $fileNamesJSONFile
  echo ", \"ocfnProjectsPreDir\"      : \"projects\""                    >> $fileNamesJSONFile
  echo ", \"ocfnQueryJSON\"           : \"query.json\""                  >> $fileNamesJSONFile
  echo ", \"ocfnDeadlineTokenNameHex\": \"deadline-token-name.hex\""     >> $fileNamesJSONFile
  echo ", \"ocfnGovernanceMinter\"    : \"governance-policy.plutus\""    >> $fileNamesJSONFile
  echo ", \"ocfnGovernanceSymbol\"    : \"governance-policy.symbol\""    >> $fileNamesJSONFile
  echo ", \"ocfnQVFGovernanceUTxO\"   : \"gov.utxo\""                    >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationMinter\"  : \"registration-policy.plutus\""  >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationSymbol\"  : \"registration-policy.symbol\""  >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationRefUTxO\" : \"registration-policy.refUTxO\"" >> $fileNamesJSONFile
  echo ", \"ocfnDonationMinter\"      : \"donation-policy.plutus\""      >> $fileNamesJSONFile
  echo ", \"ocfnDonationSymbol\"      : \"donation-policy.symbol\""      >> $fileNamesJSONFile
  echo ", \"ocfnDonationRefUTxO\"     : \"donation-policy.refUTxO\""     >> $fileNamesJSONFile
  echo ", \"ocfnQVFMainValidator\"    : \"$scriptLabel.plutus\""         >> $fileNamesJSONFile
  echo ", \"ocfnQVFRefUTxO\"          : \"$scriptLabel.refUTxO\""        >> $fileNamesJSONFile
  echo ", \"ocfnContractAddress\"     : \"$scriptLabel.addr\""           >> $fileNamesJSONFile
  echo ", \"ocfnDeadlineSlot\"        : \"deadline.slot\""               >> $fileNamesJSONFile
  echo ", \"ocfnDeadlineDatum\"       : \"deadline.govDatum\""           >> $fileNamesJSONFile
  echo ", \"ocfnInitialGovDatum\"     : \"initial.govDatum\""            >> $fileNamesJSONFile
  echo ", \"ocfnCurrentDatum\"        : \"current.datum\""               >> $fileNamesJSONFile
  echo ", \"ocfnUpdatedDatum\"        : \"updated.datum\""               >> $fileNamesJSONFile
  echo ", \"ocfnNewDatum\"            : \"new.datum\""                   >> $fileNamesJSONFile
  echo ", \"ocfnQVFRedeemer\"         : \"qvf.redeemer\""                >> $fileNamesJSONFile
  echo ", \"ocfnMinterRedeemer\"      : \"minter.redeemer\""             >> $fileNamesJSONFile
  echo ", \"ocfnProjectTokenName\"    : \"project-token-name.hex\""      >> $fileNamesJSONFile
  echo ", \"ocfnScriptUTxOs\"         : \"script.utxos\""                >> $fileNamesJSONFile
  echo ", \"ocfnRegisteredProjects\"  : \"registered-projects.json\""    >> $fileNamesJSONFile
  echo "}" >> $fileNamesJSONFile
fi
# }}}
getFileName() {
  echo $preDir/$(remove_quotes $(cat $fileNamesJSONFile | jq -c .$1))
}

# Exporting variables for required file names:
# {{{
export queryJSONFile=$(getFileName ocfnQueryJSON)
export projsPreDir=$(getFileName ocfnProjectsPreDir)
export tempBashFile="$preDir/temp.sh"
touch $tempBashFile

# Main script:
export mainScriptFile=$(getFileName ocfnQVFMainValidator)
export scriptAddressFile=$(getFileName ocfnContractAddress)

# Minters:
export govScriptFile=$(getFileName ocfnGovernanceMinter)
export govSymFile=$(getFileName ocfnGovernanceSymbol)
export regScriptFile=$(getFileName ocfnRegistrationMinter)
export regSymFile=$(getFileName ocfnRegistrationSymbol)
export donScriptFile=$(getFileName ocfnDonationMinter)
export donSymFile=$(getFileName ocfnDonationSymbol)

# Datums and redeemers:
export currentDatumFile=$(getFileName ocfnCurrentDatum)
export updatedDatumFile=$(getFileName ocfnUpdatedDatum)
export newDatumFile=$(getFileName ocfnNewDatum)
export qvfRedeemerFile=$(getFileName ocfnQVFRedeemer)
export minterRedeemerFile=$(getFileName ocfnMinterRedeemer)
 
export projectTokenNameFile=$(getFileName ocfnProjectTokenName)
touch $projectTokenNameFile
export registeredProjectsFile=$(getFileName ocfnRegisteredProjects)
touch $registeredProjectsFile
if [ ! "$(cat $registeredProjectsFile)" ]; then
  echo "[]" > $registeredProjectsFile
fi
export govUTxOFile=$(getFileName ocfnQVFGovernanceUTxO)
export qvfRefUTxOFile=$(getFileName ocfnQVFRefUTxO)
export regRefUTxOFile=$(getFileName ocfnRegistrationRefUTxO)
export donRefUTxOFile=$(getFileName ocfnDonationRefUTxO)

export referenceWallet="referenceWallet"
export keyHolder="keyHolder"
export collateralKeyHolder="collateralKeyHolder"
minStartingLovelaces=350000000
minStartingAda=350
minCollateralLovelaces=5000000
minCollateralAda=5

export deadlineSlotFile=$(getFileName ocfnDeadlineSlot)
export latestInteractionSlotFile="$preDir/latestInteraction.slot"
touch $latestInteractionSlotFile
if [ ! "$(cat $latestInteractionSlotFile)" ]; then
  echo "{\"$keyHolder\":0"        > $latestInteractionSlotFile
  echo ",\"$referenceWallet\":0" >> $latestInteractionSlotFile
  echo ",\"$scriptLabel\":0"     >> $latestInteractionSlotFile
  echo ",\"deadline\":0"         >> $latestInteractionSlotFile
  echo "}"                       >> $latestInteractionSlotFile
fi

export scriptUTxOsFile=$(getFileName ocfnScriptUTxOs)
touch $scriptUTxOsFile

# bounty escrow wallet address
export bountyEscrowWalletAddress="addr_test1qp72z5fzxc5yl8ht3wqme46reu04stq4mufm0660l7hkawl4w0e4s0x47jsnwf2g2dn7k4lq84skgrlyvz6rtvgh9l5qjmy54m"

# Generate a fresh protocol parametsrs JSON file.
generate_protocol_params() {
  # {{{
  $cli query protocol-parameters $MAGIC --out-file $protocolsFile
  # }}}
}
export protocolsFile="$preDir/protocol.json"
generate_protocol_params
export dummyTx="$preDir/tx.dummy"
export txBody="$preDir/tx.unsigned"
export txSigned="$preDir/tx.signed"
# }}}

# Convenient variable to replace the constant arguemnts for constructing a
# transaction.
export BUILD_TX_CONST_ARGS_NO_OUT_FILE="transaction build --babbage-era --cardano-mode $MAGIC --protocol-params-file $protocolsFile --cddl-format"
export BUILD_TX_CONST_ARGS="$BUILD_TX_CONST_ARGS_NO_OUT_FILE --out-file $txBody"


# CONTRACT'S CONSTANTS ================
export halfOfTheRegistrationFee=1500000
export governanceLovelaces=5000000
# =====================================


# REQUIRED FOR DEVELOPMENT:
###### tempRedeemer="$preDir/temp.redeemer"
###### touch $tempRedeemer
###### echo "{\"constructor\":11,\"fields\":[]}" > $tempRedeemer
devRedeemer="$preDir/dev.redeemer"
touch $devRedeemer
echo "{\"constructor\":20,\"fields\":[]}" > $devRedeemer
# =========================


# Takes at least 1 argument:
#   1. The signing key file.
#   *. Any additional signing key files.
sign_tx_by() {
  # {{{
  signArg=""
  for i in $@; do
    signArg="$signArg --signing-key-file $i"
  done
  $cli transaction sign    \
    --tx-body-file $txBody \
    $signArg               \
    $MAGIC                 \
    --out-file $txSigned
  # }}}
}


# Submits $txSigned to the chain.
submit_tx() {
  # {{{
  cliRes=$($cli transaction submit $MAGIC --tx-file $txSigned)
  if [ "$ENV" == "dev" ]; then
    echo $cliRes
  fi
  # }}}
}


# Takes 1 argument:
#   1. Datum file.
get_deadline_slot() {
  # {{{
  $qvf get-deadline-slot $(get_newest_slot) $1
  # }}}
}


get_current_slot() {
  # {{{
  $cli query tip $MAGIC | jq '.slot|tonumber'
  # }}}
}


# Gets the difference between current slot and latest interaction slot.
#
# Takes 1 argument:
#   1. Field of slot.
get_slot_difference(){
  # {{{
  currentSlot=$(get_current_slot)
  latest=$(cat $latestInteractionSlotFile | jq -r --arg f "$1" '.[$f]')
  echo $(expr $currentSlot - $latest)
  # }}}
}


# Returns the minimum of interaction slot count differences between 2 fields.
#
# Takes 2 argument:
#   1. Field of slot,
#   2. Second field.
get_slot_difference_2(){
  # {{{
  currentSlot=$(get_current_slot)
  latest1=$(cat $latestInteractionSlotFile | jq -r --arg f "$1" '.[$f]')
  latest2=$(cat $latestInteractionSlotFile | jq -r --arg f "$2" '.[$f]')
  diff1=$(expr $currentSlot - $latest1)
  diff2=$(expr $currentSlot - $latest2)
  if [ $diff1 -lt $diff2 ]; then
    echo $diff1
  else
    echo $diff2
  fi
  # }}}
}


# Picks the min value between a given slot, and 500 slots after the current
# slot. Meant to be used for `invalid-hereafter` argument of `cardano-cli`.
#
# Takes 1 argument:
#   1. Deadline slot.
cap_deadline_slot() {
  # {{{
  currentSlot=$(get_current_slot)
  currentSlotPlusFiveHundred=$(expr $currentSlot + 500)
  cappedSlot=$(( $1 < $currentSlotPlusFiveHundred ? $1 : $currentSlotPlusFiveHundred ))
  echo $cappedSlot
  # }}}
}


# Keeps querying the node until it first reaches a `syncProgress` of 100%.
# Continues polling until sees a "fresh" slot number to return.
get_newest_slot() {
  # {{{
  sync=$($cli query tip $MAGIC | jq '.syncProgress|tonumber|floor')
  while [ $sync -lt 100 ]; do
    sync=$($cli query tip $MAGIC | jq '.syncProgress|tonumber|floor')
  done
  initialSlot=$($cli query tip $MAGIC | jq .slot)
  newSlot=$initialSlot
  while [ $newSlot = $initialSlot ]; do
    newSlot=$($cli query tip $MAGIC | jq .slot)
  done
  echo $newSlot
  # }}}
}

# Takes 1 argument:
#   1. The target field to store.
store_current_slot() {
  # {{{
  newSlot=$(get_current_slot)
  newSlotObj=$(cat $latestInteractionSlotFile \
    | jq -c --argjson newS "$newSlot"         \
            --arg     newF "$1"               \
    '.[$newF] = $newS'
    )
  echo "$newSlotObj" > $latestInteractionSlotFile
  # }}}
}

# Takes 2 arguments:
#   1. The target field to store,
#   2. The second target field to store.
store_current_slot_2() {
  # {{{
  newSlot=$(get_current_slot)
  newSlotObj=$(cat $latestInteractionSlotFile \
    | jq -c --argjson newS "$newSlot"         \
            --arg     f0   "$1"               \
            --arg     f1   "$2"               \
    '.[$f0] = $newS | .[$f1] = $newS'
    )
  echo "$newSlotObj" > $latestInteractionSlotFile
  # }}}
}

# Takes 3 arguments:
#   1. The target field to store,
#   2. The second target field to store,
#   3. The third target field to store.
store_current_slot_3() {
  # {{{
  newSlot=$(get_current_slot)
  newSlotObj=$(cat $latestInteractionSlotFile          \
    | jq -c --argjson newS "$newSlot"                  \
            --arg     f0   "$1"                        \
            --arg     f1   "$2"                        \
            --arg     f2   "$3"                        \
    '.[$f0] = $newS | .[$f1] = $newS | .[$f2] = $newS'
    )
  echo "$newSlotObj" > $latestInteractionSlotFile
  # }}}
}


# Takes 1 argument:
#   1. The target field to store
wait_for_new_slot() {
  # {{{
  latest=$(cat $latestInteractionSlotFile | jq -r --arg f "$1" '.[$f]')
  if [ $latest == "null" ]; then
    if [ "$ENV" == "dev" ]; then
      echo "TARGET FIELD NOT FOUND."
    fi
  else
    current=$(get_current_slot)
    if [ "$ENV" == "dev" ]; then
      echo -e "\nWaiting for chain extension..."
    fi
    i=1
    sp='///---\\\|||'
    while [ $current -eq $latest ]; do
      if [ "$ENV" == "dev" ]; then
        printf "\b${sp:i++%${#sp}:1}"
      fi
      current=$(get_current_slot)
    done
    if [ "$ENV" == "dev" ]; then
      echo
      echo    "----------- CHAIN EXTENDED -----------"
      echo    "Latest interaction slot:      $latest"
      echo    "Current slot after extension: $current"
      echo -e "--------------------------------------\n"
    fi
  fi
  # }}}
}

sign_and_submit_tx() {
  # {{{
  sign_tx_by "$@"
  submit_tx
  # }}}
}


# Displays the utxo information table of one or multiple addresses.
#
# Takes at least 1 argument:
#   1. Wallet number/name,
#   *. Any additional wallet number/name.
show_utxo_tables () {
  # {{{
  for i in $@; do
    echo
    echo $i
    $cli query utxo $MAGIC --address $(cat $preDir/$i.addr)
  done
  # }}}
}


# Consumes all UTxOs at a wallet, and produces a single one in return, which
# carries all the Lovelaces. Does NOT support native tokens.
#
# Takes 1 argument:
#   1. Wallet number/name.
tidy_up_wallet() {
  # {{{
  addr=$(cat $preDir/$1.addr)
  inputs=$(get_all_input_utxos_at $1)
  $cli $BUILD_TX_CONST_ARGS $inputs --change-address $addr
  sign_and_submit_tx $preDir/$1.skey
  store_current_slot "dev"
  wait_for_new_slot "dev"
  show_utxo_tables $1
  # }}}
}


# Generates a key pair.
#
# Takes 1 argument:
#   1. Label for files.
generate_skey_and_vkey() {
  # {{{
  $cli address key-gen                      \
    --verification-key-file $preDir/$1.vkey \
    --signing-key-file $preDir/$1.skey
  # }}}
}


# Creates an address file from a verification file.
# 
# Takes 1 argument:
#   1. Label for files.
vkey_to_address() {
  # {{{
  $cli address build $MAGIC                         \
    --payment-verification-key-file $preDir/$1.vkey \
    --out-file $preDir/$1.addr
  # }}}
}


# Creates a public key hash file from a verification file.
# 
# Takes 1 argument:
#   1. Label for files.
vkey_to_public_key_hash() {
  # {{{
  $cli address key-hash                             \
    --payment-verification-key-file $preDir/$1.vkey \
    --out-file $preDir/$1.pkh
  # }}}
}


# Takes a label, and generates the corresponding wallet files.
#
# Takes 1 argument:
#   1. Label for files.
generate_wallet() {
  # {{{
  generate_skey_and_vkey $1
  vkey_to_address $1
  vkey_to_public_key_hash $1
  # }}}
}


# Creates the associated address from a Plutus script File.
# 
# Doesn't take any arguments, uses global variables.
plutus_script_to_address() {
  # {{{
  $cli address build-script       \
    $MAGIC                        \
    --script-file $mainScriptFile \
    --out-file $scriptAddressFile
  # }}}
}


# Given a numeric range (inclusive), generates all four files of a wallet
# (.vkey, .skey, .addr, .pkh) for each number.
#
# This function has builtin safety to prevent rewrites and wallet loss.
#
# For now, the maximum number of generated wallets is capped at 100000.
# 
# Takes 2 arguments:
#   1. Starting number,
#   2. Ending number.
generate_wallets_from_to() {
  # {{{
  max_amt=100000
  if [ `expr $2 - $1` -ge $max_amt ]; then
  echo "That's over 100,000 wallets generated. Please reconsider. Edit fn if you really want to."
  else
  # Main loop:
  for i in $(seq $1 $2); do
    if [ -f $preDir/$i.vkey ] || [ -f $preDir/$i.skey ] || [ -f $preDir/$i.addr ] || [ -f $preDir/$i.pkh ]; then
      echo "Error! $i.vkey, $i.skey, $i.addr, or $i.pkh already exist. Move/rename/remove them first and run again."
      break
    else
      generate_wallet $i
    fi
  done
  fi
  # }}}
}


# Returns the "first" UTxO from a wallet (the first in the table returned by
# the `cardano-cli` application), formatted as `<txId>#<txIndex>`.
#
# Takes 1 argument:
#   1. Wallet number/name.
get_first_utxo_of() {
  # {{{
  echo `$cli query utxo               \
    --address $(cat $preDir/$1.addr)  \
    $MAGIC                            \
    | sed 1,2d                        \
    | awk 'FNR == 1 {print $1"#"$2}'`
  # }}}
}

# Takes 2 arguments:
#   1. Wallet number/name,
#   2. Row of the UTxO table.
get_nth_utxo_of() {
  # {{{
  echo `$cli query utxo                  \
    --address $(cat $preDir/$1.addr)     \
    $MAGIC                               \
    | sed 1,2d                           \
    | awk 'FNR == '$2' {print $1"#"$2}'`
  # }}}
}


# Returns a list of all UTxO's available at the given wallet address file, each
# prefixed with "--tx-in" for convenient use while constructing a transaction.
# 
# Takes 1 argument:
#   1. Wallet number/name.
get_all_input_utxos_at() {
  # {{{
  echo `$cli query utxo                           \
    --address $(cat $preDir/$1.addr)              \
    $MAGIC                                        \
    | sed 1,2d                                    \
    | awk '{print $1"#"$2}'                       \
    | sed 's/^/--tx-in /'                         \
    | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g'`
  # }}}
}


# Given a numeric range (inclusive), displays the utxo information table from
# the address of the .addr file of each number, and displays any addresses
# provided after the numeric range.
# Takes at least 2 arguments:
#
#   1. Starting number,
#   2. Ending number,
#   *. Any additional wallet address files.
show_utxo_tables_from_to () {
  # {{{
    for i in $(seq $1 $2)
    do
        show_utxo_tables $i
    done

    shift 2
    if [ -n $1 ]
    then
        show_utxo_tables $@
    fi
  # }}}
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
# Takes 4 arguments:
#   1. The spending wallet number/name,
#   2. Starting number of the receiving wallets,
#   3. Ending number of the receiving wallets,
#   4. Total amount of Lovelace to be distributed equally,
distribute_from_to_wallets() {
  # {{{
    spendingAddr=$(cat $preDir/$1.addr)
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
        addr=$(cat $preDir/$i.addr)
        tx_out_str=$tx_out_str' --tx-out '$addr'+'$lovelace_amt
        # tx_out_str="$tx_out_str --tx-out \"$addr + $lovelace_amt\""
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
    $cli transaction build             \
        --babbage-era                  \
        $MAGIC                         \
        $tx_in_str                     \
        --change-address $spendingAddr \
        $tx_out_str                    \
        --cddl-format                  \
        --out-file $txBody

    $cli transaction sign                  \
        --tx-body-file $txBody             \
        --signing-key-file $preDir/$1.skey \
        $MAGIC                             \
        --out-file $txSigned

    $cli transaction submit \
        $MAGIC              \
        --tx-file $txSigned
  # }}}
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
#   3. Receiving wallet's number/name.
drain_from_wallets_to() {
  # {{{
    tx_in_str=''
    signing_keys_str=''

    # Build the string of --tx-in's
    for i in $(seq $1 $2)
    do
        # tx_in_str=$tx_in_str$(get_all_input_utxos_at $i)' '
        tx_in_str="$tx_in_str $(get_all_input_utxos_at $i)"
    done

    # Build the string of signing key files
    for i in $(seq $1 $2)
    do
        # signing_keys_str=$signing_keys_str' --signing-key-file '$i'.skey'
        signing_keys_str="$signing_keys_str --signing-key-file $preDir/$i.skey"
    done

    # Transaction
    $cli transaction build                      \
        --babbage-era                           \
        $MAGIC                                  \
        $tx_in_str                              \
        --change-address $(cat $preDir/$3.addr) \
        --out-file $txBody

    $cli transaction sign      \
        --tx-body-file $txBody \
        $signing_keys_str      \
        $MAGIC                 \
        --out-file $txSigned

    submit_tx
  # }}}
}


# Helper function that returns how much Lovelace is held in the first UTxO of
# the given wallet address.
#
# Takes 1 argument:
#   1. User's wallet address file.
get_first_lovelace_count_of() {
  # {{{
  echo `$cli query utxo          \
    --address $(cat $1)          \
    $MAGIC                       \
    | sed 1,2d                   \
    | awk 'FNR == 1 {print $3}'`
  # }}}
}


# Takes 1 arguemnt:
#   1. Wallet label.
get_wallet_lovelace_utxos() {
  # {{{
  $cli query utxo $MAGIC --address $(cat $preDir/$1.addr) --out-file $queryJSONFile
  jq -c \
    'to_entries
    | map
        ( ( .value
          | .value
          | to_entries
          | map(select(.key == "lovelace"))
          ) as $hasLovelace
        | { utxo: .key
          , lovelace: (.value | .value | .lovelace)
          }
        )' $queryJSONFile
  # }}}
}


# Takes 1 arguemnt:
#   1. Script address.
get_all_script_utxos_datums_values() {
  # {{{
  $cli query utxo $MAGIC --address $1 --out-file $queryJSONFile
  utxos=$(cat $queryJSONFile | jq -c 'to_entries
    | map(select((.value | .value | to_entries | length) == 2))
    | map
        ( ( .value
          | .value
          | to_entries
          | map(select(.key != "lovelace"))
          ) as $notLovelace
        | { utxo: .key
          , address: (.value | .address)
          , datum: (.value | .inlineDatum)
          , lovelace: (.value | .value | .lovelace)
          , asset:
              ( $notLovelace
              | map
                  ( (.value | to_entries | map(.key) | .[0]) as $tn
                  | .key + (if $tn == "" then "" else ("." + $tn) end)
                  )
              | .[0]
              )
          , assetCount:
              ( $notLovelace
              | map(.value | to_entries | map(.value) | .[])
              | .[0]
              )
          }
        )')
  datums="$(echo "$utxos" | jq -c 'map(.datum) | .[]')"
  count=0
  for d in $datums; do
    datumCBOR=$($qvf data-to-cbor "$d")
    utxos="$(echo "$utxos"            \
      | jq -c --arg cbor "$datumCBOR" \
              --argjson i "$count"    \
      '.[$i] |= (. += {"datumCBOR": ($cbor | tostring)})'
    )"
    count=$(expr $count + 1)
  done
  echo $utxos
  # }}}
}


# Takes 3 arguments:
#   1. Argument prefex (should be either "--tx-in" or "--read-only-tx-in-reference"),
#   2. The constant part for all the input UTxOs,
#   3. The array of printed UTxOs returned by `qvf-cli`.
qvf_output_to_tx_ins() {
  # {{{
  utxos="$(echo "$3" | jq -c 'map(.utxo) | .[]')"
  fnl=""
  for u in $utxos; do
    fnl="$fnl $1 $(remove_quotes $u) "$2""
  done
  echo "$fnl"
  # }}}
}


# Takes 1 argument:
#   1. The JSON output array from `qvf-cli`.
qvf_output_to_tx_outs() {
  # {{{
  dDir="$preDir/datums"
  rm -rf $dDir
  mkdir -p $dDir
  outputs=""
  for obj in $(echo "$1" | jq -c '.[]'); do
    addr="$(echo "$obj" | jq -r -c '.address')"
    lovelace="$(echo "$obj" | jq -c '.lovelace')"
    cbor="$(echo "$obj" | jq -r -c '.datumCBOR')"
    if [ "$cbor" == "null" ]; then
      outputs="$outputs --tx-out \"$addr + $lovelace lovelace\""
    else
      assetCount="$(echo "$obj" | jq -c '.assetCount')"
      asset="$(echo "$obj" | jq -r -c '.asset')"
      datum="$($qvf cbor-to-data $cbor)"
      dHash="$(hash_datum "$datum")"
      proxyFile=$dDir/$dHash
      echo "$datum" > $proxyFile
      outputs="$outputs --tx-out \"$addr + $lovelace lovelace + $assetCount $asset\"
        --tx-out-inline-datum-file $proxyFile
        "
    fi
  done
  echo $outputs
  # }}}
}


# Takes 2 arguemnts:
#   1. Script address,
#   2. Authentication asset ("$currencySymbol.$tokenName").
get_script_utxos_datums_values() {
  # {{{
  get_all_script_utxos_datums_values $1 | jq -c --arg authAsset "$2" 'map(select(.asset == $authAsset))'
  # }}}
}


get_governance_utxo() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  govAsset="$(cat $govSymFile)"
  govUTxOs="$(get_script_utxos_datums_values $qvfAddress $govAsset)"
  govUTxOObj=""
  temp0="$(echo "$govUTxOs" | jq -c 'map(.datum) | .[0]')"
  isInfo=$($qvf datum-is DeadlineDatum "$temp0")
  if [ $isInfo == "True" ]; then
    govUTxOObj="$(echo "$govUTxOs" | jq -c '.[1]')"
  else
    govUTxOObj="$(echo "$govUTxOs" | jq -c '.[0]')"
  fi
  echo $govUTxOObj
  # }}}
}


get_deadline_utxo() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  govAsset="$(cat $govSymFile)"
  govUTxOs="$(get_script_utxos_datums_values $qvfAddress $govAsset)"
  govUTxOObj=""
  temp0="$(echo "$govUTxOs" | jq -c 'map(.datum) | .[0]')"
  isInfo=$($qvf datum-is DeadlineDatum "$temp0")
  if [ $isInfo == "True" ]; then
    govUTxOObj="$(echo "$govUTxOs" | jq -c '.[0]')"
  else
    govUTxOObj="$(echo "$govUTxOs" | jq -c '.[1]')"
  fi
  echo $govUTxOObj
  # }}}
}


# Takes 1 argument:
#   1. A JSON array such that all its elements are objects that have a
#      "lovelace" field.
get_total_lovelaces_from_json() {
  # {{{
  echo "$1" | jq 'map(.lovelace) | reduce .[] as $l (0; . + $l)'
  # }}}
}


# Takes 1 argument:
#   1. Script data JSON.
hash_datum() {
  proxyFile=$preDir/tmp.json
  echo "$1" > $proxyFile
  $cli transaction hash-script-data --script-data-file $proxyFile
}


# Takes 1 argument:
#   1. A JSON.
jq_zip() {
  # {{{
  echo "$1" | jq -c '.[1:] as $a | .[:-1] as $b | [$b,$a] | transpose'
  # }}}
}


# Given a string put together by `jq`, this function applies some modifications
# to make the string usable by bash.
#
# Takes 2 arguments:
#   1. A string returned by `jq`'s `map`,
#   2. Initial array length.
jq_to_bash_3() {
  # {{{
  count=1
  last=$(echo "$2" | jq '(3 * tonumber)')
  if [ $last -eq 0 ]; then
    return 1
  fi
  output=""
  for i in $1; do
    if [ $count -eq 1 ]; then
      output="$(remove_quotes $i)"
    elif [ $count -eq $last ]; then
      lastCharRemoved=${i::-1}
      output="$output $(remove_back_slashes $lastCharRemoved)"
    else
      output="$output $(remove_back_slashes $i)"
    fi
    count=$(expr $count + 1)
  done
  echo "$output"
  # }}}
}


### FUNCTIONS THAT ARE USABLE AFTER AT LEAST ONE PROJECT REGISTRATION ###

# Takes no arguments.
get_all_projects_utxos_datums_values() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  regSym=$(cat $regSymFile)
  get_all_script_utxos_datums_values $qvfAddress | jq -c --arg regSym "$regSym" 'map(select(.asset | contains($regSym)))'
  # }}}
}

get_all_projects_info_utxos_datums_values() {
  constr=$($qvf get-constr-index ProjectInfo)
  get_all_projects_utxos_datums_values | jq -c --arg constr "$constr" 'map(select((.datum .constructor) == ($constr | tonumber)))'
}
get_all_projects_state_utxos_datums_values() {
  constr=$($qvf get-constr-index ProjectInfo)
  get_all_projects_utxos_datums_values | jq -c --arg constr "$constr" 'map(select((.datum .constructor) != ($constr | tonumber)))'
}

# Takes no arguments.
find_registered_projects_count() {
  # {{{
  cat $registeredProjectsFile | jq length
  # }}}
}


# Takes 1 argument:
#   1. The "index" of the project (0 for the project that registered first, and
#      so on). Clamps implicitly.
project_index_to_token_name() {
  # {{{
  clamped="$1"
  min=0
  max="$(find_registered_projects_count)"
  if [ "$clamped" -lt "$min" ]; then
    clamped=$min
  elif [ "$clamped" -gt "$max" ]; then
    clamped="$max"
  fi
  cat $registeredProjectsFile | jq -r --argjson i "$clamped" '.[$i] | .tn'
  # sed "${clamped}q;d" $registeredProjectsFile
  # }}}
}


# Takes 1 argument:
#   1. Project's ID (token name).
get_projects_state_utxo() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  projectAsset="$(cat $regSymFile).$1"
  projectUTxOs="$(get_script_utxos_datums_values $qvfAddress $projectAsset)"
  projectUTxOObj=""
  temp0="$(echo "$projectUTxOs" | jq -c 'map(.datum) | .[0]')"
  isInfo=$($qvf datum-is ProjectInfo "$temp0")
  if [ $isInfo == "True" ]; then
    projectUTxOObj="$(echo "$projectUTxOs" | jq -c '.[1]')"
  else
    projectUTxOObj="$(echo "$projectUTxOs" | jq -c '.[0]')"
  fi
  echo $projectUTxOObj
  # }}}
}


# Takes 1 argument:
#   1. Project's ID (token name).
get_projects_info_utxo() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  projectAsset="$(cat $regSymFile).$1"
  projectUTxOs="$(get_script_utxos_datums_values $qvfAddress $projectAsset)"
  projectUTxOObj=""
  temp0="$(echo "$projectUTxOs" | jq -c 'map(.datum) | .[0]')"
  isInfo=$($qvf datum-is ProjectInfo "$temp0")
  if [ $isInfo == "True" ]; then
    projectUTxOObj="$(echo "$projectUTxOs" | jq -c '.[0]')"
  else
    projectUTxOObj="$(echo "$projectUTxOs" | jq -c '.[1]')"
  fi
  echo $projectUTxOObj
  # }}}
}


# Takes 1 argument:
#   1. The "number" of the project (first registered project is represented
#      with 1, and so on). Clamps implicitly.
get_nth_projects_state_utxo() {
  # {{{
  get_projects_state_utxo $(project_index_to_token_name $1)
  # }}}
}


# Takes 1 argument:
#   1. Project's ID (token name).
get_projects_donation_utxos() {
  # {{{
  qvfAddress=$(cat $scriptAddressFile)
  donAsset="$(cat $donSymFile).$1"
  get_script_utxos_datums_values $qvfAddress $donAsset
  # }}}
}


# Takes 1 argument:
#   1. The "number" of the project (first registered project is represented
#      with 1, and so on). Clamps implicitly.
get_nth_projects_donation_utxos() {
  # {{{
  get_projects_donation_utxos $(project_index_to_token_name $1)
  # }}}
}


# Takes 1 argument:
#   1. The token name of the project.
get_projects_owner_address() {
  # {{{
  cat $registeredProjectsFile | jq -r --arg tn "$1" 'map(select(.tn == $tn)) | .[0] | .address'
  # }}}
}
#########################################################################


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
interact_with_smart_contract() {
  # {{{

    # Build script address from a script, if script address does not exist. 
    # The address name is the same as the script, except its extension is changed to .addr
    # script_addr=$($3 | sed "s/\..*/.addr/") # Name is $3 with its ext changed to .addr
    # Safety check to not overwrite any existing file, and to avoid rebuilding if already built.
    if [ -f $script_addr ]
    then
    echo "Using the script address $script_addr, which already exists. If this is incorrect, then move, rename, or change $script_addr and run again."
    else
    plutus_script_to_address # $3 $script_addr # Builds script file address
    fi
    script_addr=$scriptAddressFile

    users_utxo=$(get_first_utxo_of $1)
    script_holding=$(get_first_lovelace_count_of $script_addr)
    extra_output=$(expr $6 + $script_holding)

    $cli transaction build                        \
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

    $cli transaction sign                         \
        --tx-body-file tx.raw                     \
        --signing-key-file $2                     \
        $MAGIC                                    \
        --out-file tx.signed

    $cli transaction submit                       \
        $MAGIC                                    \
        --tx-file tx.signed
  # }}}
}

# Runs qvf-cli cmds with nix-shell from outside nix-shell
# Uses a HERE doc to do this
# PARAMS: $1=donor_pkh_file $2=receiver_pkh_file $3=lovelace_amt $4=current_datum
update_datum_donate_qvf_cli() {
  # {{{
    # Edit these: ---------
    path_to_plutus_apps=$HOME/plutus-apps
    path_to_quadratic_voting=$HOME/quadraticvoting
    current_path=$(pwd)
    # ---------------------

    donor_pkh_file=$1
    receiver_pkh_file=$2
    lovelace_amt=$3

    # Make the script to execute within the nix-shell with a HERE DOC
    cat > "$path_to_plutus_apps"/update-datum.sh <<EOF
#! /usr/bin/env nix-shell
#! nix-shell -i sh

cd $path_to_quadratic_voting
. scripts/test_remote.sh
donorsPKH=$(cat $current_path/$1)
obj=\$(find_utxo_with_project \$scriptAddr "\$policyId\$tokenName" \$(cat $2))
len=\$(echo \$obj | jq length)
if [ \$len -eq 0 ]; then
    echo "FAILED to find the project."
else
    currDatum="$current_path/curr.datum"
    updatedDatum="$current_path/updated.datum"
    action="$current_path/donate.redeemer"
    obj=\$(echo \$obj | jq .[0])
    utxo=\$(echo \$obj | jq .utxo)
    datumHash=\$(echo \$obj | jq .datumHash)
    datumValue=\$(echo \$obj | jq .datumValue)
    lovelace=\$(echo \$obj | jq .lovelace | jq tonumber)
    newLovelace=\$(expr \$lovelace + $3)
    echo \$lovelace
    echo \$newLovelace > newLovelace
    echo \$datumValue > \$currDatum
    $qvf donate $(cat $donor_pkh_file) $(cat $receiver_pkh_file) \$lovelace_amt \$current_datum out_datum.json out_redeem.json
    $qvf pretty-datum \$(cat \$updatedDatum)
    cp out_datum.json "$current_path" # Optional, see how workflow works out
    cp out_redeem.json "$current_path" # Optional, see how workflow works out
    cp newLovelace "$current_path" # Optional, see how workflow works out
    echo "DONE."
fi
exit # Exit nix-shell
EOF
    # Run the HERE file commands in nix-shell
    cd "$path_to_plutus_apps"
    chmod +x update-datum.sh
    ./update-datum.sh
    cd "$current_path"
  # }}}
}

# WIP
# cardano-cli transaction cmd to donate
# PARAMS: $1=donorAddrFile $2=donorSKeyFile $3=utxoFromDonor $4=utxoAtScript $5=currentDatum $6lovelace_amt_script $7=lovelace_amt_donation
donate_to_smart_contract() {
  # {{{
    # Edit these: ---------
    authAsset=62a65c6ce2c30f7040f0bc8cc5eb5f3f07521757125a03d743124a54.517561647261546f6b656e
    scriptAddr=addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx
    scriptFile="qvf.plutus"      # The Plutus script file (qvf.plutus)
    donorAddrFile="$1"   # The file that contains donor's wallet address.
    donorSKeyFile="$2"   # The file that contains donor's signing key.
    #utxoFromDonor="efd9d27b0ba008b8495aee6d4d01c5ebe0c281b55a623a31fe0b631c6365cb22"   # A UTxO from donor's wallet that has enough ADA for donation, tx fee and collateral.
    utxoFromDonor="$3"   # A UTxO from donor's wallet that has enough ADA for donation, tx fee and collateral.
    utxoAtScript="$4"    # The UTxO at the script with the current datum attached.
    currentDatum="$5"    # JSON file containing current state of the contract, about to be updated.
    newDatum="out_datum.json"        # JSON file containing updated state of the contract.
    redeemer="out_redeem.json"        # JSON file containing the `Donate` redeemer.
    lovelace_amt_script="$6"
    lovelace_amt_donation="$7"
    newLovelaceCount=$(expr lovelace_amt_script + lovelace_amt_donation) # Current Lovelace count of $utxoAtScript, plus the donated amount.
    # ---------------------

    # Construct the transaction:
    $cli transaction build --babbage-era $MAGIC                            \
        --tx-in $utxoFromDonor                                             \
        --tx-in-collateral $utxoFromDonor                                  \
        --tx-in $utxoAtScript                                              \
        --tx-in-datum-file $currentDatum                                   \
        --tx-in-script-file $scriptFile                                    \
        --tx-in-redeemer-file $redeemer                                    \
        --tx-out "$scriptAddr + $newLovelaceCount lovelace + 1 $authAsset" \
        --tx-out-datum-embed-file $newDatum                                \
        --change-address $(cat $donorAddrFile)                             \
        --protocol-params-file protocol.json                               \
        --out-file tx.unsigned

    # Sign the transaction:
    $cli transaction sign $MAGIC          \
        --tx-body-file tx.unsigned        \
        --signing-key-file $donorSKeyFile \
        --out-file tx.signed

    # Submit the transaction:
    $cli transaction submit $MAGIC --tx-file tx.signed
  # }}}
}


# Checks if the $keyHolder wallet exists (properly), and that it has a single
# UTxO with enough Ada inside.
#
# If the wallet files exist partially, this function terminates the script
# without any changes. If there are no wallet files, the $keyHolder wallet is
# generated, but the script is terminated, prompting the user to send some Ada
# to the wallet.
#
# If the wallet is present, it's made sure the total Lovelace count is more
# than the minimum, and if they are spread out between multiple UTxOs, it'll
# invoke the `tidy_up_wallet` function so that all the money is collected
# inside a single UTxO.
# {{{
if [ -f $preDir/$keyHolder.vkey ] && [ -f $preDir/$keyHolder.skey ] && [ -f $preDir/$keyHolder.addr ] && [ -f $preDir/$keyHolder.pkh ]; then
  utxos=$(get_wallet_lovelace_utxos $keyHolder)
  utxoCount=$(echo "$utxos" | jq length)
  totalLovelace=$(get_total_lovelaces_from_json "$utxos")
  if [ $totalLovelace -ge $minStartingLovelaces ] || [ -f $scriptAddressFile ]; then
    if [ $utxoCount -gt 1 ]; then
      echo "Multiple UTxOs found in the key holder's wallet. Tidying up..."
      tidy_up_wallet $keyHolder
      echo "Done. The key holder wallet is ready."
    fi
    export keyHoldersAddress=$(cat "$preDir/$keyHolder.addr")
    export keyHoldersPubKeyHash=$(cat "$preDir/$keyHolder.pkh")
    export keyHoldersSigningKeyFile="$preDir/$keyHolder.skey"
  else
    echo "The key holder wallet doesn't have enough Ada. Please make sure a"
    echo -e "minimum of $WHITE$minStartingAda Ada$NO_COLOR is available:"
    echo ""
    echo -e "$WHITE$(cat $preDir/$keyHolder.addr)$NO_COLOR"
    return 1
  fi
elif [ -f $preDir/$keyHolder.vkey ] || [ -f $preDir/$keyHolder.skey ] || [ -f $preDir/$keyHolder.addr ] || [ -f $preDir/$keyHolder.pkh ]; then
  echo "Some key holder wallet files are missing."
  return 1
else
  generate_wallet $keyHolder
  echo "No key holder wallet was found. The wallet is generated for you."
  echo -e "Please deposit a minimum of $WHITE$minStartingAda Ada$NO_COLOR before proceeding:"
  echo ""
  echo -e "$WHITE$(cat $preDir/$keyHolder.addr)$NO_COLOR"
  return 1
fi
# }}}


# A similar check for $collateralKeyHolder. This wallet is meant for providing
# the collateral for user-facing endpoints to provide a more "approachable"
# experience for non-technical users.
# {{{
if [ -f $preDir/$collateralKeyHolder.vkey ] && [ -f $preDir/$collateralKeyHolder.skey ] && [ -f $preDir/$collateralKeyHolder.addr ] && [ -f $preDir/$collateralKeyHolder.pkh ]; then
  utxos=$(get_wallet_lovelace_utxos $collateralKeyHolder)
  utxoCount=$(echo "$utxos" | jq length)
  totalLovelace=$(get_total_lovelaces_from_json "$utxos")
  if [ $totalLovelace -ge $minCollateralLovelaces ]; then
    if [ $utxoCount -gt 1 ]; then
      echo "Multiple UTxOs found in the key holder's wallet. Tidying up..."
      tidy_up_wallet $collateralKeyHolder
      echo "Done. The key holder wallet is ready."
    fi
    export collateralKeyHoldersAddress=$(cat "$preDir/$collateralKeyHolder.addr")
    export collateralKeyHoldersPubKeyHash=$(cat "$preDir/$collateralKeyHolder.pkh")
    export collateralKeyHoldersSigningKeyFile="$preDir/$collateralKeyHolder.skey"
  else
    echo "The collateral key holder wallet doesn't have enough Ada. Please make sure a"
    echo -e "minimum of $WHITE$minCollateralAda Ada$NO_COLOR is available:"
    echo ""
    echo -e "$WHITE$(cat $preDir/$collateralKeyHolder.addr)$NO_COLOR"
    return 1
  fi
elif [ -f $preDir/$collateralKeyHolder.vkey ] || [ -f $preDir/$collateralKeyHolder.skey ] || [ -f $preDir/$collateralKeyHolder.addr ] || [ -f $preDir/$collateralKeyHolder.pkh ]; then
  echo "Some key holder wallet files are missing."
  return 1
else
  generate_wallet $collateralKeyHolder
  echo "No collateral key holder wallet was found. The wallet is generated for you."
  echo -e "Please deposit a minimum of $WHITE$minCollateralAda Ada$NO_COLOR before proceeding:"
  echo ""
  echo -e "$WHITE$(cat $preDir/$collateralKeyHolder.addr)$NO_COLOR"
  return 1
fi
# }}}


# Consumes all the UTxOs sitting at the $referenceWallet, and sends them to the
# $keyHolder wallet.
#
# NOTE: This function fails if any assets other than Lovelaces are present
#       inside the $referenceWallet.
#
# Takes no arguments.
deplete_reference_wallet() {
  # {{{
  generate_protocol_params
  $cli $BUILD_TX_CONST_ARGS                    \
    $(get_all_input_utxos_at $referenceWallet) \
    --change-address $keyHoldersAddress
  sign_and_submit_tx $preDir/$referenceWallet.skey
  store_current_slot $referenceWallet
  wait_for_new_slot $referenceWallet
  show_utxo_tables $referenceWallet
  # }}}
}


# A similar check for the $referencWallet. There are 3 circumstances that lead
# to an automated depletion of the $referenceWallet into $keyHolder:
#   - There are some Lovelaces in the wallet, but the UTxO count is not exactly
#     the same as the $scriptCount,
#   - There are exactly $scriptCount UTxOs in the wallet, but there is no
#     $scriptAddressFile,
#   - There are exactly $scriptCount UTxOs in the wallet, but there are no
#     Lovelaces stored at the found script address.
# {{{
if [ -f $preDir/$referenceWallet.vkey ] && [ -f $preDir/$referenceWallet.skey ] && [ -f $preDir/$referenceWallet.addr ] && [ -f $preDir/$referenceWallet.pkh ]; then
  utxos=$(get_wallet_lovelace_utxos $referenceWallet)
  utxoCount=$(echo "$utxos" | jq length)
  totalLovelace=$(get_total_lovelaces_from_json "$utxos")
  if [ $utxoCount -eq 3 ]; then
    if [ -f $scriptAddressFile ]; then
      scriptUTxOs=$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))
      totalLovelace=$(get_total_lovelaces_from_json "$scriptUTxOs")
      if [ $totalLovelace -eq 0 ]; then
        echo "The reference wallet is not empty, while the contract is."
        echo "Depleting the reference wallet into key holder's..."
        deplete_reference_wallet
        tidy_up_wallet $keyHolder
      fi
    else
      echo "The reference wallet is not empty, while there is no contract"
      echo "address file stored. Depleting the reference wallet..."
      deplete_reference_wallet
      tidy_up_wallet $keyHolder
    fi
  # elif [ $totalLovelace -gt 0 ]; then
  #   deplete_reference_wallet
  #   tidy_up_wallet $keyHolder
  fi
elif [ -f $preDir/$referenceWallet.vkey ] || [ -f $preDir/$referenceWallet.skey ] || [ -f $preDir/$referenceWallet.addr ] || [ -f $preDir/$referenceWallet.pkh ]; then
  echo "Some reference wallet files are missing."
  return 1
else
  generate_wallet $referenceWallet
fi
# }}}

export referenceWalletAddress=$(cat "$preDir/$referenceWallet.addr")
