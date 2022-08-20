. $HOME/code/snapbrillia/quadraticvoting/scripts/env.sh
. $HOME/code/snapbrillia/quadraticvoting/scripts/blockfrost.sh

# scriptAddr="addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx"
scriptAddr=$(cat $scriptAddressFile)
policyId=$(cat $policyIdFile)
tokenName=$(cat $tokenNameHexFile)
authAsset="$policyId.$tokenName"


invalidBefore="--invalid-before"
invalidAfter="--invalid-hereafter"


# CAUTIOUSLY DEFINING GLOBAL VARIABLES:
currDatum="$preDir/curr.datum"
updatedDatum="$preDir/updated.datum"
action="$preDir/action.redeemer"
#
utxo=""
datumHash=""
datumValue=""
lovelace=""
newLovelace=""

# Takes 2 (or 3) arguments:
#   1. The JSON object,
#   2. Lovelace to add to script's UTxO,
#   3. (Optional) Explicit value for $newLovelace.
setCommonVariables() {
  utxo=$(echo $1 | jq .utxo)
  datumHash=$(echo $1 | jq .datumHash)
  datumValue=$(echo $1 | jq -c .datumValue)
  if [ ! -z "$3" ]; then
    newLovelace="$3"
  else
    lovelace=$(echo $1 | jq .lovelace | jq tonumber)
    newLovelace=$(expr $lovelace + $2)
  fi
  if [ -f $currDatum ]; then
    echo $datumValue > $currDatum
  else
    touch $currDatum
    echo $datumValue > $currDatum
  fi
}
# =====================================


get_current_state() {
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName)
  utxo=$(echo $obj | jq .utxo)
  datumHash=$(echo $obj | jq .datumHash)
  datumValue=$(bf_get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
  $qvf pretty-datum $datumValue
}


# Takes 2 (up to 7) arguments:
#   1. Payer's wallet number/name.
#   2. Payer's UTxO to be spent.
#   3. (Optional) Custom change address.
#   4. (Optional) Required signer's wallet number/name.
#   5. (Optional) Extra outputs.
#   6. (Optional) Custom time interval argument.
update_contract() {
  donorAddrFile="$preDir/$1.addr"
  donorSKeyFile="$preDir/$1.skey"
  utxoFromDonor="$2"
  utxoAtScript=$(remove_quotes $utxo)
  changeAddress=$(cat $donorAddrFile)
  deadlineArg="$invalidAfter $(cat $deadlineSlotFile)"
  if [ ! -z "$3" ]; then
    changeAddress=$3
  fi
  additionalSigner=""
  requiredSigner=""
  if [ ! -z "$4" ]; then
    additionalSigner="$preDir/$4.skey"
    requiredSigner="--required-signer $additionalSigner"
  fi
  if [ ! -z "$6" ]; then
    deadlineArg=$6
  fi

  args='--tx-in '$utxoFromDonor' --tx-in-collateral '$utxoFromDonor' --tx-in '$utxoAtScript' --tx-in-datum-file '$currDatum' --tx-in-script-file '$scriptPlutusFile' --tx-in-redeemer-file '$action' --tx-out "'$scriptAddr' + '$newLovelace' lovelace + 1 '$authAsset'" --tx-out-datum-embed-file '$updatedDatum' '$5' --change-address '$changeAddress' '$deadlineArg' --protocol-params-file '$protocolsFile' --cddl-format '$requiredSigner' --out-file '$txBody

  echo $args
  # echo $donorAddrFile
  # echo $donorSKeyFile
  # echo $utxoFromDonor
  # echo $utxoAtScript
  # echo $requiredSigner
  # echo $deadlineArg
  # echo $5
  # Generate the protocol parameters:
  generate_protocol_params

  # Construct the transaction:
  $cli transaction build --babbage-era $MAGIC                     \
    --tx-in $utxoFromDonor                                        \
    --tx-in-collateral $utxoFromDonor                             \
    --tx-in $utxoAtScript                                         \
    --tx-in-datum-file $currDatum                                 \
    --tx-in-script-file $scriptPlutusFile                         \
    --tx-in-redeemer-file $action                                 \
    --tx-out "$scriptAddr + $newLovelace lovelace + 1 $authAsset" \
    --tx-out-datum-embed-file $updatedDatum                    $5 \
    --change-address $changeAddress                  $deadlineArg \
    --protocol-params-file $protocolsFile                         \
    --cddl-format   $requiredSigner                               \
    --out-file $txBody
  
  # Sign the transaction:
  sign_tx_by $donorSKeyFile $additionalSigner
  
  # Submit the transaction:
  submit_tx

  # Store current slot number for future interactions:
  store_current_slot

  echo
  $qvf pretty-datum $(cat $updatedDatum)
  echo "DONE."
}


# Takes 1 (or 2) argument(s):
#   1. New POSIX time in milliseconds.
#   2. (Optional) Number/name of an alternate wallet for covering the fee.
#   3. (Optional) Flag to indicate whether to read the current datum from
#      local storage.
set_deadline() {
  payer=$keyHolder
  if [ ! -z "$2" ]; then
    payer=$2
  fi
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr "$policyId$tokenName" | jq -c .)
  case $3 in
    local)
      datumValue=$(cat $preDir/initial.datum | jq -c .)
      obj=$(bf_add_datum_value_to_utxo $obj "$datumValue")
      ;;
    *)
      obj=$(bf_add_datum_value_to_utxo $obj)
      datumValue=$(echo $obj | jq .datumValue)
      ;;
  esac
  setCommonVariables $(echo $obj | jq -c .) 0
  txIn=$(get_first_utxo_of $payer)
  echo "Generating datum and redeemer files..."
  $qvf set-deadline  \
       $1            \
       $currDatum    \
       $updatedDatum \
       $action
  echo "Datum and redeemer files generated."
  deadlineSlot=$(get_deadline_slot $updatedDatum)
  echo $deadlineSlot > $deadlineSlotFile
  wait_for_new_slot
  update_contract              \
    $payer                     \
    $txIn                      \
    $(cat $preDir/$payer.addr) \
    $keyHolder
}



# Takes 4 (or infinitly many) arguments:
#   1. Payer's wallet number/name,
#   2. Donor's public key hash,
#   3. Target project's public key hash,
#   4. Donation amount,
#   *. (Optional) Infinite number of public key hash and donation amounts,
#   n. (Optional last argument) Custom change address.
donate_from_to_with() {
  payer=$1
  donorsPKH=$2
  changeAddr=$(cat $preDir/$1.addr)
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
  txIn=$(get_first_utxo_of $payer)

  shift 2
  str_pair=''
  acc=0  
  
  for i in $@
  do
    if [ $# -eq 1 ]; then
      changeAddr=$1
      shift 1
    elif [ $# -ge 2 ]; then
      str_pair="$str_pair $1 $2"
      acc=$(expr $acc + $2)
      shift 2
    fi
  done
  
  setCommonVariables $(echo $obj | jq -c .) $acc
  
  echo $lovelace
  echo $newLovelace
  $qvf donate        \
       $donorsPKH    \
       $str_pair     \
       $currDatum    \
       $updatedDatum \
       $action
  update_contract $payer $txIn $changeAddr
  # scp $remoteAddr:$remoteDir/tx.signed $preDir/tx.signed
  # xxd -r -p <<< $(jq .cborHex tx.signed) > $preDir/tx.submit-api.raw
  # curl "$URL/tx/submit" -X POST -H "Content-Type: application/cbor" -H "project_id: $AUTH_ID" --data-binary @./$preDir/tx.submit-api.raw
}


# Takes 2 (or 3) arguments:
#   1. Donor's wallet number/name,
#   2. Donation amount,
#   3. (Optional) Custom change address.
contribute_from_with() {
  donorsAddr=$(cat $preDir/$1.addr)
  changeAddr=$donorsAddr
  if [ ! -z "$3" ]; then
    changeAddr=$3
  fi
  txIn=$(get_first_utxo_of $1)
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr "$policyId$tokenName" | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
  setCommonVariables $(echo $obj | jq -c .) $2
  #
  echo
  echo $currDatum
  echo $updatedDatum
  echo $action
  echo $utxo
  echo $datumHash
  echo $datumValue
  echo $lovelace
  echo $newLovelace
  echo
  #
  $qvf contribute    \
       $2            \
       $currDatum    \
       $updatedDatum \
       $action
  update_contract $1 $txIn $changeAddr
}


# Takes 4 (or 5) arguments:
#   1. Payer's wallet number/name,
#   2. Project's public key hash,
#   3. Project's label,
#   4. Requested fund,
#   5. (Optional) Custom change address.
register_project() {
  payersAddr=$(cat $preDir/$1.addr)
  changeAddr=$payersAddr
  if [ ! -z "$4" ]; then
    changeAddr=$4
  fi
  txIn=$(get_first_utxo_of $1)
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
  echo $obj
  setCommonVariables $(echo $obj | jq -c .) 2000000
  echo
  echo $2
  echo $3
  echo $4
  echo $currDatum
  echo $updatedDatum
  echo $action
  echo
  $qvf add-project   \
       $2            \
       $3            \
       $4            \
       $currDatum    \
       $updatedDatum \
       $action
  update_contract $1 $txIn $changeAddr
}


# Takes 2 arguments:
#   1. Starting wallet number of projects,
#   2. Ending wallet number of projects.
distribute() {
  txIn=$(get_first_utxo_of $keyHolder)
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
  setCommonVariables $(echo $obj | jq -c .) 0 2000000
  utxoAtScript=$(remove_quotes $utxo)
  pkhAddrs=""
  for i in $(seq $1 $2); do
    pkhAddrs="$pkhAddrs $(cat $preDir/$i.pkh) $(cat $preDir/$i.addr)"
  done
  txOuts=$($qvf distribute $keyHoldersAddress $pkhAddrs $currDatum $updatedDatum $action)
  update_contract      \
    $keyHolder         \
    $txIn              \
    $keyHoldersAddress \
    $keyHolder         \
    "$txOuts"          \
    "$invalidBefore $($cli query tip $MAGIC | jq '.slot|tonumber')"
}
