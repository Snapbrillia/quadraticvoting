. scripts/env.sh
. scripts/blockfrost.sh

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

# Takes 1 argument:
#   1. Datum file.
get_deadline_slot() {
  $qvf get-deadline-slot $(get_newest_slot) $1
}


# Takes 2 (up to 4) arguments:
#   1. Payer's wallet number/name.
#   2. Payer's UTxO to be spent.
#   3. (Optional) Argument for deadline (--invalid-before|--invalid-hereafter)
#   4. (Optional) Custom change address.
#   5. (Optional) Required signer's wallet number/name.
#   6. (Optional) Extra outputs.
update_contract() {
  donorAddrFile="$preDir/$1.addr"
  donorSKeyFile="$preDir/$1.skey"
  utxoFromDonor="$2"
  utxoAtScript=$(remove_quotes $utxo)
  changeAddress=$(cat $donorAddrFile)
  deadlineArg=""
  if [ ! -z "$3" ]; then
    deadlineArg="$3 $(get_deadline_slot $updatedDatum)"
  fi
  if [ ! -z "$4" ]; then
    changeAddress=$4
  fi
  additionalSigner=""
  requiredSigner=""
  if [ ! -z "$5" ]; then
    additionalSigner="$preDir/$5.skey"
    requiredSigner="--required-signer $additionalSigner"
  fi

  echo $scriptPlutusFile
  echo $donorAddrFile
  echo $donorSKeyFile
  echo $utxoFromDonor
  echo $utxoAtScript
  echo $requiredSigner
  echo $6

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
    --tx-out-datum-embed-file $updatedDatum                    $6 \
    --change-address $changeAddress                  $deadlineArg \
    --protocol-params-file $protocolsFile                         \
    --cddl-format   $requiredSigner                               \
    --out-file $txBody
  
  # Sign the transaction:
  sign_tx_by $donorSKeyFile $additionalSigner
  
  # Submit the transaction:
  submit_tx

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
  update_contract              \
    $payer                     \
    $txIn                      \
    $invalidAfter              \
    $(cat $preDir/$payer.addr) \
    $keyHolder
}



# Takes 3 (or 4) arguments:
#   1. Donor's wallet number/name,
#   2. Target project's public key hash,
#   3. Donation amount,
#   4. (Optional) Custom change address.
donate_from_to_with() {
  donorsPKH=$(cat $preDir/$1.pkh)
  donorsAddr=$(cat $preDir/$1.addr)
  changeAddr=$donorsAddr
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
 
  first_arg=$1
  shift 1
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
  txIn=$(get_first_utxo_of $first_arg)
  update_contract $first_arg $txIn $invalidAfter $changeAddr
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
  update_contract $1 $txIn $invalidAfter $changeAddr
}


# Takes 3 (or 4) arguments:
#   1. Project's wallet number/name,
#   2. Project's label,
#   3. Requested fund,
#   4. (Optional) Custom change address.
register_project() {
  projectsPKH=$(cat $preDir/$1.pkh)
  projectsAddr=$(cat $preDir/$1.addr)
  changeAddr=$projectsAddr
  if [ ! -z "$4" ]; then
    changeAddr=$4
  fi
  txIn=$(get_first_utxo_of $1)
  obj=$(bf_get_first_utxo_hash_lovelaces $scriptAddr $policyId$tokenName | jq -c .)
  obj=$(bf_add_datum_value_to_utxo $obj)
  echo $obj
  setCommonVariables $(echo $obj | jq -c .) 2000000
  echo
  echo $projectsPKH
  echo $2
  echo $3
  echo $currDatum
  echo $updatedDatum
  echo $action
  echo
  $qvf add-project   \
       $projectsPKH  \
       $2            \
       $3            \
       $currDatum    \
       $updatedDatum \
       $action
  update_contract $1 $txIn $invalidAfter $changeAddr
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
    $invalidBefore     \
    $keyHoldersAddress \
    $keyHolder         \
    "$txOuts"
}
