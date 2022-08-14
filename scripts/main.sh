. scripts/env.sh
. scripts/blockfrost.sh

# scriptAddr="addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx"
scriptAddr=$(cat $scriptAddressFile)
policyId=$(cat $policyIdFile)
tokenName=$(cat $tokenNameHexFile)
authAsset="$policyId.$tokenName"
remoteAddr="Keyan@172.16.42.6"
remoteDir="/e/cardanoTestnet"


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

# Takes 2 arguments:
#   1. The JSON object,
#   2. Lovelace to add to script's UTxO.
setCommonVariables() {
  utxo=$(echo $1 | jq .utxo)
  datumHash=$(echo $1 | jq .datumHash)
  datumValue=$(echo $1 | jq -c .datumValue)
  lovelace=$(echo $1 | jq .lovelace | jq tonumber)
  newLovelace=$(expr $lovelace + $2)
  echo $datumValue > $currDatum
}
# =====================================


remoteCLI() {
  scp scripts/donate.sh $remoteAddr:$remoteDir/donate.sh
  ssh $remoteAddr "cd $remoteDir && donate.sh $1 $2 $3 $4"
}


get_current_state() {
  utxosAndHashes=$(get_utxos_hashes_lovelaces $scriptAddr $policyId$tokenName)
  last=$(echo $utxosAndHashes | jq length)
  datumValues=""
  for i in $(seq 0 $(expr $last - 1)); do
    obj=$(echo $utxosAndHashes | jq .[$i])
    utxo=$(echo $obj | jq .utxo)
    datumHash=$(echo $obj | jq .datumHash)
    datumValue=$(get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
    datumValues="$datumValues $datumValue"
  done
  $qvf merge-datums $datumValues
}


# Takes 2 (up to 4) arguments:
#   1. Payer's wallet number/name.
#   2. Payer's UTxO to be spent.
#   3. (Optional) Custom change address.
#   4. (Optional) Required signer's wallet number/name.
update_contract() {
  scriptFile="$preDir/qvf.plutus"
  donorAddrFile="$preDir/$1.addr"
  donorSKeyFile="$preDir/$1.skey"
  utxoFromDonor="$2"
  utxoAtScript=$(remove_quotes $utxo)
  changeAddress=$(cat $donorAddrFile)
  if [ ! -z "$3" ]; then
    changeAddress=$3
  fi
  additionalSigner=""
  requiredSigner=""
  if [ ! -z "$4" ]; then
    additionalSigner="$preDir/$4.skey"
    requiredSigner="--required-signer $additionalSigner"
  fi

  echo $scriptFile
  echo $donorAddrFile
  echo $donorSKeyFile
  echo $utxoFromDonor
  echo $utxoAtScript

  # Generate the protocol parameters:
  generate_protocol_params

  # Construct the transaction:
  $cli transaction build --babbage-era $MAGIC                     \
    --tx-in $utxoFromDonor                                        \
    --tx-in-collateral $utxoFromDonor                             \
    --tx-in $utxoAtScript                                         \
    --tx-in-datum-file $currDatum                                 \
    --tx-in-script-file $scriptFile                               \
    --tx-in-redeemer-file $action                                 \
    --tx-out "$scriptAddr + $newLovelace lovelace + 1 $authAsset" \
    --tx-out-datum-embed-file $updatedDatum                       \
    --change-address $changeAddress                               \
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


# Takes 1 (up to 3) argument(s):
#   1. New POSIX time in milliseconds.
#   2. (Optional) Number/name of an alternate wallet for covering the fee.
#   3. (Optional) Starting index for script UTxOs.
set_earlier_deadline() {
  payer=$keyHolder
  if [ ! -z "$2" ]; then
    payer=$2
  fi
  utxosAndHashes=$(get_utxos_hashes_lovelaces $scriptAddr $policyId$tokenName)
  last=$(echo $utxosAndHashes | jq length)
  startInd=0
  if [ ! -z "$3" ] && [ $3 -lt $last ]; then
    startInd=$3
  fi
  for i in $(seq $startInd $last); do
    obj=$(echo $utxosAndHashes | jq -c .[$i])
    obj=$(add_datum_value_to_utxo $obj)
    datumValue=$(echo $obj | jq .datumValue)
    setCommonVariables $(echo $obj | jq -c .) 0
    shouldBeUpdated=$($qvf datum-has-later-deadline $datumValue $1)
    for j in $shouldBeUpdated; do
      if [ $j = "True" ] || [ $j = "False" ]; then
        shouldBeUpdated=$j
      else
        shouldBeUpdated="False"
      fi
    done
    if [ $shouldBeUpdated = "True" ]; then
      txIn=$(get_first_utxo_of $payer)
      echo "Generating datum and redeemer files..."
      $qvf set-deadline  \
	         $1            \
           $currDatum    \
	         $updatedDatum \
           $action
      echo "Datum and redeemer files generated."
      update_contract $payer $txIn $(cat $preDir/$payer.addr) $keyHolder
      break
    else
      echo "PASSED: Deadline doesn't need updating."
      echo "CURRENT DATUM:"
      echo $datumValue
      echo
      echo "NEW DEADLINE:"
      echo $1
    fi
  done
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
  if [ ! -z "$4" ]; then
    changeAddr=$4
  fi
  obj=$(find_utxo_with_project $scriptAddr "$policyId$tokenName" $2)
  len=$(echo $obj | jq length)
  if [ $len -eq 0 ]; then
    echo "FAILED to find the project."
  else
    obj=$(echo $obj | jq .[0])
    setCommonVariables $(echo $obj | jq -c .) $3
    echo $lovelace
    echo $newLovelace
    $qvf donate        \
         $donorsPKH    \
         $2            \
         $3            \
         $currDatum    \
	       $updatedDatum \
         $action
    txIn=$(get_first_utxo_of $donorsAddr)
    update_contract $1 $txIn $changeAddr
    # scp $remoteAddr:$remoteDir/tx.signed $preDir/tx.signed
    # xxd -r -p <<< $(jq .cborHex tx.signed) > $preDir/tx.submit-api.raw
    # curl "$URL/tx/submit" -X POST -H "Content-Type: application/cbor" -H "project_id: $AUTH_ID" --data-binary @./$preDir/tx.submit-api.raw
  fi
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
  txIn=$(get_first_utxo_of $donorsAddr)
  obj=$(get_random_utxo_hash_lovelaces $scriptAddr "$policyId$tokenName" 0 9 | jq -c .)
  obj=$(add_datum_value_to_utxo $obj)
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
  txIn=$(get_first_utxo_of $projectsAddr)
  obj=$(get_random_utxo_hash_lovelaces $scriptAddr "$policyId$tokenName" 0 9 | jq -c .)
  obj=$(add_datum_value_to_utxo $obj)
  setCommonVariables $(echo $obj | jq -c .) 2000000
  $qvf add-project   \
       $projectsPKH  \
       $2            \
       $3            \
       $currDatum    \
       $updatedDatum \
       $action
  update_contract $1 $txIn $changeAddr
}
