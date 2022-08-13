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


update_contract() {
  protocols="$preDir/protocol.json"
  scriptFile="$preDir/qvf.plutus"
  donorAddrFile="$preDir/$1.addr"
  donorSKeyFile="$preDir/$1.skey"
  utxoFromDonor="$2"
  utxoAtScript=$(remove_quotes $utxo)

  echo $scriptFile
  echo $donorAddrFile
  echo $donorSKeyFile
  echo $utxoFromDonor
  echo $utxoAtScript

  # Generate the protocol parameters:
  $cli query protocol-parameters $MAGIC --out-file $protocols

  # Construct the transaction:
  $cli transaction build --babbage-era $MAGIC                       \
      --tx-in $utxoFromDonor                                        \
      --tx-in-collateral $utxoFromDonor                             \
      --tx-in $utxoAtScript                                         \
      --tx-in-datum-file $currDatum                                 \
      --tx-in-script-file $scriptFile                               \
      --tx-in-redeemer-file $action                                 \
      --tx-out "$scriptAddr + $newLovelace lovelace + 1 $authAsset" \
      --tx-out-datum-embed-file $updatedDatum                       \
      --change-address $(cat $donorAddrFile)                        \
      --protocol-params-file $protocols                             \
      --cddl-format                                                 \
      --out-file $preDir/tx.unsigned
  
  # Sign the transaction:
  $cli transaction sign $MAGIC           \
      --tx-body-file $preDir/tx.unsigned \
      --signing-key-file $donorSKeyFile  \
      --out-file $preDir/tx.signed
  
  # Submit the transaction:
  $cli transaction submit $MAGIC --tx-file $preDir/tx.signed
  echo
  $qvf pretty-datum $(cat $updatedDatum)
  echo "DONE."
}



# Takes 4 arguments:
#   1. Donor's number,
#   2. Target project's public key hash,
#   3. Donation amount.
#   4. Flag to indicate whether to use a remote node or not.
donate_from_to_with() {
  donorsPKH=$(cat $preDir/$1.pkh)
  donorsAddr=$(cat $preDir/$1.addr)
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
    txIn=$(get_first_utxo_of_wallet $donorsAddr)
    update_contract $1 $txIn
    # scp $remoteAddr:$remoteDir/tx.signed $preDir/tx.signed
    # xxd -r -p <<< $(jq .cborHex tx.signed) > $preDir/tx.submit-api.raw
    # curl "$URL/tx/submit" -X POST -H "Content-Type: application/cbor" -H "project_id: $AUTH_ID" --data-binary @./$preDir/tx.submit-api.raw
  fi
}


# Takes 2 arguments:
#   1. Donor's number,
#   2. Donation amount.
contribute_from_with() {
  donorsAddr=$(cat $preDir/$1.addr)
  txIn=$(get_first_utxo_of_wallet $donorsAddr)
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
  update_contract $1 $txIn
}


# Takes 3 arguments:
#   1. Project's wallet number/name,
#   2. Project's label,
#   3. Requested fund.
register_project() {
  projectsPKH=$(cat $preDir/$1.pkh)
  projectsAddr=$(cat $preDir/$1.addr)
  txIn=$(get_first_utxo_of_wallet $projectsAddr)
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
  update_contract $1 $txIn
}
