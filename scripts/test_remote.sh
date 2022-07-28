. scripts/env.sh
. scripts/blockfrost.sh

scriptAddr="addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx"
policyId=$(cat $preDir/qvf.symbol)
tokenName=$(cat $preDir/token.hex)
authAsset="$policyId.$tokenName"
remoteAddr="Keyan@172.16.42.6"
remoteDir="/e/cardanoTestnet"


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


donate() {
  protocols="$preDir/protocol.json"
  scriptFile="$preDir/qvf.plutus"
  donorAddrFile="$preDir/$1.addr"
  donorSKeyFile="$preDir/$1.skey"
  utxoFromDonor="$2"
  utxoAtScript="$3"
  newLovelaceCount="$4"
  currentDatum="$5"
  newDatum="$6"
  redeemer="$7"

  echo $scriptFile
  echo $donorAddrFile
  echo $donorSKeyFile
  echo $utxoFromDonor
  echo $utxoAtScript
  echo $newLovelaceCount
  echo $currentDatum
  echo $newDatum
  echo $redeemer

  # Generate the protocol parameters:
  $cli query protocol-parameters $MAGIC --out-file $protocols

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
      --protocol-params-file $protocols                                  \
      --out-file $preDir/tx.unsigned
  
  # Sign the transaction:
  $cli transaction sign $MAGIC   \
      --tx-body-file $preDir/tx.unsigned        \
      --signing-key-file $donorSKeyFile \
      --out-file $preDir/tx.signed
  
  # Submit the transaction:
  $cli transaction submit $MAGIC --tx-file $preDir/tx.signed
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
    currDatum="$preDir/curr.datum"
    updatedDatum="$preDir/updated.datum"
    action="$preDir/donate.redeemer"
    obj=$(echo $obj | jq .[0])
    utxo=$(echo $obj | jq .utxo)
    datumHash=$(echo $obj | jq .datumHash)
    datumValue=$(echo $obj | jq -c .datumValue)
    lovelace=$(echo $obj | jq .lovelace | jq tonumber)
    newLovelace=$(expr $lovelace + $3)
    echo $lovelace
    echo $newLovelace
    echo $datumValue > $currDatum
    $qvf donate        \
	       $donorsPKH    \
         $2            \
         $3            \
         $currDatum    \
	       $updatedDatum \
         $action
    txIn=$(get_first_utxo_of_wallet $donorsAddr)
    case $4 in
      remote)
        cpPath="$remoteAddr:$remoteDir/qvf"
        scp $currDatum    $cpPath/$currDatum
        scp $updatedDatum $cpPath/$updatedDatum
        scp $action       $cpPath/$action
        remoteCLI $1 $txIn $utxo $newLovelace
        ;;
      *)
        donate $1 $txIn $(remove_quotes $utxo) $newLovelace $currDatum $updatedDatum $action
        ;;
    esac
    # scp $remoteAddr:$remoteDir/tx.signed $preDir/tx.signed
    # xxd -r -p <<< $(jq .cborHex tx.signed) > $preDir/tx.submit-api.raw
    # curl "$URL/tx/submit" -X POST -H "Content-Type: application/cbor" -H "project_id: $AUTH_ID" --data-binary @./$preDir/tx.submit-api.raw
    $qvf pretty-datum $(cat $updatedDatum)
    echo "DONE."
  fi
}

contribute_from_with() {
  echo
}
