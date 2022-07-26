scriptAddr="addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx"
policyId=$(cat $preDir/qvf.symbol)
tokenName=$(cat $preDir/token.hex)
authAsset="$policyId.$tokenName"
remoteAddr="Keyan@172.16.42.6"
remoteDir="/e/cardanoTestnet"
cli="cardano-cli"
qvf="cabal run qvf-cli --"

. scripts/env.sh
. scripts/blockfrost.sh


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


# Takes 3 arguments:
#   1. Donor's number,
#   2. Target project's public key hash,
#   3. Donation amount.
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
    datumValue=$(echo $obj | jq .datumValue)
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
    cpPath="$remoteAddr:$remoteDir/qvf"
    scp $currDatum    $cpPath/$currDatum
    scp $updatedDatum $cpPath/$updatedDatum
    scp $action       $cpPath/$action
    txIn=$(get_first_utxo_of_wallet $donorsAddr)
    remoteCLI $1 $txIn $utxo $newLovelace
    # scp $remoteAddr:$remoteDir/tx.signed $preDir/tx.signed
    # xxd -r -p <<< $(jq .cborHex tx.signed) > $preDir/tx.submit-api.raw
    # curl "$URL/tx/submit" -X POST -H "Content-Type: application/cbor" -H "project_id: $AUTH_ID" --data-binary @./$preDir/tx.submit-api.raw
    $qvf pretty-datum $(cat $updatedDatum)
    echo "DONE."
  fi
}
