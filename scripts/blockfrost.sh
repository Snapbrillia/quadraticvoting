AUTH_ID=$(cat ../blockfrost.id)
URL="https://cardano-testnet.blockfrost.io/api/v0"


remove_quotes() {
  echo $1           \
  | sed 's|[",]||g'
}

query_wallet() {
  curl -H                             \
    "project_id: $AUTH_ID"            \
    "$URL/addresses/$1/utxos"
}

get_utxos_hashes_lovelaces() {
  curl -H                             \
    "project_id: $AUTH_ID"            \
    "$URL/addresses/$1/utxos"         \
    | jq -c 'map(
      { utxo: (.tx_hash + "#" + (.tx_index|tostring)),
        datumHash: .data_hash,
        lovelace: (.amount | .[0] | .quantity)
      })'
    # | jq -c 'map({(.tx_hash + "#" + (.tx_index|tostring)): .data_hash}) | .[]'
    # | sed 's|[",]||g'
}

get_datum_value_from_hash() {
  curl -H                     \
    "project_id: $AUTH_ID"    \
    -s "$URL/scripts/datum/$1"
}

# Takes 2 arguements:
#   1. Script address,
#   2. Publich key hash of the target project.
find_utxo_with_project() {
  utxosAndHashes=$(get_utxos_hashes_lovelaces $1)
  last=$(echo $utxosAndHashes | jq length)
  obj=""
  for i in $(seq 0 $last); do
    obj=$(echo $utxosAndHashes | jq .[$i])
    utxo=$(echo $obj | jq .utxo)
    datumHash=$(echo $obj | jq .datumHash)
    datumValue=$(get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
    isPresent=$(cabal run qvf-cli -- datum-has-project $datumValue $2)
    for j in $isPresent; do
      if [ $j = "True" ] || [ $j = "False" ]; then
        isPresent=$j
      else
        isPresent="False"
      fi
    done
    if [ $isPresent = "True" ]; then
      obj=$(echo $obj | jq --arg datumValue "$datumValue" '[. += {datumValue: ($datumValue | fromjson)}]')
      break
    else
      obj=$(echo "[]" | jq .)
    fi
  done
  echo $obj
}
