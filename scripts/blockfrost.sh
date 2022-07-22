AUTH_ID=$(cat ../../blockfrost.id)
URL="https://cardano-testnet.blockfrost.io/api/v0"

get_utxos() {
  curl -H                             \
    "project_id: $AUTH_ID"            \
    "$URL/addresses/$1/utxos"         \
    | jq -c 'map({(.tx_hash + "#" + (.tx_index|tostring)): .data_hash}) | .[]'
    # | sed 's|[",]||g'
}

get_datum_hashes() {
  get_utxos $1 | awk '{print $2}'
}

get_datum_value_from_hash() {
  curl -H                     \
    "project_id: $AUTH_ID"    \
    -s "$URL/scripts/datum/$1"
}

get_datum_values_at() {
  for i in $(get_datum_hashes $1);do
    tempVal=$(get_datum_value_from_hash $i | jq -c .json_value)
    cabal run qvf-cli -- pretty-datum $tempVal
  done
}
