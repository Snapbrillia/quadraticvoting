. scripts/env.sh


AUTH_ID=$(cat ../blockfrost.id)
URL="https://cardano-testnet.blockfrost.io/api/v0"


bf_query_address() {
  curl -H                     \
    "project_id: $AUTH_ID"    \
    "$URL/addresses/$1/utxos"
}

# get_first_utxo_of_wallet() {
bf_get_first_utxo_of_address() {
  curl -H                             \
    "project_id: $AUTH_ID"            \
    "$URL/addresses/$1/utxos"         \
    | jq .[0]                         \
    | jq -c '(.tx_hash + "#" + (.tx_index|tostring))' \
    | sed 's|[",]||g'
}


# Takes 2 arguments:
#   1. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
#   2. The JSON value containing list of UTxO's returned
#      from Blockfrost.
keep_utxos_with_asset() {
  echo $2 \
    | jq -c --arg authAsset "$1" 'map(
        select(
          .amount | .[] | contains (
            { "unit": $authAsset
            }
          )
        )
      )'
}


# Takes 1 argument:
#   1. The JSON value containing list of UTxO's returned
#      from Blockfrost.
reconstruct_utxos() {
  echo $1 \
    | jq -c 'map(
      { utxo: (.tx_hash + "#" + (.tx_index|tostring))
      , datumHash: .data_hash
      , lovelace: (.amount | .[0] | .quantity)
      })'
}


# Returns a JSON array of objects, each having 3 fields:
# `utxo`, `datumHash`, and `lovelace`. All elements of this
# array posses an authentication token.
#
# Takes 2 arguments:
#   1. Script address,
#   2. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
bf_get_utxos_hashes_lovelaces() {
  zero=$(bf_query_address $1)
  one=$(keep_utxos_with_asset $2 $zero)
  echo $(reconstruct_utxos $one)
}


# Takes 4 arguments:
#   1. Script address,
#   2. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
#   3. Start of random range.
#   4. End of random range.
bf_get_random_utxo_hash_lovelaces() {
  rand=$(shuf -i $3-$4 -n 1)
  echo $(bf_get_utxos_hashes_lovelaces $1 $2) \
    | jq .[$rand]
}


# Takes 1 argument:
#   1. Datum hash.
bf_get_datum_value_from_hash() {
  curl -H                      \
    "project_id: $AUTH_ID"     \
    -s "$URL/scripts/datum/$1"
}


# Assumes the object format is the one returned by
# `bf_get_utxos_hashes_lovelaces`.
#
# Takes 1 argument:
#   1. JSON value with the format described above.
bf_add_datum_value_to_utxo() {
  datumHash=$(echo $1 | jq -c .datumHash)
  datumValue=$(bf_get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
  echo $1 | jq -c --arg datumValue "$datumValue" '. += {datumValue: ($datumValue | fromjson)}'
}


