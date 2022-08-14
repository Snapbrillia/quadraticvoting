. scripts/env.sh

AUTH_ID=$(cat ../blockfrost.id)
URL="https://cardano-testnet.blockfrost.io/api/v0"

remove_quotes() {
  echo $1           \
  | sed 's|[",]||g'
}

query_wallet() {
  curl -H                     \
    "project_id: $AUTH_ID"    \
    "$URL/addresses/$1/utxos"
}

get_first_utxo_of_wallet() {
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


# Takes 2 arguments:
#   1. Script address,
#   2. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
get_first_utxo_hash_lovelaces() {
  echo $(get_utxos_hashes_lovelaces $1 $2) \
    | jq .[0]
}


# Takes 4 arguments:
#   1. Script address,
#   2. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
#   3. Start of random range.
#   4. End of random range.
get_random_utxo_hash_lovelaces() {
  rand=$(shuf -i $3-$4 -n 1)
  echo $(get_utxos_hashes_lovelaces $1 $2) \
    | jq .[$rand]
}


# Takes 2 arguments:
#   1. Script address,
#   2. Authentication asset (policy ID plus the token name,
#      without a `.` in between).
get_utxos_hashes_lovelaces() {
  zero=$(query_wallet $1)
  one=$(keep_utxos_with_asset $2 $zero)
  echo $(reconstruct_utxos $one)
}


# Takes 1 argument:
#   1. Datum hash.
get_datum_value_from_hash() {
  curl -H                     \
    "project_id: $AUTH_ID"    \
    -s "$URL/scripts/datum/$1"
}


# Assumes the object format is the one returned by
# `get_utxos_hashes_lovelaces`.
#
# Takes 1 argument:
#   1. JSON value with the format described above.
add_datum_value_to_utxo() {
  datumHash=$(echo $1 | jq -c .datumHash)
  datumValue=$(get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
  echo $1 | jq -c --arg datumValue "$datumValue" '. += {datumValue: ($datumValue | fromjson)}'
}


# Takes 3 arguements:
#   1. Script address,
#   2. Authentication asset,
#   3. Publich key hash of the target project.
find_utxo_with_project() {
  utxosAndHashes=$(get_utxos_hashes_lovelaces $1 $2)
  last=$(echo $utxosAndHashes | jq length)
  obj=""
  for i in $(seq 0 $last); do
    obj=$(echo $utxosAndHashes | jq .[$i])
    utxo=$(echo $obj | jq .utxo)
    datumHash=$(echo $obj | jq .datumHash)
    datumValue=$(get_datum_value_from_hash $(remove_quotes $datumHash) | jq -c .json_value)
    isPresent=$($qvf datum-has-project $datumValue $3)
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
