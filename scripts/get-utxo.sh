#curl request for all utxos under script address,filter the response to get the data_hash and store them in a file
#needs to pass in api key and script address as argument 
curl -H "project_id: $1" \
"https://cardano-testnet.blockfrost.io/api/v0/addresses/$2/utxos" > utxos.txt