#if you have ran this script before delete the old data
rm output.txt

#curl request for all utxos under script address,filter the response to get the data_hash and store them in a file
curl -H "project_id: $1" \
"https://cardano-testnet.blockfrost.io/api/v0/addresses/$2/utxos" | grep -o '"data_hash": *"[^"]*"' | grep -o '"[^"]*"$' | sed -e 's/^"//' -e 's/"$//' > data_hashs.txt


#loop through the file and get the datum value based on the hash and store them in a file
for i in $(cat data_hashs.txt);do
	content="$(curl -H "project_id: $1" -s "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$i")"
        echo "$content"| sed "/error/d" >> output.txt
done

