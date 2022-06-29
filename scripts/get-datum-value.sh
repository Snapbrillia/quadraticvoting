## needs to pass in the file that contains utxo and api key as argument

# remove old data if script was ran already 
FILE=datum.txt
if test -f "$FILE"; then
    rm datum.txt
fi

# needs to pass in the utxos at script address as a argument , so the file where all the utxo is stored in the get-utxo.sh script
cat $1 | grep -o '"data_hash": *"[^"]*"' | grep -o '"[^"]*"$' | sed -e 's/^"//' -e 's/"$//' > data_hashs.txt


#loop through the file and get the datum value based on the hash and store them in a file
for i in $(cat data_hashs.txt);do
	content="$(curl -H "project_id: $2" -s "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$i")"
        echo "$content"| sed "/error/d" >> datum.txt
done

