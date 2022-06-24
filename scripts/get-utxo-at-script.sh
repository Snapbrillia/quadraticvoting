curl -H "project_id: $1" \
"https://cardano-testnet.blockfrost.io/api/v0/addresses/$2/utxos" | grep -o '"data_hash": *"[^"]*"' | grep -o '"[^"]*"$' | sed -e 's/^"//' -e 's/"$//' > data_hashs.txt


for i in $(cat data_hashs.txt);do
	content="$(curl -H "project_id: $1" -s "https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/$i")"
        echo "$content"| sed "/error/d"  > output.txt
done
