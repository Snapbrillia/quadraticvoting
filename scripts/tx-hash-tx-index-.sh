#get txHash
cardano-cli query utxo --address $1  --testnet-magic 1097911063 |grep -o '^\S*' | sed '/^$/d' | sed '/-/d'  > hash.txt 

#get txIndex
cardano-cli query utxo --address $1  --testnet-magic 1097911063 |grep -o '[[:space:]][0-9][[:space:]]' > index.txt 

# merge the two files line by line , combining the datahash with index
paste hash.txt index.txt > combined.txt 

# tries to remove white spaces between them
for i in $(cat combined.txt);do
	content=$i
	echo "$content" | sed 's/\([^.]*\.[^."[:space:]]*\)[[:space:]]\+"/\1"/'  >> filtered.txt
done

# remove white spaces between them and put them in the file tx-hash-index.txt
paste -sd '#\n' filtered.txt |  tr -s " " > tx-hash-index.txt

# remove unneeded files
rm hash.txt index.txt combined.txt filtered.txt
