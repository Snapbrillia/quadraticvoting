cardano-cli query utxo --address $1 --testnet-magic $MAGIC | awk '{ print $1"#"$2}' | sed '/-/d' | sed '1d'| sed 's/^/--tx-in /'
