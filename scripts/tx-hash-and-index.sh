cardano-cli query utxo --address $1 --testnet-magic 1097911063 | awk '{ print $1"#"$2}' | sed '/-/d' | sed '1d'
