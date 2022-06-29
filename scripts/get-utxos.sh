# QUery utxos of the previously generated address
# RUn the script to get permissions for executing file `chmod u+x get-utxos.sh`

cardano-cli query utxo --address $(cat testnet/payment.addr) --testnet-magic 1097911063
