# Run this script to get the hash of the initial datum. We need this for the Tx construction
# Run this in the current directory

cardano-cli transaction hash-script-data --script-data-file testnet/initial.json
