# Expects a `.plutus` script file as its first argument, and the output file to
# store the corresponding script address as its second arguement. Prints the
# result.

cardano-cli address build-script /
  --script-file $1               /
  --testnet-magic 1097911063     /
  --out-file $2
