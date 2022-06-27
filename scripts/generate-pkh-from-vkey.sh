# Expects the verification key file as its first argument, and the output file
# to store the public key hash as its second argument. Prints the result.

cardano-cli address key-hash         /
  --payment-verification-key-file $1 /
  --out-file $2

cat $2
