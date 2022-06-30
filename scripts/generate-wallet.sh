sh env.sh

cardano-cli address key-gen               \
  --verification-key-file $1.vkey         \
  --signing-key-file $1.skey

cardano-cli address build                 \
  --payment-verification-key-file $1.vkey \
  $MAGIC                                  \
  --out-file $1.addr

cardano-cli address key-hash
  --payment-verification-key-file $1.vkey \
  --out-file $1.pkh
