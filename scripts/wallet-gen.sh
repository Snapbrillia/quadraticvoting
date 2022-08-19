. $HOME/code/snapbrillia/quadraticvoting/scripts/env.sh

MAGIC="--testnet-magic 1097911063"

cardano-cli address key-gen --verification-key-file $preDir/$1.vkey --signing-key-file $preDir/$1.skey

cardano-cli address build --payment-verification-key-file $preDir/$1.vkey --out-file $preDir/$1.addr $MAGIC

vkey_to_public_key_hash $preDir/$1.vkey $preDir/$1.pkh


cat "$preDir/$1.addr"

