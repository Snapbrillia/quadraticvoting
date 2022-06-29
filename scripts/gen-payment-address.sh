# CLI commands to generate payment key addresses
# Before you run this, make sure you have created a testnet folder in your quadratic/scripts directory (/quadratic/scripts/testnet) with `mkdir testnet`
# Before you can execute this, you need to give yourself permissions to run it. RUn this first `chmod u+x gen-payment-address.sh`
cardano-cli address key-gen \
        --verification-key-file testnet/payment.vkey \
        --signing-key-file testnet/payment.skey

cardano-cli address build \
        --payment-verification-key-file testnet/payment.vkey \
        --out-file testnet/payment.addr \
        --testnet-magic 1097911063
