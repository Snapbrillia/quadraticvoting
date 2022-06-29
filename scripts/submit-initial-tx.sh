# TIPS:
# Make sure you fund your wallet first 
# Run this file inside your /scripts directory
# Make sure you have a running node in the background
# I am using 01.addr here, but maybe u need to replace it with payment.addr (as per the first step earlier of generating key pairs)
# THe datum hash is the hash of the initial datum so u can use it.
# Make sure you have built the SCRIPT PAYMENT address 

# BUILD
# Initial datum hash: 38b0bf40b1ea283842e7b726be87386a1428f476ee9ae7245b1da75946b28060

cardano-cli transaction build \
--alonzo-era \
--tx-in 984c5037f0a08afa9cc15e1dce4a242d12d8e5ac703352215e97d2218f7fa5f4#0 \
--tx-out $(cat testnet/script.addr)+85000000 \
--tx-out-datum-hash 38b0bf40b1ea283842e7b726be87386a1428f476ee9ae7245b1da75946b28060 \
--change-address $(cat testnet/01.addr) \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

# SIGN

cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file testnet/01.skey \
--testnet-magic 1097911063 \
--out-file tx.sign

# SUBMIT
cardano-cli transaction submit \
--testnet-magic 1097911063 \
--tx-file tx.sign
