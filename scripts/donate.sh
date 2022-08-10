. env.sh

# MATCH THIS WITH THE ONE FROM `env.sh`
preDir="testnet"

# DONATE TO A PROJECT
scriptAddr="addr_test1wpl9c67dav6n9gjxlyafg6dmsql8tafy3pwd3fy06tu26nqzphnsx"
authAsset="62a65c6ce2c30f7040f0bc8cc5eb5f3f07521757125a03d743124a54.517561647261546f6b656e"
MAGIC="--testnet-magic 1097911063"

# Edit these: ---------
scriptFile="qvf/qvf.plutus"            # The Plutus script file (qvf.plutus)
donorAddrFile="$1.addr"                # The file that contains donor's wallet address.
donorSKeyFile="$1.skey"                # The file that contains donor's signing key.
utxoFromDonor="$2"                     # A UTxO from donor's wallet that has enough ADA for donation and tx fee.
utxoAtScript="$3"                      # The UTxO at the script with the current datum attached.
currentDatum="qvf/$preDir/curr.datum"  # JSON file containing current state of the contract, about to be updated.
newDatum="qvf/$preDir/updated.datum"   # JSON file containing updated state of the contract.
redeemer="qvf/$preDir/action.redeemer" # JSON file containing the `Donate` redeemer.
newLovelaceCount="$4"                  # Current Lovelace count of $utxoAtScript, plus the donated amount.
# ---------------------

echo $scriptFile
echo $donorAddrFile
echo $donorSKeyFile
echo $utxoFromDonor
echo $utxoAtScript
echo $currentDatum
echo $newDatum
echo $redeemer
echo $newLovelaceCount

# Construct the transaction:
$cli transaction build --babbage-era $MAGIC                            \
    --tx-in $utxoFromDonor                                             \
    --tx-in-collateral $utxoFromDonor                                  \
    --tx-in $utxoAtScript                                              \
    --tx-in-datum-file $currentDatum                                   \
    --tx-in-script-file $scriptFile                                    \
    --tx-in-redeemer-file $redeemer                                    \
    --tx-out "$scriptAddr + $newLovelaceCount lovelace + 1 $authAsset" \
    --tx-out-datum-embed-file $newDatum                                \
    --change-address $(cat $donorAddrFile)                             \
    --protocol-params-file protocol.json                               \
    --out-file tx.unsigned

# Sign the transaction:
$cli transaction sign $MAGIC   \
    --tx-body-file tx.unsigned        \
    --signing-key-file $donorSKeyFile \
    --out-file tx.signed

# Submit the transaction:
$cli transaction submit $MAGIC --tx-file tx.signed
