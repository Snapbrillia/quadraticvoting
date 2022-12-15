. $HOME/quadraticvoting/scripts/initiation.sh

transactionCBOR=$1

newJson=$(cat $txBody | jq -c --arg cbor "$transactionCBOR" '.cborHex = $cbor')
echo "$newJson" > $txBody
sign_tx_by $preDir/$collateralKeyHolder.skey

JSON_STRING=$( jq -n                                 \
    --arg tu "$(cat $txSigned | jq -r .cborHex)"     \
    '{signedTx: $tu}' )
echo "---$JSON_STRING"

store_current_slot

