#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

. $REPO/scripts/env.sh

transactionCBOR=$1
projectTokenName=$2

if [ "$3" = '--sign-registration-tx' ] || [ "$3" = '--sign-contribution-tx' ]; then
  differenceBetweenSlots=$(get_slot_difference $scriptLabel)
else 
  differenceBetweenSlots=$(get_slot_difference $projectTokenName)
fi

if [ $differenceBetweenSlots -lt 100 ]; then
  echo "NetworkBusy"
else 
  newJson=$(cat $txBody | jq -c --arg cbor "$transactionCBOR" '.cborHex = $cbor')
  echo "$newJson" > $txBody
  sign_tx_by $preDir/$collateralKeyHolder.skey
  JSON_STRING=$( jq -n                                 \
      --arg tu "$(cat $txSigned | jq -r .cborHex)"     \
      '{signedTx: $tu}' )
  echo "$JSON_STRING"
  if [ "$3" = '--sign-registration-tx' ]; then 
    store_current_slot_2 $projectTokenName $scriptLabel
  elif [ "$3" = '--sign-contribution-tx' ]; then 
    store_current_slot $scriptLabel
  else
    store_current_slot $projectTokenName 
  fi
fi






