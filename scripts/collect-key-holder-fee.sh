#!/bin/bash

. scripts/env.sh

qvfAddress=$(cat $scriptAddressFile)
govAsset=$(cat $govSymFile)

fee=$(qvf-cli collect-key-holder-fee "$(cat $fileNamesJSONFile)")

# TODO more string processing of fee is likely
# Should be easy and straightforward to implement, below
fee=$fee

give_lovelace $scriptAddress $keyHolder $fee
