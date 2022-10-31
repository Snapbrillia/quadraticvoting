. scripts/env.sh

scriptAddress=$(cat $scriptAddressFile)
fee=$(qvf-cli collect-key-holder-fee)

# TODO more string processing of fee is likely
# Should be easy and straightforward to implement, below
fee=$fee

give_lovelace $scriptAddress $keyHolder $fee
