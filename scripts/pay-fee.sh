. env.sh

fee=$(qvf-cli collect-key-holder-fee)
give_lovelace $donation_recip $keyholder $fee
