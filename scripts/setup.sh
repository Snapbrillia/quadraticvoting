#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

# Creating the $fileNamesJSONFile:
# {{{
touch $fileNamesJSONFile
if [ ! "$(cat $fileNamesJSONFile)" ]; then
  echo "{ \"ocfnPreDir\"              : \"$preDir\""                      > $fileNamesJSONFile
  echo ", \"ocfnProjectsPreDir\"      : \"projects\""                    >> $fileNamesJSONFile
  echo ", \"ocfnQueryJSON\"           : \"query.json\""                  >> $fileNamesJSONFile
  echo ", \"ocfnScriptQueryJSON\"     : \"script-query.json\""           >> $fileNamesJSONFile
  echo ", \"ocfnGovernanceMinter\"    : \"governance-policy.plutus\""    >> $fileNamesJSONFile
  echo ", \"ocfnGovernanceSymbol\"    : \"governance-policy.symbol\""    >> $fileNamesJSONFile
  echo ", \"ocfnQVFGovernanceUTxO\"   : \"gov.utxo\""                    >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationMinter\"  : \"registration-policy.plutus\""  >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationSymbol\"  : \"registration-policy.symbol\""  >> $fileNamesJSONFile
  echo ", \"ocfnRegistrationRefUTxO\" : \"registration-policy.refUTxO\"" >> $fileNamesJSONFile
  echo ", \"ocfnDonationMinter\"      : \"donation-policy.plutus\""      >> $fileNamesJSONFile
  echo ", \"ocfnDonationSymbol\"      : \"donation-policy.symbol\""      >> $fileNamesJSONFile
  echo ", \"ocfnDonationRefUTxO\"     : \"donation-policy.refUTxO\""     >> $fileNamesJSONFile
  echo ", \"ocfnQVFMainValidator\"    : \"$scriptLabel.plutus\""         >> $fileNamesJSONFile
  echo ", \"ocfnQVFRefUTxO\"          : \"$scriptLabel.refUTxO\""        >> $fileNamesJSONFile
  echo ", \"ocfnContractAddress\"     : \"$scriptLabel.addr\""           >> $fileNamesJSONFile
  echo ", \"ocfnDeadlineSlot\"        : \"deadline.slot\""               >> $fileNamesJSONFile
  echo ", \"ocfnDeadlineDatum\"       : \"deadline.govDatum\""           >> $fileNamesJSONFile
  echo ", \"ocfnInitialGovDatum\"     : \"initial.govDatum\""            >> $fileNamesJSONFile
  echo ", \"ocfnCurrentDatum\"        : \"current.datum\""               >> $fileNamesJSONFile
  echo ", \"ocfnUpdatedDatum\"        : \"updated.datum\""               >> $fileNamesJSONFile
  echo ", \"ocfnNewDatum\"            : \"new.datum\""                   >> $fileNamesJSONFile
  echo ", \"ocfnQVFRedeemer\"         : \"qvf.redeemer\""                >> $fileNamesJSONFile
  echo ", \"ocfnMinterRedeemer\"      : \"minter.redeemer\""             >> $fileNamesJSONFile
  echo ", \"ocfnProjectTokenName\"    : \"project-token-name.hex\""      >> $fileNamesJSONFile
  echo ", \"ocfnScriptUTxOs\"         : \"script.utxos\""                >> $fileNamesJSONFile
  echo ", \"ocfnRegisteredProjects\"  : \"registered-projects.json\""    >> $fileNamesJSONFile
  echo "}" >> $fileNamesJSONFile
else
  newContent=$(cat $fileNamesJSONFile | jq -c --arg preDir "$preDir" '.ocfnPreDir = $preDir')
  echo "$newContent" > $fileNamesJSONFile
fi
# }}}
touch $tempBashFile
touch $tidyUpLogFile
touch $refDepletionLogFile
touch $projectTokenNameFile
touch $registeredProjectsFile
if [ ! "$(cat $registeredProjectsFile)" ]; then
  echo "[]" > $registeredProjectsFile
fi
touch $latestInteractionSlotFile
if [ ! "$(cat $latestInteractionSlotFile)" ]; then
  echo "{\"$keyHolder\":0"        > $latestInteractionSlotFile
  echo ",\"$referenceWallet\":0" >> $latestInteractionSlotFile
  echo ",\"$scriptLabel\":0"     >> $latestInteractionSlotFile
  echo ",\"deadline\":0"         >> $latestInteractionSlotFile
  echo "}"                       >> $latestInteractionSlotFile
fi
touch $scriptUTxOsFile
generate_protocol_params

# REQUIRED FOR DEVELOPMENT:
touch $devRedeemer
echo "{\"constructor\":20,\"fields\":[]}" > $devRedeemer
# =========================

# Checks if the $keyHolder wallet exists (properly), and that it has a single
# UTxO with enough Ada inside.
#
# If the wallet files exist partially, this function terminates the script
# without any changes. If there are no wallet files, the $keyHolder wallet is
# generated, but the script is terminated, prompting the user to send some Ada
# to the wallet.
#
# If the wallet is present, it's made sure the total Lovelace count is more
# than the minimum, and if they are spread out between multiple UTxOs, it'll
# invoke the `tidy_up_wallet` function so that all the money is collected
# inside a single UTxO.
# {{{
if [ -f $preDir/$keyHolder.vkey ] && [ -f $preDir/$keyHolder.skey ] && [ -f $preDir/$keyHolder.addr ] && [ -f $preDir/$keyHolder.pkh ]; then
  keyHolder_utxos=$(get_wallet_lovelace_utxos $keyHolder)
  keyHolder_utxoCount=$(echo "$keyHolder_utxos" | jq length)
  keyHolder_totalLovelace=$(get_total_lovelaces_from_json "$keyHolder_utxos")
  if [ $keyHolder_totalLovelace -ge $minStartingLovelaces ] || [ -f $scriptAddressFile ]; then
    if [ $keyHolder_utxoCount -gt 1 ]; then
      tidy_up_wallet $keyHolder "Multiple UTxOs found in the key holder's wallet. Tidying up...\n$keyHolder_utxos"
      echo "Done. The key holder wallet is ready."
    fi
    export keyHoldersAddress=$(cat "$preDir/$keyHolder.addr")
    export keyHoldersPubKeyHash=$(cat "$preDir/$keyHolder.pkh")
    export keyHoldersSigningKeyFile="$preDir/$keyHolder.skey"
  else
    echo "The key holder wallet doesn't have enough Ada. Please make sure a"
    echo -e "minimum of $WHITE$minStartingAda Ada$NO_COLOR is available:"
    echo ""
    echo -e "$WHITE$(cat $preDir/$keyHolder.addr)$NO_COLOR"
    return 1
  fi
elif [ -f $preDir/$keyHolder.vkey ] || [ -f $preDir/$keyHolder.skey ] || [ -f $preDir/$keyHolder.addr ] || [ -f $preDir/$keyHolder.pkh ]; then
  echo "Some key holder wallet files are missing."
  return 1
else
  generate_wallet $keyHolder
  echo "No key holder wallet was found. The wallet is generated for you."
  echo -e "Please deposit a minimum of $WHITE$minStartingAda Ada$NO_COLOR before proceeding:"
  echo ""
  echo -e "$WHITE$(cat $preDir/$keyHolder.addr)$NO_COLOR"
  return 1
fi
# }}}


# A similar check for $collateralKeyHolder. This wallet is meant for providing
# the collateral for user-facing endpoints to provide a more "approachable"
# experience for non-technical users.
# {{{
if [ -f $preDir/$collateralKeyHolder.vkey ] && [ -f $preDir/$collateralKeyHolder.skey ] && [ -f $preDir/$collateralKeyHolder.addr ] && [ -f $preDir/$collateralKeyHolder.pkh ]; then
  collateral_utxos=$(get_wallet_lovelace_utxos $collateralKeyHolder)
  collateral_utxoCount=$(echo "$collateral_utxos" | jq length)
  collateral_totalLovelace=$(get_total_lovelaces_from_json "$collateral_utxos")
  if [ $collateral_totalLovelace -ge $minCollateralLovelaces ]; then
    if [ $collateral_utxoCount -gt 1 ]; then
      tidy_up_wallet $collateralKeyHolder "Multiple UTxOs found in the collateral key holder's wallet. Tidying up...\n$collateral_utxos"
      echo "Done. The key holder wallet is ready."
    fi
    export collateralKeyHoldersAddress=$(cat "$preDir/$collateralKeyHolder.addr")
    export collateralKeyHoldersPubKeyHash=$(cat "$preDir/$collateralKeyHolder.pkh")
    export collateralKeyHoldersSigningKeyFile="$preDir/$collateralKeyHolder.skey"
  else
    echo "The collateral key holder wallet doesn't have enough Ada. Please make sure a"
    echo -e "minimum of $WHITE$minCollateralAda Ada$NO_COLOR is available:"
    echo ""
    echo -e "$WHITE$(cat $preDir/$collateralKeyHolder.addr)$NO_COLOR"
    return 1
  fi
elif [ -f $preDir/$collateralKeyHolder.vkey ] || [ -f $preDir/$collateralKeyHolder.skey ] || [ -f $preDir/$collateralKeyHolder.addr ] || [ -f $preDir/$collateralKeyHolder.pkh ]; then
  echo "Some key holder wallet files are missing."
  return 1
else
  generate_wallet $collateralKeyHolder
  echo "No collateral key holder wallet was found. The wallet is generated for you."
  echo -e "Please deposit a minimum of $WHITE$minCollateralAda Ada$NO_COLOR before proceeding:"
  echo ""
  echo -e "$WHITE$(cat $preDir/$collateralKeyHolder.addr)$NO_COLOR"
  return 1
fi
# }}}


# A similar check for the $referencWallet. There are 3 circumstances that lead
# to an automated depletion of the $referenceWallet into $keyHolder:
#   - There are some Lovelaces in the wallet, but the UTxO count is not exactly
#     the same as the $scriptCount,
#   - There are exactly $scriptCount UTxOs in the wallet, but there is no
#     $scriptAddressFile,
#   - There are exactly $scriptCount UTxOs in the wallet, but there are no
#     Lovelaces stored at the found script address.
# {{{
if [ -f $preDir/$referenceWallet.vkey ] && [ -f $preDir/$referenceWallet.skey ] && [ -f $preDir/$referenceWallet.addr ] && [ -f $preDir/$referenceWallet.pkh ]; then
  ref_wallet_utxos=$(get_wallet_lovelace_utxos $referenceWallet)
  ref_wallet_utxoCount=$(echo "$ref_wallet_utxos" | jq length)
  ref_wallet_totalLovelace=$(get_total_lovelaces_from_json "$ref_wallet_utxos")
  if [ $ref_wallet_utxoCount -eq 3 ]; then
    if [ -f $scriptAddressFile ]; then
      qvf_utxos=$(get_all_script_utxos_datums_values $(cat $scriptAddressFile))
      qvf_totalLovelace=$(get_total_lovelaces_from_json "$qvf_utxos")
      if [ $qvf_totalLovelace -eq 0 ]; then
        deplete_reference_wallet "The reference wallet is not empty, while the contract is. Depleting the reference wallet into key holder's...\n$ref_wallet_utxos"
        tidy_up_wallet $keyHolder "Tidying up the key holder's wallet after depletion of the reference wallet due to an un-initiated script address..."
      fi
    else
      deplete_reference_wallet "The reference wallet is not empty, while there is no contract address file stored. Depleting the reference wallet...\n$ref_wallet_utxos"
      tidy_up_wallet $keyHolder "Tidying up the key holder's wallet after depletion of the reference wallet due to missing script address file..."
    fi
  elif [ $ref_wallet_totalLovelace -gt 0 ]; then
    deplete_reference_wallet "The reference wallet has some Lovelaces, but the number of its UTxOs are not exactly 3 (the number of scripts). Depleting the reference wallet...\n$ref_wallet_utxos"
    tidy_up_wallet $keyHolder "Tidying up the key holder's wallet after depletion of the reference wallet due to not having exactly 3 UTxOs, but having some Lovelaces..."
  fi
elif [ -f $preDir/$referenceWallet.vkey ] || [ -f $preDir/$referenceWallet.skey ] || [ -f $preDir/$referenceWallet.addr ] || [ -f $preDir/$referenceWallet.pkh ]; then
  echo "Some reference wallet files are missing."
  return 1
else
  generate_wallet $referenceWallet
fi
export referenceWalletAddress=$(cat "$preDir/$referenceWallet.addr")
# }}}

. $REPO/scripts/env.sh
