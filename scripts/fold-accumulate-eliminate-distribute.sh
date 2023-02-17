#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
fi

if [ "$ENV" == "dev" ]; then
  echo "This script is only meant for production."
  return 1
fi

. $REPO/scripts/initiation.sh

# Takes no arguments.
get_all_token_names() {
  # {{{
  get_all_projects_state_utxos_datums_values | jq -c 'map(.asset | split(".") | .[1])'
  # }}}
}

# === FOLDING STAGE ===========================================================
# Getting all authentic projects token names to fold:
for tn in $(get_all_token_names | jq -r '.[]'); do
  . $REPO/scripts/fold-donations.sh $tn 1
done
# =============================================================================

# === PRIZE WEIGHT ACCUMULATION STAGE =========================================
# Accumulating the prize weights into the governance UTxO's datum:
. $REPO/scripts/accumulate-prize-weights.sh
# =============================================================================

# === PROJECT ELIMINATION STAGE ===============================================
# Since donation-less projects have been burnt during the folding stage, we
# re-fetch the list of available token names to eliminate non-eligible
# projects.

# As the elimination script terminates upon completion, we can traverse the
# whole token name list to cover the edge case in which all the projects are
# eliminated:
for tn in $(get_all_token_names | jq -r '.[]'); do
  . $REPO/scripts/eliminate-one-project.sh
done
# =============================================================================

# === PRIZE DISTRIBUTION STAGE ================================================
escrowConstr=$($qvf get-constr-index Escrow)
winnerTNs=$(get_all_projects_state_utxos_datums_values | jq -c --argjson constr "$escrowConstr" 'map(select(.datum.constructor != $constr)) | map(.asset | split(".") | .[1])')
for tn in $(echo "$winnerTNs" | jq -r '.[]'); do
  . $REPO/scripts/distribute-prize.sh $tn
done
# =============================================================================
