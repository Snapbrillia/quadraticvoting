#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
fi

. $REPO/scripts/local-env.sh
. $REPO/scripts/env.sh

numberOfProjects=$(get_all_projects_info_utxos_datums_values | jq 'length')

for i in $(seq 0 $(($numberOfProjects - 1))); do
 . $REPO/scripts/fold-donations.sh $(project_index_to_token_name "$i") 1
done

. scripts/accumulate-prize-weights.sh

for i in $(seq 0 $(($numberOfProjects - 1))); do
 . $REPO/scripts/eliminate-one-project.sh
done

for i in $(seq 0 $(($numberOfProjects - 1))); do
 . $REPO/scripts/distribute-prize.sh $(project_index_to_token_name "$i")
done
