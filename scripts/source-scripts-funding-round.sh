#!/bin/bash

if [ -z $REPO ]; then
  echo "The \$REPO environment variable is not defined. Please review the script at"
  echo "\`scripts/local-env.sh\` and make any desired changes, and then assign the"
  echo "absolute path to this repository to \$REPO before proceeding."
  return 1
else
  . $REPO/scripts/local-env.sh
  . $REPO/scripts/setup.sh 2> /dev/null
  . $REPO/scripts/env.sh
  . $REPO/scripts/initiation.sh

  echo "Sourcing completed."
  echo "Next, get the desired deadline for the funding round in POSIX time."
  echo "    There are helper functions that start with 'posix_ ...'"
  echo "Then, start a funding round with"
  echo "     initiate_fund [POSIXTIME DEADLINE]"
fi
