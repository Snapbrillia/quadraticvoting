#!/bin/sh -e

ARTIFACTS_DIR=$1
TARGET=$2
NIX_ATT=$3

# tidy up previous previous, if any
rm -rf ${ARTIFACTS_DIR}

# make sure we have our ARTIFACTS_DIR
mkdir -p ${ARTIFACTS_DIR}

# put the result (a link) of the build under .aws-sam because 'sam build'
# is aware of this directory and doesn't copy its contents (and error trying).
RESULT=$(mktemp -u -p ${ARTIFACTS_DIR}/.. ${TARGET}.XXXXXXXX)

# carry out the build
nix build ${NIX_ATT} -o ${RESULT}

# copy the statically linked executable and rename 'bootstrap' as required
# by the lambda runtime
cp ${RESULT}/bin/${TARGET} ${ARTIFACTS_DIR}/bootstrap

