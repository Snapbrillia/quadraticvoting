#!/bin/bash -e

TEMPLATE=$1

RESOURCES=$(yq '.Resources' -o json < ${TEMPLATE})
FUNCTION=$(echo "${RESOURCES}" | jq -r '. | keys[0]')
TARGET=$(echo "${RESOURCES}" | jq '.[]' | jq -r 'getpath(["Properties","Handler"])')

ARTIFACT_DIR=.aws-sam/build/${FUNCTION}
BUILD_DIR=$(mktemp -d -p . .aws-sam.XXXXXXXX)

docker run -t -v `pwd`:/home/builder/repo -v `pwd`/.cabal.docker:/home/builder/.cabal \
			   -w /home/builder/repo haskell-cabal-build \
			   ./cabal-build-function.sh ${BUILD_DIR} ${TARGET}

sam build -t ${TEMPLATE}

mv ${BUILD_DIR}/* ${ARTIFACT_DIR}

rm -rf ${BUILD_DIR}
