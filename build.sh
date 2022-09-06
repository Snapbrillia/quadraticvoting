#!/bin/sh -e

TARGET=$1
RESULT=./${TARGET}-exe
ARTIFACTS_DIR=$2
DRV="${TARGET}.components.exes.${TARGET}"

echo "DRV=${DRV}" 1>&2

rm -f ${RESULT}

#nix --quiet build .#${DRV} -o ${RESULT}
nix --quiet build .#qvf-generate-scripts-static.x86_64-linux -o ${RESULT}

# cabal install ${TARGET} --overwrite-policy=always

# rm -f ${ARTIFACTS_DIR}/${TARGET}
rm -f ${ARTIFACTS_DIR}/*

mkdir -p ${ARTIFACTS_DIR}

# cp $(readlink ${RESULT})/bin/${TARGET} ${ARTIFACTS_DIR}/bootstrap
cp ${RESULT}/bin/${TARGET} ${ARTIFACTS_DIR}/bootstrap


