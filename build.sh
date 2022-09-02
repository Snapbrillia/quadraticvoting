#!/bin/sh -e

TARGET=$1
RESULT=./${TARGET}-exe
ARTIFACTS_DIR=$2
DRV="${TARGET}.components.exes.${TARGET}"

echo "DRV=${DRV}" 1>&2

rm -f ${RESULT}

nix --quiet build .#${DRV} -o ${RESULT}

mkdir -p ${ARTIFACTS_DIR}
cp $(readlink ${RESULT})/bin/${TARGET} ${ARTIFACTS_DIR}/${TARGET}

rm -f ${RESULT}

# cp template.yaml .aws-sam/build/