#!/bin/sh -e

ARTIFACTS_DIR=$1
TARGET=$2
NIX_ATT=$3

LAYER_TMP_DIR=${ARTIFACTS_DIR}/tmp

# tidy up previous previous, if any
rm -rf ${ARTIFACTS_DIR}

# make sure we have our ARTIFACTS_DIR and a tmp dir to build the layer in
mkdir -p ${LAYER_TMP_DIR}

# put the result (a link) of the build under .aws-sam because 'sam build'
# is aware of this directory and doesn't copy its contents (and error trying).
RESULT="$(mktemp -u -p ${ARTIFACTS_DIR}/.. ${TARGET}.XXXXXXXX).tar.gz"

nix bundle --bundler github:NixOS/bundlers#toDockerImage ${NIX_ATT} -o ${RESULT} 2> /dev/null

# extract the contents of bundle into ${LAYER_TMP_DIR}
tar -C ${LAYER_TMP_DIR} -xf ${RESULT} 2> /dev/null

BUNDLE_TARS=$(find ${LAYER_TMP_DIR} -name 'layer.tar')

LAYER_ZIP=${ARTIFACTS_DIR}/${TARGET}.zip

SHARED_LIBS=""
for TAR in $(find ${LAYER_TMP_DIR} -name 'layer.tar') ; do
    # find the names of shared libraries (and links to shared libraries) in the tar file
    if DLIBS=$(tar --list -f ${TAR} 2> /dev/null | grep -E '*\.so(\.[0-9]+)*$') ; then
        SHARED_LIBS="${SHARED_LIBS} ${DLIBS}"
    fi 
done

zip -q ${LAYER_ZIP} ${SHARED_LIBS}

# tidy up
rm -rf ${LAYER_TMP_DIR}
rm ${RESULT}
