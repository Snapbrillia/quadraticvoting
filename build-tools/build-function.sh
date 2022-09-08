#!/bin/bash -e

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
FUNCTION_EXE=${RESULT}/bin/${TARGET}

# Create a directory to collect the contents of the zip file (i.e. the lambda package)
PKG_DIR=${ARTIFACTS_DIR}/pkg
PKG_BIN_DIR=${PKG_DIR}/bin

BOOTSTRAP=${PKG_DIR}/bootstrap

mkdir -p ${PKG_BIN_DIR}

# build the lambda
nix build ${NIX_ATT} -o ${RESULT} 2>&1 > /dev/null

# copy the lambda to the packaging dir
cp ${FUNCTION_EXE} ${PKG_BIN_DIR}

LOADER=ld-linux-x86-64.so.2

# check for dynamic exe
if LDD_OUTPUT=$(ldd ${FUNCTION_EXE} 2> /dev/null) ; then
    # we need a lib dir
    PKG_LIB_DIR=${PKG_DIR}/lib
    mkdir -p ${PKG_LIB_DIR}

    DLIBS=$(echo "${LDD_OUTPUT}" | awk '{print $(NF-1)}')
    DLIBS_QUEUE=(${DLIBS})
    # echo "DLIBS_QUEUE=${DLIBS_QUEUE[@]}"
    ALL_DLIBS=""
    while [ "${#DLIBS_QUEUE[@]}" -gt 0 ]; do
        # git the first item off the queue
        DLIB=${DLIBS_QUEUE[0]}
        # echo "Processing ${DLIB}"

        # shift the queue (i.e. drop the first item)
        DLIBS_QUEUE=("${DLIBS_QUEUE[@]:1}")

        if [[ ! -f ${DLIB} ]]; then
            # ignore linux-vdso.so.1 - which is a 'virtual' shared object 
            continue
        fi

        # ASSUMPTION: DLIB is in the nix store in its own dir.
        # We want everything that looks like a shared object (i.e. including symlinks) in that dir.
        DLIB_DIR=$(dirname ${DLIB})
        DLIB_NAME=$(basename ${DLIB})
        DLIB_ROOT=${DLIB_NAME%%.*}
        for x in $(ls ${DLIB_DIR}/${DLIB_ROOT}* | grep -E '*\.so(\.[0-9]+)*$') ; do
            # check x is not in ALL_LIBS 
            if echo ${ALL_DLIBS} | grep -vq ${x} ; then
                # add x to ALL_LIBS
                #echo "Adding ${x} to ALL_DLIBS"
                ALL_DLIBS="${ALL_DLIBS} ${x}"

                # deal with x's dependencies
                if LDD_OUTPUT=$(ldd ${x} 2> /dev/null) ; then
                    for d in $(echo "${LDD_OUTPUT}" | awk '{print $(NF-1)}') ; do
                        # check d is not 'virtual' i.e. linux-vdso.so.1
                        if [[ -f ${d} ]]; then
                            # check d is not already in ALL_LIBS
                            if echo ${ALL_DLIBS} | grep -vq ${d} ; then
                                # check d is not already in DLIBS_QUEUE
                                if echo ${DLIBS_QUEUE[@]} | grep -vq ${d} ; then
                                    # add d to the queue
                                    # echo "Adding ${d} to DLIBS_QUEUE"
                                    DLIBS_QUEUE+=(${d})
                                fi
                            fi
                        fi
                    done
                fi
            fi
            
        done
        # echo "ALL_DLIBS=${ALL_DLIBS}"
    done

    # set up PKG_LIB_DIR
    for x in ${ALL_DLIBS[@]} ; do
        if [ -f ${PKG_LIB_DIR}/$(basename ${x}) ] ; then
            # Assume they're hardlinked like e.g. libgcc_s.so.1
            continue
        fi
        if [ -L ${x} ] ; then
            TARGET_NAME=$(basename $(readlink -f ${x}))
            LINK_NAME=$(basename ${x})
            pushd ${PKG_LIB_DIR} > /dev/null
            ln -s ./${TARGET_NAME} ${LINK_NAME}
            popd > /dev/null
        else
            cp ${x} ${PKG_LIB_DIR}
        fi
    done

    # Check we've got a loader
    if [[ ! -f ${PKG_LIB_DIR}/${LOADER} ]] ; then
        echo "ERROR: file '${PKG_LIB_DIR}/${LOADER}' not found" 1>&2
        exit 1
    fi

    # create bootstrap for dynamically linked exe
    BOOTSTRAP_SCRIPT=$(cat <<EOF
#!/bin/bash
set -euo pipefail
export AWS_EXECUTION_ENV=lambda-cpp
exec \$LAMBDA_TASK_ROOT/lib/${LOADER} --library-path \$LAMBDA_TASK_ROOT/lib \$LAMBDA_TASK_ROOT/bin/${TARGET} \${_HANDLER}
EOF
)

else

    # create bootstrap for statically linked exe
    BOOTSTRAP_SCRIPT=$(cat <<EOF
#!/bin/bash
set -euo pipefail
export AWS_EXECUTION_ENV=lambda-cpp
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$LAMBDA_TASK_ROOT/lib
exec \$LAMBDA_TASK_ROOT/bin/${TARGET} \${_HANDLER}
EOF
)

fi

echo -e "${BOOTSTRAP_SCRIPT}" > ${BOOTSTRAP}
chmod + ${BOOTSTRAP}

# create the zip file
pushd ${PKG_DIR} > /dev/null

ZIP_FILE=${TARGET}.zip

#zip --symlinks --recurse-paths ${ARTIFACTS_DIR}/${ZIP_FILE} -- *
zip --symlinks --recurse-paths ${ZIP_FILE} * 2>&1 > /dev/null

popd > /dev/null

mv ${PKG_DIR}/${ZIP_FILE} ${ARTIFACTS_DIR}/${ZIP_FILE}

rm -rf ${PKG_DIR}
rm ${RESULT}


