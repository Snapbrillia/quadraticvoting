#!/bin/bash -e

ARTIFACTS_DIR=$1
TARGET=$2

# tidy up previous, if any
rm -rf ${ARTIFACTS_DIR}

# make sure we have our ARTIFACTS_DIR
mkdir -p ${ARTIFACTS_DIR}

# The (generated) bootstrap file (see the versions of BOOTSTRAP_SCRIPT below)
BOOTSTRAP=${ARTIFACTS_DIR}/bootstrap

# Where we put the exe
BIN_DIR=${ARTIFACTS_DIR}/bin
mkdir -p ${BIN_DIR}

FUNCTION_EXE=${BIN_DIR}/${TARGET}

# make sure '.cabal' is populated
cabal v2-update

# cabal v2 install options
INSTALL_OPTIONS="--enable-executable-stripping --reinstall --overwrite-policy=always --install-method=copy --installdir=${BIN_DIR}"
# build and install the lambda executable
cabal v2-install ${TARGET} ${INSTALL_OPTIONS} 2>&1 > /dev/null

LOADER=ld-linux-x86-64.so.2

MY_LD_LIBRARY_PATH="/usr/local/lib/:${LD_LIBRARY_PATH}:/lib/x86_64-linux-gnu:/lib64"

expand_lib_path () {
    local lib="$1"
    if [ ${lib} == "linux-vdso.so.1" ] ; then
        # filter this here rather than echo it only for it to be filtered later
        # echo ${lib}
        return 0
    else
        local dir=$(dirname ${lib})
        if [ ${dir} != "." ] ; then
            echo ${lib}
        else
            for path in ${MY_LD_LIBRARY_PATH//:/ /} ; do
                if [ -f ${path}/${lib} ] ; then
                    echo ${path}/${lib}
                    return 0
                fi
            done
        fi
    fi
}

expand_lib_paths ()
{
    for lib in $1 ; do
        expand_lib_path ${lib}
    done
}

get_shared_objects ()
{
    local libs=$(echo "$@" | awk '{print $(NF-1)}')
    expand_lib_paths "${libs}"
}

# check for dynamic exe
if LDD_OUTPUT=$(ldd ${FUNCTION_EXE} 2> /dev/null) ; then
    # We need to find the transitive closure of the exe's dependencies
    # and place them in LIB_DIR.

    # make sure we have a LIB_DIR
    LIB_DIR=${ARTIFACTS_DIR}/lib
    mkdir -p ${LIB_DIR}

    DLIBS=$(get_shared_objects "${LDD_OUTPUT}")
    DLIBS_QUEUE=(${DLIBS})
    # echo "DLIBS_QUEUE=${DLIBS_QUEUE[@]}"
    ALL_DLIBS=""
    while [ "${#DLIBS_QUEUE[@]}" -gt 0 ]; do
        # get the first item off the queue
        DLIB=${DLIBS_QUEUE[0]}
        # echo "Processing ${DLIB}"

        # shift the queue (i.e. drop the first item)
        DLIBS_QUEUE=("${DLIBS_QUEUE[@]:1}")

        if [[ ! -f ${DLIB} ]]; then
            # ignore anything not a file e..g linux-vdso.so.1 - which is a 'virtual' shared object
            # although that should have alread been filtered 
            continue
        fi
        
        DLIB_DIR=$(dirname ${DLIB})
        DLIB_NAME=$(basename ${DLIB})
        DLIB_ROOT=${DLIB_NAME%%.*}
        # We want everything that looks like a shared object with the same name
        # (i.e. including symlinks) from that dir.
        # TODO !@! we could be smater here i.e. we need the file and just those links
        # referenced by the shared object
        for x in $(ls ${DLIB_DIR}/${DLIB_ROOT}* | grep -E '*\.so(\.[0-9]+)*$') ; do
            # check x is not already in ALL_LIBS 
            if echo ${ALL_DLIBS} | grep -vq ${x} ; then
                # add x to ALL_LIBS
                #echo "Adding ${x} to ALL_DLIBS"
                ALL_DLIBS="${ALL_DLIBS} ${x}"
                # deal with x's dependencies
                if LDD_OUTPUT=$(ldd ${x} 2> /dev/null) ; then
                    for d in $(get_shared_objects "${LDD_OUTPUT}") ; do
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

    # set up LIB_DIR
    for x in ${ALL_DLIBS} ; do
        if [ -f ${LIB_DIR}/$(basename ${x}) ] ; then
            # This happens with e.g /nix/store/bym6162f9mf4qqsr7k9d73526ar176x4-gcc-11.3.0-lib/lib/libgcc_s.so
            # and the 64 bit version. There's nothing simple we can do about this. 
            # i.e. maybe we could come up with a scheme using 'patchelf' - but so far that's not needed
            echo "WARNING: $(basename ${x}) is already in LIB_DIR (${x})" 1>&2
            continue
        fi
        if [ -L ${x} ] ; then
            # its a symbolic link
            # TODO !@! really we should process the links after to ensure they have
            # a target in ${LIB_DIR} and at least give a warning if they don't.
            TARGET_NAME=$(basename $(readlink -f ${x}))
            LINK_NAME=$(basename ${x})
            pushd ${LIB_DIR} > /dev/null
            ln -s ./${TARGET_NAME} ${LINK_NAME}
            popd > /dev/null
        else
            cp ${x} ${LIB_DIR}
        fi
    done

    # Check we've got a loader
    if [[ ! -f ${LIB_DIR}/${LOADER} ]] ; then
        echo "ERROR: file '${LIB_DIR}/${LOADER}' not found" 1>&2
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
export AWS_EXECUTION_ENV=lambda-haskell
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$LAMBDA_TASK_ROOT/lib
exec \$LAMBDA_TASK_ROOT/bin/${TARGET} \${_HANDLER}
EOF
)

fi

echo -e "${BOOTSTRAP_SCRIPT}" > ${BOOTSTRAP}
chmod +x ${BOOTSTRAP}

# explicitly strip the executable (coz cabal doesn't do a good job - if at all - when asked)
strip -s -o ${FUNCTION_EXE}.stripped ${FUNCTION_EXE}
mv ${FUNCTION_EXE}.stripped ${FUNCTION_EXE}