#!/bin/bash
# Based on https://github.com/aws-samples/aws-lambda-extensions/blob/main/custom-runtime-extension-demo/extensionssrc/extensions/extension2.sh
# Which  bears the following 
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0

# See here for further info https://aws.amazon.com/blogs/compute/building-extensions-for-aws-lambda-in-preview/

set -euo pipefail

OWN_FILENAME="$(basename $0)"
LAMBDA_EXTENSION_NAME="${OWN_FILENAME}" # (external) extension name has to match the filename
TMPFILE=/tmp/${OWN_FILENAME}

# Graceful Shutdown
_term() {
  echo "[${LAMBDA_EXTENSION_NAME}] Received SIGTERM"
  # forward SIGTERM to child procs and exit
  kill -TERM "${EVENT_PID}" 2>/dev/null
  echo "[${LAMBDA_EXTENSION_NAME}] Exiting"
  exit 0
}

forward_sigterm_and_wait() {
  trap _term SIGTERM
  wait "${EVENT_PID}"
  trap - SIGTERM
}

# TODO !@! Revisit this when key management etc is sorted out
get_key_file() {
  if [ -z ${CNODE_KEY_FILE_CONTENTS} ] ; then
    echo "ERROR: key file contents not provided."
    exit 1
  fi
  local kf
  kf="$(mktemp)"
  echo "${CNODE_KEY_FILE_CONTENTS}" > ${kf}
  chmod 600 ${kf}
  echo ${kf}
}

# Initialization
# To run any extension processes that need to start before the runtime initializes, run them before the /register 
echo "[${LAMBDA_EXTENSION_NAME}] Initialization"

# These should come from the environment and be 'secrets' on github. Default them for now.
[ -z ${CNODE_USER} ] && CNODE_USER=ubuntu
[ -z ${LOCAL_CNODE_SOCKET_PATH} ] && LOCAL_CNODE_SOCKET_PATH=/tmp/cnode.socket
[ -z ${REMOTE_CNODE_SOCKET_PATH} ] && REMOTE_CNODE_SOCKET_PATH=/opt/cardano/cnode/sockets/node0.socket
[ -z ${CNODE_HOST} ] && CNODE_HOST=ec2-54-234-55-233.compute-1.amazonaws.com


KEY_FILE="$(get_key_file)"

# Avoid need for interaction if/when cnode host changes
STRICTHOSTKEYCHECKING="StrictHostKeyChecking accept-new"

ssh -i ${KEY_FILE} -o ${STRICTHOSTKEYCHECKING} -l ${CNODE_USER} -nNT -L ${LOCAL_CNODE_SOCKET_PATH}:${REMOTE_CNODE_SOCKET_PATH} ${CNODE_HOST} &

# Registration
# The extension registration also signals to Lambda to start initializing the runtime. 
HEADERS="$(mktemp)"
echo "[${LAMBDA_EXTENSION_NAME}] Registering at http://${AWS_LAMBDA_RUNTIME_API}/2020-01-01/extension/register"
curl -sS -LD "${HEADERS}" -XPOST "http://${AWS_LAMBDA_RUNTIME_API}/2020-01-01/extension/register" --header "Lambda-Extension-Name: ${LAMBDA_EXTENSION_NAME}" -d "{ \"events\": [\"INVOKE\", \"SHUTDOWN\"]}" > ${TMPFILE}

RESPONSE=$(<${TMPFILE})
HEADINFO=$(<${HEADERS})
# Extract Extension ID from response headers
EXTENSION_ID=$(grep -Fi Lambda-Extension-Identifier "${HEADERS}" | tr -d '[:space:]' | cut -d: -f2)
echo "[${LAMBDA_EXTENSION_NAME}] Registration response: ${RESPONSE} with EXTENSION_ID $(grep -Fi Lambda-Extension-Identifier "${HEADERS}" | tr -d '[:space:]' | cut -d: -f2)"

# Event processing
# Continuous loop to wait for events from Extensions API
while true
do
  echo "[${LAMBDA_EXTENSION_NAME}] Waiting for event. Get /next event from http://${AWS_LAMBDA_RUNTIME_API}/2020-01-01/extension/event/next"

  # Get an event. The HTTP request will block until one is received
  curl -sS -L -XGET "http://${AWS_LAMBDA_RUNTIME_API}/2020-01-01/extension/event/next" --header "Lambda-Extension-Identifier: ${EXTENSION_ID}" > ${TMPFILE} &
  EVENT_PID=$!
  forward_sigterm_and_wait

  EVENT_DATA=$(<${TMPFILE})
  if [[ ${EVENT_DATA} == *"SHUTDOWN"* ]]; then
    echo "[extension: ${LAMBDA_EXTENSION_NAME}] Received SHUTDOWN event. Exiting."  1>&2;
    exit 0 # Exit if we receive a SHUTDOWN event
  fi

  echo "[${LAMBDA_EXTENSION_NAME}] Received event: ${EVENT_DATA}" 
  
done