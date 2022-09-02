#!/bin/sh -e

ARTIFACTS_DIR=$1

nix --quiet build -o qvf-generate-scripts

mkdir -p ${ARTIFACTS_DIR}
cp $(readlink qvf-generate-scripts)/bin/qvf-generate-scripts ${ARTIFACTS_DIR}/bootstrap

# cp template.yaml .aws-sam/build/