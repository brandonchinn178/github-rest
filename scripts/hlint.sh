#!/bin/bash
#
# Runs HLint and errors if any hints are found.

set -eo pipefail

ARGS=("$@")
if [[ "${#ARGS}" == 0 ]]; then
    ARGS+=(.)
fi

stack build --stack-yaml stack-linters.yaml hlint
stack exec -- hlint "${ARGS[@]}"
