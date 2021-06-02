#!/bin/bash
#
# Runs HLint and errors if any hints are found.

set -eo pipefail

ARGS=("$@")
if [[ "${#ARGS}" == 0 ]]; then
    ARGS+=(.)
fi

stack build hlint
stack exec -- hlint "${ARGS[@]}"
