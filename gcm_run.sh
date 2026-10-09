#!/bin/bash
#######################################################################
#                     Batch Parameters for Run Job
#######################################################################
#
# Usage (normally invoked by gcm_run.py once it decides to submit, not
# run by hand):
#   sbatch [sbatch resource flags...] gcm_run.sh <path/to/run_config.yaml>
#
#######################################################################

set -e

CONFIG_FILE="$1"

if [ -z "${CONFIG_FILE}" ]; then
    echo "usage: gcm_run.sh <run_config.yaml>" >&2
    exit 1
fi

umask 022

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

# GEOSBIN is expected to already be set in the submitting environment
# (it is exported by gcm_setup / the site module environment). Fall back
# to the directory containing this script if it is not.
if [ -z "${GEOSBIN}" ]; then
    GEOSBIN="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    export GEOSBIN
fi

source "${GEOSBIN}/g5_modules"

#######################################################################
#                 Hand off to the Python orchestrator
#######################################################################

exec python3 "${GEOSBIN}/gcmpy/gcm_run.py" --config "${CONFIG_FILE}"
