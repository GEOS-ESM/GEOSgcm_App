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

# gcm_run.j:21 -- limit stacksize unlimited
ulimit -s unlimited

#######################################################################
#           Architecture Specific Environment Variables
#           (gcm_run.j:26-50)
#######################################################################

# GEOSBIN is expected to already be set in the submitting environment
# (it is exported by gcm_setup / the site module environment). Fall back
# to the directory containing this script if it is not.
if [ -z "${GEOSBIN}" ]; then
    GEOSBIN="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    export GEOSBIN
fi

export ARCH="$(uname)"

# GEOSBIN is $install/bin; derive the install root and etc directory
# from it (gcm_run.j baked GEOSDIR/GEOSETC/GEOSUTIL in via Jinja).
export GEOSDIR="$(dirname "${GEOSBIN}")"
export GEOSUTIL="${GEOSDIR}"
export GEOSETC="${GEOSDIR}/etc"

# SITE comes from etc/SITE.rc, whose last whitespace-separated token is
# the site name (mirrors setup_envs.py: read().split()[-1]).
SITE_RC="${GEOSETC}/SITE.rc"
if [ -f "${SITE_RC}" ]; then
    export SITE="$(tr -s '[:space:]' '\n' < "${SITE_RC}" | grep -v '^$' | tail -1)"
else
    export SITE=""
fi

source "${GEOSBIN}/g5_modules"

# gcm_run.j:36-45 -- prepend to DY/LD_LIBRARY_PATH (DYLD on macOS), and
# add BASEDIR when defined (i.e., not running with Spack).
if [ "${ARCH}" = "Darwin" ]; then
    LD_VAR="DYLD_LIBRARY_PATH"
else
    LD_VAR="LD_LIBRARY_PATH"
fi
eval "CURRENT_LDPATH=\${${LD_VAR}-}"
export "${LD_VAR}=${CURRENT_LDPATH:+${CURRENT_LDPATH}:}${GEOSDIR}/lib"

if [ -n "${BASEDIR:-}" ]; then
    eval "CURRENT_LDPATH=\${${LD_VAR}-}"
    export "${LD_VAR}=${CURRENT_LDPATH:+${CURRENT_LDPATH}:}${BASEDIR}/${ARCH}/lib"
    export PATH="${PATH}:${BASEDIR}/${ARCH}/bin"
fi

#######################################################################
#                 Hand off to the Python orchestrator
#######################################################################

exec python3 "${GEOSBIN}/gcmpy/gcm_run.py" --config "${CONFIG_FILE}"
