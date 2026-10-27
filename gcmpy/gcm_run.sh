#!/bin/bash
#######################################################################
#                     Batch Parameters for Run Job
#######################################################################
#
# Usage (normally invoked by gcm_run.py once it decides to submit, not
# run by hand):
#   sbatch [sbatch resource flags...] gcm_run.sh <path/to/run_config.yaml>
#   qsub [PBS resource flags...] -v GEOS_RUN_CONFIG=<absolute config path> gcm_run.sh
#
#######################################################################

set -e

CONFIG_FILE="${1:-${GEOS_RUN_CONFIG:-}}"

if [ -n "${PBS_O_WORKDIR:-}" ]; then
    cd "${PBS_O_WORKDIR}"
fi

if [ -z "${CONFIG_FILE}" ]; then
    echo "usage: gcm_run.sh <run_config.yaml>" >&2
    exit 1
fi

umask 022

export ARCH="$(uname)"

# macOS caps the stack; raise its soft limit to the permitted hard limit.
if [ "${ARCH}" = "Darwin" ]; then
    ulimit -S -s "$(ulimit -H -s)"
else
    ulimit -s unlimited
fi

#######################################################################
#           Architecture Specific Environment Variables
#           (gcm_run.j:26-50)
#######################################################################

# GEOSBIN is passed by gcm_run.py at submission time
# (Slurm --export or PBS -v). Fall back to the directory containing this
# script -- but only if that directory really looks like an install bin/:
# inside a SLURM job, BASH_SOURCE[0] is the spool copy (slurm_script), so
# a blind fallback would point GEOSBIN at /var/spool/slurmd/job<id>/.
if [ -z "${GEOSBIN}" ]; then
    _geosbin_candidate="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    if [ -f "${_geosbin_candidate}/g5_modules.sh" ]; then
        GEOSBIN="${_geosbin_candidate}"
    fi
    unset _geosbin_candidate
fi

if [ -z "${GEOSBIN}" ] || [ ! -f "${GEOSBIN}/g5_modules.sh" ]; then
    echo "gcm_run.sh: cannot locate g5_modules.sh (GEOSBIN='${GEOSBIN:-}')." >&2
    echo "  GEOSBIN must be provided at submission time; submit this job" >&2
    echo "  via gcm_run.py, which exports GEOSBIN for Slurm or PBS." >&2
    exit 1
fi
export GEOSBIN

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

source "${GEOSBIN}/g5_modules.sh"

# gcm_run.j:88-97 -- DSL configuration: expose the GEOS Python libs.
if [ -n "${USE_DSL:-}" ]; then
    export PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${GEOSDIR}/lib/Python/"
fi

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

# Mark completion so direct Python launches can initialize through this
# internal wrapper without entering an exec loop.
export GEOS_RUN_ENV_READY="${GEOSBIN}"
exec python3 "${GEOSBIN}/gcmpy/gcm_run.py" --config "${CONFIG_FILE}"
