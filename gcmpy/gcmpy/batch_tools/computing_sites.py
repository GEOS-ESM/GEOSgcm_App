import platform
import re
from pathlib import Path

sites_list = ['NCCS', 'NAS']

site_nodes_dict = {
        'NCCS': dict(mil=120),
        'NAS': dict(bro=24, sky=40, cas=40, rom=120, mil=120, tur=240)
        }


# Default processor first.
_NAS_OS_PROCESSORS = {
    ('rhel', '8'): ('rom', 'mil', 'cas', 'bro', 'sky'),
    ('rhel', '9'): ('tur',),
}


def host_os():
    """Return (ID, major VERSION_ID) of the running OS; empty strings if unknown."""
    try:
        release = platform.freedesktop_os_release()
    except OSError:
        release = {}
    return release.get('ID', ''), release.get('VERSION_ID', '').split('.')[0]


def build_os(etc):
    """Return (ID, major version) from <etc>/OS.rc, or None for installs without it.

    Malformed metadata exits rather than silently falling back to the host OS.
    """
    path = Path(etc) / 'OS.rc' if etc else None
    if path is None or not path.is_file():
        return None
    fields = {}
    for line in path.read_text().splitlines():
        key, _, value = line.partition(':')
        if key.strip():
            fields[key.strip()] = value.strip()
    version = fields.get('OS_VERSION', '')
    if not fields.get('OS_ID') or not re.fullmatch(r'\d+(\.\d+)*', version):
        raise SystemExit(f'ERROR: {path} is malformed: need OS_ID and a numeric OS_VERSION.')
    return fields['OS_ID'], version.split('.')[0]


def nas_processor_nodes(etc=None):
    """Return NAS processors for the build OS (OS.rc), default first.

    Without OS.rc, fall back to the OS running setup and warn.
    """
    os_key = build_os(etc)
    label = 'build'
    if os_key is None:
        os_key, label = host_os(), 'setup'
        print('WARNING: <install>/etc/OS.rc not found; the build OS cannot be verified, '
              'using the OS running setup.')
    if os_key not in _NAS_OS_PROCESSORS:
        raise SystemExit(f'ERROR: Cannot identify NAS {label} OS as RHEL 8 (TOSS4) or RHEL 9 (TOSS5); '
                         'check OS.rc or /etc/os-release.')
    return {name: site_nodes_dict['NAS'][name] for name in _NAS_OS_PROCESSORS[os_key]}


def check_nas_submit(etc, run_p):
    """Exit unless the submitting OS matches the build OS and the PBS model in run_p.

    TOSS5 jobs must be submitted from TOSS5 (and TOSS4 from TOSS4), including resubmission.
    """
    build, host = build_os(etc), host_os()
    if build is not None and build != host:
        raise SystemExit(f'ERROR: this install was built on {build[0]} {build[1]} but you are submitting from '
                         f'{host[0]} {host[1]}; submit from a matching NAS login node.')
    match = re.search(r'model=([a-z]+)', run_p)
    model = match.group(1) if match else None
    if model not in _NAS_OS_PROCESSORS.get(build or host, ()):
        raise SystemExit(f"ERROR: run_p model '{model}' cannot run on {(build or host)[0]} {(build or host)[1]}; "
                         'fix run_p in the run YAML or use a matching install.')

"""
  As far as possible, define the following parameters:

     batch_cmd               - PBS Batch command
     batch_group             - PBS Syntax for GROUP
     batch_time              - PBS Syntax for walltime
     batch_jobname           - PBS Syntax for job name
     batch_outputname        - PBS Syntax for job output name
     batch_joinouterr        - PBS Syntax for joining output and error
     run_ft                  - Wallclock Time for gcm_forecast.j
     run_ft                  - Wallclock Time for gcm_run.j
     post_t                  - Wallclock Time for gcm_post.j
     plot_t                  - Wallclock Time for gcm_plot.j
     archive_t               - Wallclock Time for gcm_archive.j
     run_q                   - Batch queue name for gcm_run.j
     run_p                   - PE Configuration for gcm_run.j
     run_fp                  - PE Configuration for gcm_forecast.j
     regress_p               - PE Configuration for gcm_regress.j
     post_q                  - Batch queue name for gcm_post.j
     plot_q                  - Batch queue name for gcm_plot.j
     move_q                  - Batch queue name for gcm_moveplot.j
     archive_q               - Batch queue name for gcm_archive.j
     move_p                  - PE Configuration for gcm_moveplot.j
"""
job_script_header = dict()
job_script_header['NAS'] = dict(
        batch_cmd = "qsub",
        batch_group = "PBS -W group_list=",
        batch_time = "PBS -l walltime=",
        batch_jobname = "PBS -N",
        batch_outputname = "PBS -o ",
        batch_joinouterr = "PBS -j oe -k oed",
        run_ft = "4:00:00",
        run_t = "4:00:00",
        post_t = "4:00:00",
        plot_t = "8:00:00",
        archive_t = "4:00:00",
        run_q = f"PBS -q normal",
        post_q = f"PBS -q normal",
        plot_q = f"PBS -q normal",
        move_q = f"PBS -q normal",
        archive_q = f"PBS -q normal",
        move_p = "PBS -l select=1:ncpus=1"
        )

job_script_header['NCCS'] = dict(
        batch_cmd  = "sbatch",
        batch_group = "SBATCH --account=",
        batch_time = "SBATCH --time=",
        batch_jobname = "SBATCH --job-name=",
        batch_outputname = "SBATCH --output=",
        batch_joinouterr = "DELETE",
        run_ft = "4:00:00",
        run_t  = "4:00:00",
        post_t = "4:00:00",
        plot_t = "12:00:00",
        archive_t = "2:00:00",
        move_q = "SBATCH --partition=datamove",
        archive_q = "SBATCH --partition=datamove",
        plot_p = f"SBATCH --nodes=4 --ntasks=4",
        archive_p = "SBATCH --ntasks=1",
        move_p = "SBATCH --ntasks=1"
        )

job_script_header['AWS'] = dict(
        batch_cmd        = "sbatch",
        batch_group      = "#DELETE",
        batch_time       = "SBATCH --time=",
        batch_jobname    = "SBATCH --job-name=",
        batch_outputname = "SBATCH --output=",
        batch_joinouterr = "DELETE",
        run_ft           = "4:00:00",
        run_t            = "4:00:00",
        post_t           = "4:00:00",
        plot_t           = "12:00:00",
        archive_t        = "1:00:00",
        post_q           = "NULL",
        plot_q           = "NULL",
        move_q           = "NULL",
        archive_q        = "NULL",
        plot_p           = f"SBATCH --nodes=4 --ntasks=4",
        archive_p        = "SBATCH --ntasks=1",
        move_p           = "SBATCH --ntasks=1"
        )

job_script_header['Azure'] = job_script_header['AWS']

job_script_header['Other'] = dict(
        batch_cmd = "sbatch",
        batch_group = "SBATCH --account=",
        batch_time = "SBATCH --time=",
        batch_jobname = "SBATCH --job-name=",
        batch_outputname = "SBATCH --output=",
        batch_joinouterr = "DELETE",
        run_ft = "4:00:00",
        run_t  = "4:00:00",
        post_t = "4:00:00",
        plot_t = "12:00:00",
        archive_t = "1:00:00",
        run_q = "NULL",
        run_p = "NULL",
        run_fp = "NULL",
        regress_p = "NULL",
        post_q = "NULL",
        plot_q = "NULL",
        move_q = "NULL",
        archive_q = "NULL",
        post_p = "NULL",
        plot_p = "NULL",
        archive_p = "NULL",
        move_p = "NULL"
        )


