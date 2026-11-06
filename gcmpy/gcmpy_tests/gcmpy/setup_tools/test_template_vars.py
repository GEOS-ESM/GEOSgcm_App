"""
Template-variable coverage audit.

Every ``{{ ... }}`` placeholder in the templates rendered by SetupGCM must
resolve against the Jinja context that a real setup produces (config
builders + SetupGCM attributes + the runtime attributes set later in the
setup flow). This mechanically guards zero-diff parity with the old csh
tooling: a new template variable (e.g. ``atmos.SHALLOW_SHOC``,
``atmos.run_hydrostatic``) is an immediate test failure until gcmpy's
setup supplies it.
"""

import re
from pathlib import Path

import pytest

from gcmpy.setup_tools.setup_main import SetupGCM

REPO_ROOT = Path(__file__).resolve().parents[4]

# Mirrors SetupGCM.templates (setup_main.py) plus the ocean history
# template appended by config_models().
TEMPLATES = [
    'gcm_post.j',
    'gcm_archive.j',
    'gcm_regress.j',
    'gcm_plot.tmpl',
    'gcm_quickplot.csh',
    'gcm_moveplot.j',
    'gcm_forecast.tmpl',
    'gcm_forecast.setup',
    'gcm_emip.setup',
    'CAP.rc.tmpl',
    'AGCM.rc.tmpl',
    'logging.yaml',
    'fvcore_layout.rc',
    'HISTORY.AGCM.rc.tmpl',
]

# Runtime attributes set after SetupGCM.__init__ (set_num_CPUs,
# set_batch_params, set_site_params, ...) -- present in the real Jinja
# context at render time.
RUNTIME_ATTRS = {
    'NCAR_NRDG': 16,
    'M2_replay_ana_location': '',
    'replay_ana_expID': '',
    'bcs_dir': '',
    'coupled_dir': '',
    'sst_dir': '',
    'gwdrs_dir': '',
    'site': 'NCCS',
    'preload_command': '',
    'ld_library_path_command': '',
    'run_command': '',
    'install_path': '',
    'bin_path': '',
    'etc_path': '',
    'mpi_config': '',
    'num_CPUs': 40,
    'job_sgmt': '00000016 000000',
    'batch_cmd': 'sbatch',
    'batch_group_full': 'SBATCH --account=x',
    'batch_time': 'SBATCH --time=',
    'batch_jobname': 'SBATCH --job-name=',
    'batch_outputname': 'SBATCH --output=',
    'batch_joinouterr': 'DELETE',
    'run_fn': 'test_exp_FCST',
    'run_fp': '',
    'run_ft': '4:00:00',
    'run_q': '',
    'run_t': '4:00:00',
    'post_n': 1,
    'post_p': '',
    'post_q': '',
    'post_t': '4:00:00',
    'plot_p': '',
    'plot_q': '',
    'plot_t': '12:00:00',
    'move_n': 1,
    'move_p': '',
    'move_q': '',
    'archive_n': 1,
    'archive_p': '',
    'archive_q': '',
    'archive_t': '1:00:00',
    'regress_n': 1,
    'regress_p': '',
}


def _exp_config(tmp_path):
    """Representative coupled MOM6 c180 LM=91 experiment config."""
    return {
        'experiment_id': 'test_exp',
        'experiment_description': 'template audit',
        'exp_dir': str(tmp_path),
        'AGCM_grid': 'c180',
        'AGCM_LM': '91',
        'MOIST_version': 'GFDL_1M',
        'heartbeat': 600,
        'io_server': 0,
        'OCEAN_type': 'MOM6',
        'OCEAN_coupled': True,
        'OCEAN_data_atmosphere': False,
        'DATAOCEAN_resolution': 'o180',
        'MOM6_resolution': 'o1440',
        'SEAICE_model': 'CICE6',
        'history_template': 'HISTORY.AGCM.rc.tmpl',
        'LAND_bcs': 'v14',
        'LAND_model': 'Catchment',
        'GOCART_aerosols': 'Actual',
        'GOCART_emission': 'AMIP',
    }


@pytest.fixture
def jinja_context(tmp_path, monkeypatch):
    etc = tmp_path / 'etc'
    etc.mkdir()
    (etc / '.AGCM_VERSION').write_text('v-test\n')

    import gcmpy.setup_tools.setup_main as sm
    monkeypatch.setitem(sm.pathdict, 'etc', etc)

    setup = SetupGCM(_exp_config(tmp_path))
    setup.config_models()
    return {**vars(setup), **RUNTIME_ATTRS}


_VAR_RE = re.compile(r'\{\{\s*([^{}]+?)\s*\}\}')


def _resolve(context, dotted):
    obj = context
    for part in dotted.split('.'):
        if isinstance(obj, dict):
            if part not in obj:
                return False
            obj = obj[part]
        elif hasattr(obj, part):
            obj = getattr(obj, part)
        else:
            return False
    return True


@pytest.mark.parametrize('template_name', TEMPLATES)
def test_template_placeholders_resolve(jinja_context, template_name):
    path = REPO_ROOT / template_name
    if not path.exists():
        pytest.skip(f'{template_name} not present in source tree')

    exprs = _VAR_RE.findall(path.read_text())
    missing = sorted({
        expr for expr in exprs
        if not _resolve(jinja_context, expr.split('|')[0].strip())
    })
    assert not missing, (
        f'{template_name}: placeholders not supplied by the python setup: '
        f'{missing}'
    )


def test_context_covers_new_upstream_vars(jinja_context):
    """Spot-check the vars added by the develop hand-merge (LM=91, c180)."""
    assert jinja_context['atmos']['SHALLOW_UW'] == '#'
    assert jinja_context['atmos']['SHALLOW_SHOC'] == ''
    assert jinja_context['atmos']['RRTMGP_RADIATION'] == ''
    assert jinja_context['atmos']['RRTMG_RADIATION'] == '#DELETE'
    assert jinja_context['atmos']['run_hydrostatic'] == 'hydrostatic = .true.'
    assert jinja_context['atmos']['nx'] == 20
    assert jinja_context['atmos']['ny'] == 96
    assert jinja_context['atmos']['conv_dt'] == 450
    assert jinja_context['atmos']['chem_dt'] == 900
    assert jinja_context['atmos']['dt_ocean'] == jinja_context['atmos']['dt'] == 600
    assert jinja_context['ocean']['DATAATM'] == '#DELETE'
    # AMIP emissions use the merra-2 MERRA2OX species staged by stagebcs
    # (bcs_catalog pchem_species/merra-2: 197902-202608 -> 48 years).
    assert jinja_context['gocart']['pchem_clim_years'] == 48
    assert jinja_context['MOM5'] == '#DELETE'
