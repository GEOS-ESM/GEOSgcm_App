"""
csh-name placeholder parity for the coupled-only plot templates.

Upstream gcm_setup substituted UPPER_CASE names ({{ EXPID }}, {{ HOMDIR }},
{{ GEOSDIR }}, {{ COUPLEDIR }}, {{ AGCM_IM }}, {{ OGCM_IM }}, {{ PLOT_T }},
...) through one sed table applied to every template (gcm_setup:2091-2327).
gcmpy renders the same files with Jinja, and an unknown name there is
Jinja's Undefined -- which prints as nothing.  The only templates still
carrying those names are the coupled-only pair plotocn.j and confocn.py
(copied to the experiment as plotocn.j / __init__.py), so without the
aliases they silently render as "set EXPID=", "expid=''" and
"basin_mask='/basin_mask/x/basin_mask.nc'" instead of the values the csh
would have written.
"""

from pathlib import Path

import yaml
import pytest

import gcmpy.setup_tools.setup_main as sm
from gcmpy.setup_tools.setup_main import SetupGCM


def _coupled_setup(tmp_path, monkeypatch, exp_dir):
    # pathdict/envdict are filled by setup_envs.set_env_dicts(), which
    # gcm_setup.py calls but the test import path does not -- seed them.
    etc = tmp_path / 'etc'
    etc.mkdir()
    (etc / '.AGCM_VERSION').write_text('v-test\n')
    monkeypatch.setitem(sm.pathdict, 'install', tmp_path)
    monkeypatch.setitem(sm.pathdict, 'bin', tmp_path / 'bin')
    monkeypatch.setitem(sm.pathdict, 'etc', etc)

    for key, value in {
        'site': 'NCCS',
        'preload_command': 'LD_PRELOAD',
        'ld_library_path_command': 'LD_LIBRARY_PATH',
        'run_command': '$GEOSBIN/esma_mpirun -np ',
    }.items():
        monkeypatch.setitem(sm.envdict, key, value)

    cfg = {
        'experiment_id': 'test_exp',
        'experiment_description': 'csh alias test',
        'exp_dir': str(exp_dir),
        'AGCM_grid': 'c12',
        'AGCM_LM': '72',
        'MOIST_version': 'GFDL_1M',
        'heartbeat': 450,
        'io_server': 0,
        'OCEAN_type': 'MOM6',
        'OCEAN_coupled': True,
        'OCEAN_data_atmosphere': False,
        'DATAOCEAN_resolution': None,
        'MOM6_resolution': 'o72',
        'SEAICE_model': 'CICE6',
        'history_template': 'HISTORY.AOGCM.rc.tmpl',
        'LAND_bcs': 'NL3',
        'LAND_model': 'Catchment',
        'GOCART_aerosols': 'Actual',
        'GOCART_emission': 'AMIP',
        'group_root': 's1873',
    }
    setup = SetupGCM(cfg)
    setup.config_models()
    # normally set by set_batch_params()/set_site_params() in the real flow
    setup.batch_group = 'SBATCH --account='
    setup.coupled_dir = '/coupled/bcs'
    setup.plot_t = '12:00:00'
    return setup


def test_generated_run_yaml_defaults_environment_logging_off(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    setup.templates = []
    setup.template()
    config = yaml.safe_load((exp_dir / 'run_test_exp.yaml').read_text())
    assert config['print_run_environment'] is False


@pytest.mark.parametrize('site', ['NCCS', 'NAS', 'DESKTOP'])
@pytest.mark.parametrize('mpi', ['intelmpi', 'openmpi', 'mvapich', 'mpich', 'mpt'])
def test_generated_run_yaml_uses_mpi_mapping(tmp_path, monkeypatch, site, mpi):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    mpi_settings = yaml.safe_load(
        (Path(sm.__file__).parent / 'config' / 'mpi_config.yaml').read_text()
    )
    monkeypatch.setitem(sm.pathdict, 'config', Path(sm.__file__).parent / 'config')
    monkeypatch.setitem(sm.envdict, 'mpi', mpi)
    monkeypatch.setitem(sm.envdict, 'site', site)
    setup.gwdrs_dir = str(tmp_path)
    setup.mpistacksettings()
    # Exercise the actual legacy templates, not a second hand-written renderer.
    repo = Path(sm.__file__).resolve().parents[3]
    templates = ['gcm_post.j', 'gcm_archive.j', 'gcm_forecast.tmpl']
    for template in templates:
        (exp_dir / template).write_text((repo / template).read_text())
    setup.templates = templates
    setup.template()
    text = (exp_dir / 'run_test_exp.yaml').read_text()
    config = yaml.safe_load(text)
    expected = dict(mpi_settings[mpi] or {})
    if mpi == 'intelmpi' and site in ('NCCS', 'NAS'):
        expected.update(mpi_settings['intelmpi_NASA'])
    if mpi == 'mpt':
        expected['MPI_SHEPHERD'] = 'true'
    assert config['mpi_environment'] == expected
    assert 'mpi_config' not in config
    assert 'mpt_shepherd' not in config['ocean']
    assert 'setenv ' not in text
    assert 'unsetenv ' not in text
    for template in templates:
        rendered = (exp_dir / template).read_text()
        assert '{{ mpi_config }}' not in rendered
        if mpi == 'mpt':
            assert "setenv MPI_MEMMAP_OFF ''\n" in rendered
            assert 'unsetenv PMI_RANK\n' in rendered
            assert rendered.count('setenv MPI_SHEPHERD true') == 1
        if mpi == 'openmpi':
            assert "setenv OMPI_MCA_sharedfp '^lockedfile,individual'" in rendered
        if mpi == 'intelmpi' and site in ('NCCS', 'NAS'):
            assert 'setenv I_MPI_FABRICS shm:ofi' in rendered


def test_uncoupled_mpt_does_not_enable_shepherd(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    monkeypatch.setitem(sm.pathdict, 'config', Path(sm.__file__).parent / 'config')
    monkeypatch.setitem(sm.envdict, 'mpi', 'mpt')
    setup.ocean['running_ocean'] = False
    setup.gwdrs_dir = str(tmp_path)
    setup.mpistacksettings()
    assert 'MPI_SHEPHERD' not in setup.mpi_environment
    assert setup.several_tries == '/u/scicon/tools/bin/several_tries'
    assert setup.restart_by_oserver == 'YES'


def test_plotocn_and_confocn_render_csh_names(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)

    # Same placeholder set as the installed coupled_diagnostics/g5lib pair.
    (exp_dir / 'plotocn.j').write_text(
        '#!/bin/csh\n'
        '#SBATCH --job-name={{ PLOT_T }}\n'
        'set SCRDIR={{ GEOSDIR }}/coupled_diagnostics\n'
        'set EXPID={{ EXPID }}\n'
        'set HOMDIR={{ HOMDIR }}\n'
        'set EXPDIR={{ EXPDIR }}\n'
    )
    (exp_dir / '__init__.py').write_text(
        '{{ EXPDSC }}\n'
        "expid='{{ EXPID }}'\n"
        "data_path='{{ EXPDIR }}'\n"
        "basin_mask='{{ COUPLEDIR }}/basin_mask/"
        '{{ OGCM_IM }}x{{ OGCM_JM }}/basin_mask.nc\'\n'
        "grid_spec='{{ COUPLEDIR }}/a{{ AGCM_IM }}x{{ AGCM_JM }}"
        '_o{{ OGCM_IM }}x{{ OGCM_JM }}/INPUT/grid_spec.nc\'\n'
    )
    setup.templates = ['plotocn.j', '__init__.py']

    setup.template()

    plot = (exp_dir / 'plotocn.j').read_text()
    assert '#SBATCH --job-name=12:00:00' in plot
    assert f"set SCRDIR={sm.pathdict['install']}/coupled_diagnostics" in plot
    assert 'set EXPID=test_exp' in plot
    assert f'set HOMDIR={exp_dir}' in plot
    assert f'set EXPDIR={exp_dir}' in plot
    assert '{{' not in plot

    init = (exp_dir / '__init__.py').read_text()
    assert 'csh alias test' in init
    assert "expid='test_exp'" in init
    assert f"data_path='{exp_dir}'" in init
    assert "basin_mask='/coupled/bcs/basin_mask/0072x0036/basin_mask.nc'" not in init
    assert "basin_mask='/coupled/bcs/basin_mask/72x36/basin_mask.nc'" in init
    assert "grid_spec='/coupled/bcs/a12x72_o72x36/INPUT/grid_spec.nc'" in init
    assert '{{' not in init


def test_csh_aliases_do_not_leak_into_the_run_yaml(tmp_path, monkeypatch):
    # The run yaml is the gcmpy-native receipt; the UPPER_CASE aliases are a
    # render-time concern only.
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)

    (exp_dir / 'plotocn.j').write_text('set EXPID={{ EXPID }}\n')
    setup.templates = ['plotocn.j']
    setup.template()

    receipt = (exp_dir / 'run_test_exp.yaml').read_text()
    assert 'stage_restarts_in_scratch: copy\n' in receipt
    assert 'EXPID:' not in receipt
    assert 'OGCM_IM:' not in receipt
    assert 'expid: test_exp' in receipt or 'experiment_id: test_exp' in receipt
