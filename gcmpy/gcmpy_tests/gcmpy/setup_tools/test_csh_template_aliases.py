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

import gcm_run
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
    monkeypatch.setitem(sm.pathdict, 'config', Path(sm.__file__).parent / 'config')

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
    assert 'cpus_per_node' in config
    assert config['cpus_per_node'] is None


def test_run_yaml_fields_follow_catalog_without_restricting_templates(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    config_dir = tmp_path / 'config'
    config_dir.mkdir()
    (config_dir / 'run_yaml_fields.yaml').write_text(
        'top_level: [exp_dir, plot_t, absent_optional]\n'
        'nested:\n  expConfig: [experiment_id, experiment_description]\n'
    )
    monkeypatch.setitem(sm.pathdict, 'config', config_dir)
    (exp_dir / 'context.tmpl').write_text('{{ ocean.history_template }}')
    setup.templates = ['context.tmpl']
    setup.template()

    config = yaml.safe_load((exp_dir / 'run_test_exp.yaml').read_text())
    assert config == {
        'expConfig': {'experiment_id': 'test_exp', 'experiment_description': 'csh alias test'},
        'exp_dir': str(exp_dir), 'plot_t': '12:00:00',
    }
    assert (exp_dir / 'context.tmpl').read_text() == 'HISTORY.AOGCM.rc.tmpl'


@pytest.mark.parametrize('catalog', [
    '',
    'top_level: exp_dir\nnested: {expConfig: [experiment_id]}\n',
    'top_level: [exp_dir, 4]\nnested: {expConfig: [experiment_id]}\n',
    'top_level: [exp_dir]\nnested: [expConfig]\n',
    'top_level: [exp_dir]\nnested: {expConfig: experiment_id}\n',
    'top_level: [exp_dir]\nnested: {expConfig: [null]}\n',
    'top_level: [expConfig]\nnested: {expConfig: [experiment_id]}\n',
])
def test_invalid_run_yaml_catalog_does_not_overwrite_config(tmp_path, monkeypatch, catalog):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    config_dir = tmp_path / 'config'
    config_dir.mkdir()
    (config_dir / 'run_yaml_fields.yaml').write_text(catalog)
    monkeypatch.setitem(sm.pathdict, 'config', config_dir)
    run_yaml = exp_dir / 'run_test_exp.yaml'
    run_yaml.write_text('existing: value\n')

    with pytest.raises(ValueError, match='Invalid run YAML field catalog'):
        setup.template()
    assert run_yaml.read_text() == 'existing: value\n'


def test_missing_run_yaml_catalog_does_not_overwrite_config(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    monkeypatch.setitem(sm.pathdict, 'config', tmp_path)
    run_yaml = exp_dir / 'run_test_exp.yaml'
    run_yaml.write_text('existing: value\n')

    with pytest.raises(FileNotFoundError, match='run_yaml_fields.yaml'):
        setup.template()
    assert run_yaml.read_text() == 'existing: value\n'


def test_unparseable_run_yaml_catalog_does_not_overwrite_config(tmp_path, monkeypatch):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    (tmp_path / 'run_yaml_fields.yaml').write_text('top_level: [broken\n')
    monkeypatch.setitem(sm.pathdict, 'config', tmp_path)
    run_yaml = exp_dir / 'run_test_exp.yaml'
    run_yaml.write_text('existing: value\n')

    with pytest.raises(yaml.YAMLError):
        setup.template()
    assert run_yaml.read_text() == 'existing: value\n'


@pytest.mark.parametrize('coupled', [False, True])
@pytest.mark.parametrize('scheduler', ['slurm', 'pbs'])
def test_focused_run_yaml_keeps_runtime_settings_and_full_template_context(
        tmp_path, monkeypatch, coupled, scheduler):
    exp_dir = tmp_path / 'exp'
    exp_dir.mkdir()
    setup = _coupled_setup(tmp_path, monkeypatch, exp_dir)
    setup.ocean['coupled'] = '' if coupled else '#DELETE'
    setup.ocean['preload'] = 'libtest.so'
    setup.atmos['mp_turn_off_wsub_extdata'] = 'YES'
    setup.GCMEMIP = True
    setup.RSTDATE = '20000101'
    setup.stop_after_run = True
    setup.skip_initial_restart_archive = True
    setup.grid_dir = '/grid'
    setup.abcs_dir = '/abcs'
    setup.num_CPUs = 40
    setup.run_t = '1:00:00'
    setup.run_n = 'test_RUN'
    if scheduler == 'slurm':
        setup.batch_time = 'SBATCH --time='
        setup.batch_jobname = 'SBATCH --job-name='
        setup.run_p = 'SBATCH --nodes=1 --ntasks-per-node=40'
    else:
        setup.batch_time = 'PBS -l walltime='
        setup.batch_jobname = 'PBS -N'
        setup.run_p = 'PBS -l select=1:ncpus=40:mpiprocs=40:model=cas'
        setup.batch_group = 'PBS -W group_list='
    (exp_dir / 'AGCM.rc').write_text('NX: 4\nNY: 24\nOGCM.NX: 4\nOGCM.NY: 24\nOGCM.LM: 50\n')
    (exp_dir / 'context.tmpl').write_text(
        '{{ expConfig.experiment_description }}|{{ atmos.im }}|'
        '{{ ocean.history_template }}|{{ plot_t }}\n'
    )
    setup.templates = ['context.tmpl']
    setup.template()

    config = yaml.safe_load((exp_dir / 'run_test_exp.yaml').read_text())
    assert config['expConfig'] == {'experiment_id': 'test_exp', 'io_server': 0}
    assert set(config['atmos']) == {
        'res', 'nx', 'ny', 'mp_turn_off_wsub_extdata', 'RRTMGP_RADIATION',
    }
    assert set(config['ocean']) == {
        'coupled', 'out', 'MOM6', 'CICE6', 'DATAATM', 'res', 'data', 'model', 'preload',
        'mit_remap_name',
    }
    assert set(config['land']) == {'bcs', 'GCMRUN_catchment'}
    assert set(config['gocart']) == {'emissions'}
    assert config['ocean']['preload'] == 'libtest.so'
    assert config['atmos']['mp_turn_off_wsub_extdata'] == 'YES'
    assert config['GCMEMIP'] is True and config['RSTDATE'] == '20000101'
    assert config['stop_after_run'] is True and config['skip_initial_restart_archive'] is True
    assert config['grid_dir'] == '/grid' and config['abcs_dir'] == '/abcs'
    assert 'templates' not in config and 'plot_t' not in config
    assert 'history_template' not in config['ocean']
    assert setup.ocean['history_template'] == 'HISTORY.AOGCM.rc.tmpl'
    assert (exp_dir / 'context.tmpl').read_text() == (
        'csh alias test|12|HISTORY.AOGCM.rc.tmpl|12:00:00'
    )
    if scheduler == 'slurm':
        assert '--nodes=3' in gcm_run.build_sbatch_args(config)
    else:
        assert 'select=3:ncpus=40:mpiprocs=40:model=cas' in gcm_run.build_qsub_args(config)


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
