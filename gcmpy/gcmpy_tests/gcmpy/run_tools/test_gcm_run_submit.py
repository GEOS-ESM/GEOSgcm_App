"""
submit_job() / build_sbatch_args() coverage.

The in-job gcm_run.sh sources ${GEOSBIN}/g5_modules.sh, but inside a
SLURM job BASH_SOURCE[0] is the spool copy (slurm_script), so GEOSBIN
cannot be rediscovered in-job -- it must be carried by the submission
itself.  Regressions guarded here:

* submit_job must pass --export=ALL,GEOSBIN=<bin_path> to sbatch (without
  it every batch job died at gcm_run.sh:56 with
  "/var/spool/slurmd/job<id>/g5_modules.sh: No such file or directory").
* the resource flags in run_<expid>.yaml must translate to sbatch flags.
"""

import subprocess
from types import SimpleNamespace
from pathlib import Path

import pytest

import gcm_run
from gcmpy.batch_tools.computing_sites import site_nodes_dict

BIN_PATH = "/fake/install/bin"


def _run_config(**overrides):
    """Minimal run_<expid>.yaml dict mirroring a real NCCS submission."""
    cfg = {
        "bin_path": BIN_PATH,
        "batch_time": "SBATCH --time=",
        "run_t": "4:00:00",
        "run_p": "SBATCH --nodes=2 --ntasks-per-node=120",
        "exp_dir": ".",
        "num_CPUs": 120,
        "ocean": {"coupled": "#DELETE"},
        "expConfig": {"io_server": 0},
        "batch_jobname": "SBATCH --job-name=",
        "run_n": "exp_RUN",
        "run_q": "SBATCH --constraint=mil",
        "batch_group_full": "SBATCH --account=s1873",
        "batch_outputname": "SBATCH --output=",
    }
    cfg.update(overrides)
    return cfg


@pytest.fixture
def captured_sbatch(monkeypatch, tmp_path):
    """Capture the sbatch command instead of running it."""
    captured = {}

    def fake_run(cmd, check=True):
        captured["cmd"] = cmd
        return subprocess.CompletedProcess(cmd, 0)

    monkeypatch.setattr(gcm_run.subprocess, "run", fake_run)
    monkeypatch.chdir(tmp_path)
    (tmp_path / 'AGCM.rc').write_text('NX: 10\nNY: 24\n')
    return captured


def test_submit_job_exports_geosbin_to_job(captured_sbatch):
    gcm_run.submit_job(Path("run_exp.yaml"), _run_config())

    cmd = captured_sbatch["cmd"]
    export_flag = f"--export=ALL,GEOSBIN={BIN_PATH}"
    assert cmd[0] == "sbatch"
    assert export_flag in cmd
    # Export flag must precede the positional script/config arguments.
    assert cmd.index(export_flag) < cmd.index(f"{BIN_PATH}/gcm_run.sh")
    assert cmd[-1] == "run_exp.yaml"


def test_submit_job_requires_bin_path():
    with pytest.raises(KeyError, match="bin_path"):
        gcm_run.submit_job(Path("run_exp.yaml"), {})


@pytest.mark.parametrize('scheduler_id', ['SLURM_JOB_ID', 'PBS_JOBID'])
@pytest.mark.parametrize('previous_install', [None, '/another/install/bin'])
def test_direct_job_initializes_runtime(monkeypatch, tmp_path, scheduler_id, previous_install):
    bin_dir = tmp_path / 'install' / 'bin'
    bin_dir.mkdir(parents=True)
    wrapper = bin_dir / 'gcm_run.sh'
    wrapper.write_text('# fake wrapper\n')
    cfg = _run_config(bin_path=str(bin_dir))
    monkeypatch.delenv('SLURM_JOB_ID', raising=False)
    monkeypatch.delenv('PBS_JOBID', raising=False)
    monkeypatch.delenv('GEOS_RUN_ENV_READY', raising=False)
    if previous_install:
        monkeypatch.setenv('GEOS_RUN_ENV_READY', previous_install)
    monkeypatch.setenv(scheduler_id, '123')
    monkeypatch.setenv('GEOSBIN', '/stale/install/bin')
    monkeypatch.setattr(gcm_run, 'capture_arguments', lambda: SimpleNamespace(config='run_exp.yaml'))
    monkeypatch.setattr(gcm_run, 'import_yaml', lambda path: cfg)
    monkeypatch.setattr(gcm_run, 'execute', lambda *args: pytest.fail('Must initialize before executing'))

    class ExecCalled(Exception):
        pass

    def fake_execve(executable, argv, env):
        assert executable == '/bin/bash'
        assert argv == ['bash', str(wrapper), str(Path('run_exp.yaml').resolve())]
        assert env['GEOSBIN'] == str(bin_dir)
        assert env[scheduler_id] == '123'
        raise ExecCalled

    monkeypatch.setattr(gcm_run.os, 'execve', fake_execve)
    with pytest.raises(ExecCalled):
        gcm_run.main()


def test_initialized_runtime_does_not_reexec(monkeypatch):
    monkeypatch.setenv('GEOS_RUN_ENV_READY', BIN_PATH)
    monkeypatch.setattr(gcm_run.os, 'execve', lambda *args: pytest.fail('Already initialized'))
    gcm_run.ensure_runtime_environment(Path('run_exp.yaml'), _run_config())


def test_runtime_requires_bin_path():
    with pytest.raises(KeyError, match='bin_path'):
        gcm_run.ensure_runtime_environment(Path('run_exp.yaml'), {})


def test_runtime_requires_wrapper(monkeypatch, tmp_path):
    monkeypatch.delenv('GEOS_RUN_ENV_READY', raising=False)
    with pytest.raises(FileNotFoundError, match='Cannot initialize runtime'):
        gcm_run.ensure_runtime_environment(
            Path('run_exp.yaml'), _run_config(bin_path=str(tmp_path)),
        )


def test_submit_job_builds_resource_flags(captured_sbatch):
    gcm_run.submit_job(Path("run_exp.yaml"), _run_config())

    cmd = captured_sbatch["cmd"]
    for expected in (
        "--time=4:00:00",
        "--nodes=2",
        "--ntasks-per-node=120",
        "--job-name=exp_RUN",
        "--constraint=mil",
        "--account=s1873",
        "--output=gcm_run.o%j",
        f"{BIN_PATH}/gcm_run.sh",
    ):
        assert expected in cmd, f"missing {expected!r} in {cmd}"


def _pbs_config(**overrides):
    return _run_config(
        batch_cmd='qsub', batch_time='PBS -l walltime=',
        batch_jobname='PBS -N', run_q='PBS -q normal',
        batch_group_full='PBS -W group_list=g0620',
        batch_joinouterr='PBS -j oe -k oed',
        run_p='PBS -l select=1:ncpus=120:mpiprocs=120:model=mil',
        **overrides,
    )


@pytest.mark.parametrize('scheduler', ['sbatch', 'qsub'])
@pytest.mark.parametrize('nx,ny,expected', [(4, 24, 1), (10, 60, 5), (11, 60, 6), (1, 6, 1)])
@pytest.mark.parametrize('io_nodes', [0, 2])
def test_submit_resizes_from_current_agcm(captured_sbatch, monkeypatch, scheduler, nx, ny, expected, io_nodes):
    cfg = _pbs_config() if scheduler == 'qsub' else _run_config()
    cfg['atmos'] = {'nx': 4, 'ny': 24}  # Deliberately stale setup metadata.
    cfg['expConfig']['io_server'] = int(io_nodes > 0)
    # Simulate resubmitting from a smaller allocation: it must not affect sizing.
    monkeypatch.setenv('SLURM_NTASKS', '120')
    monkeypatch.setenv('SLURM_NTASKS_PER_NODE', '120')
    Path('AGCM.rc').write_text(
        f'NX: {nx}\nNY: {ny}\nIOSERVER_NODES: {io_nodes}\nNUM_BACKEND_PES: 1\n'
    )
    gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    cmd = captured_sbatch['cmd']
    if scheduler == 'sbatch':
        assert f'--nodes={expected + io_nodes}' in cmd
        assert '--ntasks-per-node=120' in cmd
        assert sum(arg.startswith('--nodes=') for arg in cmd) == 1
    else:
        assert f'select={expected + io_nodes}:ncpus=120:mpiprocs=120:model=mil' in cmd
        assert cmd[0] == 'qsub'
        assert ['-q', 'normal'] == cmd[cmd.index('-q'):cmd.index('-q') + 2]
        assert 'group_list=g0620' in cmd
        assert 'walltime=4:00:00' in cmd
        assert 'oe' in cmd and 'oed' in cmd
        assert '-V' in cmd
        assert f'GEOSBIN={BIN_PATH},GEOS_RUN_CONFIG={Path("run_exp.yaml").resolve()}' in cmd
        assert cmd[-1] == f'{BIN_PATH}/gcm_run.sh'


@pytest.mark.parametrize('scheduler', ['sbatch', 'qsub'])
def test_submission_uses_coupled_layout(captured_sbatch, scheduler):
    cfg = _pbs_config() if scheduler == 'qsub' else _run_config()
    cfg['ocean']['coupled'] = ''
    Path('AGCM.rc').write_text('NX: 4\nNY: 24\nOGCM.NX: 10\nOGCM.NY: 60\nOGCM.LM: 75\n')
    gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    assert any(arg.startswith('select=5:') or arg == '--nodes=5' for arg in captured_sbatch['cmd'])


def test_slurm_preserves_other_flags_and_updates_explicit_task_count(captured_sbatch):
    Path('AGCM.rc').write_text('NX: 10\nNY: 60\n')
    cfg = _run_config(run_p='SBATCH -N 1 -n 96 --ntasks-per-node 40 --exclusive')
    gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    cmd = captured_sbatch['cmd']
    assert '--nodes=15' in cmd
    assert '--ntasks=600' in cmd
    assert '--ntasks-per-node=40' in cmd
    assert '--exclusive' in cmd
    assert '-N' not in cmd and '-n' not in cmd


@pytest.mark.parametrize('layout', ['NX: 0\nNY: 6\n', 'NX: -1\nNY: 6\n', 'NX: bad\nNY: 6\n'])
@pytest.mark.parametrize('scheduler', ['sbatch', 'qsub'])
def test_invalid_layout_does_not_submit(captured_sbatch, layout, scheduler):
    Path('AGCM.rc').write_text(layout)
    cfg = _pbs_config() if scheduler == 'qsub' else _run_config()
    with pytest.raises(ValueError):
        gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    assert captured_sbatch == {}


@pytest.mark.parametrize('scheduler', ['sbatch', 'qsub'])
def test_missing_agcm_does_not_submit(captured_sbatch, scheduler):
    Path('AGCM.rc').unlink()
    with pytest.raises(FileNotFoundError):
        gcm_run.submit_job(Path('run_exp.yaml'), _pbs_config() if scheduler == 'qsub' else _run_config())
    assert captured_sbatch == {}


def test_pbs_job_enters_execute_instead_of_resubmitting(monkeypatch):
    cfg = _pbs_config()
    monkeypatch.setenv('GEOS_RUN_ENV_READY', BIN_PATH)
    monkeypatch.delenv('SLURM_JOB_ID', raising=False)
    monkeypatch.setenv('PBS_JOBID', '123.pbs')
    monkeypatch.setattr(gcm_run, 'capture_arguments', lambda: SimpleNamespace(config='run_exp.yaml'))
    monkeypatch.setattr(gcm_run, 'import_yaml', lambda path: cfg)
    monkeypatch.setattr(gcm_run, 'load_env_vars', lambda: {})
    calls = []
    monkeypatch.setattr(gcm_run, 'execute', lambda *args: calls.append(args))
    monkeypatch.setattr(gcm_run, 'submit_job', lambda *args: pytest.fail('Must execute inside PBS'))
    gcm_run.main()
    assert calls == [(Path('run_exp.yaml'), cfg)]


@pytest.mark.parametrize('scheduler', ['sbatch', 'qsub'])
def test_resubmission_rereads_changed_layout(captured_sbatch, monkeypatch, scheduler):
    cfg = _pbs_config() if scheduler == 'qsub' else _run_config()
    Path('AGCM.rc').write_text('NX: 4\nNY: 24\n')
    gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    first = captured_sbatch['cmd']
    assert '--nodes=1' in first or any(arg.startswith('select=1:') for arg in first)
    Path('AGCM.rc').write_text('NX: 10\nNY: 60\n')
    monkeypatch.setattr(gcm_run, 'run_segments', lambda *args: True)
    gcm_run.execute(Path('run_exp.yaml'), cfg)
    second = captured_sbatch['cmd']
    assert '--nodes=5' in second or any(arg.startswith('select=5:') for arg in second)
    assert cfg['run_p'] == (_pbs_config() if scheduler == 'qsub' else _run_config())['run_p']


def test_pbs_preserves_processor_and_other_select_qualifiers(captured_sbatch):
    cfg = _pbs_config()
    cfg['run_p'] = 'PBS -l select=1:ncpus=80:mpiprocs=40:model=cas:mem=100gb'
    Path('AGCM.rc').write_text('NX: 10\nNY: 60\n')
    gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    assert 'select=15:ncpus=80:mpiprocs=40:model=cas:mem=100gb' in captured_sbatch['cmd']


@pytest.mark.parametrize('model', site_nodes_dict['NAS'])
@pytest.mark.parametrize('io_nodes', [0, 2])
def test_nas_node_types_size_600_rank_layout(captured_sbatch, model, io_nodes):
    # Explicit expectations catch capacity changes and newly added, untested types.
    expected = {
        'bro': (24, 25), 'sky': (40, 15), 'cas': (40, 15),
        'rom': (120, 5), 'mil': (120, 5), 'tur': (240, 3),
    }
    per_node, model_nodes = expected[model]
    assert site_nodes_dict['NAS'][model] == per_node
    cfg = _pbs_config()
    cfg['num_CPUs'] = per_node
    cfg['expConfig'].update(processor=model, io_server=int(io_nodes > 0))
    cfg['atmos'] = {'nx': 4, 'ny': 24}
    cfg['run_p'] = f'PBS -l select=1:ncpus={per_node}:mpiprocs={per_node}:model={model}'
    Path('AGCM.rc').write_text(
        f'NX: 10\nNY: 60\nIOSERVER_NODES: {io_nodes}\nNUM_BACKEND_PES: 1\n'
    )

    gcm_run.submit_job(Path('run_exp.yaml'), cfg)

    assert (
        f'select={model_nodes + io_nodes}:ncpus={per_node}:mpiprocs={per_node}:model={model}'
        in captured_sbatch['cmd']
    )


def test_pbs_rejects_heterogeneous_select(captured_sbatch):
    cfg = _pbs_config()
    cfg['run_p'] = 'PBS -l select=1:ncpus=40+1:ncpus=120'
    with pytest.raises(ValueError, match='homogeneous'):
        gcm_run.submit_job(Path('run_exp.yaml'), cfg)
    assert captured_sbatch == {}
