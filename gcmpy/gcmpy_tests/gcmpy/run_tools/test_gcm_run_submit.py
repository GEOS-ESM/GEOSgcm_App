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
from pathlib import Path

import pytest

import gcm_run

BIN_PATH = "/fake/install/bin"


def _run_config(**overrides):
    """Minimal run_<expid>.yaml dict mirroring a real NCCS submission."""
    cfg = {
        "bin_path": BIN_PATH,
        "batch_time": "SBATCH --time=",
        "run_t": "4:00:00",
        "run_p": "SBATCH --nodes=2 --ntasks-per-node=120",
        "batch_jobname": "SBATCH --job-name=",
        "run_n": "exp_RUN",
        "run_q": "SBATCH --constraint=mil",
        "batch_group_full": "SBATCH --account=s1873",
        "batch_outputname": "SBATCH --output=",
    }
    cfg.update(overrides)
    return cfg


@pytest.fixture
def captured_sbatch(monkeypatch):
    """Capture the sbatch command instead of running it."""
    captured = {}

    def fake_run(cmd, check=True):
        captured["cmd"] = cmd
        return subprocess.CompletedProcess(cmd, 0)

    monkeypatch.setattr(gcm_run.subprocess, "run", fake_run)
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
