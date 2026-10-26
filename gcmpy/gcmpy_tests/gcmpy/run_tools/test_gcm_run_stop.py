"""Exercise the real segment/execute control flow without launching a model."""

from importlib import import_module
from unittest.mock import Mock

import pytest

import gcm_run
from gcmpy.run_tools.mpi_runner import EgressMissingError, GEOSRunError


@pytest.fixture
def segment_run(tmp_path, monkeypatch):
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    (tmp_path / "CAP.rc").write_text("END_DATE: 20000103 000000\n")
    (scratch / "cap_restart").write_text("20000102 000000\n")

    # Stub external work, keeping execute/run_segments/run_one_segment real.
    mocks = {}
    for module, names in {
        "segment_time": ["compute_segment_finish_time"],
        "heartbeat_sync": ["sync_heartbeat_dt", "patch_yaml_levels"],
        "extdata": ["stage_extdata"],
        "linkbcs_ops": ["apply_linkbcs"],
        "pre_run": [
            "configure_gwd_restarts", "ensure_tile_bin",
            "validate_openwater_tiles", "check_merra2ox_dates", "configure_replay",
        ],
        "mpi_env": ["apply_setenv_block"],
        "env_builder": ["setup_omp_env", "teardown_omp_env"],
        "mpi_runner": ["run_gcm"],
        "stage_restarts": ["read_edate"],
        "archiver": ["archive_checkpoints"],
        "post_run": [
            "move_history_to_holding", "move_ocean_outputs",
            "run_post_processing", "run_forecast_setup", "copy_restarts_to_exp_dir",
        ],
    }.items():
        for name in names:
            mocks[name] = Mock()
            monkeypatch.setattr(import_module(f"gcmpy.run_tools.{module}"), name, mocks[name])
    mocks["compute_segment_finish_time"].return_value = {
        "current_date": "20000101", "current_time": "000000",
    }
    mocks["read_edate"].return_value = "e20000102_00z"
    monkeypatch.setattr(gcm_run.subprocess, "run", Mock())
    mocks["submit_job"] = Mock()
    monkeypatch.setattr(gcm_run, "submit_job", mocks["submit_job"])

    context = {
        "exp_dir": tmp_path, "scratch_dir": scratch,
        "geosbin": tmp_path, "geosexe": tmp_path / "GEOSgcm.x",
        "gcmemip": False, "rstdate": "", "wavewatch": False,
        "coupled": False, "mom6": False, "cice6": False,
        "rst_by_face": False, "singularity_run": None,
        "expid": "test", "gcmver": "test", "bctag": "test", "bcrslv": "test",
        "collections": [],
        "segment_control": {
            "NUM_SGMT": "1", "END_DATE": "20000103 000000",
            "USE_SHMEM": "0", "FCST_SEGMENT": "00000000",
        },
        "resources": {
            "AGCM_LM": 91, "TOTAL_PES": 6, "USE_IOSERVER": 0,
            "NX": 1, "NY": 6, "NUM_OSERVER_NODES": 0, "NUM_BACKEND_PES": 0,
        },
    }
    monkeypatch.setattr(gcm_run, "run_job_setup", Mock(return_value=context))
    config = {"ocean": {"res": "o1"}, "gocart": {"emissions": "none"},
              "atmos": {"RRTMGP_RADIATION": "#DELETE"}, "run_command": "mpiexec -n"}
    return config, mocks


def test_stop_after_successful_run(segment_run, capsys):
    config, mocks = segment_run
    config["stop_after_run"] = True

    with pytest.raises(SystemExit) as exc:
        gcm_run.execute("run_test.yaml", config)

    assert exc.value.code == 0
    mocks["run_gcm"].assert_called_once()
    mocks["teardown_omp_env"].assert_called_once()
    for name in (
        "archive_checkpoints", "move_history_to_holding", "move_ocean_outputs",
        "run_post_processing", "run_forecast_setup", "copy_restarts_to_exp_dir",
        "submit_job",
    ):
        mocks[name].assert_not_called()
    assert "stop_after_run enabled: leaving model outputs in" in capsys.readouterr().out


@pytest.mark.parametrize("setting", [None, False])
def test_normal_workflow_without_stop_flag(segment_run, setting):
    config, mocks = segment_run
    if setting is not None:
        config["stop_after_run"] = setting

    gcm_run.execute("run_test.yaml", config)

    for name in (
        "run_gcm", "teardown_omp_env", "archive_checkpoints",
        "move_history_to_holding", "move_ocean_outputs", "run_post_processing",
        "run_forecast_setup", "copy_restarts_to_exp_dir", "submit_job",
    ):
        mocks[name].assert_called_once()


@pytest.mark.parametrize("error", [GEOSRunError, EgressMissingError])
def test_stop_flag_does_not_mask_model_failure(segment_run, error):
    config, mocks = segment_run
    config["stop_after_run"] = True
    mocks["run_gcm"].side_effect = error("model failed")

    with pytest.raises(error, match="model failed"):
        gcm_run.execute("run_test.yaml", config)

    mocks["archive_checkpoints"].assert_not_called()
    mocks["copy_restarts_to_exp_dir"].assert_not_called()
    mocks["submit_job"].assert_not_called()
