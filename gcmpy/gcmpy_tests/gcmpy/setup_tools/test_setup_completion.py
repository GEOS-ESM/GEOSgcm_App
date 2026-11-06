"""The setup entry point reports the experiment location after success."""

import sys
from unittest.mock import Mock

import pytest

import gcm_setup


@pytest.mark.parametrize("use_yaml", [False, True])
@pytest.mark.parametrize("clone", [False, True])
def test_setup_reports_experiment_location(tmp_path, monkeypatch, capsys, use_yaml, clone):
    exp_dir = str(tmp_path / "experiment with spaces")
    config = {
        "exp_dir": exp_dir,
        "group_root": "test_group",
        "clone_experiment": "old_experiment" if clone else None,
    }
    argv = ["gcm_setup.py", "--yaml", "driver.yaml"] if use_yaml else ["gcm_setup.py"]
    monkeypatch.setattr(sys, "argv", argv)
    monkeypatch.setattr(gcm_setup, "set_env_dicts", Mock())
    monkeypatch.setattr(gcm_setup, "get_home_dir", lambda: str(tmp_path))
    monkeypatch.setattr(gcm_setup, "ask_questions", lambda: config)
    monkeypatch.setattr(gcm_setup, "read_yaml_file", lambda path: config)
    monkeypatch.setattr(gcm_setup, "yaml_receipt", Mock())
    monkeypatch.setattr(gcm_setup, "clone_exp", Mock())
    monkeypatch.setattr(gcm_setup, "copy_src_tarfile", Mock())
    setup = Mock(spec=gcm_setup.SetupGCM)
    setup.organize_exp_dir.side_effect = lambda: print("Finished organizing experiment")
    monkeypatch.setattr(gcm_setup, "SetupGCM", Mock(return_value=setup))

    # YAML-driven setup explicitly disables cloning, even if the driver has it set.
    if clone and not use_yaml:
        with pytest.raises(SystemExit):
            gcm_setup.main()
        gcm_setup.copy_src_tarfile.assert_called_once_with(exp_dir)
        gcm_setup.SetupGCM.assert_not_called()
    else:
        gcm_setup.main()
        setup.organize_exp_dir.assert_called_once_with()

    lines = capsys.readouterr().out.splitlines()
    assert lines[-1] == f"Experiment can be found in {exp_dir}"
    if not (clone and not use_yaml):
        assert lines[-2] == "Finished organizing experiment"
