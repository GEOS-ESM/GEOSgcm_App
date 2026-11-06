"""Readable YAML output must preserve the values consumed by the runner."""

import pytest
import yaml

from gcmpy.utils.yaml_ops import write_dict_into_yaml


@pytest.mark.parametrize("ending, indicator", [("", "|-"), ("\n", "|"), ("\n\n", "|+")])
def test_multiline_strings_use_literal_blocks(tmp_path, ending, indicator):
    mpi_config = (
        "# Uncomment if needed\n"
        "#setenv I_MPI_DEBUG 9\n"
        "\n"
        "setenv I_MPI_ADJUST_ALLREDUCE 12\n"
        "setenv I_MPI_FABRICS shm:ofi"
    ) + ending
    config = {
        "mpi_config": mpi_config,
        "nested": {"script": "first\n  indented\n"},
        "site": "NCCS",
        "enabled": False,
        "count": 16,
        "empty": "",
        "quoted_boolean": "false",
    }
    output = tmp_path / "run_test.yaml"
    write_dict_into_yaml(output, config)
    text = output.read_text()

    assert f"mpi_config: {indicator}\n" in text
    assert "  # Uncomment if needed\n  #setenv I_MPI_DEBUG 9\n\n" in text
    assert "  setenv I_MPI_ADJUST_ALLREDUCE 12\n  setenv I_MPI_FABRICS shm:ofi" in text
    assert "site: NCCS\n" in text
    assert list(yaml.safe_load(text)) == list(config)
    assert yaml.safe_load(text) == config


def test_multiline_formatting_does_not_change_default_dumper(tmp_path):
    config = {"mpi_config": "first\nsecond\n"}
    before = yaml.dump(config)
    write_dict_into_yaml(tmp_path / "run_test.yaml", config)
    assert yaml.dump(config) == before
