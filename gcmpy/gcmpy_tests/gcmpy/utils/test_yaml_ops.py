import pytest
from datetime import datetime, timedelta
from pathlib import Path
import os

from gcmpy.utils.yaml_ops import *

yaml_file_text = """
{% set NX = 20 %}
nx: {{ NX }}
ny: {{ NX * 6 * 2 }}
run_id: "{{ '%08d' | format(id_number) }}"
exp_id: "{{ '%08d' | format(15) }}"
experiment_name: "test_run"
"""

yaml_file_iso_text = """
fivemins: &id05mins !duration PT5M
my_timer1: *id05mins
my_timer2: !duration P5DT12H30M
timestamp: 2026-03-26T14:30:00
"""

def test_read_iso_yaml_file() -> bool:
    yaml_file = "myfile.yaml" 
    Path(yaml_file).write_text(yaml_file_iso_text)
    yaml_dict = read_iso_yaml_file(yaml_file)
    os.remove(yaml_file) 
    assert yaml_dict['my_timer1'] == timedelta(minutes=5)
    assert yaml_dict['my_timer2'] == timedelta(days=5, hours=12, minutes=30)


@pytest.mark.parametrize("id_number, result", 
                         [(1,   f"{1:08d}"), 
                          (15,  f"{15:08d}"), 
                          (175, f"{175:08d}") 
                          ])
def test_read_templated_yaml(id_number: str, result: str):
    yaml_file = "myfile.yaml" 
    Path(yaml_file).write_text(yaml_file_text)
    var_val = read_templated_yaml(yaml_file, dict(id_number=id_number))
    os.remove(yaml_file) 
    assert var_val["ny"] == var_val["nx"] * 6 * 2
    assert var_val["run_id"] == result
    assert var_val["exp_id"] == "00000015"

