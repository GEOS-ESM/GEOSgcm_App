import pytest

from gcmpy.utils.run_cmd_ops import *

data = """
John,Doe,Engineering,555-1234
Jane,Smith,Marketing,555-5678
Bob,Johnson,Sales,555-9012
InvalidLineWithoutDelimiter
"""
data_cut = """
John,Doe,Engineering,555-1234
"""
cut_settings = lambda NR, NF, F: f"{F[1]} {F[3]}"

data_awk = """
apple red fruit
"""

@pytest.mark.parametrize("text, delimeter, fields, result", 
                         [(data_cut, ",", [1], "\nJohn")]
                         )
def test_cut_cmd(text, delimeter, fields, result):
    cut_res = cut_cmd(text, delimeter, fields)
    assert cut_res == result

@pytest.mark.parametrize("text, action, delimeter, result", 
                         [(data_awk, cut_settings, None, " \napple fruit")]
                         )
def test_awk_cmd(text, action, delimeter, result):
    awk_res = awk_cmd(text, action, delimeter )
    assert awk_res == result


