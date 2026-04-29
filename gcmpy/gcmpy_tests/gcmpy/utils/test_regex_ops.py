import pytest
from pathlib import Path
import os

from gcmpy.utils.regex_ops import *

sample_text = "Line 1: The quick brown fox\nLine 2: Jumps over the lazy dog\nLine 3: The FOX is fast"
# Setup dummy files
#Path("log1.txt").write_text("Error: Connection timeout\nInfo: Server started\n")
#Path("log2.txt").write_text("Info: User logged in\nError: Database crash\n")


@pytest.mark.parametrize("mystring, result", 
                         [pytest.param("759", True, id="only digit"), 
                          pytest.param("759 ", False, id="space"), 
                          pytest.param("759.0", False, id="decimal"), 
                          pytest.param("-759", False, id="negative sign"),  
                          pytest.param("850d76", False, id="include character")])
def test_is_string_digits(mystring: str, result: bool):
    isdigit = is_string_digits(mystring)
    assert isdigit == result

@pytest.mark.parametrize("mystring, result", 
                         [("759", True), 
                          ("759.0", False), 
                          ("-759", True),  
                          ("850b76", False)])
def test_is_string_integer(mystring: str, result: bool):
    isinteger = is_string_integer(mystring)
    assert isinteger == result

@pytest.mark.parametrize("mystring, result", 
                         [("759", True), 
                          ("759.0", True), 
                          ("-759", True),  
                          ("-7.59e-9", True),  
                          ("850b76d2", False)])
def test_is_string_float(mystring: str, result: bool):
    isfloat = is_string_float(mystring)
    assert isfloat == result

@pytest.mark.parametrize("mystring, result", 
                         [("759", False), 
                          ("BytrADLP", True), 
                          ("Testing does_not_contain_digit", True),  
                          ("850b76", False)])
def test_does_not_contain_digit(mystring: str, result: bool):
    nodigit = does_not_contain_digit(mystring)
    assert nodigit == result


@pytest.mark.parametrize("search_pattern, repl_pattern, mystring, result", 
                         [(r'(ENABLE_PCHEM:\s*\.).*(\.)', r'\1'+'FALSE'+r'\2', "ENABLE_PCHEM:       .TRUE.", "ENABLE_PCHEM:       .FALSE."),
                          (r'(ENABLE_GOCART_DATA:\s*\.).*(\.)', r'\1FALSE\2', "ENABLE_GOCART_DATA: .TRUE.", "ENABLE_GOCART_DATA: .FALSE.")])
def test_search_replace_in_string(search_pattern: str, repl_pattern: str, 
                                  mystring: str, result: bool):
    new_string = search_replace_in_string(search_pattern, repl_pattern, mystring)
    assert new_string == result

#@pytest.mark.parametrize("pattern, files, text, ignore_case, result",
#                         [
#                             (r"juMP", None, sample_text, True, "Line 2: Jumps over the lazy dog"), 
#                             #(r"^Error", ["log1.txt", "log2.txt"], None, False, "Error: Connection timeout\nError: Database crash")
#                             ])
#def test_grep_file(pattern: str, files, text: str, ignore_case: bool, result: str):
#    new_text = grep_file(pattern, files=files, text=text, ignore_case=True)
#    assert new_text == result

def test_grep_file():
    pattern = r"juMP"
    files = None
    text = sample_text
    ignore_case = True
    result1 = "Line 2: Jumps over the lazy dog"
    new_text1 = grep_file(pattern, files, text, ignore_case)

    pattern = r"Error"
    test_file1 = "log1.txt"
    test_file2 = "log2.txt"
    Path(test_file1).write_text("Error: Connection timeout\nInfo: Server started\n")
    Path(test_file2).write_text("Info: User logged in\nError: Database crash\n")
    files = [test_file1, test_file2]
    text = None
    ignore_case = False
    result2 = f"{test_file1}:Error: Connection timeout\n{test_file2}:Error: Database crash"
    new_text2 = grep_file(pattern, files, text, ignore_case)
    os.remove(test_file1)
    os.remove(test_file2)

    assert new_text1 == result1
    assert new_text2 == result2

def test_extract_line_starting_with():
    fname = "CAP.rc"
    result = "END_DATE:     29990302 210000"
    search_string = "END_DATE:"
    Path(fname).write_text(f" ROOT_NAME: GCM\nROOT_CF: AGCM.rc\n {result}   \nJOB_SGMT:     00000015 000000")
    text1 = extract_line_starting_with(fname, search_string)
    text2 = extract_line_starting_with(fname, "The END_DATE:")
    os.remove(fname)

    assert text1 == result
    assert text2 == None



