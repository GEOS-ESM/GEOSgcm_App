import pytest

from gcmpy.utils.path_ops import *

cwd = Path.cwd()
home = Path.home().resolve()


"""def test_check_file_exists(config_file):
    with open(config_file, 'r') as _:
        assert check_file_exists(config_file) is True"""

def test_get_current_dir():
    assert isinstance(get_current_dir(), Path)

def test_get_home_dir():
    assert isinstance(get_home_dir(), Path)

@pytest.mark.parametrize("path, name",
                         [('/Users/Guest/script.py', 'script.py'),
                          ('/Users/Guest/Downloads/', 'Downloads'),
                          ('/Users/Guest/Documents/.hidden', '.hidden')])
def test_get_name(path: str, name: str):
    assert get_name(path) == name

@pytest.mark.parametrize("path, stem",
                         [('/Users/Guest/script.py', 'script'),
                          ('/Users/Guest/Downloads/', 'Downloads'),
                          ('/Users/Guest/Documents/.hidden', '.hidden')])
def test_get_stem(path: str, stem: str):
    assert get_stem(path) == stem

@pytest.mark.parametrize("path, suffix",
                         [('/Users/Guest/script.py', '.py'),
                          ('/Users/Guest/Downloads/', ''),
                          ('/Users/Guest/Documents/.hidden', '')])
def test_get_suffix(path: str, suffix: str):
    assert get_suffix(path) == suffix

@pytest.mark.parametrize("path, anchor",
                         [('/Users/Guest/script.py', '/'),
                          ('/Users/Guest/Downloads/', '/'),
                          ('/Users/Guest/Documents/.hidden', '/')])
def test_get_anchor(path: str, anchor: str):
    assert get_anchor(path) == anchor

@pytest.mark.parametrize("path, parent",
                         [('/Users/Guest/script.py',
                           Path('/Users/Guest/')),
                          ('/Users/Guest/Downloads/',
                           Path('/Users/Guest/')),
                          ('/Users/Guest/Documents/.hidden',
                           Path('/Users/Guest/Documents/'))])
def test_get_parent(path: str, parent: Path):
    assert get_parent(path) == parent

@pytest.mark.parametrize("path, address",
                         [(Path('/Users/Guest/script.py'),
                           '/Users/Guest/script.py'),
                          (Path('/Users/Guest/Downloads/'),
                           '/Users/Guest/Downloads'),
                          (Path('/Users/Guest/Documents/.hidden'),
                           '/Users/Guest/Documents/.hidden')])
def test_get_path_address(path: Path, address: str):
    assert get_path_address(path) == address

@pytest.mark.parametrize("path, current_dir",
                         [(str(Path.home()), home),
                          (str(cwd), cwd)])
def test_change_dir(path, current_dir):
    change_dir(path)
    assert Path.cwd() == current_dir

def test_create_dir_none():
    with pytest.raises(TypeError, match='Cannot create directory. Please specify a directory path.'):
        create_dir(None)


def test_cat_files():         
    test_file1 = "file1.txt"   
    test_file2 = "file2.txt"   
    text1 = "Hello from file 1!\n"
    text2 = "And hello from file 2!\n"
    Path(test_file1).write_text(text1)
    Path(test_file2).write_text(text2)

    file_paths = test_file1            
    result1 = text1
    new_text1 = cat_files(file_paths)        
                              
    file_paths = [test_file1, test_file2]
    result2 = f"{text1}{text2}"
    new_text2 = cat_files(file_paths)        

    os.remove(test_file1)     
    os.remove(test_file2)     
                              
    assert new_text1 == result1
    assert new_text2 == result2
