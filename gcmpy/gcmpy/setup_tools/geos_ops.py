import os
import shutil

from gcmpy.setup_tools.setup_envs import pathdict
from gcmpy.utils.color_ops import Color

def copy_src_tarfile(exp_dir):
    bool_install_tarfile = '@CFG_INSTALL_SOURCE_TARFILE@'
    tarfile_name = '@CMAKE_PROJECT_NAME@.tar.gz'

    if bool_install_tarfile != 'TRUE':
        return

    src_dir = f"{exp_dir}/src"
    tarfile_path = f"{pathdict['install']}/src/{tarfile_name}"

    # remove and recreate src directory
    if os.path.exists(src_dir):
        shutil.rmtree(src_dir)
    os.makedirs(src_dir, exist_ok=True)
    print(f"Copying build source code into {Color.GREEN}{src_dir}{Color.RESET}")

    if os.path.exists(tarfile_path):
        shutil.copy(tarfile_path, src_dir)
    else:
        print(f"{tarfile_path} not found, yet CMake was asked to make and install a tarfile")
        print("Something went wrong.")

