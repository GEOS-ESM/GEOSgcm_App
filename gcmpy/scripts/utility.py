import os, platform, yaml, shutil

# pretty font
class color:
    PURPLE      = '\033[95m'
    CYAN        = '\033[96m'
    DARKCYAN    = '\033[36m'
    BLUE        = '\033[94m'
    GREEN       = '\033[92m'
    YELLOW      = '\033[93m'
    RED         = '\033[91m'
    BOLD        = '\033[1m'
    UNDERLINE   = '\033[4m'
    END         = '\033[0m'
    RESET       = '\033[0m'

    # accepts any string and styles it
    def color_path(path):
        return color.BLUE + color.BOLD + path + color.RESET

    def color_file(file):
        return color.GREEN + color.BOLD + file + color.RESET

# raises expections
class exceptions:
    # Displays usage information to user
    def print_usage():
        print(color.GREEN + pathdict['SCRIPTNAME'] + ", a setup script for the GEOS GCM\n\n" +  \
                "\tUsage: " + pathdict['SCRIPTNAME'] + " [optional flag]\n\n" +                 \
                "\t    --link         Link GEOSgcm.x into experiment directory\n" +             \
                "\t    --singularity  Set up Singularity experiment\n" +                        \
                "\t -h --help         Show usage" +                                             \
                "\n\nIf invoked alone, the script runs as normal." +                            \
                "\nFor more information, please contact Matt Thompson, Scott Rabenhorst, or Shayon Shakoorzadeh.\n")
        exit(1)

    # Display misconfiguration messages to the user
    def raise_user_exception(msg):
        print(msg)
        exit(1)

    def raise_fatal_exception(msg):
        exceptions.raiseuserexception(msg)
        exit(1)

    # This function will clean output files if script is interrupted
    def cleanup():
        pass

# Helper function for cp'ing 
def cpfile(src, destination, filename):
    if os.path.exists(src):
        shutil.copy(src, destination)
        print(f"Creating {color.RED}{filename}{color.RESET}")

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
    print(f"Copying build source code into {color.GREEN}{src_dir}{color.RESET}")

    if os.path.exists(tarfile_path):
        shutil.copy(tarfile_path, src_dir)
    else:
        print(f"{tarfile_path} not found, yet CMake was asked to make and install a tarfile")
        print("Something went wrong.")

# open yaml file and create dictionary from it's contents
def load_yamls():

    # list of question files (*MAKE SURE THEY ARE IN THE ORDER YOU WANT THEM TO BE ASKED*)
    file_list = ["../yaml/exp_setup.yaml",          \
                 "../yaml/atmospheric_model.yaml",  \
                 "../yaml/ocean_model.yaml",        \
                 "../yaml/land_model.yaml",         \
                 "../yaml/gocart.yaml",             \
                 "../yaml/directory_setup.yaml"]
    all_yaml_questions = {}

    for filename in file_list:
        try:
            with open(filename, "r") as file:
                yaml_questions = yaml.safe_load(file)
                all_yaml_questions.update(yaml_questions)
        except IOError:
            print(f"{color.RED}YAML file '{filename}' could not be located. Exiting.")
            exit(1)

    return all_yaml_questions


#######################################################################
#      Directory and Environment Variable Locations Dictionaries
#######################################################################
pathdict = {}
pathdict['scripts']     = os.getcwd()
pathdict['gcmpy']       = os.path.dirname(pathdict['scripts'])
pathdict['yaml']        = os.path.join(pathdict['gcmpy'], 'yaml')
pathdict['bin']         = os.path.dirname(pathdict['gcmpy'])
pathdict['install']     = os.path.dirname(pathdict['bin'])
pathdict['etc']         = os.path.join(pathdict['install'], 'etc')
pathdict['GEOSgcm']     = os.path.dirname(pathdict['install'])
pathdict['build']       = os.path.join(pathdict['GEOSgcm'], 'build')
pathdict['GEOSgcm_App'] = os.path.join(pathdict['GEOSgcm'], 'src/Applications/@GEOSgcm_App')
pathdict['GEOS_Util']   = os.path.join(pathdict['GEOSgcm'], 'src/Shared/GEOS_Util')


envdict = {}
envdict['node'] = platform.node()
envdict['arch'] = platform.system()
envdict['mpi'] = '@MPI_STACK@'
if envdict['arch'] == 'Darwin':
    envdict['preload_command'] = 'DYLD_INSERT_LIBRARIES'
    envdict['ld_library_path_command'] = 'DYLD_LIBRARY_PATH'
    # On macOS we seem to need to call mpirun directly and not use esma_mpirun
    # For some reason SIP does not let the libraries be preloaded
    envdict['run_command'] = 'mpirun -np '
else:
    envdict['preload_command'] = 'LD_PRELOAD'
    envdict['ld_library_path_command'] = 'LD_LIBRARY_PATH'
    envdict['run_command'] = '$GEOSBIN/esma_mpirun -np '

envdict['site'] = open(os.path.join(pathdict['etc'], 'SITE.rc'), 'r').read().split()[-1]

