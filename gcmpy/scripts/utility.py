import os, sys, platform

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



#######################################################################
#      Directory and Environment Variable Locations Dictionaries
#######################################################################
# set up envirnoment dictionary for later
envdict = {}
pathdict = {}  # Start empty - cannot reference self before initialization
pathdict['cwd']         = os.getcwd()
pathdict['bin']         = os.path.dirname(pathdict['cwd'])
pathdict['install']     = os.path.dirname(pathdict['bin'])
pathdict['etc']         = os.path.join(pathdict['install'], 'etc')
pathdict['GEOSgcm']     = os.path.dirname(pathdict['install'])
pathdict['build']       = os.path.join(pathdict['GEOSgcm'], 'build')
#pathdict['SCRIPT']      = os.path.realpath(__file__)
#pathdict['SCRIPTNAME']  = os.path.split(pathdict['SCRIPT'])[-1]
#pathdict['PY_METHOD']   = os.path.join(pathdict['BIN'], 'py_method')
