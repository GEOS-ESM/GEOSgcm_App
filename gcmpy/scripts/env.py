import os, sys, platform
import process_questions as pq
from utility import envdict, pathdict, color, exceptions
import questionary

# PROBABLY WANT TO REMOVE/MODIFY THIS!
'''
# Check if GEOSgcm.x is here, which means you are in the correct directory
if not os.path.exists(os.path.join(pathdict['BIN'], 'GEOSgcm.x')):
    exceptions.raise_user_exception(
        "You are trying to run " + color.color_file(pathdict['SCRIPTNAME']) +             \
        " in the " + color.color_path(pathdict['CWD']) + " directory." +                 \
        "\nThis is no longer supported.\nPlease run from the " + color.color_path('bin/') + \
        " directory in your installation.")
    exit(1)
'''


#######################################################################
#                   Check for Command Line Flags
#######################################################################
# Set default behavior of switches
linkx = False
exe_verb = 'copied'
bool_usingSingularity = False

# Loop through arguments and mtch to each case
# If argument is not recognized, display usage and exit
for arg in enumerate(sys.argv[1:]):
    if (arg[-1] == '--link'):
        linkx = True
    elif (arg[-1] == '--singularity'):
        bool_usingSingularity == True
    elif (arg[-1] == '--help' or arg[-1] == '-h'):
        exceptions.printusage()
    else:
        exceptions.raiseuserexception("Command line arguemnt \"" + arg[-1] + "\" not \
                                        recognized. \nSee usage:\n" )
        exceptions.printusage()


#######################################################################
#                        Determine site
#######################################################################
envdict['node'] = platform.node()
envdict['arch'] = platform.system()
#print(f"{color.RED}{pathdict['bin']}{color.RESET}")
envdict['site'] = open(os.path.join(pathdict['etc'], 'SITE.rc'), 'r').read().split()[-1]

#######################################################################
#                 Test for Compiler and MPI Setup
#######################################################################
# Extract BASEDIR tail
basedir = open(os.path.join(pathdict['etc'], 'BASEDIR.rc'), 'r').read().split()[-1]
mpi = os.path.split(basedir)[-1]

# Map MPI dirname to correct MPI implementation
if any(tag in mpi for tag in ['openmpi','hpcx']): mpi = 'openmpi'
elif any(tag in mpi for tag in ['impi', 'intelmpi']): mpi = 'intelmpi'
elif 'mvapich2' in mpi: mpi = 'mvapich2'
elif 'mpich' in mpi: mpi = 'mpich'
elif 'mpt' in mpi: mpi = 'mpt'
else: mpi = 'intelmpi'
envdict['mpi'] = mpi
#print("MPI implementation is: " + color.GREEN + MPI)


answerdict = pq.process()
#for i in answerdict:
#    print(answerdict[i].q_answer)


#######################################################################
#                    Set Number of CPUs per Node
#######################################################################
if envdict['site'] == 'NCCS':
    '''
    NCCS currently recommends that users do not run with
    48 cores per n_CPUs on SCU16 due to OS issues and
    recommends that CPU-intensive works run with 46 or less
    cores. As 45 is a multiple of 3, it's the best value
    that doesn't waste too much
    '''
    if answerdict['processor'].q_answer == 'cas':
        envdict['n_CPUs'] = 40
    elif answerdict['processor'].q_answer == 'mil':
        envdict['n_CPUs'] = 120

elif envdict['site'] == 'NAS':
    if answerdict['processor'].q_answer == 'has':
        envdict['n_CPUs'] = 24
    elif answerdict['processor'].q_answer == 'bro':
        envdict['n_CPUs'] = 24
    elif answerdict['processor'].q_answer == 'sky':
        answerdict['processor'].q_answer = 'sky_ele'
        envdict['n_CPUs'] = 40
    elif answerdict['processor'].q_answer == 'cas':
        answerdict['processor'].q_answer = 'cas_ait'
        envdict['n_CPUs'] = 40
    elif answerdict['processor'].q_answer == 'rom' or answerdict['processor'].q_answer == 'mil':
        answerdict['processor'].q_answer += '_ait'
        envdict['n_CPUs'] = 120

elif envdict['site'] == 'AWS' or envdict['site'] == 'AZURE':
    # Because we do not know the name of the model or the number of CPUs
    # per node. We ask the user to set these variables in the script
    print(color.RED + "\nSince you are running on ",  envdict['site'], \
                       " you must set the processor and # of CPUs yourself.")
    # ASK FOR PROCESSOR TYPE <-------------------------------------------------------------------------------
    envdict['n_CPUs'] = questionary.text("Enter the number of CPUs per node: ").ask()

else:
    envdict['site'] = 'UNKNOWN'
    if envdict['arch'] == 'Linux':
        # Get the number of CPU cores on Linux
        try:
            with open('/proc/cpuinfo') as f:
                cpuinfo = f.read()
                envdict['n_CPUs'] = cpuinfo.count('processor')
        except IOError:
            print(color.RED + "ERROR: Unable to retrieve the number of CPUs.")
            sys.exit(1)
    elif envdict['arch'] == 'Darwin':
        # Get the number of CPU cores on macOS
        try:
            import multiprocessing
            envdict['n_CPUs'] = multiprocessing.cpu_count()
        except NotImplementedError:
            print(color.RED + "ERROR: Unable to retrieve the number of CPUs.")
            sys.exit(1)
    else:
        print(f"ERROR: Unknown architecture", envdict['arch'])
        sys.exit(1)

#######################################################################
#                     Build Directory Locations
#######################################################################
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


