import platform
from pathlib import Path

pathdict = dict() 
envdict = dict()

def set_env_dicts(root_dir: Path) -> None:
    """
    Set directory and enviriron variable locations directories.
    """

    pathdict['config'] = Path(root_dir, "gcmpy", "setup_tools", "questions")
    pathdict['gcmpy'] = root_dir
    pathdict['bin'] = Path(pathdict['gcmpy']).parent
    pathdict['install'] = Path(pathdict['bin']).parent
    pathdict['etc'] = Path(pathdict['install'], 'etc')
    pathdict['GEOSgcm'] = Path(pathdict['install']).parent
    pathdict['build'] = Path(pathdict['GEOSgcm'], 'build')
    pathdict['GEOSgcm_App'] = Path(pathdict['GEOSgcm'], 'src', 'Applications', '@GEOSgcm_App')
    pathdict['GEOS_Util'] = Path(pathdict['GEOSgcm'], 'src', 'Shared', 'GEOS_Util')

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

    envdict['site'] = open(Path(pathdict['etc'], 'SITE.rc'), 'r').read().split()[-1]

