#!/usr/bin/env python3

import math, os, shutil, tempfile, yaml, re, glob, sys
from ocean import ocean
from atmosphere import atmosphere as atmos
from land import land
from gocart import gocart
from process_questions import ask_questions
from clone import create_exp_yaml
from utility import envdict, pathdict, color, exceptions
from pathlib import Path
from jinja2 import Environment, FileSystemLoader, Undefined


# combines all models (atmos, ocean, land, gocart) into one big one
class setup:
    def __init__(self, expConfig):
        self.expConfig          = expConfig
        self.is_FCST            = False
        self.fv_cubed           = ''
        self.begin_date         = '18910301 000000'
        self.end_date           = '29990302 210000'
        self.n_oserver_nodes    = None
        self.n_backend_pes      = None
        self.n_nodes            = None
        self.exp_dir            = self.expConfig['exp_dir']
        self.restart_by_oserver = 'NO'
        self.several_tries      = ''
        self.gcm_version        = Path(f"{pathdict['etc']}/.AGCM_VERSION").read_text()
        self.linkx              = False
        self.templates          = ['gcm_run.j',
                                   'gcm_post.j',
                                   'gcm_archive.j',
                                   'gcm_regress.j',
                                   'gcm_plot.tmpl',
                                   'gcm_quickplot.csh',
                                   'gcm_moveplot.j',
                                   'gcm_forecast.tmpl',
                                   'gcm_forecast.setup',
                                   'gcm_emip.setup',
                                   'CAP.rc.tmpl',
                                   'AGCM.rc.tmpl',
                                   'logging.yaml',
                                   'fvcore_layout.rc',
                                   'linkbcs.tmpl']
    def initialize_models(self):
        self.ocean = ocean(self.expConfig)
        self.atmos = atmos(self.expConfig)
        self.land = land(self.expConfig)
        self.gocart = gocart(self.expConfig)

    def config_models(self):
        self.ocean.config()
        self.atmos.config(self.ocean.NX, self.ocean.NY)
        self.land.config()
        self.gocart.config()
        self.templates.append(self.ocean.history_template)

    def check_flags(self):
        # If argument is not recognized, display usage and exit
        for arg in enumerate(sys.argv[1:]):
            if (arg[-1] == '--link'):
                self.linkx = True
            elif (arg[-1] == '--singularity'):
                self.bool_usingSingularity == True
            elif (arg[-1] == '--help' or arg[-1] == '-h'):
                exceptions.printusage()
            else:
                exceptions.raiseuserexception("Command line arguemnt \"" + arg[-1] + "\" not \
                                        recognized. \nSee usage:\n" )
            exceptions.printusage()

    def set_num_CPUs(self):
        if envdict['site'] == 'NCCS':
            '''
            NCCS currently recommends that users do not run with
            48 cores per n_CPUs on SCU16 due to OS issues and
            recommends that CPU-intensive works run with 46 or less
            cores. As 45 is a multiple of 3, it's the best value
            that doesn't waste too much
            '''
            if self.expConfig['processor'] == 'cas':
                self.num_CPUs = 40
            elif self.expConfig['processor'] == 'mil':
                self.num_CPUs = 120

        elif envdict['site'] == 'NAS':
            if self.expConfig['processor'] == 'has':
                self.num_CPUs = 24
            elif self.expConfig['processor'] == 'bro':
                self.num_CPUs = 24
            elif self.expConfig['processor'] == 'sky':
                self.expConfig['processor'] = 'sky_ele'
                self.num_CPUs = 40
            elif self.expConfig['processor'] == 'cas':
                self.expConfig['processor'] = 'cas_ait'
                self.num_CPUs = 40
            elif self.expConfig['processor'] == 'rom' or self.expConfig['processor'] == 'mil':
                self.expConfig['processor'] += '_ait'
                self.num_CPUs = 120

        elif envdict['site'] == 'AWS' or envdict['site'] == 'AZURE':
            # Because we do not know the name of the model or the number of CPUs
            # per node. We ask the user to set these variables in the script
            print(color.RED + "\nSince you are running on ",  envdict['site'], \
                               " you must set the processor and # of CPUs yourself.")
            self.num_CPUs = questionary.text("Enter the number of CPUs per node: ").ask()

        else:
            envdict['site'] = 'UNKNOWN'
            if envdict['arch'] == 'Linux':
                # Get the number of CPU cores on Linux
                try:
                    with open('/proc/cpuinfo') as f:
                        cpuinfo = f.read()
                        self.num_CPUs = cpuinfo.count('processor')
                except IOError:
                    print(color.RED + "ERROR: Unable to retrieve the number of CPUs.")
                    sys.exit(1)
            elif envdict['arch'] == 'Darwin':
                # Get the number of CPU cores on macOS
                try:
                    import multiprocessing
                    self.num_CPUs = multiprocessing.cpu_count()
                except NotImplementedError:
                    print(color.RED + "ERROR: Unable to retrieve the number of CPUs.")
                    sys.exit(1)
            else:
                print(f"ERROR: Unknown architecture", envdict['arch'])
                sys.exit(1)



    def set_some_stuff(self):
        if self.atmos.hist_im >= self.ocean.IM:
            self.interpolate_sst = True
        else:
            self.interpolate_sst = False
        self.bcs_res    = f"{self.atmos.res}_{self.ocean.res}"
        self.tile_data  = f"{self.atmos.res}_{self.ocean.res}_Pfafstetter.til"
        self.tile_bin   = f"{self.atmos.res}_{self.ocean.res}_Pfafstetter.TIL"
        self.job_sgmt   = f"{self.atmos.job_sgmt} 000000"


    # setup experiment nodes
    def set_nodes(self):
        model_npes = self.atmos.nx * self.atmos.ny

        # Calculate OSERVER nodes based on recommended algorithm
        if self.expConfig['io_server'] == True:

            # First we calculate the number of model nodes
            n_model_nodes = math.ceil(model_npes / self.num_CPUs)

            # Next the number of frontend PEs is 10% of the model PEs
            n_frontend_pes = math.ceil(model_npes * 0.1)

            # Now we roughly figure out the number of collections in the HISTORY.rc
            n_hist_collections = 0
            with open(f"{pathdict['etc']}/{self.expConfig['history_template']}", 'r') as file:
                in_collections = False
                for line in file:
                    if line.split(' ', 1)[0] == "COLLECTIONS:":
                        in_collections = True
                        continue
                    if in_collections and line.split(' ', 1)[0] != "#":
                        n_hist_collections += 1
                    if line.strip() == "::":
                        break

            # The total number of oserver PEs is frontend PEs plus number of history collections
            n_oserver_pes = n_frontend_pes + n_hist_collections

            # calculate the number of oserver nodes
            n_oserver_nodes = math.ceil(n_oserver_pes / self.num_CPUs)

            # The number of backend PEs is the number of history collections divided by the number of oserver nodes
            n_backend_pes = math.ceil(n_hist_collections / n_oserver_nodes)

            # multigroup requires at least two backend pes
            if (n_backend_pes < 2): n_backend_pes = 2

            # Calculate the total number of nodes to request from batch
            self.nodes = n_model_nodes + n_oserver_nodes

        else:
            self.nodes = math.ceil(model_npes / self.num_CPUs)
            self.n_oserver_nodes = 0
            self.n_backend_pes   = 0

    def set_stuff(self):
        self.set_nodes()
        # Longer job names are now supported with SLURM and PBS. Limits seem to be 1024 characters with SLURM
        # and 230 with PBS. To be safe, we will limit to 200
        self.run_n     = f"{self.expConfig['experiment_id'][:200]}_RUN"    # RUN      Job Name
        self.run_fn    = f"{self.expConfig['experiment_id'][:200]}_FCST"   # Forecast Job Name
        self.post_n    = f"{self.expConfig['experiment_id'][:200]}_POST"   # POST     Job Name
        self.plot_n    = f"{self.expConfig['experiment_id'][:200]}_PLT"    # PLOT     Job Name
        self.move_n    = f"{self.expConfig['experiment_id'][:200]}_MOVE"   # MOVE     Job Name
        self.archive_n = f"{self.expConfig['experiment_id'][:200]}_ARCH"   # ARCHIVE  Job Name
        self.regress_n = f"{self.expConfig['experiment_id'][:200]}_RGRS"   # REGRESS  Job Name

        # Here we need to convert POST_NDS to total tasks. Using 16 cores
        # per task as a good default
        post_npes = self.atmos.post_NDS * 16
        NPCUS = (post_npes + self.num_CPUs - 1)/self.num_CPUs

        '''
        Definition for each variable in the following if-else block:

        batch_cmd               - PBS Batch command
        batch_group             - PBS Syntax for GROUP
        batch_time              - PBS Syntax for walltime
        batch_jobname           - PBS Syntax for job name
        batch_outputname        - PBS Syntax for job output name
        batch_joinouterr        - PBS Syntax for joining output and error
        run_ft                  - Wallclock Time for gcm_forecast.j
        run_ft                  - Wallclock Time for gcm_run.j
        post_t                  - Wallclock Time for gcm_post.j
        plot_t                  - Wallclock Time for gcm_plot.j
        archive_t               - Wallclock Time for gcm_archive.j
        run_q                   - Batch queue name for gcm_run.j
        run_p                   - PE Configuration for gcm_run.j
        run_fp                  - PE Configuration for gcm_forecast.j
        regress_p               - PE Configuration for gcm_regress.j
        post_q                  - Batch queue name for gcm_post.j
        plot_q                  - Batch queue name for gcm_plot.j
        move_q                  - Batch queue name for gcm_moveplot.j
        archive_q               - Batch queue name for gcm_archive.j
        post_p                  - PE Configuration for gcm_post.j
        plot_p                  - PE Configuration for gcm_plot.j
        archive_p               - PE Configuration for gcm_archive.j
        move_p                  - PE Configuration for gcm_moveplot.j
        bcs_dir                 - Location of Boundary Conditions
        replay_ana_expID        - Default Analysis Experiment for REPLAY
        replay_ana_location     - Default Analysis Location for REPLAY
        M2_replay_ana_location  - Default Analysis Location for M2 REPLAY
        sst_dir                 - Location of SST Boundary Conditions
        chem_dir                - Locations of Aerosol Chemistry BCs
        work_dir                - User work directory <----------------- change this later
        gwdrs_dir               - Location of GWD_RIDGE files
        coupled_dir             - Coupled Ocean/Atmos Forcing
        bcs_input_base          - location of SST Boundary Conditions
        '''

        if envdict['site'] == "NAS":
            self.batch_cmd        = "qsub"
            self.batch_group      = "PBS -W group_list="
            self.batch_time       = "PBS -l walltime="
            self.batch_jobname    = "PBS -N"
            self.batch_outputname = "PBS -o "
            self.batch_joinouterr = "PBS -j oe -k oed"
            self.run_ft           = "6:00:00"
            self.run_t            = "8:00:00"
            self.post_t           = "8:00:00"
            self.plot_t           = "8:00:00"
            self.archive_t        = "8:00:00"
            self.run_q            = f"PBS -q normal"
            self.run_p            = f"PBS -l select={self.nodes}:ncpus={self.num_CPUs}:mpiprocs={self.num_CPUs}:model={self.expConfig['processor']}"
            self.run_fp           = f"PBS -l select=24:ncpus={self.num_CPUs}:mpiprocs={self.num_CPUs}:model={self.expConfig['processor']}"
            self.regress_p        = f"PBS -l select={self.nodes * 2}:ncpus={self.num_CPUs // 2}:mpiprocs={self.num_CPUs // 2}:model={self.expConfig['processor']}"
            self.post_q           = "PBS -q normal"
            self.plot_q           = "PBS -q normal"
            self.move_q           = "PBS -q normal"
            self.archive_q        = "PBS -q normal"
            self.post_p           = f"PBS -l select={NPCUS}:ncpus={self.num_CPUs}:mpiprocs={self.num_CPUs}:model={self.expConfig['processor']}"
            self.plot_p           = f"PBS -l select=1:ncpus={self.num_CPUs}:mpiprocs=1:model={self.expConfig['processor']}"
            self.archive_p        = f"PBS -l select=1:ncpus={self.num_CPUs}:mpiprocs={self.num_CPUs}:model={self.expConfig['processor']}"
            self.move_p           = "PBS -l select=1:ncpus=1"
            self.boundary_dir     = "/nobackup/gmao_SIteam/ModelData"
            self.bc_base          = f"{self.boundary_dir}/bcs_shared/fvInput/ExtData/esm/tiles"
            self.bcs_dir          = f"{self.boundary_dir}/bcs/{self.land.bcs}/{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID        = "ONLY_MERRA2_SUPPORTED"
            self.replay_ana_location     = "ONLY_MERRA2_SUPPORTED"
            self.M2_replay_ana_location  = f"{self.boundary_dir}/merra2/data"
            self.bcs_input_base = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/atmosphere"

            # defines location of SST Boundary Conditions
            oceanres = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                self.sst_dir = f"{self.boundary_dir}/fvInput/g5gcm/bcs/SST/{oceanres}"
            elif self.ocean.gridtyp == "MITLLC":
                self.sst_dir = "/nobackupp2/estrobac/geos5/SSTDIR"
            else:
                self.sst_dir = f"{self.boundary_dir}/fvInput/g5gcm/bcs/realtime/{self.ocean.sst_name}/{oceanres}"

            self.chem_dir         = f"{self.boundary_dir}/fvInput_nc3"
            self.work_dir         = f"/nobackup/{os.environ.get('LOGNAME')}"
            self.gwdrs_dir        = f"{self.boundary_dir}/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/ocean"


        elif envdict['site'] == "NCCS":
            self.batch_cmd        = "sbatch"
            self.batch_group      = "SBATCH --account="
            self.batch_time       = "SBATCH --time="
            self.batch_jobname    = "SBATCH --job-name="
            self.batch_outputname = "SBATCH --output="
            self.batch_joinouterr = "DELETE"
            self.run_ft           = "06:00:00"
            self.run_t            = "12:00:00"
            self.post_t           = "8:00:00"
            self.plot_t           = "12:00:00"
            self.archive_t        = "2:00:00"
            self.run_q            = f"SBATCH --constraint={self.expConfig['processor']}"
            self.run_p            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={self.num_CPUs}"
            self.run_fp           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={self.num_CPUs}"
            self.regress_p        = f"SBATCH --nodes={self.nodes * 2} --ntasks-per-node={self.num_CPUs // 2}"
            self.post_q           = f"SBATCH --constraint={self.expConfig['processor']}"
            self.plot_q           = f"SBATCH --constraint={self.expConfig['processor']}"
            self.move_q           = "SBATCH --partition=datamove"
            self.archive_q        = "SBATCH --partition=datamove"
            self.post_p           = f"SBATCH --nodes={NPCUS} --ntasks-per-node={self.num_CPUs}"
            self.plot_p           = f"SBATCH --nodes=4 --ntasks=4"
            self.archive_p        = "SBATCH --ntasks=1"
            self.move_p           = "SBATCH --ntasks=1"
            self.boundary_dir     = "/discover/nobackup/projects/gmao"
            self.bc_base          = f"{self.boundary_dir}/bcs_shared/fvInput/ExtData/esm/tiles"
            self.bcs_dir          = f"{self.bc_base}/{self.land.bcs}"
            self.replay_ana_expID       = "x0039"
            self.replay_ana_location    = f"{self.boundary_dir}/g6dev/ltakacs/x0039"
            self.M2_replay_ana_location = f"{self.boundary_dir}/merra2/data"
            self.bcs_input_base = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/atmosphere"

            # define location of SST Boundary Conditions
            oceanres    = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                self.sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/SST/{oceanres}"
            elif self.ocean.gridtyp == "MITLLC":
                self.sst_dir = "/discover/nobackup/estrobac/geos5/SSTDIR"
            else:
                self.sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/realtime/{self.ocean.sst_name}/{oceanres}"

            self.chem_dir         = f"{os.environ.get('SHARE')}/gmao_ops/fvInput_nc3"
            self.work_dir         = f"/discover/nobackup/{os.environ.get('LOGNAME')}"
            self.gwdrs_dir        = f"{self.boundary_dir}/osse2/stage/BCS_FILES/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/ocean"


        elif envdict['site'] == "AWS" or envdict['site'] == "Azure":
            self.batch_cmd        = "sbatch"
            self.batch_group      = "#DELETE"
            self.batch_time       = "SBATCH --time="
            self.batch_jobname    = "SBATCH --job-name="
            self.batch_outputname = "SBATCH --output="
            self.batch_joinouterr = "DELETE"
            self.run_ft           = "06:00:00"
            self.run_t            = "12:00:00"
            self.post_t           = "8:00:00"
            self.plot_t           = "12:00:00"
            self.archive_t        = "1:00:00"
            self.run_q            = f"SBATCH --constraint={self.expConfig['processor']}"
            self.run_p            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={self.num_CPUs}"
            self.run_fp           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={self.num_CPUs}"
            self.regress_p        = f"SBATCH --nodes={self.nodes * 2} --ntasks-per-node={self.num_CPUs // 2}"
            self.post_q           = "NULL"
            self.plot_q           = "NULL"
            self.move_q           = "NULL"
            self.archive_q        = "NULL"
            self.post_p           = f"SBATCH --ntasks={post_npes}"
            self.plot_p           = f"SBATCH --nodes=4 --ntasks=4"
            self.archive_p        = "SBATCH --ntasks=1"
            self.move_p           = "SBATCH --ntasks=1"
            self.boundary_dir     = "/ford1/share/gmao_SIteam/ModelData"
            self.bc_base          = f"{self.boundary_dir}/bcs_shared/fvInput/ExtData/esm/tiles"
            self.bcs_dir          = f"{self.boundary_dir}/bcs/{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID       = "REPLAY_UNSUPPORTED"
            self.replay_ana_location    = "REPLAY_UNSUPPORTED"
            self.M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            self.bcs_input_base   = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/atmosphere"
            self.sst_dir          = f"{self.boundary_dir}/{self.ocean.sst_name}/{self.ocean.IM}x{self.ocean.JM}"
            self.chem_dir         = f"{self.boundary_dir}/fvInput_nc3"
            self.work_dir         = os.environ.get('HOME')
            self.gwdrs_dir        = f"{self.boundary_dir}/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_dir}/aogcm"

        else:
            # These are defaults for the desktop
            self.batch_cmd        = "sbatch"
            self.batch_group      = "SBATCH --account="
            self.batch_time       = "SBATCH --time="
            self.batch_jobname    = "SBATCH --job-name="
            self.batch_outputname = "SBATCH --output="
            self.batch_joinouterr = "DELETE"
            self.run_ft           = "06:00:00"
            self.run_t            = "12:00:00"
            self.post_t           = "8:00:00"
            self.plot_t           = "12:00:00"
            self.archive_t        = "1:00:00"
            self.run_q            = "NULL"
            self.run_p            = "NULL"
            self.run_fp           = "NULL"
            self.regress_p        = "NULL"
            self.post_q           = "NULL"
            self.plot_q           = "NULL"
            self.move_q           = "NULL"
            self.archive_q        = "NULL"
            self.post_p           = "NULL"
            self.plot_p           = "NULL"
            self.archive_p        = "NULL"
            self.move_p           = "NULL"
            self.boundary_dir     = "/ford1/share/gmao_SIteam/ModelData"
            self.bc_base          = f"{self.boundary_dir}/bcs_shared/fvInput/ExtData/esm/tiles"
            self.bcs_dir          = f"{self.boundary_dir}/bcs/{self.land.bcs} /{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID       = "REPLAY_UNSUPPORTED"
            self.replay_ana_location    = "REPLAY_UNSUPPORTED"
            self.M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            self.bcs_input_base   = f"{self.boundary_dir}/bcs_shared/make_bcs_inputs/atmosphere"
            self.sst_dir          = f"{self.boundary_dir}/{self.ocean.sst_name}/{self.ocean.IM}x{self.ocean.JM}"
            self.chem_dir         = f"{self.boundary_dir}/fvInput_nc3"
            self.work_dir         = os.environ.get('HOME')
            self.gwdrs_dir        = f"{self.boundary_dir}/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_dir}/aogcm"

        if envdict['site'] == 'GMAO.desktop':
            # By default on desktop, just ignore IOSERVER for now
            self.atmos.NX = 1
            self.atmos.NY = 6
            self.expConfig["io_server"] = False
            self.n_oserver_nodes = 0
            self.n_backend_pes = 0



    # mainly used to create .{*}root files and/or populate them
    def create_dotfile(self, path, content):
        try:
            path = Path(path)
            path.parent.mkdir(parents=True, exist_ok=True)
            path.touch()
            with open(path, 'w') as file:
                file.write(os.path.dirname(content))
        except Exception as e:
            print(f"An error occurred while creating directory: {str(e)}")
            exit(1)


    #######################################################################
    #      Copy Model Executable and RC Files to Experiment Directory
    #######################################################################
    def RC_setup(self):

        # Make the experiment directory and the RC directory inside of it
        RC_dir = os.path.join(self.exp_dir, 'RC')

        # Delete the directory if it exists already
        if os.path.exists(RC_dir):
            shutil.rmtree(RC_dir)

        # Copy over all files and subdirs in install/etc, keeping symlinks, and ignoring *.tmpl files
        shutil.copytree(pathdict['etc'], RC_dir, symlinks=True, ignore=shutil.ignore_patterns('*.tmpl', 'fvcore.layout.rc'))

        # Copy or symlink GEOSgcm.x (((IGNORE SINGULARITY/NATIVE BUILDS FOR NOW!!)))
        geosgcmx_path = os.path.join(pathdict['bin'], 'GEOSgcm.x')
        if self.linkx == True:
            os.symlink(geosgcmx_path, os.path.join(self.exp_dir, 'GEOSgcm.x'))
        else:
            shutil.copy(geosgcmx_path, self.exp_dir)


    #######################################################################
    #               Set Recommended MPI Stack Settings
    #######################################################################
    def mpistacksettings(self):

        # load mpi config from YAML
        with open('../yaml/mpi_config.yaml') as file:
            mpidict = yaml.load(file, Loader=yaml.FullLoader)

        # retrieve config from correlating mpi setting being used
        self.mpi_config = mpidict.get(envdict['mpi'])

        # restart by oserver if using openmpi or mvapich
        if envdict['mpi'] == 'openmpi' or envdict['mpi'] == 'mvapich':
            self.restart_by_oserver = 'YES'

        # NAS recommends several_tries for MPT job issues
        # https://www.nas.nasa.gov/hecc/support/kb/mpt-startup-failures-workarounds_526.html
        if envdict['mpi'] == 'mpt':
            self.several_tries = '/u/scicon/tools/bin/several_tries'
            # Testing at NAS shows that coupled runs *require* MPI_SHEPHERD=true
            # to run. We believe this is due to LD_PRELOAD. For now we only set
            # this for coupled runs.
            if self.ocean.running_ocean:
                self.ocean.mpt_shepherd = "setenv MPI_SHEPHERD true"

            # Also, for low res runs (c12, c24, c48) at NAS with MPT,
            # MPT has an issue with writing restarts on a single node
            # due to issues with many MPI_GatherV calls. We can "avoid"
            # this by having the oserver write the restarts. But
            # we only need to do this for single-node runs. For simplicity,
            # we assume LOW_ATM_RES = TRUE means single-node
            if self.atmos.low_res:
                self.restart_by_oserver = 'YES'


        if envdict['site'] == 'NCCS':
            self.mpi_config += f"\n{mpidict.get('NCCS')}"

        # Check for gwd_internal for Ridge Scheme
        # For ICA and NL3 the gwd files are in a non-bcs location
        # and may or may not exist. If they don't we set NCAR_NRDG to 0
        self.NCAR_NRDG = 16
        if not self.land.gwd_in_bcs and \
           not os.path.exists(f"{self.gwdrs_dir}/gwd_internal_c{self.atmos.im}"):
            self.NCAR_NRDG = 0



    #######################################################################
    #            Create directories and copy files over
    #######################################################################
    # A little helper function for copying files and displaying the info to the user
    def copy_helper(self, src, destination, filename):
        if os.path.exists(src):
            shutil.copy(src, destination)
            print(f"Creating {color.RED}{filename}{color.RESET} for Experiment: {self.expConfig['experiment_id']}")

    def copy_files_into_exp(self):
        print("\n\n\n")

        for file in self.templates:
            self.copy_helper(f"{pathdict['install']}/bin/{file}", f"{self.exp_dir}/{file}", file)
            self.copy_helper(f"{pathdict['install']}/etc/{file}", f"{self.exp_dir}/{file}", file)

        self.copy_helper(f"{pathdict['install']}/post/plot.rc", f"{self.exp_dir}/plot.rc", "plot.rc")
        self.copy_helper(f"{pathdict['install']}/post/post.rc", f"{self.exp_dir}/post.rc", "post.rc")

        # These files will be added if user chose to run coupled, regardless of ocean model selected.
        if self.ocean.running_ocean == True and self.ocean.model != 'MIT':
            self.copy_helper(f"{pathdict['install']}/coupled_diagnostics/g5lib/plotocn.j", f"{self.exp_dir}/plotocn.j", "plotocn.j")
            self.copy_helper(f"{pathdict['install']}/coupled_diagnostics/g5lib/confocn.py", f"{self.exp_dir}/__init__.py", "confocn.py")
            self.templates.extend(['input.nml', 'diag_table','plotocn.j', '__init__.py'])

        if self.ocean.model == 'MOM5':
            self.templates.append('field_table')
            self.copy_helper(f"{pathdict['etc']}/MOM5/geos5/{self.ocean.IM}x{self.ocean.JM}/INPUT/input.nml", f"{self.exp_dir}/input.nml", "input.nml")
            MOM5_path = os.path.join(pathdict['etc'], 'MOM5', 'geos5', f"{self.ocean.IM}x{self.ocean.JM}", 'INPUT', '*table')
            files = glob.glob(MOM5_path)
            for file in files:
                file_name = os.path.basename(file)
                self.copy_helper(file, f"{self.exp_dir}/{file_name}", file_name)
        elif self.ocean.model == 'MOM6':
            self.templates.extend(['MOM_input', 'MOM_override', 'data_table'])

            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/MOM_input", f"{self.exp_dir}/MOM_input", "MOM_input")
            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/MOM_override", f"{self.exp_dir}/MOM_override", "MOM_override")
            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/input.nml", f"{self.exp_dir}/input.nml", "input.nml")
            MOM6_path = os.path.join(pathdict['etc'], 'MOM6', 'mom6_app', f"{self.ocean.IM}x{self.ocean.JM}", '*table')
            files = glob.glob(MOM6_path)
            for file in files:
                file_name = os.path.basename(file)
                self.copy_helper(file, f"{self.exp_dir}/{file_name}", file_name)

        if self.ocean.seaice_model == 'CICE6':
            self.copy_helper(f"{pathdict['etc']}/CICE6/cice6_app/{self.ocean.IM}x{self.ocean.JM}/ice_in", f"{self.exp_dir}/ice_in", "ice_in")
            self.templates.append('ice_in')

        print(f"{color.GREEN}Done!{color.RESET}\n")



    #######################################################################
    #                 Produce Final script and .rc files
    #######################################################################

    # THIS WHOLE SECTION IS WILDLY OUT OF DATE, HOWEVER I KEPT IT AS IT WAS
    # IN THE ORIGINAL SCRIPT FOR NOW
    def restarts(self):
        # comment or un-comment restarts based on exp configuration
        # ---------------------------------------------------------
        rsnames = {'H2O': False,
                   'MAM': False,
                   'CARMA': False,
                   'GMICHEM': False,
                   'STRATCHEM': False}
        rstypes = ['INTERNAL','IMPORT']

        with open(f"{self.expConfig['exp_dir']}/AGCM.rc.tmpl", 'r') as file:
            file_content = file.read()

        # Template in a "#" if restart is set to false
        for rst in rsnames:
            for typ in rstypes:
                rst_string = f"{rst}_{typ}"
                comment = "" if rsnames[rst] else "#"
                file_content = file_content.replace(rst_string, f"{comment}{rst_string}")

        with open(f"{self.expConfig['exp_dir']}/AGCM.rc.tmpl", 'w') as file:
            file.write(file_content)

    #######################################################################
    #      Modify RC Directory for LM and GOCART.data/GOCART Options
    #######################################################################
    def mod_RC_dir_for_pchem(self):
        if self.atmos.lm == 72:
            return

        rc_dir = f"{self.expConfig['exp_dir']}/RC"

        # if atmospheric vertical resolution != 72, we loop through every
        # file in the RC dir and modify the atmos.lm values
        for file_name in os.listdir(rc_dir):
            file_path = os.path.join(rc_dir, file_name)

            # ignore the subdirectories in RC/
            if not os.path.isfile(file_path):
                continue

            with open(file_path, 'r') as file:
                file_content = file.read()

            file_content = file_content.replace("/L72/", f"/L{self.atmos.lm}/")
            file_content = file_content.replace("z72", f"z{self.atmos.lm}")
            file_content = file_content.replace("_72_", f"_{self.atmos.lm}_")

            with open(file_path, 'w') as file:
                file.write(file_content)

    # configure pchem and TR in GEOS_ChemGridComp.rc
    def config_chemGridComp(self):
        if self.gocart.rats_provider == 'PCHEM':
            pchem = 'TRUE'
        else:
            pchem = 'FALSE'

        chemgridcomp = f"{self.expConfig['exp_dir']}/RC/GEOS_ChemGridComp.rc"
        with open(chemgridcomp, 'r') as file:
            file_content = file.read()

        # we always enable TR and gocart
        file_content = re.sub(r'(ENABLE_PCHEM:\s*\.).*(\.)', r'\1'+pchem+r'\2', file_content)
        file_content = re.sub(r'(ENABLE_TR:\s*\.).*(\.)', r'\1TRUE\2', file_content)
        file_content = re.sub(r'(ENABLE_GOCART_DATA:\s*\.).*(\.)', r'\1FALSE\2', file_content)

        with open(chemgridcomp, 'w') as file:
            file.write(file_content)

    # update LAND_PARAMS choices
    def config_surfaceGridComp(self):
        surfacegridcomp = f"{self.expConfig['exp_dir']}/RC/GEOS_SurfaceGridComp.rc"
        with open(surfacegridcomp, 'r') as file:
            file_content = file.read()

        file_content = re.sub(r'# GEOSagcm=>', r'            ', file_content)
        if self.land.model == 'CatchmentCN-CLM4.0':
            file_content = re.sub(r'(LAND_PARAMS:\s*).*', r'\1CN_CLM40', file_content)

        if self.land.bcs == 'ICA':
            file_content = re.sub(r'(LAND_PARAMS:\s*).*', r'\1Icarus', file_content)
            file_content = re.sub(r'(Z0_FORMULATION:\s*).*', r'\1 2', file_content)

        with open(surfacegridcomp, 'w') as file:
            file.write(file_content)

    # enable DATA_DRIVEN gocart2G
    def config_gocartGridComp(self):
        gocartgridcomp = f"{self.expConfig['exp_dir']}/RC/GOCART2G_GridComp.rc"
        with open(gocartgridcomp, 'r') as file:
            file_content = file.read()

        if self.gocart.data_driven == True:
            file_content = re.sub(r'(ACTIVE_INSTANCES_DU:\s*)DU', r'\1DU.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_SS:\s*)SS', r'\1SS.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_SU:\s*)SU', r'\1SU.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_NI:\s*)NI', r'\1NI.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_CA:\s*)CA.oc', r'\1CA.oc.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_CA:\s*)CA.bc', r'\1CA.bc.data', file_content)
            file_content = re.sub(r'(ACTIVE_INSTANCES_CA:\s*)CA.br', r'\1CA.br.data', file_content)

        with open(gocartgridcomp, 'w') as file:
            file.write(file_content)


    # Modify MOM input files to match HEARTBEAT
    def config_heartbeat(self):
        # With MOM5 we need to change dt lines in input.nml to
        # use $OCEAN_DT instead. NOTE: This regex assumes integer followed by comma
        if self.ocean.model == 'MOM5':

            with open(f"{self.expConfig['exp_dir']}/input.nml", 'r') as file:
                file_content = file.read()

            file_content = re.sub(r'dt_cpld\s*=\s*.*(,)', rf"dt_cpld = {self.atmos.dt_ocean}\1", file_content)
            file_content = re.sub(r'dt_atmos\s*=\s*.*(,)', rf"dt_atmos = {self.atmos.dt_ocean}\1", file_content)

            with open(f"{self.expConfig['exp_dir']}/input.nml", 'w') as file:
                file.write(file_content)

        # We also need to change the dt in ice_in as well for CICE6
        if self.ocean.seaice_model == 'CICE6':
            file_path = f"{self.expConfig['exp_dir']}/icein"
            with open(file_path, 'r') as file:
                content = file.read()

            file_content = re.sub(r"^(\s*dt\s*=\s*)[0-9]+(\.[0-9]+)?", r"\1" + self.atmos.dt_ocean, content)

            with open(file_path, 'w') as file:
                file.write(file_content)


        # We also must change the MOM_override file to
        # have consistent DTs with the AGCM. So we use OCEAN_DT
        # and change MOM_override to match. NOTE: This assumes
        # floating point number with a decimal
        if self.ocean.model == 'MOM6':
            with open(f"{self.expConfig['exp_dir']}/MOM_override", 'r') as file:
                file_content = file.read()

            file_content = re.sub(r'DT\s*=\s*.*', rf"DT = {self.atmos.dt_ocean}", file_content)
            file_content = re.sub(r'DT_THERM\s*=\s*.*', rf"DT_THERM = {self.atmos.dt_ocean}", file_content)

            with open(f"{self.expConfig['exp_dir']}/MOM_override", 'w') as file:
                file.write(file_content)


    #######################################################################
    #                     Copy over Source Tarfile
    #######################################################################
    def copy_src_tarfile(self):
        bool_install_tarfile = '@CFG_INSTALL_SOURCE_TARFILE@'
        tarfile_name = '@CMAKE_PROJECT_NAME@.tar.gz'

        if bool_install_tarfile != 'TRUE':
            return

        src_dir = f"{self.exp_dir}/src"
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


    # We create a RESTART dir if running ocean
    def make_RESTART_dir(self):
        if self.ocean.running_ocean == True:
            os.makedirs(f"{self.exp_dir}/RESTART", exist_ok=True)


    # Templating helper function -- Removes lines marked with "#DELETE"
    def cleanup(self, file_path):
        with open(file_path, 'r') as file:
            content = file.read()

        filtered_content = re.sub(r'(?m)^\s*#+DELETE\w*.*\n?', '', content)

        with open(file_path, 'w') as file:
            file.write(filtered_content)


    def template(self):
        # this dictionary holds template values for the default jinja2 delimiter "{{ val }}"
        jinja_dict = {
            'SETENVS': self.mpi_config,
            'GCMVER': self.gcm_version,
            'EXPSRC': self.gcm_version,
            'EXPID': self.expConfig['experiment_id'],
            'RUN_N': self.run_n,
            'RUN_FN': self.run_fn,
            'RUN_FT': self.run_ft,
            'RUN_T': self.run_t,
            'RUN_P': self.run_p,
            'RUN_FP': self.run_fp,
            'REGRESS_P': self.regress_p,
            'RUN_Q': self.run_q,
            'POST_N': self.post_n,
            'POST_T': self.post_t,
            'POST_P': self.post_p,
            'POST_Q': self.post_q,
            'MOVE_N': self.move_n,
            'PLOT_N': self.plot_n,
            'PLOT_T': self.plot_t,
            'PLOT_P': self.plot_p,
            'PLOT_Q': self.plot_q,
            'MOVE_Q': self.move_q,
            'MOVE_P': self.move_p,
            'ARCHIVE_N': self.archive_n,
            'ARCHIVE_T': self.archive_t,
            'ARCHIVE_P': self.archive_p,
            'ARCHIVE_Q': self.archive_q,
            'REGRESS_N': self.regress_n,
            'BCSDIR': self.bcs_dir,
            'SSTDIR': self.sst_dir,
            'SSTNAME': self.ocean.sst_name,
            'OCEANOUT': self.ocean.out,
            'LSMBCS': self.land.bcs,
            'EMIP_BCS_IN': self.land.emip_bcs_in,
            'EMIP_MERRA2': self.land.emip_MERRA2,
            'BCSTAG': self.ocean.tag,
            'SSTFILE': self.ocean.sst_file,
            'ICEFILE': self.ocean.ice_file,
            'KPARFILE': self.ocean.kpar_file,
            'CHMDIR': self.chem_dir,
            'COUPLEDIR': self.coupled_dir,
            'GWDRSDIR': self.gwdrs_dir,
            'NCAR_NRDG': self.NCAR_NRDG,
            'EXPDIR': self.exp_dir,
            'EXPDSC': self.expConfig['experiment_description'],
            'HOMDIR': self.exp_dir,
            'BATCH_GROUP': self.batch_group+self.expConfig['group_root'],
            'BATCH_TIME': self.batch_time,
            'BATCH_CMD': self.batch_cmd,
            'BATCH_JOBNAME': self.batch_jobname,
            'BATCH_OUTPUTNAME': self.batch_outputname,
            'BATCH_JOINOUTERR': self.batch_joinouterr,
            'SITE': envdict['site'],
            'GEOSDIR': pathdict['install'],
            'GEOSSRC': pathdict['install'],
            'GEOSBIN': pathdict['bin'],
            'GEOSETC': pathdict['etc'],
            'GEOSUTIL': pathdict['install'],
            'SINGULARITY_BUILD': '#DELETE',
            'NATIVE_BUILD': '',
            'MPT_SHEPHERD': self.ocean.mpt_shepherd,
            'SINGULARITY_SANDBOX': '',
            'REAL_BIND_PATH': '',
            'BASE_BIND_PATH': '',
            'BC_BASE': self.bc_base,
            'BOUNDARY_DIR': self.boundary_dir,
            'BCS_INPUT_BASE': self.bcs_input_base,
            'CHECKPOINT_TYPE': 'default',
            'OGCM_NX': self.ocean.NX,
            'OGCM_NY': self.ocean.NY,
            'OGCM_NPROCS': self.ocean.nprocs,
            'OBSERVER_FRQ': 0,
            'DASTUNING': '#',
            'COUPLED': self.ocean.coupled,
            'CLDMICRO': self.atmos.microphysics,
            'MOM5': self.ocean.MOM5,
            'MOM6': self.ocean.MOM6,
            'OCNMODEL': self.ocean.model,
            'CICE4': self.ocean.CICE4,
            'CICE6': self.ocean.CICE6,
            'HIST_CICE4': self.ocean.hist_CICE4,
            'MIT': self.ocean.MIT,
            'DATAOCEAN': self.ocean.data,
            'OPS_SPECIES': self.gocart.ops_species,
            'CMIP_SPECIES': self.gocart.cmip_species,
            'MERRA2OX_SPECIES': self.gocart.MERRA2OX_species,
            'HIST_GOCART': self.gocart.gocart_hist,
            'LSM_PARMS': self.land.parameters,
            'OCEAN_NAME': self.ocean.name,
            'OCEAN_PRELOAD': self.ocean.preload,
            'ana4replay.eta.%y4%m2%d2_%h2z.nc4': '/discover/nobackup/projects/gmao/merra2/data/ana/MERRA2_all/Y%y4/M%m2/MERRA2.ana.eta.%y4%m2%d2_%h2z.nc4?g',
            'REPLAY_ANA_EXPID': self.replay_ana_expID,
            'REPLAY_ANA_LOCATION': self.replay_ana_location,
            'M2_REPLAY_ANA_LOCATION': self.M2_replay_ana_location,
            'OX_RELAXTIME': self.gocart.ox_relaxtime,
            'PCHEM_CLIM_YEARS': self.gocart.pchem_clim_years,
            'RATS_PROVIDER': self.gocart.rats_provider,
            'AERO_PROVIDER': self.gocart.aero_provider,
            'OANA_PROVIDER': 'PCHEM',
            'EMISSIONS': self.gocart.emissions,
            'CH4_PROVIDER': self.gocart.ch4_provider,
            'CO2_PROVIDER': self.gocart.c02_provider,
            'DYCORE': 'FV3',
            'AGCM_GRIDNAME': self.atmos.gridname,
            'OGCM_GRIDNAME': self.ocean.gridname,
            'OGCM_IS_FCST': '0',
            'BOOT': 'YES',
            'BCSRES': self.bcs_res,
            'OCEANtag': self.ocean.res,
            'ATMOStag': self.atmos.res,
            'RES_DATELINE': self.atmos.res_dateline,
            'TILEDATA': self.tile_data,
            'TILEBIN': self.tile_bin,
            'DT': self.atmos.dt,
            'CONV_DT': self.atmos.conv_dt,
            'CHEM_DT': self.atmos.chem_dt,
            'SOLAR_DT': self.atmos.dt_solar,
            'IRRAD_DT': self.atmos.dt_irrad,
            'OCEAN_DT': self.atmos.dt_ocean,
            'NX': self.atmos.nx,
            'NY': self.atmos.ny,
            'USE_SHMEM': int(self.atmos.use_SHMEM),
            'USE_IOSERVER': int(self.expConfig['io_server']),
            'NUM_OSERVER_NODES': self.n_oserver_nodes,
            'NUM_BACKEND_PES': self.n_backend_pes,
            'RESTART_BY_OSERVER': self.restart_by_oserver,
            'NCPUS_PER_NODE': self.num_CPUs,
            'NUM_READERS': self.atmos.num_readers,
            'NUM_WRITERS': self.atmos.num_writers,
            'LATLON_AGCM': self.atmos.latlon,
            'LATLON_OGCM': self.ocean.latlon,
            'CUBE_AGCM': self.atmos.cube,
            'CUBE_OGCM': self.ocean.cube,
            'GRID_TYPE': 'Cubed-Sphere',
            'AGCM_NF': self.atmos.nf,
            'AGCM_IM': self.atmos.im,
            'AGCM_JM': self.atmos.jm,
            'AGCM_LM': self.atmos.lm,
            'OGCM_IM': self.ocean.IM,
            'OGCM_JM': self.ocean.JM,
            'OGCM_LM': self.ocean.LM,
            'OGCM_NF': self.ocean.NF,
            'OGCM_GRID_TYPE': self.ocean.grid_type,
            'BEG_DATE': self.begin_date,
            'END_DATE': self.end_date,
            'JOB_SGMT': self.job_sgmt,
            'NUM_SGMT': self.atmos.num_sgmt,
            'CONUS': self.atmos.conus,
            'CONVPAR_OPTION': self.atmos.convpar_option,
            'STRETCH_FACTOR': self.atmos.stretch_factor,
            'INTERPOLATE_SST': self.interpolate_sst,
            'HIST_IM': self.atmos.hist_im,
            'HIST_JM': self.atmos.hist_jm,
            'CLIM_IM': self.atmos.CLIM_IM,
            'CLIM_JM': self.atmos.CLIM_JM,
            'ISCCP_SATSIM': 1,
            'MODIS_SATSIM': 0,
            'RADAR_SATSIM': 0,
            'LIDAR_SATSIM': 0,
            'MISR_SATSIM': 0,
            'SATSIM': 0,
            'USE_SKIN_LAYER': 1,
            'ANALYZE_TS': 0,
            'LSM_CHOICE': self.land.model,
            'MP_TURN_OFF_WSUB_EXTDATA': self.atmos.mp_turn_off_wsub_extdata,
            'BACM_1M_': self.atmos.BACM_1M,
            'GFDL_1M_': self.atmos.GFDL_1M,
            'MGB2_2M_': self.atmos.MGB2_2M,
            'PRELOAD_COMMAND': envdict['preload_command'],
            'LD_LIBRARY_PATH_CMD': envdict['ld_library_path_command'],
            'RUN_CMD': envdict['run_command'],
            'MODELATM': self.ocean.modelatm,
            'USE_DATA_ATM4OCN': self.ocean.use_data_ATM4OCN,
            'FV_SCHMIDT': self.atmos.schmidt,
            'FV_STRETCH_FAC': self.atmos.FV_stretch_fac,
            'FV_TARGET_LON': self.atmos.target_lon,
            'FV_TARGET_LAT': self.atmos.target_lat,
            'FORCEDAS': self.atmos.force_das,
            'FORCEGCM': self.atmos.force_gcm,
            'FVCUBED': '',
            'HIST_CATCHCN': self.land.HIST_catchment,
            'GCMRUN_CATCHCN': self.land.GCMRUN_catchment,
            'EMIP_OLDLAND': self.land.emip_oldland,
            'EMIP_NEWLAND': self.land.emip_newland,
            '_4DIAUDAS': '#DELETE',
            'SEVERAL_TRIES': self.several_tries,
            'REGULAR_REPLAY': '#',
            'REGULAR_REPLAY_GMAO': '#',
            'REGULAR_REPLAY_NCEP': '#DELETE',
            'REGULAR_REPLAY_ECMWF': '#DELETE'
        }

        # this is an edge-case that can't be handled with jinja2
        for file in self.templates:
            with open(f"{self.exp_dir}/{file}", 'r') as tmpl:
                file_content = tmpl.read()

            file_content = re.sub(r'[ \t]*RECORD_', r'#RECORD_', file_content)

            with open(f"{self.exp_dir}/{file}", 'w') as tmpl:
                tmpl.write(file_content)

        # this block handles the default case for jinja templating
        default_env = Environment(
            loader=FileSystemLoader(self.exp_dir)
        )
        for file in self.templates:
            template = default_env.get_template(file)
            content = template.render(jinja_dict)
            with open(f"{self.exp_dir}/{file}", 'w') as tmpl:
                tmpl.write(content)

        # remove #DELETE lines
        for file in self.templates:
            file_path = f"{self.exp_dir}/{file}"
            self.cleanup(file_path)


    # organize files into sub directories and update file permissions
    def organize_exp_dir(self):
        exp_dir = self.expConfig['exp_dir']

        # make sub dirs
        sub_dirs = ['archive', 'forecasts', 'plot', 'post' , 'regress']
        for i in sub_dirs:
            os.makedirs(os.path.join(exp_dir, i), exist_ok=True)
        self.copy_src_tarfile()
        self.make_RESTART_dir()

        # archive dir
        shutil.move(f"{exp_dir}/gcm_archive.j", f"{exp_dir}/archive/gcm_archive.j")

        # forecasts dir
        shutil.move(f"{exp_dir}/gcm_forecast.setup", f"{exp_dir}/forecasts/gcm_forecast.setup")
        shutil.move(f"{exp_dir}/gcm_forecast.tmpl", f"{exp_dir}/forecasts/gcm_forecast.tmpl")

        # plot dir
        shutil.move(f"{exp_dir}/gcm_moveplot.j", f"{exp_dir}/plot/gcm_moveplot.j")
        shutil.move(f"{exp_dir}/gcm_plot.tmpl", f"{exp_dir}/plot/gcm_plot.tmpl")
        shutil.move(f"{exp_dir}/gcm_quickplot.csh", f"{exp_dir}/plot/gcm_quickplot.csh")
        shutil.move(f"{exp_dir}/plot.rc", f"{exp_dir}/plot/plot.rc")
        try: #only if using MOM ocean
            shutil.move(f"{exp_dir}/plotocn.j", f"{exp_dir}/plot/plotocn.j")
        except FileNotFoundError:
            pass

        # post dir
        shutil.move(f"{exp_dir}/gcm_post.j", f"{exp_dir}/post/gcm_post.j")
        shutil.move(f"{exp_dir}/post.rc", f"{exp_dir}/post/post.rc")

        # regress dir
        shutil.move(f"{exp_dir}/gcm_regress.j", f"{exp_dir}/regress/gcm_regress.j")

        # rename tmpl files
        os.rename(f"{exp_dir}/CAP.rc.tmpl", f"{exp_dir}/CAP.rc")
        os.rename(f"{exp_dir}/AGCM.rc.tmpl", f"{exp_dir}/AGCM.rc")
        os.rename(f"{exp_dir}/{self.ocean.history_template}", f"{exp_dir}/HISTORY.rc")
        os.rename(f"{exp_dir}/linkbcs.tmpl", f"{exp_dir}/linkbcs")

        # update file permissions
        os.chmod(f"{exp_dir}/CAP.rc", 0o755)
        os.chmod(f"{exp_dir}/fvcore_layout.rc", 0o755)
        os.chmod(f"{exp_dir}/archive/gcm_archive.j", 0o755)
        os.chmod(f"{exp_dir}/linkbcs", 0o755)
        os.chmod(f"{exp_dir}/logging.yaml", 0o755)
        os.chmod(f"{exp_dir}/forecasts/gcm_forecast.tmpl", 0o644)
        os.chmod(f"{exp_dir}/plot/gcm_plot.tmpl", 0o644)



def main():
    expConfig = ask_questions()
    experiment = setup(expConfig)
    experiment.initialize_models()
    experiment.check_flags()
    experiment.set_num_CPUs()
    experiment.config_models()
    experiment.set_some_stuff()
    experiment.set_nodes()
    experiment.set_stuff()
    experiment.create_dotfile(f"{os.environ.get('HOME')}/.EXPDIRroot", expConfig['exp_dir'])
    experiment.create_dotfile(f"{os.environ.get('HOME')}/.GROUProot", expConfig['group_root'])
    experiment.RC_setup()
    create_exp_yaml(expConfig)
    experiment.mpistacksettings()
    experiment.copy_files_into_exp()
    experiment.restarts()
    experiment.mod_RC_dir_for_pchem()
    experiment.config_chemGridComp()
    experiment.config_surfaceGridComp()
    experiment.config_gocartGridComp()
    experiment.config_heartbeat()
    experiment.template()
    experiment.organize_exp_dir()

if __name__ == "__main__":
    main()
