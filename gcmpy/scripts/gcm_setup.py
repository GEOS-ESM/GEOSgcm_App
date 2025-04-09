from ocean import ocean
from atmosphere import atmosphere as atmos
from land import land
from gocart import gocart
from env import answerdict, linkx
from utility import envdict, pathdict, color
import math, os, shutil, tempfile, yaml, re, glob
from pathlib import Path
from jinja2 import Environment, FileSystemLoader, Undefined


# combines all models (atmos, ocean, land, gocart) into one big one
class setup:
    def __init__(self):
        self.ocean              = ocean()
        self.atmos              = atmos()
        self.land               = land()
        self.gocart             = gocart()
        self.is_FCST            = False
        self.fv_cubed           = ''
        self.bcs_res            = None
        self.tile_data          = None
        self.tile_bin           = None
        self.interpolate_sst    = None
        self.job_sgmt           = None
        self.begin_date         = '18910301 000000'
        self.end_date           = '29990302 210000'
        self.n_oserver_nodes    = None
        self.n_backend_pes      = None
        self.n_nodes            = None
        self.exp_dir            = answerdict['exp_dir'].q_answer   
        self.restart_by_oserver = 'NO'
        self.gcm_version        = Path(f"{pathdict['etc']}/.AGCM_VERSION").read_text() 
        self.file_list          = ['gcm_run.j',
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

    def config_models(self):
        self.ocean.config()
        self.atmos.config(self.ocean.NX, self.ocean.NY)
        self.land.config()
        self.gocart.config()
        self.file_list.append(self.ocean.history_template)


    # setup some variables idk
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
        if answerdict['io_server'].q_answer == True:

            # First we calculate the number of model nodes
            n_model_nodes = math.ceil(model_npes / envdict["n_CPUs"])

            # Next the number of frontend PEs is 10% of the model PEs
            n_frontend_pes = math.ceil(model_npes * 0.1)

            # Now we roughly figure out the number of collections in the HISTORY.rc 
            n_hist_collections = 0
            with open(f"{pathdict['etc']}/{answerdict['history_template'].q_answer}", 'r') as file:
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
            n_oserver_nodes = math.ceil(n_oserver_pes / envdict["n_CPUs"])

            # The number of backend PEs is the number of history collections divided by the number of oserver nodes
            n_backend_pes = math.ceil(n_hist_collections / n_oserver_nodes)
            
            # multigroup requires at least two backend pes
            if (n_backend_pes < 2): n_backend_pes = 2

            # Calculate the total number of nodes to request from batch
            self.nodes = n_model_nodes + n_oserver_nodes

        else:
            self.nodes = math.ceil(model_npes / envdict["n_CPUs"])
            self.n_oserver_nodes = 0
            self.n_backend_pes   = 0


    
    def set_stuff(self):
        self.set_nodes()
        # Longer job names are now supported with SLURM and PBS. Limits seem to be 1024 characters with SLURM
        # and 230 with PBS. To be safe, we will limit to 200
        self.run_n     = f"{answerdict['experiment_id'].q_answer[:200]}_RUN"    # RUN      Job Name
        self.run_fn    = f"{answerdict['experiment_id'].q_answer[:200]}_FCST"   # Forecast Job Name
        self.post_n    = f"{answerdict['experiment_id'].q_answer[:200]}_POST"   # POST     Job Name
        self.plot_n    = f"{answerdict['experiment_id'].q_answer[:200]}_PLT"    # PLOT     Job Name
        self.move_n    = f"{answerdict['experiment_id'].q_answer[:200]}_PLT"    # MOVE     Job Name
        self.archive_n = f"{answerdict['experiment_id'].q_answer[:200]}_ARCH"   # ARCHIVE  Job Name
        self.regress_n = f"{answerdict['experiment_id'].q_answer[:200]}_RGRS"   # REGRESS  Job Name

        # Here we need to convert POST_NDS to total tasks. Using 16 cores
        # per task as a good default
        post_npes = self.atmos.post_NDS * 16
        NPCUS = (post_npes + envdict["n_CPUs"] - 1)/envdict["n_CPUs"]

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
            self.run_p            = f"PBS -l select={self.nodes}:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor'].q_answer}" 
            self.run_fp           = f"PBS -l select=24:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor'].q_answer}"          
            self.regress_p        = f"PBS -l select={self.nodes * 2}:ncpus=${NCPUS_PER_NODE_HALF}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor'].q_answer / 2}"
            self.post_q           = "PBS -q normal"                      
            self.plot_q           = "PBS -q normal"                     
            self.move_q           = "PBS -q normal"
            self.archive_q        = "PBS -q normal"
            self.post_p           = f"PBS -l select={NPCUS}:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor'].q_answer}"
            self.plot_p           = f"PBS -l select=1:ncpus={envdict['n_CPUs']}:mpiprocs=1:model={answerdict['processor'].q_answer}"
            self.archive_p        = f"PBS -l select=1:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor'].q_answer}"
            self.move_p           = "PBS -l select=1:ncpus=1"   
            self.boundary_path    = "/nobackup/gmao_SIteam/ModelData"
            self.bcs_dir          = f"{self.boundary_path}/bcs/{self.land.bcs}/{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID        = "ONLY_MERRA2_SUPPORTED"
            self.replay_ana_location     = "ONLY_MERRA2_SUPPORTED"
            self.M2_replay_ana_location  = f"{self.boundary_path}/merra2/data"

            # defines location of SST Boundary Conditions
            oceanres = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                self.sst_dir = f"{self.boundary_path}/fvInput/g5gcm/bcs/SST/{oceanres}"
            else:
                self.sst_dir = f"{self.boundary_path}/fvInput/g5gcm/bcs/realtime/@SSTNAME/{oceanres}"
            if self.ocean.gridtyp == "LL":
                self.sst_dir = "/nobackupp2/estrobac/geos5/SSTDIR"

            self.chem_dir         = f"{self.boundary_path}/fvInput_nc3"
            self.work_dir         = f"/nobackup/{os.environ.get('LOGNAME')}"       
            self.gwdrs_dir        = f"{self.boundary_path}/GWD_RIDGE"

            # Coupled Ocean/Atmos Forcing
            if self.ocean.model == "MIT":
                self.coupled_dir  = "/nobackupp2/estrobac/geos5/GRIDDIR"
            else:
                self.coupled_dir  = f"{boundary_path}/aogcm"
            

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
            self.run_q            = f"SBATCH --constraint={answerdict['processor'].q_answer}"
            self.run_p            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            self.run_fp           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            self.regress_p        = f"SBATCH --nodes={self.nodes * 2} --ntasks-per-node={envdict['n_CPUs'] / 2}"
            self.post_q           = f"SBATCH --constraint={answerdict['processor'].q_answer}"
            self.plot_q           = f"SBATCH --constraint={answerdict['processor'].q_answer}"
            self.move_q           = "SBATCH --partition=datamove"
            self.archive_q        = "SBATCH --partition=datamove"
            self.post_p           = f"SBATCH --nodes={NPCUS} --ntasks-per-node={envdict['n_CPUs']}"
            self.plot_p           = f"SBATCH --nodes=4 --ntasks=4"
            self.archive_p        = "SBATCH --ntasks=1"
            self.move_p           = "SBATCH --ntasks=1"
            self.boundary_path    = "/discover/nobackup/projects/gmao"
            self.bcs_dir          = f"{self.boundary_path}/bcs_shared/fvInput/ExtData/esm/tiles/{self.land.bcs}"
            self.replay_ana_expID       = "x0039"
            self.replay_ana_location    = f"{self.boundary_path}/g6dev/ltakacs/x0039"
            self.M2_replay_ana_location = f"{self.boundary_path}/merra2/data"


            # define location of SST Boundary Conditions
            oceanres    = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                self.sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/SST/{oceanres}"
            else:
                self.sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/realtime/@SSTNAME/{oceanres}"
            if self.ocean.gridtyp == "LL":
                self.sst_dir = "/discover/nobackup/estrobac/geos5/SSTDIR"

            self.chem_dir         = f"{os.environ.get('SHARE')}/gmao_ops/fvInput_nc3"
            self.work_dir         = f"/discover/nobackup/{os.environ.get('LOGNAME')}"
            self.gwdrs_dir        = f"{self.boundary_path}/osse2/stage/BCS_FILES/GWD_RIDGE"

            # Coupled Ocean/Atmos Forcing
            if self.ocean.model == "MIT":
                self.coupled_dir  = "/gpfsm/dnb32/estrobac/geos5/GRIDDIR"
            else:
                self.coupled_dir  = f"{self.boundary_path}/bcs_shared/make_bcs_inputs/ocean"


        elif envdict['site'] == "AWS" or envdict['SITE'] == "Azure":
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
            self.run_q            = f"SBATCH --constraint={answerdict['processor'].q_answer}"
            self.run_p            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}" 
            self.run_fp           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            self.regress_p        = f"SBATCH --nodes={self.nodes * 2} --ntasks-per-node={envdict['n_CPUs'] / 2}"
            self.post_q           = "NULL"
            self.plot_q           = "NULL"
            self.move_q           = "NULL"
            self.archive_q        = "NULL"
            self.post_p           = f"SBATCH --ntasks={post_npes}"
            self.plot_p           = f"SBATCH --nodes=4 --ntasks=4"
            self.archive_p        = "SBATCH --ntasks=1"
            self.move_p           = "SBATCH --ntasks=1"
            self.boundary_path    = "/ford1/share/gmao_SIteam/ModelData"
            self.bcs_dir          = f"{self.boundary_path}/bcs/{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID       = "REPLAY_UNSUPPORTED"
            self.replay_ana_location    = "REPLAY_UNSUPPORTED"
            self.M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            self.sst_dir          = f"{self.boundary_path}/@SSTNAME/{self.ocean.IM}x{self.ocean.JM}"
            self.chem_dir         = f"{self.boundary_path}/fvInput_nc3"
            self.work_dir         = os.environ.get('HOME')
            self.gwdrs_dir        = f"{self.boundary_path}/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_path}/aogcm"

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
            self.boundary_path    = "/ford1/share/gmao_SIteam/ModelData"
            self.bcs_dir          = f"{self.boundary_path}/bcs/{self.land.bcs} /{self.land.bcs}_{self.ocean.tag}"
            self.replay_ana_expID       = "REPLAY_UNSUPPORTED"
            self.replay_ana_location    = "REPLAY_UNSUPPORTED"
            self.M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            self.sst_dir          = f"{self.boundary_path}/@SSTNAME/{self.ocean.IM}x{self.ocean.JM}"
            self.chem_dir         = f"{self.boundary_path}/fvInput_nc3"
            self.work_dir         = os.environ.get('HOME') 
            self.gwdrs_dir        = f"{self.boundary_path}/GWD_RIDGE"
            self.coupled_dir      = f"{self.boundary_path}/aogcm"

            # By default on desktop, just ignore IOSERVER for now
            self.atmos.NX = 1
            self.atmos.NY = 6
            answerdict["io_server"].q_answer = False
            self.n_oserver_nodes = 0
            self.n_backend_pes = 0


        # For ICA and NL3 the gwd files are in a non-bcs location
        # and may or may not exist. If they don't we set NCAR_NRDG to 0
        self.NCAR_NRDG = 16
        if self.land.gwd_in_bcs == False and not os.path.exists(f"{self.gwdrs_dir}/gwd_internal_c{self.atmos.im}"):
            self.NCAR_NRDG = 0
            



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
        
        # Delete the destination directory if it exists
        if os.path.exists(RC_dir):
            shutil.rmtree(RC_dir)

        # Copy over all files and subdirs in install/etc, keeping symlinks, and ignoring *.tmpl files
        shutil.copytree(pathdict['etc'], RC_dir, symlinks=True, ignore=shutil.ignore_patterns('*.tmpl', 'fvcore.layout.rc'))

        # Copy or symlink GEOSgcm.x (((IGNORE SINGULARITY/NATIVE BUILDS FOR NOW!!)))
        geosgcmx_path = os.path.join(pathdict['bin'], 'GEOSgcm.x')
        if linkx == True:
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

        # restart by oserver if using openmpi or mvapich
        if envdict['mpi'] == 'openmpi' or envdict['mpi'] == 'mvapich':
            self.restart_by_oserver = 'YES'

        # retrieve config from correlating mpi setting being used
        self.mpi_config = mpidict.get(envdict['mpi'])

        # These are options determined to be useful at NCCS
        # Not setting generally as they are more fabric/cluster
        # specific compared to the above adjustments
        if envdict['site'] == 'NCCS':
            self.mpi_config += "\nsetenv I_MPI_SHM_HEAP_VSIZE 512 \
                                \nsetenv PSM2_MEMORY large"
        
        # as of right now, sigularity is not an option so this will always be added
        self.mpi_config += "\nsetenv I_MPI_EXTRA_FILESYSTEM 1 \
                            \nsetenv I_MPI_EXTRA_FILESYSTEM_FORCE gpfs"




    #######################################################################
    #            Create directories and copy files over 
    #######################################################################
    # A little helper function for copying files and displaying the info to the user
    def copy_helper(self, src, destination, filename):
        shutil.copy(src, destination)
        print(f"Creating {color.RED}{filename}{color.RESET} for Experiment: {answerdict['experiment_id'].q_answer}")

    def copy_files_into_exp(self):
        print("\n\n\n")

        for file in self.file_list:
            self.copy_helper(f"{pathdict['GEOSgcm_App']}/{file}", f"{self.exp_dir}/{file}", file)
       
        self.copy_helper(f"{pathdict['install']}/post/plot.rc", f"{self.exp_dir}/plot.rc", "plot.rc")
        self.copy_helper(f"{pathdict['install']}/post/post.rc", f"{self.exp_dir}/post.rc", "post.rc")

        # These files will be added if user chose to run coupled, regardless of ocean model selected.
        if self.ocean.running_ocean == True and self.ocean.model != 'MIT':
            self.copy_helper(f"{pathdict['install']}/coupled_diagnostics/g5lib/plotocn.j", f"{self.exp_dir}/plotocn.j", "plotocn.j")
            self.copy_helper(f"{pathdict['install']}/coupled_diagnostics/g5lib/confocn.py", f"{self.exp_dir}/__init__.py", "confocn.py")
            self.file_list.extend(['input.nml', 'diag_table','plotocn.j', '__init__.py'])

        if self.ocean.model == 'MOM5':
            self.file_list.append('field_table')
            self.copy_helper(f"{pathdict['etc']}/MOM5/geos5/{self.ocean.IM}x{self.ocean.JM}/INPUT/input.nml", f"{self.exp_dir}/input.nml", "input.nml")
            MOM5_path = os.path.join(pathdict['etc'], 'MOM5', 'geos5', f"{self.ocean.IM}x{self.ocean.JM}", 'INPUT', '*table')
            files = glob.glob(MOM5_path)
            for file in files: 
                file_name = os.path.basename(file)
                self.copy_helper(file, f"{self.exp_dir}/{file_name}", file_name)
        elif self.ocean.model == 'MOM6':
            self.file_list.extend(['MOM_input', 'MOM_override', 'data_table'])

            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/MOM_input", f"{self.exp_dir}/MOM_input", "MOM_input")
            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/MOM_override", f"{self.exp_dir}/MOM_override", "MOM_override")
            self.copy_helper(f"{pathdict['etc']}/MOM6/mom6_app/{self.ocean.IM}x{self.ocean.JM}/input.nml", f"{self.exp_dir}/input.nml", "input.nml")
            MOM6_path = os.path.join(pathdict['etc'], 'MOM6', 'mom6_app', f"{self.ocean.IM}x{self.ocean.JM}", '*table')
            files = glob.glob(MOM6_path)
            for file in files: 
                file_name = os.path.basename(file)
                self.copy_helper(file, f"{self.exp_dir}/{file_name}", file_name)

        if self.ocean.seaice_model == 'CICE6':
            self.copy_helper(f"{pathdict['etc']}/CICE6/cice6_app/{self.ocean.IM}x{self.ocean.JM}/ice_in", f"{self.exp_dir}/ice_in") 
            self.file_list.append('ice_in')

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
        
        with open(f"{answerdict['exp_dir'].q_answer}/AGCM.rc.tmpl", 'r') as file:
            file_content = file.read()

        # Template in a "#" if restart is set to false
        for rst in rsnames:
            for typ in rstypes:
                rst_string = f"{rst}_{typ}"
                comment = "" if rsnames[rst] else "#"
                file_content = file_content.replace(rst_string, f"{comment}{rst_string}")

        with open(f"{answerdict['exp_dir'].q_answer}/AGCM.rc.tmpl", 'w') as file:
            file.write(file_content)

    #######################################################################
    #      Modify RC Directory for LM and GOCART.data/GOCART Options
    #######################################################################
    def mod_RC_dir_for_pchem(self):
        if self.atmos.lm == 72:
            return
        
        rc_dir = f"{answerdict['exp_dir'].q_answer}/RC"
 
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

        chemgridcomp = f"{answerdict['exp_dir'].q_answer}/RC/GEOS_ChemGridComp.rc"
        with open(chemgridcomp, 'r') as file:
            file_content = file.read()

        # we always enable TR and gocart
        file_content = re.sub(r'(ENABLE_PCHEM:\s*\.).*(\.)', r'\1'+pchem+r'\2', file_content)
        file_content = re.sub(r'(ENABLE_TR:\s*\.).*(\.)', r'\1TRUE\2', file_content)
        file_content = re.sub(r'(ENABLE_GOCART_DATA:\s*\.).*(\.)', r'\1TRUE\2', file_content)

        with open(chemgridcomp, 'w') as file:
            file.write(file_content)

    # update LAND_PARAMS choices
    def config_surfaceGridComp(self):
        surfacegridcomp = f"{answerdict['exp_dir'].q_answer}/RC/GEOS_SurfaceGridComp.rc"
        with open(surfacegridcomp, 'r') as file:
            file_content = file.read()

        if self.land.model == 'CatchmentCN-CLM4.0':
            file_content = re.sub(r'(LAND_PARAMS:\s*).*', r'\1CN_CLM40', file_content)

        if self.land.bcs == 'ICA':
            file_content = re.sub(r'(LAND_PARAMS:\s*).*', r'\1Icarus', file_content)
            file_content = re.sub(r'(Z0_FORMULATION:\s*).*', r'\1 2', file_content)

        with open(surfacegridcomp, 'w') as file:
            file.write(file_content)

    # enable DATA_DRIVEN gocart2G
    def config_gocartGridComp(self):
        gocartgridcomp = f"{answerdict['exp_dir'].q_answer}/RC/GOCART2G_GridComp.rc"
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


    def config_heartbeat(self):

        # With MOM5 we need to change dt lines in input.nml to
        # use $OCEAN_DT instead. NOTE: This regex assumes integer followed by comma
        if self.ocean.model == 'MOM5':
            
            with open(f"{answerdict['exp_dir'].q_answer}/input.nml", 'r') as file:
                file_content = file.read()
            
            file_content = re.sub(r'dt_cpld\s*=\s*.*(,)', rf"dt_cpld = {self.atmos.dt_ocean}\1", file_content)
            file_content = re.sub(r'dt_atmos\s*=\s*.*(,)', rf"dt_atmos = {self.atmos.dt_ocean}\1", file_content)
            
            with open(f"{answerdict['exp_dir'].q_answer}/input.nml", 'w') as file:
                file.write(file_content)
            

        # We also must change the MOM_override file to
        # have consistent DTs with the AGCM. So we use OCEAN_DT
        # and change MOM_override to match. NOTE: This sed
        # assumes floating point number with a decimal
        if self.ocean.model == 'MOM6':
            with open(f"{answerdict['exp_dir'].q_answer}/MOM_override", 'r') as file:
                file_content = file.read()

            file_content = re.sub(r'DT\s*=\s*.*(,)', rf"DT = {self.atmos.dt_ocean}\1", file_content)
            file_content = re.sub(r'DT_THERM\s*=\s*.*(,)', rf"DT_THERM = {self.atmos.dt_ocean}\1", file_content)

            with open(f"{answerdict['exp_dir'].q_answer}/MOM_override", 'w') as file:
                file.write(file_content)


    # Templating helper function -- Removes lines marked with "#DELETE"
    def cleanup(self, file_path):
        with open(file_path, 'r') as file:
            content = file.read()

        content = re.sub(r'.*#DELETE.*\n?', r'', content)

        with open(file_path, 'w') as file:
            file.write(content)



    def template(self):  
        # this dictionary holds template values for the default jinja2 delimiter "{{ val }}"
        jinja_dict = {
            'SETENVS': self.mpi_config,
            'GCMVER': self.gcm_version,
            'EXPSRC': self.gcm_version,
            'EXPID': answerdict['experiment_id'].q_answer,
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
            'EXPDSC': answerdict['experiment_description'].q_answer,
            'HOMDIR': self.exp_dir,
            'BATCH_GROUP': self.batch_group,
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
            'BOUNDARY_DIR': self.boundary_path,
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
            'SOLAR_DT': self.atmos.dt_solar,
            'IRRAD_DT': self.atmos.dt_irrad,
            'OCEAN_DT': self.atmos.dt_ocean,
            'LONG_DT': self.atmos.dt_long,
            'NX': self.atmos.nx,
            'NY': self.atmos.ny,
            'USE_SHMEM': int(self.atmos.use_SHMEM),
            'USE_IOSERVER': int(answerdict['io_server'].q_answer),
            'NUM_OSERVER_NODES': self.n_oserver_nodes,
            'NUM_BACKEND_PES': self.n_backend_pes,
            'RESTART_BY_OSERVER': self.restart_by_oserver,
            'NCPUS_PER_NODE': envdict['n_CPUs'], 
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
            'HIST_CICE4': '#DELETE',
            'FVCUBED': '',
            'HIST_CATCHCN': self.land.HIST_catchment,
            'GCMRUN_CATCHCN': self.land.GCMRUN_catchment,
            'EMIP_OLDLAND': self.land.emip_oldland,
            'EMIP_NEWLAND': self.land.emip_newland,
            '_4DIAUDAS': '#DELETE',
            'REGULAR_REPLAY': '#',
            'REGULAR_REPLAY_GMAO': '#',
            'REGULAR_REPLAY_NCEP': '#DELETE',
            'REGULAR_REPLAY_ECMWF': '#DELETE'
        }
        

        exp_dir = answerdict['exp_dir'].q_answer

        # this is an edge-case that can't be handled with jinja2  
        for file in self.file_list:
            with open(f"{exp_dir}/{file}", 'r') as tmpl:
                file_content = tmpl.read()

            file_content = re.sub(r'[ \t]*RECORD_', r'#RECORD_', file_content)

            with open(f"{exp_dir}/{file}", 'w') as tmpl:
                tmpl.write(file_content) 

        # this block handles the default case for jinja templating
        default_env = Environment(
            loader=FileSystemLoader(exp_dir)
        )    
        for file in self.file_list:
            template = default_env.get_template(file)
            content = template.render(jinja_dict)
            with open(f"{exp_dir}/{file}", 'w') as tmpl:
                tmpl.write(content)

        # remove #DELETE lines
        for file in self.file_list:
            file_path = f"{exp_dir}/{file}"
            self.cleanup(file_path)


    # organize files into sub directories and update file permissions
    def organize_exp_dir(self):
        exp_dir = answerdict['exp_dir'].q_answer

        # make sub dirs
        sub_dirs = ['archive', 'forecasts', 'plot', 'post' , 'regress']
        for i in sub_dirs:
            os.makedirs(os.path.join(exp_dir, i), exist_ok=True)
        
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


my_exp = setup()
my_exp.config_models()
#my_exp.print_all_vars()
my_exp.set_some_stuff()
my_exp.set_nodes()
my_exp.set_stuff()
my_exp.create_dotfile(f"{os.environ.get('HOME')}/.EXPDIRroot", answerdict['exp_dir'].q_answer)
my_exp.create_dotfile(f"{os.environ.get('HOME')}/.GROUProot", answerdict['group_root'].q_answer)
my_exp.RC_setup()
my_exp.mpistacksettings()
my_exp.copy_files_into_exp()
my_exp.restarts()
my_exp.mod_RC_dir_for_pchem()
my_exp.config_chemGridComp()
my_exp.config_surfaceGridComp()
my_exp.config_gocartGridComp()
my_exp.config_heartbeat()
my_exp.template()
my_exp.organize_exp_dir()
