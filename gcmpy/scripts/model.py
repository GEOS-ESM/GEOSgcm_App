from ocean import ocean
from atmosphere import atmosphere as atmos
from land import land
from gocart import gocart
from env import answerdict, linkx
from utility import envdict, pathdict
import math, os, shutil, tempfile, yaml
from pathlib import Path


# combines all models (atmos, ocean, land, gocart) into one big one
class model:
    def __init__(self):
        self.ocean              = ocean()
        self.atmos              = atmos()
        self.land               = land()
        self.gocart             = gocart()
        self.is_FCST            = False
        self.fv_cubed           = ""
        self.bcs_res            = None
        self.tile_data          = None
        self.tile_bin           = None
        self.interpolate_SST    = None
        self.job_sgmt           = None
        self.begin_date         = "18910301 000000"
        self.end_date           = "29990302 210000"
        self.n_oserver_nodes    = None
        self.n_backend_pes      = None
        self.n_nodes            = None
        self.exp_dir            = answerdict['exp_dir'].q_answer   
        self.oserver_restart    = "NO"
    

    def print_all_vars(self):
        self.atmos.print_vars()
        self.land.print_vars()
        self.gocart.print_vars()


    def config_models(self):
        self.ocean.config()
        self.atmos.config(self.ocean.NX, self.ocean.NY)
        self.land.config()
        self.gocart.config()


    # setup some variables idk
    def set_some_stuff(self):
        if self.atmos.IM_hist >= self.ocean.IM:
            self.interpolate_SST = True
        else:
            self.interpolate_SST = False
        self.bcs_res    = f"{self.atmos.res}_{self.ocean.res}"
        self.tile_data  = f"{self.atmos.res}_{self.ocean.res}_Pfafstetter.til"
        self.tile_bin   = f"{self.atmos.res}_{self.ocean.res}_Pfafstetter.TIL"
        self.job_sgmt   = f"{self.atmos.job_sgmt} 000000" 


    # setup experiment nodes
    def set_nodes(self):
        model_npes = self.atmos.NX * self.atmos.NY

        # Calculate OSERVER nodes based on recommended algorithm
        if answerdict["io_server"].q_answer == True:

            # First we calculate the number of model nodes
            n_model_nodes = math.ceil(model_NPES / envdict["n_CPUs"])

            # Next the number of frontend PEs is 10% of the model PEs
            n_frontend_pes = math.ceil(model_NPES * 0.1)

            # Now we roughly figure out the number of collections in the HISTORY.rc 
            n_hist_collections = 0
            with open(answerdict['history_template'].q_answer, 'r') as file:
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
            nodes = n_model_nodes + n_oserver_nodes

        else:
            self.nodes = math.ceil(model_npes / envdict["n_CPUs"])
            self.n_oserver_nodes = 0
            self.n_backend_pes   = 0


    
    def set_stuff(self):
        self.set_nodes()
        # Longer job names are now supported with SLURM and PBS. Limits seem to be 1024 characters with SLURM
        # and 230 with PBS. To be safe, we will limit to 200
        run_n     = f"{answerdict['experiment_id'].q_answer[:200]}_RUN"    # RUN      Job Name
        run_fn    = f"{answerdict['experiment_id'].q_answer[:200]}_FCST"   # Forecast Job Name
        post_n    = f"{answerdict['experiment_id'].q_answer[:200]}_POST"   # POST     Job Name
        plot_n    = f"{answerdict['experiment_id'].q_answer[:200]}_PLT"    # PLOT     Job Name
        move_n    = f"{answerdict['experiment_id'].q_answer[:200]}_PLT"    # MOVE     Job Name
        archive_n = f"{answerdict['experiment_id'].q_answer[:200]}_ARCH"   # ARCHIVE  Job Name
        regress_n = f"{answerdict['experiment_id'].q_answer[:200]}_RGRS"   # REGRESS  Job Name


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
        run_FT                  - Wallclock Time for gcm_forecast.j
        run_FT                  - Wallclock Time for gcm_run.j
        post_T                  - Wallclock Time for gcm_post.j
        plot_T                  - Wallclock Time for gcm_plot.j
        archive_T               - Wallclock Time for gcm_archive.j
        run_Q                   - Batch queue name for gcm_run.j
        run_P                   - PE Configuration for gcm_run.j
        run_FP                  - PE Configuration for gcm_forecast.j
        post_Q                  - Batch queue name for gcm_post.j
        plot_Q                  - Batch queue name for gcm_plot.j
        move_Q                  - Batch queue name for gcm_moveplot.j
        archive_Q               - Batch queue name for gcm_archive.j
        post_P                  - PE Configuration for gcm_post.j
        plot_P                  - PE Configuration for gcm_plot.j
        archive_P               - PE Configuration for gcm_archive.j
        move_P                  - PE Configuration for gcm_moveplot.j
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
            batch_cmd        = "qsub"                               
            batch_group      = "PBS -W group_list="                 
            batch_time       = "PBS -l walltime="                   
            batch_jobname    = "PBS -N"                             
            batch_outputname = "PBS -o "                            
            batch_joinouterr = "PBS -j oe -k oed"                   
            run_FT           = "6:00:00"                            
            run_T            = "8:00:00"                            
            post_T           = "8:00:00"                            
            plot_T           = "8:00:00"                            
            archive_T        = "8:00:00"                                                      
            run_Q            = f"PBS -q normal"                    
            run_P            = f"PBS -l select={self.nodes}:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor']}" 
            run_FP           = f"PBS -l select=24:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor']}"           
            post_Q           = "PBS -q normal"                      
            plot_Q           = "PBS -q normal"                     
            move_Q           = "PBS -q normal"
            archive_Q        = "PBS -q normal"
            post_P           = f"PBS -l select={NPCUS}:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor']}"
            plot_P           = f"PBS -l select=1:ncpus={envdict['n_CPUs']}:mpiprocs=1:model={answerdict['processor']}"
            archive_P        = f"PBS -l select=1:ncpus={envdict['n_CPUs']}:mpiprocs={envdict['n_CPUs']}:model={answerdict['processor']}"
            move_P           = "PBS -l select=1:ncpus=1"   
            boundary_path    = "/nobackup/gmao_SIteam/ModelData"
            bcs_dir          = f"{boundary_path}/bcs/{self.land.bcs}/{self.land.bcs}_{self.ocean.tag}"
            replay_ana_expID        = "ONLY_MERRA2_SUPPORTED"
            replay_ana_location     = "ONLY_MERRA2_SUPPORTED"
            M2_replay_ana_location  = f"{boundary_path}/merra2/data"#

            # defines location of SST Boundary Conditions
            oceanres = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                sst_dir = f"{boundary_path}/fvInput/g5gcm/bcs/SST/{oceanres}"
            else:
                sst_dir = f"{boundary_path}/fvInput/g5gcm/bcs/realtime/{self.ocean.sst_name}/{oceanres}"
            if self.ocean.gridtype_abrv == "LL":
                sst_dir = "/nobackupp2/estrobac/geos5/SSTDIR"

            chem_dir         = f"{boundary_path}/fvInput_nc3"
            work_dir         = f"/nobackup/{os.environ.get('LOGNAME')}"       
            gwdrs_dir        = f"{boundary_path}/GWD_RIDGE"

            # Coupled Ocean/Atmos Forcing
            if self.ocean.name == "MIT":
                coupled_dir  = "/nobackupp2/estrobac/geos5/GRIDDIR"
            else:
                coupled_dir  = f"{boundary_path}/aogcm"
            

        elif envdict['site'] == "NCCS":
            batch_cmd        = "sbatch"
            batch_group      = "SBATCH --account="
            batch_time       = "SBATCH --time="
            batch_jobname    = "SBATCH --job-name="
            batch_outputname = "SBATCH --output="
            batch_joinouterr = "DELETE"
            run_FT           = "06:00:00"
            run_T            = "12:00:00" 
            post_T           = "8:00:00" 
            plot_T           = "12:00:00" 
            archive_T        = "2:00:00"
            run_Q            = f"SBATCH --constraint={answerdict['processor']}"
            run_P            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            run_FP           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            post_Q           = f"SBATCH --constraint={answerdict['processor']}"
            plot_Q           = f"SBATCH --constraint={answerdict['processor']}"
            move_Q           = "SBATCH --partition=datamove"
            archive_Q        = "SBATCH --partition=datamove"
            post_P           = f"SBATCH --nodes={NPCUS} --ntasks-per-node={envdict['n_CPUs']}"
            plot_P           = f"SBATCH --nodes=4 --ntasks=4"
            archive_P        = "SBATCH --ntasks=1"
            move_P           = "SBATCH --ntasks=1"
            boundary_path    = "/discover/nobackup/projects/gmao"
            bcs_dir          = f"{boundary_path}bcs_shared/fvInput/ExtData/esm/tiles/{self.land.bcs}"
            replay_ana_expID       = "x0039"
            replay_ana_location    = f"{boundary_path}/g6dev/ltakacs/x0039"
            M2_replay_ana_location = f"{boundary_path}/merra2/data"


            # define location of SST Boundary Conditions
            oceanres    = f"{self.ocean.IM}x{self.ocean.JM}"
            if oceanres == "1440x720":
                sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/SST/{self.ocean.IM}x{self.ocean.JM}"
            else:
                sst_dir = f"{os.environ.get('SHARE')}/gmao_ops/fvInput/g5gcm/bcs/realtime/{self.ocean.sst_name}/{self.ocean.IM}x{self.ocean.JM}"
            if self.ocean.gridtype_abrv == "LL":
                sst_dir = "/discover/nobackup/estrobac/geos5/SSTDIR"

            chem_dir         = f"{os.environ.get('SHARE')}/gmao_ops/fvInput_nc3"
            work_dir         = f"/discover/nobackup/{os.environ.get('LOGNAME')}"
            gwdrs_dir        = f"{boundary_path}/osse2/stage/BCS_FILES/GWD_RIDGE"

            # Coupled Ocean/Atmos Forcing
            if self.ocean.name == "MIT":
                coupled_dir  = "/gpfsm/dnb32/estrobac/geos5/GRIDDIR"
            else:
                coupled_dir  = f"{boundary_path}/bcs_shared/make_bcs_inputs/ocean"


        elif envdict['site'] == "AWS" or envdict['SITE'] == "Azure":
            batch_cmd        = "sbatch" 
            batch_group      = "#DELETE"
            batch_time       = "SBATCH --time="
            batch_jobname    = "SBATCH --job-name="
            batch_outputname = "SBATCH --output="
            batch_joinouterr = "DELETE"
            run_FT           = "06:00:00"
            run_T            = "12:00:00"
            post_T           = "8:00:00"
            plot_T           = "12:00:00"
            archive_T        = "1:00:00"
            run_Q            = f"SBATCH --constraint={answerdict['processor']}"
            run_P            = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}" 
            run_FP           = f"SBATCH --nodes={self.nodes} --ntasks-per-node={envdict['n_CPUs']}"
            post_Q           = "NULL"
            plot_Q           = "NULL"
            move_Q           = "NULL"
            archive_Q        = "NULL"
            post_P           = f"SBATCH --ntasks={post_npes}"
            plot_P           = f"SBATCH --nodes=4 --ntasks=4"
            archive_P        = "SBATCH --ntasks=1"
            move_P           = "SBATCH --ntasks=1"
            boundary_path    = "/ford1/share/gmao_SIteam/ModelData"
            bcs_dir          = f"{boundary_path}/bcs/{self.land.bcs}_{self.ocean.tag}"
            replay_ana_expID       = "REPLAY_UNSUPPORTED"
            replay_ana_location    = "REPLAY_UNSUPPORTED"
            M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            sst_dir          = f"{boundary_path}/{self.ocean.sst_name}/{self.ocean.IM}x{self.ocean.JM}"
            chem_dir         = f"{boundary_path}/fvInput_nc3"
            work_dir         = os.environ.get('HOME')
            gwdrs_dir        = f"{boundary_path}/GWD_RIDGE"
            coupled_dir      = f"{boundary_path}/aogcm"

        else:
            # These are defaults for the desktop
            batch_cmd        = "sbatch"
            batch_group      = "SBATCH --account=" 
            batch_time       = "SBATCH --time=" 
            batch_jobname    = "SBATCH --job-name="
            batch_outputname = "SBATCH --output="
            batch_joinouterr = "DELETE"
            run_FT           = "06:00:00"
            run_T            = "12:00:00"
            post_T           = "8:00:00"
            plot_T           = "12:00:00"
            archive_T        = "1:00:00"
            run_Q            = "NULL"
            run_P            = "NULL"
            run_FP           = "NULL"
            post_Q           = "NULL"
            plot_Q           = "NULL"
            move_Q           = "NULL"
            archive_Q        = "NULL"
            post_P           = "NULL"
            plot_P           = "NULL"
            archive_P        = "NULL"
            move_P           = "NULL"
            boundary_path    = "/ford1/share/gmao_SIteam/ModelData"
            bcs_dir          = f"{boundary_path}/bcs/{self.land.bcs} /{self.land.bcs}_{self.ocean.tag}"
            replay_ana_expID       = "REPLAY_UNSUPPORTED"
            replay_ana_location    = "REPLAY_UNSUPPORTED"
            M2_replay_ana_location = "REPLAY_UNSUPPORTED"
            sst_dir          = f"{boundary_path}/{self.ocean.sst_name}/{self.ocean.IM}x{self.ocean.JM}"
            chem_dir         = f"{boundary_path}/fvInput_nc3"
            work_dir         = os.environ.get('HOME') 
            gwdrs_dir        = f"{boundary_path}/GWD_RIDGE"
            coupled_dir      = f"{boundary_path}/aogcm"

            # By default on desktop, just ignore IOSERVER for now
            self.atmos.NX = 1
            self.atmos.NY = 6
            answerdict["io_server"] = False
            self.n_oserver_nodes = 0
            self.n_backend_pes = 0

    '''
    def set_hist_temp(self):
        tmphist_d, tmphist_path = tempfile.mkstemp()
        print(self.ocean.history_template)
        shutil.copy(self.ocean.history_template, tmphist_path)
        return tmphist_d, tmphist_path 
    '''

    '''
    mainly used to create .{*}root files and/or populate them
    '''
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
        shutil.copytree(pathdict['etc'], RC_dir, symlinks=True, ignore=shutil.ignore_patterns('*.tmpl'))

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
        with open('mpi_config.yaml') as file:
            mpidict = yaml.load(file, Loader=yaml.FullLoader)

        # retrieve config from correlating mpi setting being used
        mpi_config = mpidict.get(envdict['mpi'])
        
        print(mpi_config)





mymodel = model()   
mymodel.config_models()
#mymodel.print_all_vars()
mymodel.set_nodes()
mymodel.set_stuff()
mymodel.create_dotfile(f"{os.environ.get('HOME')}/.HOMDIRroot", answerdict['home_dir'].q_answer)
mymodel.create_dotfile(f"{os.environ.get('HOME')}/.EXPDIRroot", answerdict['exp_dir'].q_answer)
mymodel.create_dotfile(f"{os.environ.get('HOME')}/.GROUProot", answerdict['group_root'].q_answer)
mymodel.RC_setup()
mymodel.mpistacksettings()
