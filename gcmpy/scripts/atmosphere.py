from env import answerdict
from utility import color

class atmosphere:
    def __init__(self):
        self.use_SHMEM          = False
        self.force_das          = "#"
        self.force_gcm          = "#"
        self.num_readers        = 1
        self.num_writers        = 1
        self.dt                 = answerdict["heartbeat"].q_answer
        self.dt_solar           = None
        self.dt_irrad           = None
        self.dt_ocean           = None
        self.dt_long            = None
        self.lm                 = int(answerdict['AM_vertical_res'].q_answer)
        self.im                 = int(answerdict['AM_horizontal_res'].q_answer[1:])
        self.jm                 = self.im * 6
        self.nx                 = None
        self.ny                 = None
        self.nf                 = 6
        self.use_hydrostatic    = answerdict["use_hydrostatic"].q_answer
        self.microphysics       = answerdict["AM_microphysics"].q_answer
        self.im_hist            = self.im * 4
        self.jm_hist            = self.jm * 2 + 1
        self.gridfile           = f"Gnomonic_c{self.im}.dat"
        self.job_sgmt           = None
        self.num_sgmt           = None
        self.res                = f"CF{self.im:04}x6C"
        self.post_NDS           = None
        self.nx_convert         = 2
        self.ny_convert         = 24
        self.conus              = "#"
        self.stretch_factor     = None
        self.gridname           = f"PE{self.im}x{self.jm}-CF"
        self.res_dateline       = f"{self.im}x{self.jm}"
        self.BACM_1M            = "#"
        self.GFDL_1M            = "#"
        self.MGB2_2M            = "#"
        self.GFDL_hydro         = ".TRUE."
        self.GFDL_prog_ccn      = "prog_ccn = .true."
        self.GFDL_use_ccn       = "use_ccn = .true."
        self.MP_turnoff_wsub    = None
        self.FV_make_NH         = None
        self.FV_hydro           = None
        self.FV_hwt             = None
        self.schmidt            = None
        self.target_lon         = None
        self.target_lat         = None
        self.convpar_option     = 'GF'
        self.mp_turn_off_wsub_extdata = None

        # These are superfluous for GCM, but are needed SCM (considered latlon)
        self.latlon             = '#DELETE'
        self.cube               = ''


    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.BLUE}{var_name}: {var_value}{color.RESET}")

    def hres(self, ocean_nx, ocean_ny):
        match answerdict["AM_horizontal_res"].q_answer:
            case "c12":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                if answerdict["OM_name"].q_answer == "MOM6":
                    self.nx         = 1
                else:
                    self.nx         = 2
                self.ny             = self.nx * 6
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.nx_convert     = 1
                self.ny_convert     = 6

            case "c24":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                self.nx             = 4
                self.ny             = self.nx * 6
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.nx_convert     = 1
                self.ny_convert     = 6

            case "c48":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                self.nx             = 4
                self.ny             = self.nx * 6
                self.im_hist        = 180
                self.jm_hist        = 91
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4

            case "c90":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_long        = self.dt
                match answerdict["OM_name"].q_answer:
                    case "MIT":
                        self.nx     = 10
                        self.ny     = 36
                        self.dt_ocean   = self.dt
                    case "MOM5","MOM6":
                        self.nx     = ocean_nx
                        self.ny     = ocean_ny
                        self.dt_ocean   = self.dt
                    case _:
                        self.nx     = 3
                        self.ny     = self.nx * 6
                        self.dt_ocean   = self.dt_irrad
                self.job_sgmt       = f"{32:08}"
                self.num_sgmt       = 4
                self.post_NDS       = 8

            case "c180":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_long        = self.dt
                if answerdict["OM_coupled"].q_answer == True:
                    self.nx         = ocean_nx
                    self.ny         = ocean_ny
                    self.dt_ocean   = self.dt
                else:
                    self.nx         = 6
                    self.ny         = self.nx * 6
                    self.dt_ocean   = self.dt_irrad
                self.job_sgmt       = f"{16:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 8
                self.num_readers    = 2

            case "c360":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                self.nx             = 12
                self.ny             = self.nx * 6
                self.num_readers    = 4
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 12
                self.nx_convert     = 4

            case "c720":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 450
                self.nx             = 24
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.nx_convert     = 8
                self.use_SHMEM      = True

            case "c1440":
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 48
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True

            case "c2880":
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 96
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.convpar_option = 'NONE'

            case "c5760":
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 192
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.convpar_option = 'NONE'

            case "c270":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                self.nx             = 18
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.conus          = ""
                self.stretch_factor = 2.5

            case "c540":
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = self.dt
                self.nx             = 36
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.conus          = ""
                self.stretch_factor = 2.5

            case "c1080":
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 72
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.conus          = ""
                self.stretch_factor = 2.

            case "c1536":
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 96
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.conus          = ""
                self.stretch_factor = 3.0

            case "c2160":
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = self.dt_irrad
                self.dt_long        = 300
                self.nx             = 192
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.conus          = ""
                self.stretch_factor = 2.5

        if answerdict["OM_name"].q_answer == "MIT":
            self.dt_ocean = self.dt

    def set_microphysics(self):
        match self.microphysics:
            case "BACM_1M":
                self.BACM_1M = ""
            case "GFDL_1M":
                self.GFDL_1M = ""
            case "MGB2_2M":
                self.MGB2_2M = ""

    def set_turnoff_wsub(self):
        if self.microphysics == "MGB2_2M":
            self.MP_turnoff_wsub = "#DELETE"
        else:
            self.MP_turnoff_wsub = ""

    # settings for fvcore_layour.rc
    def set_fvcore_layout(self):
        if self.use_hydrostatic == True:
            self.FV_make_NH = "Make_NH     = .F."
            self.FV_hydro   = "hydrostatic = .T."
            self.FV_hwt     = '#'
        else:
            self.FV_make_NH = "Make_NH     = .T."
            self.FV_hydro   = "hydrostatic = .F."
            self.FV_hwt     = ''
            if self.microphysics == "MGB2_2M":
                self.FV_hydro = ".FALSE."

    def set_conus(self):
        if self.conus == "#":
            self.schmidt        = "do_schmidt  = .false."
            self.stretch_factor = "stretch_fac = 1.0"
            self.target_lon     = "target_lon  = 0.0"
            self.target_lat     = "target_lat  = 0.0"
        else:
            self.schmidt        = "do_schmidt  = .true."
            self.stretch_factor = "stretch_fac = $STRETCH_FACTOR"
            self.target_lon     = "target_lon  = -98.35"
            self.target_lat     = "target_lat  = 39.5"

    def set_wsub_extdata(self):
        if self.microphysics == 'BACM_1M' or self.microphysics == 'GFDL_1M':
            self.mp_turn_off_wsub_extdata = ''
        else:
            self.mp_turn_off_wsub_extdata = '#DELETE#'

      
    def config(self, ocean_nx, ocean_ny):
        self.hres(ocean_nx, ocean_ny)
        self.set_microphysics()
        self.set_fvcore_layout()
        self.set_conus()
        self.set_wsub_extdata()
