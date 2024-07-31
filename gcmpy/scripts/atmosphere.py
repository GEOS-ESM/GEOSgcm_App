from env import answerdict
from utility import color

class atmosphere:
    def __init__(self):
        self.use_SHMEM          = False
        self.force_das          = "#"
        self.force_gcm          = "#"
        self.num_readers        = 1
        self.num_writers        = 1
        self.DT                 = answerdict["heartbeat"].q_answer
        self.DT_solar           = None
        self.DT_irrad           = None
        self.DT_ocean           = None
        self.DT_long            = None
        self.IM                 = int(answerdict["AM_horizontal_res"].q_answer[1:])
        self.JM                 = self.IM * 6
        self.NX                 = None
        self.NY                 = None
        self.use_hydrostatic    = answerdict["use_hydrostatic"].q_answer
        self.microphysics       = answerdict["AM_microphysics"].q_answer
        self.IM_hist            = self.IM * 4
        self.JM_hist            = self.JM * 2 + 1
        self.gridfile           = f"Gnomonic_c{self.IM}.dat"
        self.job_sgmt           = None
        self.num_sgmt           = None
        self.res                = f"CF{self.IM:04}x6C"
        self.post_NDS           = None
        self.NX_convert         = 2
        self.NY_convert         = 24
        self.CONUS              = "#"
        self.stretch_factor     = None
        self.gridname           = f"PE{self.IM}x{self.JM}-CF"
        self.res_dateline       = f"{self.IM}x{self.JM}"
        self.BACM_1M            = "#"
        self.GFDL_1M            = "#"
        self.MGB2_2M            = "#"
        self.GFDL_hydro         = ".TRUE."
        self.GFDL_prog_ccn      = "prog_ccn = .true."
        self.GFDL_use_ccn       = "use_ccn = .true."
        self.MP_turnoff_wsub    = None
        self.FV_make_NH         = None
        self.FV_hydro           = None
        self.schmidt            = None
        self.target_lon         = None
        self.target_lat         = None

    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.BLUE}{var_name}: {var_value}{color.RESET}")

    def hres(self, ocean_NX, ocean_NY):
        match answerdict["AM_horizontal_res"].q_answer:
            case "c12":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                if answerdict["OM_name"].q_answer == "MOM6":
                    self.NX         = 1
                else:
                    self.NX         = 2
                self.NY             = self.NX * 6
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.NX_convert     = 1
                self.NY_convert     = 6

            case "c24":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                self.NX             = 4
                self.NY             = self.NX * 6
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.NX_convert     = 1
                self.NY_convert     = 6

            case "c48":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                self.NX             = 4
                self.NY             = self.NX * 6
                self.IM_hist        = 180
                self.JM_hist        = 91
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4

            case "c90":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_long        = self.DT
                match answerdict["OM_name"].q_answer:
                    case "MIT":
                        self.NX     = 10
                        self.NY     = 36
                        self.DT_ocean   = self.DT
                    case "MOM5","MOM6":
                        self.NX     = ocean_NX
                        self.NY     = ocean_NY
                        self.DT_ocean   = self.DT
                    case _:
                        self.NX     = 3
                        self.NY     = self.NX * 6
                        self.DT_ocean   = self.DT_irrad
                self.job_sgmt       = f"{32:08}"
                self.num_sgmt       = 4
                self.post_NDS       = 8

            case "c180":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_long        = self.DT
                if answerdict["OM_coupled"].q_answer == True:
                    self.NX         = ocean_NX
                    self.NY         = ocean_NY
                    self.DT_ocean   = self.DT
                else:
                    self.NX         = 6
                    self.NY         = self.NX * 6
                    self.DT_ocean   = self.DT_irrad
                self.job_sgmt       = f"{16:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 8
                self.num_readers    = 2

            case "c360":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                self.NX             = 12
                self.NY             = self.NX * 6
                self.num_readers    = 4
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 12
                self.NX_convert     = 4

            case "c720":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 450
                self.NX             = 24
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.NX_convert     = 8
                self.use_SHMEM      = True

            case "c1440":
                self.DT_solar       = 1800
                self.DT_irrad       = 1800
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 48
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True

            case "c2880":
                self.DT_solar       = 1800
                self.DT_irrad       = 1800
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 96
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True

            case "c5760":
                self.DT_solar       = 900
                self.DT_irrad       = 900
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 192
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True

            case "c270":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                self.NX             = 18
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True
                self.CONUS          = ""
                self.stretch_factor = 2.5

            case "c540":
                self.DT_solar       = 3600
                self.DT_irrad       = 3600
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = self.DT
                self.NX             = 36
                self.NY             = self.NX * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True
                self.CONUS          = ""
                self.stretch_factor = 2.5

            case "c1080":
                self.DT_solar       = 900
                self.DT_irrad       = 900
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 72
                self.NY             = self.NX * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True
                self.CONUS          = ""
                self.stretch_factor = 2.

            case "c1536":
                self.DT_solar       = 900
                self.DT_irrad       = 900
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 96
                self.NY             = self.NX * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.NX_convert     = 8
                self.use_SHMEM      = True
                self.CONUS          = ""
                self.stretch_factor = 3.0

            case "c2160":
                self.DT_solar       = 900
                self.DT_irrad       = 900
                self.DT_ocean       = self.DT_irrad
                self.DT_long        = 300
                self.NX             = 192
                self.NY             = self.NX * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.NX_convert     = 8
                self.use_SHMEM      = True
                self.CONUS          = ""
                self.stretch_factor = 2.5

        if answerdict["OM_name"].q_answer == "MIT":
            self.DT_ocean = self.DT

    def set_microphysics(self):
        match self.microphysics:
            case "BACM_1M":
                self.BACM_1M = ""
                self.DT_long = 450
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
        match self.use_hydrostatic:
            case True:
                self.FV_make_NH = "Make_NH     = .F."
                self.FV_hydro   = "hydrostatic = .T."
            case False:
                self.FV_make_NH = "Make_NH     = .T."
                self.FV_hydro   = "hydrostatic = .F."
                if self.microphysics == "MGB2_2M":
                    self.FV_hydro = ".FALSE."

    def set_CONUS(self):
        if self.CONUS == "#":
            self.schmidt        = "do_schmidt  = .false."
            self.stretch_factor = "stretch_fac = 1.0"
            self.target_lon     = "target_lon  = 0.0"
            self.target_lat     = "target_lat  = 0.0"
        else:
            self.schmidt        = "do_schmidt  = .true."
            self.stretch_factor = "stretch_fac = $STRETCH_FACTOR"
            self.target_lon     = "target_lon  = -98.35"
            self.target_lat     = "target_lat  = 39.5"

      
    def config(self, ocean_NX, ocean_NY):
        self.hres(ocean_NX, ocean_NY)
        self.set_microphysics()
        self.set_fvcore_layout()
        self.set_CONUS()
