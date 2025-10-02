from env import answerdict
from utility import color

class atmosphere:
    def __init__(self):
        self.use_SHMEM          = 0
        self.force_das          = "#"
        self.force_gcm          = "#"
        self.num_readers        = 1
        self.num_writers        = 1
        self.dt                 = answerdict['heartbeat'].q_answer
        self.dt_solar           = None
        self.dt_irrad           = None
        self.dt_ocean           = answerdict['heartbeat'].q_answer
        self.lm                 = int(answerdict['AM_vertical_res'].q_answer)
        self.im                 = int(answerdict['AM_horizontal_res'].q_answer[1:])
        self.jm                 = self.im * 6
        self.nx                 = None
        self.ny                 = None
        self.nf                 = 6
        self.microphysics       = answerdict["AM_microphysics"].q_answer
        self.hist_im            = self.im * 4
        self.hist_jm            = self.im * 2 + 1
        self.gridfile           = f"Gnomonic_c{self.im}.dat"
        self.job_sgmt           = None
        self.num_sgmt           = None
        self.res                = f"CF{self.im:04}x6C"
        self.post_NDS           = None
        self.nx_convert         = 2
        self.ny_convert         = 24
        self.conus              = '#'
        self.stretch_factor     = ''
        self.FV_stretch_fac     = ''
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
                self.conv_dt        = 3600
                self.chem_dt        = 3600
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
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
                self.res            = 'CF0012x6C'


            case "c24":
                self.conv_dt        = 1800
                self.chem_dt        = 3600
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 4
                self.ny             = self.nx * 6
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.nx_convert     = 1
                self.ny_convert     = 6
                self.res            = 'CF0024x6C'

            case "c48":
                self.conv_dt        = 1200
                self.chem_dt        = 3600
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 6
                self.ny             = self.nx * 6
                self.im_hist        = 180
                self.jm_hist        = 91
                self.job_sgmt       = f"{15:08}"
                self.num_sgmt       = 20
                self.post_NDS       = 4
                self.res            = 'CF0048x6C'
                self.hist_im        = 180
                self.hist_jm        = 91

            case "c90":
                self.conv_dt        = 900
                self.chem_dt        = 1800
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt
                if answerdict['OM_name'].q_answer == 'MIT':
                    self.nx     = 10
                    self.ny     = 36
                elif answerdict['OM_name'].q_answer == 'MOM5':
                    self.nx     = ocean_nx
                    self.ny     = ocean_ny
                elif answerdict['OM_name'].q_answer == 'MOM6':
                    self.nx     = 5
                    self.ny     = 36
                else:
                    self.nx     = 10
                    self.ny     = self.nx * 6
                    self.dt_ocean = 3600
                self.job_sgmt       = f"{32:08}"
                self.num_sgmt       = 4
                self.post_NDS       = 8
                self.res            = 'CF0090x6C'

            case "c180":
                self.conv_dt        = 600
                self.chem_dt        = 1200
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = self.dt
                if answerdict['OM_name'].q_answer == 'MOM6':
                    self.nx         = 30
                    self.ny         = 36
                elif answerdict['OM_name'].q_answer == 'MOM5' or answerdict['OM_name'].q_answer == 'MIT':
                    self.nx         = ocean_nx
                    self.ny         = ocean_ny
                else:
                    self.nx         = 20
                    self.ny         = self.nx * 6
                    self.dt_ocean   = 3600
                self.job_sgmt       = f"{16:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 8
                self.num_readers    = 2
                self.res            = 'CF0180x6C'

            case "c360":
                self.conv_dt        = 450
                self.chem_dt        = 900
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 4600
                self.nx             = 30
                self.ny             = self.nx * 6
                self.num_readers    = 4
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 12
                self.nx_convert     = 4
                self.res            = 'CF0360x6C'

            case "c720":
                self.conv_dt        = 300
                self.chem_dt        = 900
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 40
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.res            = 'CF0720x6C'

            case "c1120":
                self.conv_dt        = 300
                self.chem_dt        = 900
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 60
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.res            = 'CF1120x6C'

            case "c1440":
                self.conv_dt        = 225
                self.chem_dt        = 900
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = 1800
                self.nx             = 80
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.res            = 'CF1440x6C'

            case "c2880":
                self.conv_dt        = 150
                self.chem_dt        = 900
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = 1800
                self.nx             = 80
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.convpar_option = 'NONE'
                self.res            = 'CF2880x6C'

            case "c5760":
                self.conv_dt        = 75
                self.chem_dt        = 900
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = 1800
                self.nx             = 80
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = True
                self.convpar_option = 'NONE'
                self.res            = 'CF5760x6C'

            case "c270":
                self.conv_dt        = 600
                self.chem_dt        = 1800
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 20
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 2.5
                self.res            = 'CF0270x6C-SG001'

            case "c540":
                self.conv_dt        = 300
                self.chem_dt        = 900
                self.dt_solar       = 3600
                self.dt_irrad       = 3600
                self.dt_ocean       = 3600
                self.nx             = 30
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 2.5
                self.res            = 'CF0540x6C-SG001'

            case "c1080":
                self.conv_dt        = 300
                self.chem_dt        = 600
                self.dt_solar       = 1800
                self.dt_irrad       = 1800
                self.dt_ocean       = 1800
                self.nx             = 40
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{1:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 2.5
                self.res            = 'CF1080x6C-SG001'

            case "c1536":
                self.conv_dt        = 300
                self.chem_dt        = 900
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = 900
                self.nx             = 60
                self.ny             = self.nx * 6
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 16
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 3.0
                self.res            = 'CF1536x6C-SG002'

            case "c2160":
                self.conv_dt        = 300
                self.chem_dt        = 300
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = 900
                self.nx             = 80
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 2.5
                self.res            = 'CF2160x6C-SG001'

            case 'c4320':
                self.conv_dt        = 300
                self.chem_dt        = 300
                self.dt_solar       = 900
                self.dt_irrad       = 900
                self.dt_ocean       = 900
                self.nx             = 80
                self.ny             = self.nx * 6 * 2
                self.num_readers    = 6
                self.job_sgmt       = f"{5:08}"
                self.num_sgmt       = 1
                self.post_NDS       = 32
                self.nx_convert     = 8
                self.use_SHMEM      = 1
                self.conus          = ""
                self.stretch_factor = 2.5
                self.res            = 'CF4320x6C'



    def set_microphysics(self):
        if self.microphysics == "BACM_1M":
            self.BACM_1M = ""
            self.conv_dt = 450
            self.chem_dt = 3600
        elif self.microphysics == "GFDL_1M":
            self.GFDL_1M = ""
        elif self.microphysics == "MGB2_2M":
            self.MGB2_2M = ""

    def set_turnoff_wsub(self):
        if self.microphysics == "MGB2_2M":
            self.MP_turnoff_wsub = "#DELETE"
        else:
            self.MP_turnoff_wsub = ""

    def set_conus(self):
        if self.conus == "#":
            self.schmidt        = "do_schmidt  = .false."
            self.FV_stretch_fac = "stretch_fac = 1.0"
            self.target_lon     = "target_lon  = 0.0"
            self.target_lat     = "target_lat  = -90.0"
        else:
            self.schmidt        = "do_schmidt  = .true."
            self.FV_stretch_fac = f"stretch_fac = {self.stretch_factor}"
            self.target_lon     = "target_lon  = -98.35"
            self.target_lat     = "target_lat  = 39.5"
            self.FV_hwt         = ''

    def set_wsub_extdata(self):
        if self.microphysics == 'BACM_1M' or self.microphysics == 'GFDL_1M':
            self.mp_turn_off_wsub_extdata = ''
        else:
            self.mp_turn_off_wsub_extdata = '#DELETE#'

    # Set coarse resolution CLIM output
    def set_CLIM(self):
        self.CLIM_IM = 576
        self.CLIM_JM = 361
        if (self.CLIM_IM > self.im_hist):
            self.CLIM_IM = im_hist
            self.CLIM_JM = jm_hist

    def config(self, ocean_nx, ocean_ny):
        self.hres(ocean_nx, ocean_ny)
        self.set_microphysics()
        self.set_conus()
        self.set_wsub_extdata()
        self.set_CLIM()
