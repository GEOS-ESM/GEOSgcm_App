from utility import color

class atmosphere:
    def __init__(atmos, expConfig):
        atmos.expConfig          = expConfig
        atmos.use_SHMEM          = 0
        atmos.force_das          = "#"
        atmos.force_gcm          = "#"
        atmos.num_readers        = 1
        atmos.num_writers        = 1
        atmos.dt                 = atmos.expConfig['heartbeat']
        atmos.dt_solar           = None
        atmos.dt_irrad           = None
        atmos.dt_ocean           = atmos.expConfig['heartbeat']
        atmos.lm                 = int(atmos.expConfig['AM_vertical_res'])
        atmos.im                 = int(atmos.expConfig['AM_horizontal_res'][1:])
        atmos.jm                 = atmos.im * 6
        atmos.nx                 = None
        atmos.ny                 = None
        atmos.nf                 = 6
        atmos.microphysics       = atmos.expConfig["AM_microphysics"]
        atmos.hist_im            = atmos.im * 4
        atmos.hist_jm            = atmos.im * 2 + 1
        atmos.gridfile           = f"Gnomonic_c{atmos.im}.dat"
        atmos.job_sgmt           = None
        atmos.num_sgmt           = None
        atmos.res                = f"CF{atmos.im:04}x6C"
        atmos.post_NDS           = None
        atmos.nx_convert         = 2
        atmos.ny_convert         = 24
        atmos.conus              = '#'
        atmos.stretch_factor     = ''
        atmos.FV_stretch_fac     = ''
        atmos.gridname           = f"PE{atmos.im}x{atmos.jm}-CF"
        atmos.res_dateline       = f"{atmos.im}x{atmos.jm}"
        atmos.BACM_1M            = "#"
        atmos.GFDL_1M            =     atmos.MGB2_2M            = "#"
        atmos.GFDL_hydro         = ".TRUE."
        atmos.GFDL_prog_ccn      = "prog_ccn = .true."
        atmos.GFDL_use_ccn       = "use_ccn = .true."
        atmos.MP_turnoff_wsub    = None
        atmos.FV_make_NH         = None
        atmos.FV_hydro           = None
        atmos.FV_hwt             = None
        atmos.schmidt            = None
        atmos.target_lon         = None
        atmos.target_lat         = None
        atmos.convpar_option     = 'GF'
        atmos.mp_turn_off_wsub_extdata = None
        atmos.low_res            = False

        # These are superfluous for GCM, but are needed SCM (considered latlon)
        atmos.latlon             = '#DELETE'
        atmos.cube               = ''


    def hres(atmos, ocean_nx, ocean_ny):
        match atmos.expConfig["AM_horizontal_res"]:
            case "c12":
                atmos.conv_dt        = 3600
                atmos.chem_dt        = 3600
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                if atmos.expConfig["OM_name"] == "MOM6":
                    atmos.nx         = 1
                else:
                    atmos.nx         = 2
                atmos.ny             = atmos.nx * 6
                atmos.job_sgmt       = f"{15:08}"
                atmos.num_sgmt       = 20
                atmos.post_NDS       = 4
                atmos.nx_convert     = 1
                atmos.ny_convert     = 6
                atmos.res            = 'CF0012x6C'
                atmos.low_res        = True

            case "c24":
                atmos.conv_dt        = 1800
                atmos.chem_dt        = 3600
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 4
                atmos.ny             = atmos.nx * 6
                atmos.job_sgmt       = f"{15:08}"
                atmos.num_sgmt       = 20
                atmos.post_NDS       = 4
                atmos.nx_convert     = 1
                atmos.ny_convert     = 6
                atmos.res            = 'CF0024x6C'
                atmos.low_res        = True

            case "c48":
                atmos.conv_dt        = 1200
                atmos.chem_dt        = 3600
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 6
                atmos.ny             = atmos.nx * 6
                atmos.job_sgmt       = f"{15:08}"
                atmos.num_sgmt       = 20
                atmos.post_NDS       = 4
                atmos.res            = 'CF0048x6C'
                atmos.hist_im        = 180
                atmos.hist_jm        = 91
                atmos.low_res        = True

            case "c90":
                atmos.conv_dt        = 900
                atmos.chem_dt        = 1800
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = atmos.dt
                if atmos.expConfig['OM_name'] == 'MIT':
                    atmos.nx     = 10
                    atmos.ny     = 36
                elif atmos.expConfig['OM_name'] == 'MOM5':
                    atmos.nx     = ocean_nx
                    atmos.ny     = ocean_ny
                elif atmos.expConfig['OM_name'] == 'MOM6':
                    atmos.nx     = 5
                    atmos.ny     = 36
                else:
                    atmos.nx     = 10
                    atmos.ny     = atmos.nx * 6
                    atmos.dt_ocean = 3600
                atmos.job_sgmt       = f"{32:08}"
                atmos.num_sgmt       = 4
                atmos.post_NDS       = 8
                atmos.res            = 'CF0090x6C'

            case "c180":
                atmos.conv_dt        = 600
                atmos.chem_dt        = 1200
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = atmos.dt
                if atmos.expConfig['OM_name'] == 'MOM6':
                    atmos.nx         = 30
                    atmos.ny         = 36
                elif atmos.expConfig['OM_name'] == 'MOM5' or atmos.expConfig['OM_name'] == 'MIT':
                    atmos.nx         = ocean_nx
                    atmos.ny         = ocean_ny
                else:
                    atmos.nx         = 20
                    atmos.ny         = atmos.nx * 6
                    atmos.dt_ocean   = 3600
                atmos.job_sgmt       = f"{16:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 8
                atmos.num_readers    = 2
                atmos.res            = 'CF0180x6C'

            case "c360":
                atmos.conv_dt        = 450
                atmos.chem_dt        = 900
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 4600
                atmos.nx             = 30
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 4
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 12
                atmos.nx_convert     = 4
                atmos.res            = 'CF0360x6C'

            case "c720":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 600
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 40
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 16
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.res            = 'CF0720x6C'

            case "c1120":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 600
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 60
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 16
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.res            = 'CF1120x6C'

            case "c1440":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 600
                atmos.dt_solar       = 1200
                atmos.dt_irrad       = 1200
                atmos.dt_ocean       = 1200
                atmos.nx             = 80
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.res            = 'CF1440x6C'

            case "c2880":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 300
                atmos.dt_solar       = 900
                atmos.dt_irrad       = 900
                atmos.dt_ocean       = 900
                atmos.nx             = 80
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = True
                atmos.convpar_option = 'NONE'
                atmos.res            = 'CF2880x6C'

            case "c5760":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 300
                atmos.dt_solar       = 600
                atmos.dt_irrad       = 600
                atmos.dt_ocean       = 600
                atmos.nx             = 80
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = True
                atmos.convpar_option = 'NONE'
                atmos.res            = 'CF5760x6C'

            case "c270":
                atmos.conv_dt        = 600
                atmos.chem_dt        = 1800
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 20
                atmos.ny             = atmos.nx * 6 * 2
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 2.5
                atmos.res            = 'CF0270x6C-SG001'

            case "c540":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 900
                atmos.dt_solar       = 3600
                atmos.dt_irrad       = 3600
                atmos.dt_ocean       = 3600
                atmos.nx             = 30
                atmos.ny             = atmos.nx * 6 * 2
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 2.5
                atmos.res            = 'CF0540x6C-SG001'

            case "c1080":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 600
                atmos.dt_solar       = 1800
                atmos.dt_irrad       = 1800
                atmos.dt_ocean       = 1800
                atmos.nx             = 40
                atmos.ny             = atmos.nx * 6 * 2
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{1:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 2.5
                atmos.res            = 'CF1080x6C-SG001'

            case "c1536":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 900
                atmos.dt_solar       = 900
                atmos.dt_irrad       = 900
                atmos.dt_ocean       = 900
                atmos.nx             = 60
                atmos.ny             = atmos.nx * 6
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 16
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 3.0
                atmos.res            = 'CF1536x6C-SG002'

            case "c2160":
                atmos.conv_dt        = 300
                atmos.chem_dt        = 300
                atmos.dt_solar       = 900
                atmos.dt_irrad       = 900
                atmos.dt_ocean       = 900
                atmos.nx             = 80
                atmos.ny             = atmos.nx * 6 * 2
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 2.5
                atmos.res            = 'CF2160x6C-SG001'

            case 'c4320':
                atmos.conv_dt        = 300
                atmos.chem_dt        = 300
                atmos.dt_solar       = 900
                atmos.dt_irrad       = 900
                atmos.dt_ocean       = 900
                atmos.nx             = 80
                atmos.ny             = atmos.nx * 6 * 2
                atmos.num_readers    = 6
                atmos.job_sgmt       = f"{5:08}"
                atmos.num_sgmt       = 1
                atmos.post_NDS       = 32
                atmos.nx_convert     = 8
                atmos.use_SHMEM      = 1
                atmos.conus          = ""
                atmos.stretch_factor = 2.5
                atmos.res            = 'CF4320x6C'



    def set_microphysics(atmos):
        if atmos.microphysics == "BACM_1M":
            atmos.BACM_1M = ""
            atmos.conv_dt = 450
            atmos.chem_dt = 3600
        elif atmos.microphysics == "GFDL_1M":
            atmos.GFDL_1M = ""
        elif atmos.microphysics == "MGB2_2M":
            atmos.MGB2_2M = ""

    def set_turnoff_wsub(atmos):
        if atmos.microphysics == "MGB2_2M":
            atmos.MP_turnoff_wsub = "#DELETE"
        else:
            atmos.MP_turnoff_wsub = ""

    def set_conus(atmos):
        if atmos.conus == "#":
            atmos.schmidt        = "do_schmidt  = .false."
            atmos.FV_stretch_fac = "stretch_fac = 1.0"
            atmos.target_lon     = "target_lon  = 0.0"
            atmos.target_lat     = "target_lat  = -90.0"
        else:
            atmos.schmidt        = "do_schmidt  = .true."
            atmos.FV_stretch_fac = f"stretch_fac = {atmos.stretch_factor}"
            atmos.target_lon     = "target_lon  = -98.35"
            atmos.target_lat     = "target_lat  = 39.5"
            atmos.FV_hwt         = ''

    def set_wsub_extdata(atmos):
        if atmos.microphysics == 'BACM_1M' or atmos.microphysics == 'GFDL_1M':
            atmos.mp_turn_off_wsub_extdata = ''
        else:
            atmos.mp_turn_off_wsub_extdata = '#DELETE#'

    # Set coarse resolution CLIM output
    def set_CLIM(atmos):
        atmos.CLIM_IM = 576
        atmos.CLIM_JM = 361
        if (atmos.CLIM_IM > atmos.hist_im):
            atmos.CLIM_IM = atmos.hist_im
            atmos.CLIM_JM = atmos.hist_jm

    def config(atmos, ocean_nx, ocean_ny):
        atmos.hres(ocean_nx, ocean_ny)
        atmos.set_microphysics()
        atmos.set_conus()
        atmos.set_wsub_extdata()
        atmos.set_CLIM()
