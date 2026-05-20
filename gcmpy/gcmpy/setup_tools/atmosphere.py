from gcmpy.utils.color_ops import Color
from gcmpy.geos_settings.horizontal_resolution import get_horiz_res_settings

class Atmosphere:
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
        atmos.GFDL_1M            = "#"
        atmos.MGB2_2M            = "#"
        atmos.RRTMGP_RADIATION   = ""
        atmos.RRTMG_RADIATION    = "#DELETE"
        atmos.KLID               = ""
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
        hres_value = atmos.expConfig["AM_horizontal_res"]
        hres_settings = get_horiz_res_settings()
        hres_settings = hres_settings[hres_value]
        for key, value in hres_settings.items():
            setattr(atmos, key, value)

        # Now treat special cases
        match hres_value:
            case "c12":
                if atmos.expConfig["OM_name"] == "MOM6":
                    atmos.nx = 1
                    atmos.ny = atmos.nx * 6

            case "c90":
                if atmos.expConfig['OM_name'] == 'MIT':
                    atmos.nx = 10
                    atmos.ny = 36
                    atmos.dt_ocean = atmos.dt
                elif atmos.expConfig['OM_name'] == 'MOM5':
                    atmos.nx = ocean_nx
                    atmos.ny = ocean_ny
                    atmos.dt_ocean = atmos.dt
                elif atmos.expConfig['OM_name'] == 'MOM6':
                    atmos.nx = 5
                    atmos.ny = 36
                    atmos.dt_ocean = atmos.dt

            case "c180":
                if atmos.expConfig['OM_name'] == 'MOM6':
                    atmos.nx = 30
                    atmos.ny = 36
                    atmos.dt_ocean = atmos.dt
                elif atmos.expConfig['OM_name'] == 'MOM5' or atmos.expConfig['OM_name'] == 'MIT':
                    atmos.nx = ocean_nx
                    atmos.ny = ocean_ny
                    atmos.dt_ocean = atmos.dt

    def set_microphysics(atmos):
        if atmos.microphysics == "BACM_1M":
            atmos.BACM_1M = ""
            atmos.conv_dt = 450
            atmos.chem_dt = 450
            atmos.RRTMGP_RADIATION = "#DELETE"
            atmos.RRTMG_RADIATION  = ""
            if atmos.lm >= 181:
                atmos.KLID = "19.0"
            elif atmos.lm == 137:
                atmos.KLID = "14.0"
            elif atmos.lm == 91:
                atmos.KLID = "13.0"
            elif atmos.lm == 72:
                atmos.KLID = "14.0"
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
