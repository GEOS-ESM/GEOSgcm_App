from utility import color

class land:
    def __init__(land, expConfig):
        land.expConfig = expConfig
        # If data atmosphere is enabled, land questions are skipped. Use default values.
        if land.expConfig['OM_data_atmos'] == True:
            land.model = "Catchment"
            land.bcs = "NL3"
        else:
            land.model              = land.expConfig["LS_model"]
            land.bcs                = land.expConfig["LS_boundary_conditions"]
        land.parameters         = ''
        land.emip_bcs_in        = ''
        land.emip_oldland       = ''
        land.emip_newland       = ''
        land.emip_MERRA2        = ''
        land.HIST_catchment     = ''
        land.GCMRUN_catchment   = ''

    def set_bcs(land):
        if land.bcs == "ICA":
            land.parameters         = "#DELETE"
            land.emip_bcs_in        = "GM4"
            land.emip_oldland       = ""
            land.emip_newland       = "#DELETE"
            land.emip_MERRA2        = "MERRA2"
            land.gwd_in_bcs         = False
        elif land.bcs == "NL3":
            land.parameters         = ""
            land.emip_bcs_in        = "NL3"
            land.emip_oldland       = "#DELETE"
            land.emip_newland       = ""
            land.emip_MERRA2        = "MERRA2_NewLand"
            land.gwd_in_bcs         = False
        elif land.bcs == "v12":
            land.parameters         = ""
            land.emip_bcs_in        = "NL3"
            land.emip_oldland       = "#DELETE"
            land.emip_newland       = ""
            land.emip_MERRA2        = "MERRA2_NewLand"
            land.gwd_in_bcs         = True


    def set_catchment(land):
        if land.model == "Catchment":
            land.model              = 1
            land.HIST_catchment     = "#DELETE"
            land.GCMRUN_catchment   = "#DELETE"
        elif land.model == "CatchmentCN-CLM4.0":
            land.model              = 2
            land.HIST_catchment     = ""
            land.GCMRUN_catchment   = ""

    def config(land):
        land.set_bcs()
        land.set_catchment()








