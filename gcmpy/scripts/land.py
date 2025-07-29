from env import answerdict
from utility import color

class land:
    def __init__(self):
        # If data atmosphere is enabled, land questions are skipped. Use default values.
        if answerdict['OM_data_atmos'].q_answer == True:
            self.model = "Catchment"
            self.bcs = "NL3"
        else:
            self.model              = answerdict["LS_model"].q_answer
            self.bcs                = answerdict["LS_boundary_conditions"].q_answer
        self.parameters         = ''
        self.emip_bcs_in        = ''
        self.emip_oldland       = ''
        self.emip_newland       = ''
        self.emip_MERRA2        = ''
        self.HIST_catchment     = ''
        self.GCMRUN_catchment   = ''

    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.CYAN}{var_name}: {var_value}{color.RESET}")

    def set_bcs(self):
        if self.bcs == "ICA":
            self.parameters         = "#DELETE"
            self.emip_bcs_in        = "GM4"
            self.emip_oldland       = ""
            self.emip_newland       = "#DELETE"
            self.emip_MERRA2        = "MERRA2"
            self.gwd_in_bcs         = False
        elif self.bcs == "NL3":
            self.parameters         = ""
            self.emip_bcs_in        = "NL3"
            self.emip_oldland       = "#DELETE"
            self.emip_newland       = ""
            self.emip_MERRA2        = "MERRA2_NewLand"
            self.gwd_in_bcs         = False
        elif self.bcs == "v12":
            self.parameters         = ""
            self.emip_bcs_in        = "v12"
            self.emip_oldland       = "#DELETE"
            self.emip_newland       = ""
            self.emip_MERRA2        = "MERRA2_NewLand"
            self.gwd_in_bcs         = True


    def set_catchment(self):
        if self.model == "Catchment":
            self.model              = 1
            self.HIST_catchment     = "#DELETE"
            self.GCMRUN_catchment   = "#DELETE"
        elif self.model == "CatchmentCN-CLM4.0":
            self.model              = 2
            self.HIST_catchment     = ""
            self.GCMRUN_catchment   = ""

    def config(self):
        self.set_bcs()
        self.set_catchment()








