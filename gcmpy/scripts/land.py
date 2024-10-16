from env import answerdict
from utility import color

class land:
    def __init__(self):
        self.model              = answerdict["LS_model"].q_answer
        self.bcs                = answerdict["LS_boundary_conditions"].q_answer
        self.parameters         = None
        self.emip_bcs_in        = None
        self.emip_oldland       = None
        self.emip_newland       = None
        self.emip_MERRA2        = None  
        self.HIST_catchment     = None
        self.GCMRUN_catchment   = None

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
        elif self.bcs == "NL3":
            self.parameters         = ""
            self.emip_bcs_in        = "NL3"
            self.emip_oldland       = "#DELETE"
            self.emip_newland       = ""
            self.emip_MERRA2        = "MERRA2_NewLand"  
        elif self.bcs == "v12":
            self.parameters         = ""
            self.emip_bcs_in        = "NL3"
            self.emip_oldland       = "#DELETE"
            self.emip_newland       = ""
            self.emip_MERRA2        = "MERRA2_NewLand"             


    def set_catchment(self):
        if self.model == "Catchment":
            self.land_choice        = 1
            self.HIST_catchment     = "#DELETE"
            self.GCMRUN_catchment   = "#DELETE"
        elif self.model == "CatchmentCN-CLM4.0":
            self.model              = 2
            self.HIST_catchment     = ""
            self.GCMRUN_catchment   = ""

    def config(self):
        self.set_bcs()
        self.set_catchment()








