from env import answerdict
from utility import color

class land:
    def __init__(self):
        self.land_choice        = answerdict["LS_model"].q_answer
        self.bcs                = answerdict["LS_boundary_conditions"].q_answer
        self.bound_parameters   = None
        self.emip_BCS_IN        = None
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
        match self.bcs:
            case "Icarus":
                self.bound_parameters   = "#DELETE"
                self.emip_BCS_IN        = "Ganymed-4_0"
                self.emip_oldland       = ""
                self.emip_newland       = "#DELETE"
                self.emip_MERRA2        = "MERRA2"  
            case "Icarus-NLv3":
                self.bound_parameters   = ""
                self.emip_BCS_IN        = "Icarus-NLv3"
                self.emip_oldland       = "#DELETE"
                self.emip_newland       = ""
                self.emip_MERRA2        = "MERRA2_NewLand"  

    def set_catchment(self):
        if self.bcs == "Icarus-NLv3":
            match self.land_choice:
                case "Catchment":
                    self.HIST_catchment     = "#DELETE"
                    self.GCMRUN_catchment   = "#DELETE"
                case "CatchmentCN-CLM4.0":
                    self.HIST_catchment     = ""
                    self.GCMRUN_catchment   = ""
                    print(f"{color.RED}IMPORTANT: please set LAND_PARAMS: to CN_CLM40 in RC/GEOS_SurfaceGridComp.rc in the experiment directory.{color.RESET}")
                case "CatchmentCN-CLM4.5":
                    self.HIST_catchment     = ""
                    self.GCMRUN_catchment   = ""
                    print(f"{color.RED}IMPORTANT: please set LAND_PARAMS: to CN_CLM45 in RC/GEOS_SurfaceGridComp.rc in the experiment directory.{color.RESET}")
        else:
            self.land_choice        = "Catchment"
            self.HIST_catchment     = "#DELETE"
            self.GCMRUN_catchment   = "#DELETE"

    def config(self):
        self.set_bcs()
        self.set_catchment()








