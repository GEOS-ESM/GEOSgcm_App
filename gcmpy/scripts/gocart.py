from env import answerdict
from utility import color

class gocart:
    def __init__(self):
        self.aerosol            = answerdict["gocart_aerosols"].q_answer
        self.emissions          = f"{answerdict['gocart_emission'].q_answer}_EMISSIONS"
        self.data_driven        = None
        self.OPS_species        = "#"
        self.CMIP_species       = "#"
        self.MERRA2OX_species   = "#"
        self.pchem_clim_years   = ""
        self.gocart             = None
        self.gocart_hist        = None
        self.aero_provider      = "GOCART2G"
        self.RATS_provider      = "PCHEM"

    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.BLUE}{var_name}: {var_value}{color.RESET}")

    def set_gocart(self):
        match self.aerosol:
            case "Actual":
                self.data_driven    = False
                self.gocart         = ""
                self.gocart_hist    = ""
            case "Climatological":
                self.data_driven    = True
                self.gocart         = ""
                self.gocart_hist    = ""

    def set_emissions(self):
        match self.emissions.split("_")[0]:
            case "AMIP":
                self.MERRA2OX_species = ""
                self.pchem_clim_years = 1
            case "OPS":
                self.OPS_species      = ""
                self.pchem_clim_years = 39


    def config(self):
        self.set_gocart()
        self.set_emissions()

