from env import answerdict
from utility import color

class gocart:
    def __init__(self):
        self.aerosol            = answerdict["gocart_aerosols"].q_answer
        self.emissions          = f"{answerdict['gocart_emission'].q_answer}_EMISSIONS"
        self.data_driven        = None
        self.ops_species        = '#'
        self.cmip_species       = '#'
        self.MERRA2OX_species   = '#'
        self.pchem_clim_years   = ''
        self.ox_relaxtime       = None
        self.gocart             = ''
        self.gocart_hist        = None
        self.aero_provider      = 'GOCART2G'
        self.rats_provider      = 'PCHEM'

    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.BLUE}{var_name}: {var_value}{color.RESET}")

    def set_gocart(self):
        if self.aerosol == 'Actual':
            self.data_driven    = False
            self.gocart         = ''
            self.gocart_hist    = ''
        elif self.aerosol == 'Climatological':
            self.data_driven    = True
            self.gocart         = '#'
            self.gocart_hist    = '#DELETE'

    def set_emissions(self):
        if self.emissions.split('_')[0] == 'AMIP':
            self.MERRA2OX_species = ''
            self.pchem_clim_years = 1
            self.ox_relaxtime = '0.00'
        elif self.emissions.split('_')[0] == 'OPS':
            self.ops_species      = ''
            self.pchem_clim_years = 39
            self.ox_relaxtime = '259200.'


    def config(self):
        self.set_gocart()
        self.set_emissions()

