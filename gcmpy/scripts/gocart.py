from utility import color

class gocart:
    def __init__(gocart, expConfig):
        gocart.expConfig = expConfig
        # Gocart questions are skipped if data atmosphere is turned on. Use defaults
        if gocart.expConfig['OM_data_atmos'] == True:
            gocart.aerosol = 'Actual'
            gocart.emissions = 'OPS_EMISSIONS'
        else:
            gocart.aerosol = gocart.expConfig["gocart_aerosols"]
            gocart.emissions = f"{gocart.expConfig['gocart_emission']}_EMISSIONS"
        gocart.data_driven        = None
        gocart.ops_species        = '#'
        gocart.cmip_species       = '#'
        gocart.MERRA2OX_species   = '#'
        gocart.pchem_clim_years   = ''
        gocart.ox_relaxtime       = None
        gocart.gocart             = ''
        gocart.gocart_hist        = None
        gocart.aero_provider      = 'GOCART2G'
        gocart.rats_provider      = 'PCHEM'

        # Additional RATS settings for specific GHGs
        gocart.ch4_provider       = 'none'
        gocart.c02_provider       = 'none'

    # for debugging purposes
    def print_vars(gocart):
        all_vars = vars(gocart)
        for var_name, var_value in all_vars.items():
            print(f"{color.BLUE}{var_name}: {var_value}{color.RESET}")

    def set_gocart(gocart):
        if gocart.aerosol == 'Actual':
            gocart.data_driven    = False
            gocart.gocart         = ''
            gocart.gocart_hist    = ''
        elif gocart.aerosol == 'Climatological':
            gocart.data_driven    = True
            gocart.gocart         = '#'
            gocart.gocart_hist    = '#DELETE'

    def set_emissions(gocart):
        if gocart.emissions.split('_')[0] == 'OPS':
            gocart.ops_species      = '' 
            gocart.pchem_clim_years = 1
            gocart.ox_relaxtime = '0.00'
        elif gocart.emissions.split('_')[0] == 'AMIP':
            gocart.MERRA2OX_species = ''
            gocart.pchem_clim_years = 39
            gocart.ox_relaxtime = '259200.'


    def config(gocart):
        gocart.set_gocart()
        gocart.set_emissions()

