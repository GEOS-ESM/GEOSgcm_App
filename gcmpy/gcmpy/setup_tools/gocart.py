from gcmpy.utils.color_ops import Color

class Gocart:
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

    def ajust_tuning_coefficients(gocart):
        if  int(gocart.expConfig['AM_vertical_res'] != 72):
            return

        # Switch GOCART2G tuning coefficients to L072 values if needed
        # -------------------------------------------------------------
        file_list = [
            Path(f"{gocart.expConfig['exp_dir']}/RC/DU2G_instance_DU.rc"),
            Path(f"{gocart.expConfig['exp_dir']}/RC/SS2G_instance_SS.rc"),
        ]
        for file in file_list:
            with open(filepath, "r") as f:
                content = f.read()

            # Comment out L181 lines
            content = re.sub(r"^(.*# L181)", r"#\1", content, flags=re.MULTILINE)
            # Uncomment L072 lines
            content = re.sub(r"^#(.*# L072)", r"\1", content, flags=re.MULTILINE)

            with open(filepath, "w") as f:
                f.write(content)



    def config(gocart):
        gocart.set_gocart()
        gocart.set_emissions()
        gocart.adjust_tuning_coefficients()

