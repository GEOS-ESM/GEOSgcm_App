from env import answerdict
from utility import color
from datetime import date

class ocean:
    def __init__(self):
        self.model = answerdict['OM_name'].q_answer
        self.coupled = answerdict['OM_coupled'].q_answer
        self.seaice_model = answerdict['OM_seaice_model'].q_answer
        self.gridtype = ''
        self.gridtype_abrv = ''
        self.gridname = ''
        self.data = ''
        self.preload = ''
        self.history_template = answerdict['history_template'].q_answer
        self.im = None
        self.jm = None
        self.lm = answerdict['OM_vertical_res'].q_answer
        self.imo = None
        self.jmo = None
        self.res = ''
        self.tag = 'Reynolds'
        self.sst_name = ''
        self.sst_file = ''
        self.ice_file = ''
        self.kpar_file = ''
        self.ostia = ''
        self.out = ''
        self.nx = None
        self.ny = None
        self.nf = None
        self.latlon = ''
        self.cube = ''
        self.n_procs = None
        self.MOM5 = ''
        self.MOM6 = ''
        self.MIT = ''
        self.mpt_shepherd = ''


    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.CYAN}{var_name}: {var_value}{color.RESET}")


    def set_IMO(self):
        self.imo = f"{str(self.im):04}"

    def set_JMO(self):
        self.jmo = f"{str(self.jm):04}"

    def set_res(self):
        hres = answerdict["OM_horizontal_res"].q_answer
        if self.coupled == False and hres == "CS":
            self.res = f"{self.gridtype_abrv}{self.imo}x6C"
        elif self.coupled == False:
            self.res = f"{self.gridtype_abrv}{self.imo}xPE{self.jmo}"
        elif self.coupled == True:
            self.res = f"{self.gridtype_abrv}{self.imo}x{self.gridtype_abrv}{self.jmo}"
            # Testing at NAS shows that coupled runs *require* MPI_SHEPHERD=true
            # to run. We believe this is due to LD_PRELOAD. For now we only set
            # this for coupled runs.
            self.mpt_shepherd = "setenv MPI_SHEPHERD true"

    def set_gridname(self):
        if self.gridtype_abrv == "CF":
            self.gridname = f"OC{self.im}x{self.jm}-{self.gridtype_abrv}"
        elif self.model == "MIT":
            self.gridname = f"{self.gridtype_abrv}{self.im}x{self.jm}-{self.gridtype_abrv}"
        else:
            self.gridname = f"PE{self.im}x{self.jm}-{self.gridtype_abrv}"

    def set_kpar_file(self):
        self.kpar_file = f"SEAWIFS_KPAR_mon_clim.{self.im}x{self.jm}"

    def coupled_hres(self):
        if self.model == 'MOM5':
            self.model = 'MOM'
            self.preload = "env LD_PRELOAD=$GEOSDIR/lib/libmom.dylib" 
            mom5_warning = (
                ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                "You have chosen to set up a coupled model experiment with MOM5.\n"
                "Be aware that such a set up is _known_ to have problems. See following for more details:\n"
                "https://github.com/GEOS-ESM/MOM5/issues/19\n"
                "If your intent is to help _fix_ above issue, your help is much appreciated. Thank you and good luck!\n"
                "Otherwise, until this above issue is _fixed_ you are on your own with above choice.\n"
                "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
            )
            print(color.GREEN + mom5_warning + color.RESET)
        elif self.model == 'MOM6':
            self.preload = "env LD_PRELOAD=$GEOSDIR/lib/libmom6.dylib"
        elif self.model == 'MIT':
            self.gridtype_abrv = "llc"

        if self.model == 'MIT' and answerdict["OM_MIT_horizontal_res"].q_answer == 'cs32':
            self.jm = 32
            self.im = self.jm * 6
            self.gridtype_abrv = "CM"
        elif self.model == 'MIT' and answerdict["OM_MIT_horizontal_res"].q_answer == 'llc90':
            self.gridtype_abrv = "LL"
            if answerdict["AM_horizontal_res"].q_answer == "c48":
                self.jm = 30
                self.im = self.jm * 96
            else:
                self.jm = 15
                self.im = self.jm * 360
        elif self.model == 'MIT' and answerdict["OM_MIT_horizontal_res"].q_answer == 'llc1080':
            self.gridtype_abrv = "LL"
            self.jm = 60
            self.im = self.jm * 2880
        elif self.model == 'MIT' and answerdict["OM_MIT_horizontal_res"].q_answer == 'llc2160':
            self.gridtype_abrv = "LL"
            self.jm = 72
            self.im = self.jm * 7776
        elif self.model == "MOM" or self.model == "MOM6":
            temp = answerdict["OM_MOM_horizontal_res"].q_answer.split()
            self.im = int(temp[0])
            self.jm = int(temp[1])
            self.gridtype = "TM"


    def coupled_vres(self):
        if answerdict["AM_horizontal_res"].q_answer == "c12":
            self.nx = 3
            self.ny = 2 
        else:
            self.nx = 36
            self.ny = 10

        self.n_procs = self.nx*self.ny
        
        if self.model == "MOM" or self.model == "MOM6":
            self.gridtype = "Tripolar"
        elif self.model == "MIT":
            if self.gridtype_abrv == "CM":
                self.nx = 6
                self.ny = 1
            elif answerdict["AM_horizontal_res"].q_answer == 'c48':
                self.nx = 96
                self.ny = 1
            elif answerdict["AM_horizontal_res"].q_answer == 'c90':
                self.nx = 360
                self.ny = 1
            elif answerdict["AM_horizontal_res"].q_answer == 'c720':
                self.nx = 2880
                self.ny = 1
            elif answerdict["AM_horizontal_res"].q_answer == 'c1440':
                self.nx = 7776
                self.ny = 1


    def uncoupled_hres(self):
        todays_date = date.today()
        match answerdict["OM_horizontal_res"].q_answer:
            case "o1":
                temp_res = "360 180"
                self.im, self.jm = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.nf = 1
                self.tag = "Reynolds"
                self.sst_name = "SST"
                self.out = "c"
                self.sst_file = f"dataoceanfile_MERRA_sst_1971-current.{self.im}x{self.jm}.LE"
                self.ice_file = f"dataoceanfile_MERRA_fraci_1971-current.{self.im}x{self.jm}.LE"
                self.set_kpar_file()
                self.gridtype_abrv = "DE"
                self.latlon = ""
                self.cube = "#DELETE"
                self.ostia = "#DELETE"
                self.data = ""
            case "o2":
                temp_res = "1440 720"
                self.im, self.jm = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.nf = 1
                self.tag = "MERRA-2"
                self.sst_name  = "MERRA2"
                self.out = "e"
                self.sst_file = f"dataoceanfile_MERRA2_SST.{self.im}x{self.jm}.{todays_date.year}.data"
                self.ice_file  = f"dataoceanfile_MERRA2_ICE.{self.im}x{self.jm}.{todays_date.year}.data"
                self.set_kpar_file()
                self.gridtype_abrv = "DE"
                self.latlon = ""
                self.cube = "#DELETE"
                self.ostia = ""
                self.data = ""
            case "o3":
                temp_res = "2880 1440"
                self.im, self.jm = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.nf = 1
                self.tag = "Ostia"
                self.sst_name  = "OSTIA_REYNOLDS"
                self.out = "f"
                self.sst_file = f"dataoceanfile_OSTIA_REYNOLDS_SST.{OGCM_IM}x{OGCM_JM}.{todays_date.year}.data"
                self.ice_file  = f"dataoceanfile_OSTIA_REYNOLDS_ICE.{OGCM_IM}x{OGCM_JM}.{todays_date.year}.data"
                self.set_kpar_file()
                self.gridtype_abrv = "DE"
                self.latlon = ""
                self.cube = "#DELETE"
                self.ostia = ""
                self.data = ""
            case "CS":
                if int(answerdict["AM_horizontal_res"].q_answer[1:]) >= 90:
                    self.im = int(answerdict["AM_horizontal_res"].q_answer[1:])
                    self.jm  = self.im * 6
                    self.gridtype = "Cubed-Sphere"
                    self.nf = 6
                    self.tag = "Ostia"
                    self.sst_name = "OSTIA_REYNOLDS"
                    self.out = "f"
                    self.sst_file  = f"dataoceanfile_OSTIA_REYNOLDS_SST.{self.im}x{self.jm}.{todays_date.year}.data"
                    self.ice_file  = f"dataoceanfile_OSTIA_REYNOLDS_ICE.{self.im}x{self.jm}.{todays_date.year}.data"
                    self.set_kpar_file()
                    self.gridtype_abrv = "CF"
                    self.latlon = "#DELETE"
                    self.cube = ""
                    self.ostia = ""
                    self.data = "#DELETE"
                else:
                    print(color.RED + "ERROR: Cubed-Sphere Ocean with " + color.BLUE + \
                            answerdict["AM_horizontal_res"].q_answer + color.RED + \
                            " is not currently supported. Must be c90 or higher!")
                    exit(1)

        self.set_IMO()
        self.set_JMO()
        self.set_res() 
        self.lm = 34
        self.model = f"Data Ocean ({answerdict['AM_horizontal_res'].q_answer})"
        self.coupled = "#DELETE"
        self.MOM5 = "#DELETE"
        self.MOM6 = "#DELETE"
        self.MIT = "#DELETE"



    # ocean model driver 
    def config(self):
        match answerdict["OM_coupled"].q_answer:
            case True:
                self.coupled_hres()
                self.coupled_vres()
            case False:
                self.uncoupled_hres()







