from env import answerdict
from utility import color
from datetime import date

class ocean:
    def __init__(self):
        self.name = answerdict["OM_name"].q_answer
        self.coupled = answerdict["OM_coupled"].q_answer
        self.gridtype = ""
        self.gridtype_abrv = ""
        self.gridname = ""
        self.data = ""
        self.preload = ""
        self.history_template = answerdict["history_template"].q_answer
        self.IM = None
        self.JM = None
        self.LM = answerdict["OM_vertical_res"].q_answer
        self.IMO = None
        self.JMO = None
        self.res = ""
        self.tag = "Reynolds"
        self.sst_name = ""
        self.sst_file = ""
        self.ice_file = ""
        self.kpar_file = ""
        self.ostia = ""
        self.out = ""
        self.NX = None
        self.NY = None
        self.NF = None
        self.latlon = ""
        self.cube = ""
        self.n_procs = None
        self.MOM5 = ""
        self.MOM6 = ""
        self.MIT = ""
        self.mpt_shepherd = ""

    # for debugging purposes
    def print_vars(self):
        all_vars = vars(self)
        for var_name, var_value in all_vars.items():
            print(f"{color.CYAN}{var_name}: {var_value}{color.RESET}")

    def set_IMO(self):
        self.IMO = f"{str(self.IM):04}"

    def set_JMO(self):
        self.JMO = f"{str(self.JM):04}"

    def set_res(self):
        hres = answerdict["OM_horizontal_res"].q_answer
        if self.coupled == False and hres == "CS":
            self.res = f"{self.gridtype_abrv}{self.IMO}x6C"
        elif self.coupled == False:
            self.res = f"{self.gridtype_abrv}{self.IMO}xPE{self.JMO}"
        elif self.coupled == True:
            self.res = f"{self.gridtype_abrv}{self.IMO}x{self.gridtype_abrv}{self.JMO}"
            # Testing at NAS shows that coupled runs *require* MPI_SHEPHERD=true
            # to run. We believe this is due to LD_PRELOAD. For now we only set
            # this for coupled runs.
            self.mpt_shepherd = "setenv MPI_SHEPHERD true"

    def set_gridname(self):
        if self.gridtype_abrv == "CF":
            self.gridname = f"OC{self.IM}x{self.JM}-{self.gridtype_abrv}"
        elif self.name == "MIT":
            self.gridname = f"{self.gridtype_abrv}{self.IM}x{self.JM}-{self.gridtype_abrv}"
        else:
            self.gridname = f"PE{self.IM}x{self.JM}-{self.gridtype_abrv}"

    def set_kpar_file(self):
        self.kpar_file = f"SEAWIFS_KPAR_mon_clim.{self.IM}x{self.JM}"

    def coupled_hres(self):
        match self.name:
            case "MOM5":
                self.name = "MOM"
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
            case "MOM6":
                self.preload = "env LD_PRELOAD=$GEOSDIR/lib/libmom6.dylib"
            case "MIT":
                self.gridtype_abrv = "llc"

        match self.name:
            case "MIT":
                match answerdict["OM_MIT_horizontal_res"].q_answer:
                    case "cs32":
                        self.JM = 32
                        self.IM = self.JM * 6
                        self.gridtype_abrv = "CM"
                    case "llc90":
                        self.gridtype_abrv = "LL"
                        if answerdict["AM_horizontal_res"].q_answer == "c48":
                            self.JM = 30
                            self.IM = self.JM * 96
                        else:
                            self.JM = 15
                            self.IM = self.JM * 360
                    case "llc1080":
                        self.gridtype_abrv = "LL"
                        self.JM = 60
                        self.IM = self.JM * 2880
                    case "llc2160":
                        self.gridtype_abrv = "LL"
                        self.JM = 72
                        self.IM = self.JM * 7776
            case "MOM", "MOM6":
                temp = answerdict["OM_MOM_horizontal_res"].q_answer.split()
                self.IM = int(temp[0])
                self.JM = int(temp[1])
                self.gridtype = "TM"


    def coupled_vres(self):
        if answerdict["AM_horizontal_res"].q_answer == "c12":
            self.NX = 3
            self.NY = 2 
        else:
            self.NX = 36
            self.NY = 10

        self.n_procs = self.NX*self.NY
        
        match self.name:
            case "MOM", "MOM6":
                self.gridtype = "Tripolar"
            case "MIT":
                if self.gridtype_abrv == "CM":
                    self.NX = 6
                    self.NY = 1
                else:
                    match answerdict["AM_horizontal_res"].q_answer:
                        case "c48":
                            self.NX = 96
                            self.NY = 1
                        case "c90":
                            self.NX = 360
                            self.NY = 1
                        case "c720":
                            self.NX = 2880
                            self.NY = 1
                        case "c1440":
                            self.NX = 7776
                            self.NY = 1

    def uncoupled_hres(self):
        todays_date = date.today()
        match answerdict["OM_horizontal_res"].q_answer:
            case "o1":
                temp_res = "360 180"
                self.IM, self.JM = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.NF = 1
                self.tag = "Reynolds"
                self.sst_name = "SST"
                self.out = "c"
                self.sst_file = f"dataoceanfile_MERRA_sst_1971-current.{self.IM}x{self.JM}.LE"
                self.ice_file = f"dataoceanfile_MERRA_fraci_1971-current.{self.IM}x{self.JM}.LE"
                self.set_kpar_file()
                self.gridtype_abrv = "DE"
                self.latlon = ""
                self.cube = "#DELETE"
                self.ostia = "#DELETE"
                self.data = ""
            case "o2":
                temp_res = "1440 720"
                self.IM, self.JM = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.NF = 1
                self.tag = "MERRA-2"
                self.sst_name  = "MERRA2"
                self.out = "e"
                self.sst_file = f"dataoceanfile_MERRA2_SST.{self.IM}x{self.JM}.{todays_date.year}.data"
                self.ice_file  = f"dataoceanfile_MERRA2_ICE.{self.IM}x{self.JM}.{todays_date.year}.data"
                self.set_kpar_file()
                self.gridtype_abrv = "DE"
                self.latlon = ""
                self.cube = "#DELETE"
                self.ostia = ""
                self.data = ""
            case "o3":
                temp_res = "2880 1440"
                self.IM, self.JM = map(int, temp_res.split())
                self.gridtype = "LatLon"
                self.NF = 1
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
                    self.IM = int(answerdict["AM_horizontal_res"].q_answer[1:])
                    self.JM  = self.IM * 6
                    self.gridtype = "Cubed-Sphere"
                    self.NF = 6
                    self.tag = "Ostia"
                    self.sst_name = "OSTIA_REYNOLDS"
                    self.out = "f"
                    self.sst_file  = f"dataoceanfile_OSTIA_REYNOLDS_SST.{self.IM}x{self.JM}.{todays_date.year}.data"
                    self.ice_file  = f"dataoceanfile_OSTIA_REYNOLDS_ICE.{self.IM}x{self.JM}.{todays_date.year}.data"
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
        self.LM = 34
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







