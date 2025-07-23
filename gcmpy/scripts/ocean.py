from env import answerdict 
from utility import color
from datetime import date

class ocean:
    def __init__(ocean):
        ocean.running_ocean = answerdict['OM_coupled'].q_answer
        ocean.model = answerdict['OM_name'].q_answer
        ocean.seaice_model = answerdict['OM_seaice_model'].q_answer
        ocean.history_template = answerdict['history_template'].q_answer 
        ocean.LM = answerdict['OM_vertical_res'].q_answer
        ocean.data_atmos = answerdict['OM_data_atmos'].q_answer
        ocean.name = ''
        ocean.preload = ''
        ocean.seaice_name = ''
        ocean.seaice_preload = ''
        ocean.gridtyp = ''
        ocean.mpt_shepherd = ''


    def config(ocean):
        if ocean.running_ocean == True:
            ocean.set_coupled()
            ocean.set_seaice()
            ocean.set_preload()
            ocean.set_data_atmosphere()
        else:
            ocean.set_uncoupled()
            ocean.set_data_atmosphere()

    
    def set_coupled(ocean):
        # NOTE: We use a CMake variable here because the shared library
        # suffix is different on Linux and macOS. This is set by configure_file()
        if ocean.model == 'MOM5':
            ocean.name = 'MOM'
            ocean.gridtyp = 'M5TP'
            ocean.grid_type = 'Tripolar'
            ocean.preload = 'env @PRELOAD_COMMAND=$GEOSDIR/lib/libmom@CMAKE_SHARED_LIBRARY_SUFFIX@'
            ocean.MOM5 = ''
            ocean.MOM6 = '#DELETE'
            ocean.MIT = '#DELETE'
            MOM5_warning = (
                "######################################################\n"
                "You have chosen to set up a coupled model experiment with MOM5.\n"
                "Be aware that such a set up is _known_ to have problems. See following for more details:\n"
                "https://github.com/GEOS-ESM/MOM5/issues/19\n"
                "If your intent is to help _fix_ above issue, your help is much appreciated. Thank you and good luck!\n"
                "Otherwise, until this above issue is _fixed_ you are on your own with above choice.\n"
                "######################################################"
            )
            print(color.GREEN + MOM5_warning + color.RESET)
            ocean.NX = 36
            ocean.NY = 10
            resolution = answerdict['OM_MOM_horizontal_res'].q_answer.split()
            ocean.IM = int(resolution[0])
            ocean.JM = int(resolution[1])
        elif ocean.model == 'MOM6':
            ocean.name = 'MOM6'
            ocean.gridtyp = 'M6TP'
            ocean.grid_type = 'Tripolar'
            ocean.preload = 'env @PRELOAD_COMMAND=$GEOSDIR/lib/libmom6@CMAKE_SHARED_LIBRARY_SUFFIX@'
            ocean.MOM5 = '#DELETE'
            ocean.MOM6 = ''
            ocean.MIT = '#DELETE'
            ocean.set_MOM6_resolution()
        elif ocean.model == 'MIT':
            ocean.name = 'MIT'
            ocean.grid_type = 'llc'
            ocean.gridtyp = 'MITLLC'
            ocean.MOM5 = '#DELETE'
            ocean.MOM6 = '#DELETE'
            ocean.MIT = ''
            ocean.JM = 15
            ocean.IM = ocean.jm * 360
            ocean.NX = 360
            ocean.NY = 1
            #ocean.gridspec = 'mit.ascii' <-- dead code(?)

        ocean.coupled = ''
        IMO = '%04d' % ocean.IM
        JMO = '%04d' % ocean.JM
        ocean.res = f"{ocean.gridtyp}{IMO}x{JMO}"
        ocean.tag = 'Reynolds'
        ocean.sst_name = '#DELETE'
        ocean.sst_file = '#DELETE'
        ocean.ice_file = '#DELETE'
        ocean.kpar_file = f"SEAWIFS_KPAR_mon_clim.{ocean.IM}x{ocean.JM}"
        ocean.ostia = '#DELETE'
        ocean.out = '#DELETE'
        ocean.latlon = ''
        ocean.cube = '#DELETE'
        ocean.data = '#DELETE'
        ocean.NF = 1

        ocean.nprocs = ocean.NX * ocean.NY # This might be bugged(!)
        ocean.set_gridname()


    def set_uncoupled(ocean):
        #rename this to 'uncoupled_hres'
        hr_code = answerdict['OM_horizontal_res'].q_answer
        todays_date = date.today()

        if hr_code == 'o1':
            ocean.IM = 360
            ocean.JM = 180
            ocean.grid_type = 'LatLon'
            ocean.NF = 1
            ocean.tag = 'Reynolds'
            ocean.sst_name = 'SST'
            ocean.out = '360x180'
            ocean.sst_file = f"dataoceanfile_MERRA_sst_1971-current.{ocean.IM}x{ocean.JM}.LE"
            ocean.ice_file = f"dataoceanfile_MERRA_fraci_1971-current.{ocean.IM}x{ocean.JM}.LE"
            ocean.kpar_file = f"SEAWIFS_KPAR_mon_clim.{ocean.IM}x{ocean.JM}"
            ocean.gridtyp = 'DE'
            ocean.latlon = ''
            ocean.cube = '#DELETE'
            ocean.ostia = '#DELETE'
            ocean.data = ''
        elif hr_code == 'o2':
            ocean.IM = 1440
            ocean.JM = 720
            ocean.grid_type = 'LatLon'
            ocean.NF = 1
            ocean.tag = 'MERRA-2'
            ocean.sst_name = 'MERRA2'
            ocean.out = '1440x720'
            ocean.sst_file = f"dataoceanfile_MERRA2_SST.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data"
            ocean.ice_file = f"dataoceanfile_MERRA2_ICE.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data"
            ocean.kpar_file = f"SEAWIFS_KPAR_mon_clim.{ocean.IM}x{ocean.JM}"
            ocean.gridtyp = 'DE'
            ocean.latlon = ''
            ocean.cube = '#DELETE'
            ocean.ostia = ''
            ocean.data = ''
        elif hr_code == 'o3':
            ocean.IM = 2880
            ocean.JM = 1440
            ocean.grid_type = 'LatLon'
            ocean.NF = 1
            ocean.tag = 'Ostia'
            ocean.sst_name = 'OSTIA_REYNOLDS'
            ocean.out = '2880x1440'
            ocean.sst_file = f"dataoceanfile_OSTIA_REYNOLDS_SST.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data"
            ocean.ice_file = f"dataoceanfile_OSTIA_REYNOLDS_ICE.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data"
            ocean.kpar_file = f"SEAWIFS_KPAR_mon_clim.{ocean.IM}x{ocean.JM}"
            ocean.gridtyp = 'DE'
            ocean.latlon = ''
            ocean.cube = '#DELETE'
            ocean.ostia = ''
            ocean.data = ''
        elif hr_code == 'CS':
            # need to add input validation for this case
            ocean.IM = int(answerdict['AM_horizontal_res'].q_answer[1:])
            ocean.JM = ocean.IM * 6
            ocean.grid_type = 'Cubed-Sphere'
            ocean.NF = 6
            ocean.tag = 'Ostia'
            ocean.sst_name = 'OSTIA_REYNOLDS'
            ocean.out = 'CS'
            ocean.sst_file = f"dataoceanfile_OSTIA_REYNOLDS_SST.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data" 
            ocean.ice_file = f"dataoceanfile_OSTIA_REYNOLDS_ICE.{ocean.IM}x{ocean.JM}.\\{todays_date.year}.data"
            ocean.kpar_file = f"SEAWIFS_KPAR_mon_clim.{ocean.IM}x{ocean.JM}"
            ocean.gridtyp = 'CF'
            ocean.latlon = '#DELETE'
            ocean.cube = ''
            ocean.ostia = ''
            ocean.data = '#DELETE'
        
        IMO = '%04d' % ocean.IM
        JMO = '%04d' % ocean.JM
        if hr_code == 'cs':
            ocean.res = f"CF{IMO}x6C"
        else:
            ocean.res = f"DE{IMO}xPE{JMO}"
        #ocean.data = #THIS IS WILL OVERWRITE THE BLOCK ABOVE
        ocean.name = ''
        ocean.preload = ''
        ocean.LM = 34
        ocean.coupled = '#DELETE'
        ocean.MOM5 = '#DELETE'
        ocean.MOM6 = '#DELETE'
        ocean.MIT = '#DELETE'
        ocean.CICE4 = '#DELETE'
        ocean.CICE6 = '#DELETE'
        ocean.hist_CICE4 = '#DELETE'
        ocean.model = f"Data Ocean ({hr_code})"
        ocean.NX = ''
        ocean.NY = ''
        ocean.nprocs = ''
        ocean.set_gridname()




    def set_MOM6_resolution(ocean):
        # For MOM6 we currently have only 3 allowed ocean resolutions based on the
        # atmospheric resolution. The allowed are:
        #
        # Atm Res  Atm NXxNY  Atm IMxJM  Ocean NXxNY  Ocean IMxJM  Ocean LM
        # -------  ---------  ---------  -----------  -----------  --------
        # c12      1x6        12x72      3x2          72x36        50
        # c90      5x36       90x540     90x2         540x458      50
        # c180     30x36      180x1080   36x30        1440x1080    75
        #
        # See https://github.com/GEOS-ESM/GEOSgcm/wiki/Coupled-model-configurations-(GEOS-MOM6)

        atmos_res = answerdict['AM_horizontal_res'].q_answer

        if atmos_res == 'c12':
            ocean.NX = 3
            ocean.NY = 2
            ocean.IM = 72
            ocean.JM = 36
            ocean.LM = 50
        elif atmos_res == 'c90':
            ocean.NX = 90
            ocean.NY = 2
            ocean.IM = 540
            ocean.JM = 458
            ocean.LM = 50
        elif atmos_res == 'c180':
            ocean.NX = 36
            ocean.NY = 30
            ocean.IM = 1440
            ocean.JM = 1080
            ocean.LM = 75

    def set_preload(ocean):
        ocean.preload = f"{ocean.preload}:{ocean.seaice_preload}"


    def set_seaice(ocean):
        if ocean.seaice_model == 'CICE4':
            ocean.seaice_preload = '$GEOSDIR/lib/libCICE4@CMAKE_SHARED_LIBRARY_SUFFIX@'
            ocean.CICE4 = ''
            ocean.CICE6 = '#DELETE'
            ocean.hist_CICE4 = ''
        elif ocean.seaice_model == 'CICE6':
            ocean.seaice_preload = '$GEOSDIR/lib/libcice6@CMAKE_SHARED_LIBRARY_SUFFIX@'
            ocean.CICE4 = '#DELETE'
            ocean.CICE6 = ''
            ocean.hist_CICE4 = '#DELETE'


    def set_data_atmosphere(ocean):
        if ocean.data_atmos == True:
            ocean.modelatm = '#DELETE'
            ocean.use_data_ATM4OCN = '.TRUE.'
        else:
            ocean.modelatm = ''
            ocean.use_data_ATM4OCN = '.FALSE.'

    def set_gridname(ocean):
        if ocean.gridtyp == 'CF':
            ocean.gridname = f"OC{ocean.IM}x{ocean.JM}-{ocean.gridtyp}"
        elif ocean.model == 'MIT':
            ocean.gridname = f"{ocean.gridtyp}{ocean.IM}x{ocean.JM}-{ocean.gridtyp}"
        else:
            ocean.gridname = f"PE{ocean.IM}x{ocean.JM}-{ocean.gridtyp}"


