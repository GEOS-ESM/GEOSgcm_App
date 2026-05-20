
from pathlib import Path

from gcmpy.utils.yaml_ops import read_iso_yaml_file

script_dir = Path(__file__).parent.resolve()

def get_horiz_res_settings() -> dict():
    """
    Read the YAML file containing the parameter
    settings for each model horizontal resolution.

    Returns
    -------
    hor_res_settings : dict
       A dictionary which main keys are the model
       available horizontal resolutions. For each
       resolution, we provide data on the model heartbeat,
       the heartbeats for various model components, etc.
    """

    #yaml_file = Path(script_dir) / "horizontal_res_settings.yaml"
    yaml_file = Path(script_dir) / "hres_catalog.yaml"

    hor_res_settings = read_iso_yaml_file(yaml_file)

    return hor_res_settings

"""

heartbeat_dict = {
        'c12': 1200,
        'c24': 1200,
        'c48': 1200,
        'c90': 900,
        'c180': 600,
        'c270': 600,
        'c360': 450,
        'c540': 300,
        'c720': 300,
        'c1120': 300,
        'c1080': 150,
        'c1440': 150,
        'c1536': 75,
        'c2160': 75,
        'c2880': 75,
        'c4320': 75,
        'c5760': 75,
        }

atmos_horizontal_res = list(heartbeat_dict.keys())

ocean_res_default_dict = dict()
for hor_res in atmos_horizontal_res:
    if hor_res in ['c12', 'c24', 'c48']:
        ocean_res_default_dict[hor_res] = [
                'o2 (1/4-deg, 1440x720  MERRA-2)',  
                'o3 (1/8-deg, 2880x1440 OSTIA)',  
                'o1 (1  -deg,  360x180  Reynolds, ends in 2022)' 
                ] 
    else:
        ocean_res_default_dict[hor_res] = [
                'CS (Cubed-Sphere OSTIA)', 
                'o2 (1/4-deg, 1440x720  MERRA-2)', 
                'o3 (1/8-deg, 2880x1440 OSTIA)',  
                'o1 (1  -deg,  360x180  Reynolds, ends in 2022)'
                ]

mit_horizontal_res = dict()
for hor_res in atmos_horizontal_res:
    if hor_res == 'c720':
        mit_horizontal_res[hor_res] = ["llc1080 (1/12-deg, Lat-Lon-Cube)"]
    elif hor_res == 'c1440':
        mit_horizontal_res[hor_res] = ["llc2160 (1/24-deg, Lat-Lon-Cube)"]
    else:
        mit_horizontal_res[hor_res] = [""]

atmos_hres_settings = dict()
atmos_hres_settings['c12'] = dict(
        conv_dt = 1200,
        chem_dt = 3600,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        job_sgmt = f"{15:08}",
        num_sgmt = 20,
        post_NDS = 4,
        nx_convert = 1,
        ny_convert = 6,
        res = 'CF0012x6C',
        low_res = True
        )

atmos_hres_settings['c24'] = dict(
        conv_dt = 1200,
        chem_dt = 3600,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        job_sgmt = f"{15:08}",
        num_sgmt = 20,
        post_NDS = 4,
        nx_convert = 1,
        ny_convert = 6,
        res = 'CF0024x6C',
        low_res = True
        )

atmos_hres_settings['c48'] = dict(
        conv_dt = 1200,
        chem_dt = 3600,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        job_sgmt = f"{15:08}",
        num_sgmt = 20,
        post_NDS = 4,
        res = 'CF0048x6C',
        hist_im = 180,
        hist_jm = 91,
        low_res = True
        )

atmos_hres_settings['c90'] = dict(
        conv_dt = 900,
        chem_dt = 1800,
        dt_solar = 3600,
        dt_irrad = 3600,
        #dt_ocean = 3600,
        job_sgmt = f"{32:08}",
        num_sgmt = 4,
        post_NDS = 8,
        res = 'CF0090x6C'
        )

atmos_hres_settings['c180'] = dict(
        conv_dt = 600,
        chem_dt = 1200,
        dt_solar = 3600,
        dt_irrad = 3600,
        #dt_ocean = 3600,
        job_sgmt = f"{16:08}",
        num_sgmt = 1,
        post_NDS = 8,
        num_readers = 2,
        res = 'CF0180x6C'
        )

atmos_hres_settings['c270'] = dict(
        conv_dt = 600,
        chem_dt = 1800,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 2.5,
        res = 'CF0270x6C-SG001'
        )

atmos_hres_settings['c360'] = dict(
        conv_dt = 450,
        chem_dt = 900,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        num_readers = 4,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 12,
        nx_convert = 4,
        res = 'CF0360x6C'
        )

atmos_hres_settings['c540'] = dict(
        conv_dt = 300,
        chem_dt = 900,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 2.5,
        res = 'CF0540x6C-SG001'
        )

atmos_hres_settings['c720'] = dict(
        conv_dt = 300,
        chem_dt = 600,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        num_readers = 6,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 16,
        nx_convert = 8,
        use_SHMEM = 1,
        res = 'CF0720x6C'
        )

atmos_hres_settings['c1080'] = dict(
        conv_dt = 300,
        chem_dt = 600,
        dt_solar = 1800,
        dt_irrad = 1800,
        dt_ocean = 1800,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 2.5,
        res = 'CF1080x6C-SG001'
        )

atmos_hres_settings['c1120'] = dict(
        conv_dt = 300,
        chem_dt = 600,
        dt_solar = 3600,
        dt_irrad = 3600,
        dt_ocean = 3600,
        num_readers = 6,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 16,
        nx_convert = 8,
        use_SHMEM = 1,
        res = 'CF1120x6C'
        )

atmos_hres_settings['c1440'] = dict(
        conv_dt = 300,
        chem_dt = 600,
        dt_solar = 1200,
        dt_irrad = 1200,
        dt_ocean = 1200,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        res = 'CF1440x6C'
        )

atmos_hres_settings['c1536'] = dict(
        conv_dt = 300,
        chem_dt = 900,
        dt_solar = 1800,
        dt_irrad = 1800,
        dt_ocean = 1800,
        num_readers = 6,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 16,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 3.0,
        res = 'CF1536x6C-SG002'
        )

atmos_hres_settings['c2160'] = dict(
        conv_dt = 300,
        chem_dt = 300,
        dt_solar = 900,
        dt_irrad = 900,
        dt_ocean = 900,
        num_readers = 6,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 2.5,
        res = 'CF2160x6C-SG001'
        )

atmos_hres_settings['c2880'] = dict(
        conv_dt = 300,
        chem_dt = 300,
        dt_solar = 900,
        dt_irrad = 900,
        dt_ocean = 900,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = True,
        convpar_option = 'NONE',
        res = 'CF2880x6C'
        )

atmos_hres_settings['c4320'] = dict(
        conv_dt = 300,
        chem_dt = 300,
        dt_solar = 900,
        dt_irrad = 900,
        dt_ocean = 900,
        num_readers = 6,
        job_sgmt = f"{5:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = 1,
        conus = "",
        stretch_factor = 2.5,
        res = 'CF4320x6C'
        )

atmos_hres_settings['c5760'] = dict(
        conv_dt = 300,
        chem_dt = 300,
        dt_solar = 600,
        dt_irrad = 600,
        dt_ocean = 600,
        num_readers = 6,
        job_sgmt = f"{1:08}",
        num_sgmt = 1,
        post_NDS = 32,
        nx_convert = 8,
        use_SHMEM = True,
        convpar_option = 'NONE',
        res = 'CF5760x6C'
        )
"""
