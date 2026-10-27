from gcmpy.setup_tools.atmosphere import CATALOG_PATH
from gcmpy.utils.catalog_ops import load_catalog


atmos_horizontal_res = list(load_catalog(CATALOG_PATH)['resolutions'])

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
