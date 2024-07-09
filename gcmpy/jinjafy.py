'''
The purpose of this script is to "jinja-fy" the template files affected by gcm_setup.

Jinja can't process items unless they are enclosed on both sides with an
identifier. The old/current way to identify template items is "@ITEM_NAME".
This script will replace all these instances with "{{ ITEM_NAME }}".

Variables templated at run time (identified with ">>>ITEM<<<") are kept 
the same but included in case someone wants this changed.

This script only needs to be run once.
'''

import os

def replace_strings_in_file(file_path, replacements):
    # Read the content of the file
    with open(file_path, 'r') as file:
        content = file.read()

    # Replace all occurrences of the keys with their corresponding values
    for key, value in replacements.items():
        content = content.replace(key, value)

    # Write the modified content back to the file
    with open(file_path, 'w') as file:
        file.write(content)

def main(file_list, replacements):
    for file_path in file_list:
        file_path = f"{os.path.dirname(os.getcwd())}/{file_path}"
        if os.path.isfile(file_path):
            replace_strings_in_file(file_path, replacements)
        else:
            print(f"File {file_path} not found.")

if __name__ == "__main__":
    # List of files to process
    file_list = [
        'gcm_run.j',
        'gcm_post.j',
        'gcm_archive.j',
        'gcm_regress.j',
        'gcm_plot.tmpl',
        'gcm_quickplot.csh',
        'gcm_moveplot.j',
        'gcm_forecast.tmpl',
        'gcm_forecast.setup',
        'gcm_emip.setup',
        'CAP.rc.tmpl',
        'AGCM.rc.tmpl',
        'HISTORY.rc.tmpl',
        'logging.yaml',
        'fvcore_layout.rc'
    ]

    # Dictionary with keys to find and values to replace
    replacements = {
                      '@GCMVER': '{{ GCMVER }}',
                      '@EXPSRC': '{{ EXPSRC }}',
                       '@EXPID': '{{ EXPID }}',
                       '@RUN_N': '{{ RUN_N }}',
                      '@RUN_FN': '{{ RUN_FN }}',
                      '@RUN_FT': '{{ RUN_FT }}',
                       '@RUN_T': '{{ RUN_T }}',
                       '@RUN_P': '{{ RUN_P }}',
                      '@RUN_FP': '{{ RUN_FP }}',
                       '@RUN_Q': '{{ RUN_Q }}',
                      '@POST_N': '{{ POST_N }}',
                      '@POST_T': '{{ POST_T }}',
                      '@POST_P': '{{ POST_P }}',
                      '@POST_Q': '{{ POST_Q }}',
                      '@MOVE_N': '{{ MOVE_N }}',
                      '@PLOT_N': '{{ PLOT_N }}',
                      '@PLOT_T': '{{ PLOT_T }}',
                      '@PLOT_P': '{{ PLOT_P }}',
                      '@PLOT_Q': '{{ PLOT_Q }}',
                      '@MOVE_Q': '{{ MOVE_Q }}',
                      '@MOVE_P': '{{ MOVE_P }}',
                   '@ARCHIVE_N': '{{ ARCHIVE_N }}',
                   '@ARCHIVE_T': '{{ ARCHIVE_T }}',
                   '@ARCHIVE_P': '{{ ARCHIVE_P }}',
                   '@ARCHIVE_Q': '{{ ARCHIVE_Q }}',
                   '@REGRESS_N': '{{ REGRESS_N }}',
                      '@BCSDIR': '{{ BCSDIR }}',
                      '@SSTDIR': '{{ SSTDIR }}',
                     '@SSTNAME': '{{ SSTNAME }}',
                    '@OCEANOUT': '{{ OCEANOUT }}',
                      '@LSMBCS': '{{ LSMBCS }}',
                 '@EMIP_BCS_IN': '{{ EMIP_BCS_IN }}',
                 '@EMIP_MERRA2': '{{ EMIP_MERRA2 }}',
                      '@BCSTAG': '{{ BCSTAG }}',
                     '@SSTFILE': '{{ SSTFILE }}',
                     '@ICEFILE': '{{ ICEFILE }}',
                    '@KPARFILE': '{{ KPARFILE }}',
                      '@CHMDIR': '{{ CHMDIR }}',
                   '@COUPLEDIR': '{{ COUPLEDIR }}',
              '@shared_COUPLED': '{{ shared_COUPLED }}',
                    '@GWDRSDIR': '{{ GWDRSDIR }}',
                      '@EXPDIR': '{{ EXPDIR }}',
                      '@EXPDSC': '{{ EXPDSC }}',
                      '@HOMDIR': '{{ HOMDIR }}',
                 '@BATCH_GROUP': '{{ BATCH_GROUP }}',
                  '@BATCH_TIME': '{{ BATCH_TIME }}',
                   '@BATCH_CMD': '{{ BATCH_CMD }}',
               '@BATCH_JOBNAME': '{{ BATCH_JOBNAME }}',
            '@BATCH_OUTPUTNAME': '{{ BATCH_OUTPUTNAME }}',
            '@BATCH_JOINOUTERR': '{{ BATCH_JOINOUTERR }}',
                        '@SITE': '{{ SITE }}',
                     '@GEOSDIR': '{{ GEOSDIR }}',
                     '@GEOSSRC': '{{ GEOSSRC }}',
                     '@GEOSBIN': '{{ GEOSBIN }}',
                     '@GEOSETC': '{{ GEOSETC }}',
                    '@GEOSUTIL': '{{ GEOSUTIL }}',
           '@SINGULARITY_BUILD': '{{ SINGULARITY_BUILD }}',
                '@NATIVE_BUILD': '{{ NATIVE_BUILD }}',
                '@MPT_SHEPHERD': '{{ MPT_SHEPHERD }}',
         '@SINGULARITY_SANDBOX': '{{ SINGULARITY_SANDBOX }}',
              '@REAL_BIND_PATH': '{{ REAL_BIND_PATH }}',
              '@BASE_BIND_PATH': '{{ BASE_BIND_PATH }}',
                '@BOUNDARY_DIR': '{{ BOUNDARY_DIR }}',
             '@CHECKPOINT_TYPE': '{{ CHECKPOINT_TYPE }}',
                     '@OGCM_NX': '{{ OGCM_NX }}',
                     '@OGCM_NY': '{{ OGCM_NY }}',
                 '@OGCM_NPROCS': '{{ OGCM_NPROCS }}',
                '@OBSERVER_FRQ': '{{ OBSERVER_FRQ }}',
                   '@DASTUNING': '{{ DASTUNING }}',
               '>>>FORCEDAS<<<': '>>>FORCEDAS<<<',
               '>>>FORCEGCM<<<': '>>>FORCEGCM<<<',
                     '@COUPLED': '{{ COUPLED }}',
                    '@CLDMICRO': '{{ CLDMICRO }}',
                        '@MOM5': '{{ MOM5 }}',
                        '@MOM6': '{{ MOM6 }}',
                    '@OCNMODEL': '{{ OCNMODEL }}',
                       '@CICE4': '{{ CICE4 }}',
                       '@CICE6': '{{ CICE6 }}',
             '>>>HIST_CICE4<<<': '{{ >>>HIST_CICE4<<< }}',
                         '@MIT': '{{ MIT }}',
                   '@DATAOCEAN': '{{ DATAOCEAN }}',
                 '>>>GOCART<<<': '>>>GOCART<<<',
                 '@OPS_SPECIES': '{{ OPS_SPECIES }}',
                '@CMIP_SPECIES': '{{ CMIP_SPECIES }}',
            '@MERRA2OX_SPECIES': '{{ MERRA2OX_SPECIES }}',
                '>>>FVCUBED<<<': '>>>FVCUBED<<<',
                 '@HIST_GOCART': ' {{ HIST_GOCART }}',
                  '>>>OSTIA<<<': '>>>OSTIA<<<',
           '>>>HIST_CATCHCN<<<': '>>>HIST_CATCHCN<<<',
         '>>>GCMRUN_CATCHCN<<<': '>>>GCMRUN_CATCHCN<<<',
           '>>>EMIP_OLDLAND<<<': '>>>EMIP_OLDLAND<<<',
           '>>>EMIP_NEWLAND<<<': '>>>EMIP_NEWLAND<<<',
                   '@LSM_PARMS': '{{ LSM_PARMS }}',
                  '@OCEAN_NAME': '{{ OCEAN_NAME }}',
               '@OCEAN_PRELOAD': '{{ OCEAN_PRELOAD }}',
               '>>>4DIAUDAS<<<': '>>>4DIAUDAS<<<',
         '>>>REGULAR_REPLAY<<<': '>>>REGULAR_REPLAY<<<',
    '>>>REGULAR_REPLAY_GMAO<<<': '>>>REGULAR_REPLAY_GMAO<<<',
    '>>>REGULAR_REPLAY_NCEP<<<': '>>>REGULAR_REPLAY_NCEP<<<',
   '>>>REGULAR_REPLAY_ECMWF<<<': '>>>REGULAR_REPLAY_ECMWF<<<',
'ana4replay.eta.%y4%m2%d2_%h2z.nc4': '{{ ana4replay.eta.%y4%m2%d2_%h2z.nc4 }}',
            '@REPLAY_ANA_EXPID': '{{ REPLAY_ANA_EXPID }}',
         '@REPLAY_ANA_LOCATION': '{{ REPLAY_ANA_LOCATION }}',
      '@M2_REPLAY_ANA_LOCATION': '{{ M2_REPLAY_ANA_LOCATION }}',
                '@OX_RELAXTIME': '{{ OX_RELAXTIME }}',
            '@PCHEM_CLIM_YEARS': '{{ PCHEM_CLIM_YEARS }}',
               '@RATS_PROVIDER': '{{ RATS_PROVIDER }}',
               '@AERO_PROVIDER': '{{ AERO_PROVIDER }}',
               '@OANA_PROVIDER': '{{ OANA_PROVIDER }}',
                   '@EMISSIONS': '{{ EMISSIONS }}',
                      '@DYCORE': '{{ DYCORE }}',
               '@AGCM_GRIDNAME': '{{ AGCM_GRIDNAME }}',
               '@OGCM_GRIDNAME': '{{ OGCM_GRIDNAME }}',
                '@OGCM_IS_FCST': '{{ OGCM_IS_FCST }}',
                        '@BOOT': '{{ BOOT }}',
                      '@BCSRES': '{{ BCSRES }}',
                    '@OCEANtag': '{{ OCEANtag }}',
                    '@ATMOStag': '{{ ATMOStag }}',
                '@RES_DATELINE': '{{ RES_DATELINE }}',
                    '@TILEDATA': '{{ TILEDATA }}',
                     '@TILEBIN': '{{ TILEBIN }}',
                          '@DT': '{{ DT }}',
                    '@SOLAR_DT': '{{ SOLAR_DT }}',
                    '@IRRAD_DT': '{{ IRRAD_DT }}',
                    '@OCEAN_DT': '{{ OCEAN_DT }}',
                     '@LONG_DT': '{{ LONG_DT }}',
                          '@NX': '{{ NX }}',
                          '@NY': '{{ NY }}',
                   '@USE_SHMEM': '{{ USE_SHMEM }}',
                '@USE_IOSERVER': '{{ USE_IOSERVER }}',
           '@NUM_OSERVER_NODES': '{{ NUM_OSERVER_NODES }}',
             '@NUM_BACKEND_PES': '{{ NUM_BACKEND_PES }}',
          '@RESTART_BY_OSERVER': '{{ RESTART_BY_OSERVER }}',
              '@NCPUS_PER_NODE': '{{ NCPUS_PER_NODE }}',
                 '@NUM_READERS': '{{ NUM_READERS }}',
                 '@NUM_WRITERS': '{{ NUM_WRITERS }}',
                 '@LATLON_AGCM': '{{ LATLON_AGCM }}',
                 '@LATLON_OGCM': '{{ LATLON_OGCM }}',
                   '@CUBE_AGCM': '{{ CUBE_AGCM }}',
                   '@CUBE_OGCM': '{{ CUBE_OGCM }}',
                   '@GRID_TYPE': '{{ GRID_TYPE }}',
                     '@AGCM_NF': '{{ AGCM_NF }}',
                     '@AGCM_IM': '{{ AGCM_IM }}',
                     '@AGCM_JM': '{{ AGCM_JM }}',
                     '@AGCM_LM': '{{ AGCM_LM }}',
                     '@OGCM_IM': '{{ OGCM_IM }}',
                     '@OGCM_JM': '{{ OGCM_JM }}',
                     '@OGCM_LM': '{{ OGCM_LM }}',
                     '@OGCM_NF': '{{ OGCM_NF }}',
              '@OGCM_GRID_TYPE': '{{ OGCM_GRID_TYPE }}',
                    '@BEG_DATE': '{{ BEG_DATE }}',
                    '@END_DATE': '{{ END_DATE }}',
                    '@JOB_SGMT': '{{ JOB_SGMT }}',
                    '@NUM_SGMT': '{{ NUM_SGMT }}',
                       '@CONUS': '{{ CONUS }}',
                      '@FV_HWT': '{{ FV_HWT }}',
              '@CONVPAR_OPTION': '{{ CONVPAR_OPTION }}',
              '@STRETCH_FACTOR': '{{ STRETCH_FACTOR }}',
             '@INTERPOLATE_SST': '{{ INTERPOLATE_SST }}',
                     '@HIST_IM': '{{ HIST_IM }}',
                     '@HIST_JM': '{{ HIST_JM }}',
                '@ISCCP_SATSIM': '{{ ISCCP_SATSIM }}',
                '@MODIS_SATSIM': '{{ MODIS_SATSIM }}',
                '@RADAR_SATSIM': '{{ RADAR_SATSIM }}',
                '@LIDAR_SATSIM': '{{ LIDAR_SATSIM }}',
                 '@MISR_SATSIM': '{{ MISR_SATSIM }}',
                      '@SATSIM': '{{ SATSIM }}',
              '@USE_SKIN_LAYER': '{{ USE_SKIN_LAYER }}',
                  '@ANALYZE_TS': '{{ ANALYZE_TS }}',
                  '@LSM_CHOICE': '{{ LSM_CHOICE }}',
    '@MP_TURN_OFF_WSUB_EXTDATA': '{{ MP_TURN_OFF_WSUB_EXTDATA }}',
                    '@BACM_1M_': '{{ BACM_1M_ }}',
                    '@GFDL_1M_': '{{ GFDL_1M_ }}',
                    '@MGB2_2M_': '{{ MGB2_2M_ }}',
             '@PRELOAD_COMMAND': '{{ PRELOAD_COMMAND }}',
         '@LD_LIBRARY_PATH_CMD': '{{ LD_LIBRARY_PATH_CMD }}',
                     '@RUN_CMD': '{{ RUN_CMD }}',
                 '@HYDROSTATIC': '{{ HYDROSTATIC }}',
                  '@FV_SCHMIDT': '{{ FV_SCHMIDT }}',
              '@FV_STRETCH_FAC': '{{ FV_STRETCH_FAC }}',
               '@FV_TARGET_LON': '{{ FV_TARGET_LON }}',
               '@FV_TARGET_LAT': '{{ FV_TARGET_LAT }}',
                   '@FV_MAKENH': '{{ FV_MAKENH }}',
                    '@FV_HYDRO': '{{ FV_HYDRO }}',
               '@GFDL_PROG_CCN': '{{ GFDL_PROG_CCN }}',
                '@GFDL_USE_CCN': '{{ GFDL_USE_CCN }}',
                  '@GFDL_HYDRO': '{{ GFDL_HYDRO }}'

    }

    main(file_list, replacements)

