#!/bin/tcsh
#
# set_resolution_params.csh
#
# Usage: source set_resolution_params.csh <AGCM_IM> <CLDMICRO> <OGCM> <OCNMODEL>
# Example: source set_resolution_params.csh c180 GFDL_1M FALSE MOM6
#
# NOTE: If OGCM is FALSE, the OCNMODEL doesn't matter
#

# Require all four arguments
if ( $#argv < 4 ) then
    echo "Usage: source set_resolution_params.csh <AGCM_IM> <CLDMICRO> <OGCM> <OCNMODEL>"
    exit 1
endif

# Input from argument
# NOTE: We use LOCALs for OCNMODEL and CLDMICRO to avoid
#       overwriting any global variables.
#       We actually *want* to overwrite AGCM_IM globally.
set AGCM_IM        = $argv[1]
set LOCAL_CLDMICRO = $argv[2]
set LOCAL_OGCM     = $argv[3]
set LOCAL_OCNMODEL = $argv[4]

# Default Run Parameters
# ----------------------
if( $AGCM_IM ==  "c12" ) then
     set       DT = 3600
     set  CONV_DT = 3600
     set  CHEM_DT = 3600
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 12
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     # C12 MOM6 should be 1x6 to match the default 3x2 ocean layout
     if ( "$LOCAL_OCNMODEL" == "MOM6") then
        set    NX = 1
     else
        set    NX = 2
     endif
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set JOB_SGMT = 00000015
     set NUM_SGMT = 20
     set ATMOS_RES = CF0012x6C
     set POST_NDS = 4
endif
if( $AGCM_IM ==  "c24" ) then
     set       DT = 1800
     set  CONV_DT = 1800
     set  CHEM_DT = 3600
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 24
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 4
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set JOB_SGMT = 00000015
     set NUM_SGMT = 20
     set ATMOS_RES = CF0024x6C
     set POST_NDS = 4
endif
if( $AGCM_IM ==  "c48" ) then
     set       DT = 1200
     set  CONV_DT = 1200
     set  CHEM_DT = 3600
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 48
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 6
     set       NY = `expr $NX \* 6`
     set HIST_IM  = 180
     set HIST_JM  =  91
     set JOB_SGMT = 00000015
     set NUM_SGMT = 20
     set ATMOS_RES = CF0048x6C
     set POST_NDS = 4
endif
if( $AGCM_IM ==  "c90" ) then
     set       DT = 900
     set  CONV_DT = 900
     set  CHEM_DT = 1800
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set AGCM_IM  = 90
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     if( $LOCAL_OGCM == TRUE ) then
        if( "$LOCAL_OCNMODEL" == "MIT" ) then
           set  NX = 10
           set  NY = 36
        else if ( "$LOCAL_OCNMODEL" == "MOM6") then
           # For MOM6 c90 means atm NXxNY = 5x36
           set  NX = 5
           set  NY = 36
        else
           set  NX = $OGCM_NY
           set  NY = $OGCM_NX
        endif
        set OCEAN_DT = $DT
     else
        set  NX = 10
        set  NY = `expr $NX \* 6`
        set OCEAN_DT = 3600
     endif
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set JOB_SGMT = 00000032
     set NUM_SGMT = 4
     set ATMOS_RES = CF0090x6C
     set POST_NDS = 8
endif
if( $AGCM_IM ==  "c180" ) then
     set       DT = 600
     set  CONV_DT = 600
     set  CHEM_DT = 1200
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set AGCM_IM  = 180
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     if( $LOCAL_OGCM == TRUE ) then
        if ( "$LOCAL_OCNMODEL" == "MOM6") then
           # For MOM6 c180 means atm NXxNY = 30x36
           set  NX = 30
           set  NY = 36
        else
           set  NX = $OGCM_NY
           set  NY = $OGCM_NX
        endif
        set OCEAN_DT = $DT
     else
        set  NX = 20
        set  NY = `expr $NX \* 6`
        set OCEAN_DT = 3600
     endif
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 2
     set JOB_SGMT = 00000016
     set NUM_SGMT = 1
     set ATMOS_RES = CF0180x6C
     set POST_NDS = 8
endif
if( $AGCM_IM == "c360" ) then
     set       DT = 450
     set  CONV_DT = 450
     set  CHEM_DT = 900
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 360
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 30
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 4
     set JOB_SGMT = 00000005
     set NUM_SGMT = 1
     set ATMOS_RES = CF0360x6C
     set POST_NDS = 12
endif
if( $AGCM_IM == "c720" ) then
     set       DT = 300
     set  CONV_DT = 300
     set  CHEM_DT = 600
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 720
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 40
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000005
     set NUM_SGMT = 1
     set ATMOS_RES = CF0720x6C
     set POST_NDS = 16
     set USE_SHMEM = 1
endif
if( $AGCM_IM == "c1120" ) then
     set       DT = 300
     set  CONV_DT = 300
     set  CHEM_DT = 600
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 1120
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 60
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000005
     set NUM_SGMT = 1
     set ATMOS_RES = CF1120x6C
     set POST_NDS = 16
     set USE_SHMEM = 1
endif
if( $AGCM_IM == "c1440" ) then
     set       DT = 150
     set  CONV_DT = 300
     set  CHEM_DT = 600
     set SOLAR_DT = 1200
     set IRRAD_DT = 1200
     set OCEAN_DT = 1200
     set AGCM_IM  = 1440
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 80
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF1440x6C
     set POST_NDS = 32
     set USE_SHMEM = 1
endif
if( $AGCM_IM == "c2880" ) then
     set       DT = 75
     set  CONV_DT = 300
     set  CHEM_DT = 300
     set SOLAR_DT = 900
     set IRRAD_DT = 900
     set OCEAN_DT = 900
     set AGCM_IM  = 2880
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 80
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF2880x6C
     set POST_NDS = 32
     set USE_SHMEM = 1
     set DEF_IOS_NDS = 4
     set CONVPAR_OPTION = NONE
endif
if( $AGCM_IM == "c5760" ) then
     set       DT = 75
     set  CONV_DT = 300
     set  CHEM_DT = 300
     set SOLAR_DT = 600
     set IRRAD_DT = 600
     set OCEAN_DT = 600
     set AGCM_IM  = 5760
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 80
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF5760x6C
     set POST_NDS = 32
     set USE_SHMEM = 1
     set DEF_IOS_NDS = 4
     set CONVPAR_OPTION = NONE
endif
# CONUS Stretched Grids
set CONUS = '#'
set STRETCH_FACTOR = ""
if( $AGCM_IM == "c270" ) then
     set       DT = 600
     set  CONV_DT = 600
     set  CHEM_DT = 1800
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 270
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 20
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF0270x6C-SG001
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 2.5
endif
if( $AGCM_IM == "c540" ) then
     set       DT = 300
     set  CONV_DT = 300
     set  CHEM_DT = 900
     set SOLAR_DT = 3600
     set IRRAD_DT = 3600
     set OCEAN_DT = 3600
     set AGCM_IM  = 540
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 30
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF0540x6C-SG001
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 2.5
endif
if( $AGCM_IM == "c1080" ) then
     set       DT = 150
     set  CONV_DT = 300
     set  CHEM_DT = 600
     set SOLAR_DT = 1800
     set IRRAD_DT = 1800
     set OCEAN_DT = 1800
     set AGCM_IM  = 1080
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 40
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF1080x6C-SG001
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 2.5
endif
if( $AGCM_IM == "c1536" ) then
     set       DT = 75
     set  CONV_DT = 300
     set  CHEM_DT = 900
     set SOLAR_DT = 1800
     set IRRAD_DT = 1800
     set OCEAN_DT = 1800
     set AGCM_IM  = 1536
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 60
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF1536x6C-SG002
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 3.0
endif
if( $AGCM_IM == "c2160" ) then
     set       DT = 75
     set  CONV_DT = 300
     set  CHEM_DT = 300
     set SOLAR_DT = 900
     set IRRAD_DT = 900
     set OCEAN_DT = 900
     set AGCM_IM  = 2160
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 80
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF2160x6C-SG001
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 2.5
endif
if( $AGCM_IM == "c4320" ) then
     set       DT = 75
     set  CONV_DT = 300
     set  CHEM_DT = 300
     set SOLAR_DT = 900
     set IRRAD_DT = 900
     set OCEAN_DT = 900
     set AGCM_IM  = 4320
     set AGCM_JM  = `expr $AGCM_IM \* 6`
     set       NX = 80
     set       NY = `expr $NX \* 6`
     set HIST_IM  = `expr $AGCM_IM \* 4`
     set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
     set NUM_READERS = 6
     set JOB_SGMT = 00000001
     set NUM_SGMT = 1
     set ATMOS_RES = CF4320x6C
     set POST_NDS = 32
     set USE_SHMEM = 1
     set CONUS = ''
     set STRETCH_FACTOR = 2.5
endif

if ($CONUS == '#') then
  set SCHMIDT     = "do_schmidt  = .false."
  set STRETCH_FAC = "stretch_fac = 1.0"
  set TARGET_LON  = "target_lon  = 0.0"
  set TARGET_LAT  = "target_lat  = -90.0"
else
  set SCHMIDT     = "do_schmidt  = .true."
  set STRETCH_FAC = "stretch_fac = $STRETCH_FACTOR"
  set TARGET_LON  = "target_lon  = -98.35"
  set TARGET_LAT  = "target_lat  = 39.5"
endif

# Set coarse resolution CLIM output
set  CLIM_IM  = 576
set  CLIM_JM  = 361
if ($CLIM_IM > $HIST_IM) then
    set CLIM_IM = $HIST_IM
    set CLIM_JM = $HIST_JM
endif

if( "$LOCAL_CLDMICRO" == "BACM_1M" ) then
   set DT      = 450
   set CONV_DT = 450
   set CHEM_DT = 3600
endif

