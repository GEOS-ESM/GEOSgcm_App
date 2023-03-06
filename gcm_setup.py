# Required libraries
import os
# NOTE: this script depends on questionary
# pip install questionary
import questionary

# Testing libraries
import pdb
   
    # #!/bin/tcsh -f

    # #######################################################################
    # #                            Define Colors
    # #         Note:  For No Colors, set C1 and C2 to NONE
    # #######################################################################

    # set BLACK   = `tput setaf 0`
    # set RED     = `tput setaf 1`
    # set GREEN   = `tput setaf 2`
    # set YELLOW  = `tput setaf 3`
    # set BLUE    = `tput setaf 4`
    # set MAGENTA = `tput setaf 5`
    # set CYAN    = `tput setaf 6`
    # set WHITE   = `tput setaf 7`
    # set RESET   = `tput sgr0`
    # set BOLD    = `tput bold`
    # set COLORS  = `echo $BLACK $RED $GREEN $YELLOW $BLUE $MAGENTA $CYAN $WHITE $RESET`

# Define dictionary of name-path pairs for reference
pathdict = {
    'HOME' : os.path.expanduser( '~' )
}

#   Check if .GCMSETUP is in home directory
#   

    # if( -e $HOME/.GCMSETUP ) then
    #      set GCMSETUPinfo = `cat $HOME/.GCMSETUP`
    #      set C1 = $GCMSETUPinfo[1]
    #      set C2 = $GCMSETUPinfo[2]
    # else
    #      set C1 = $RED
    #      set C2 = $BLUE
    # endif
    #      set CN = $RESET

    # #######################################################################
    # #                     Build Directory Locations
    # #######################################################################

    # # Set Current Working Path to gcm_setup
    # # -------------------------------------
    # setenv ARCH `uname -s`
    # if ($ARCH == Darwin) then
    #    set FINDPATH = realpath
    # else
    #    set FINDPATH = 'readlink -f'
    # endif
    # set GCMSETUP = `$FINDPATH $0`
    # set BINDIR   = `dirname $GCMSETUP`
    # set GEOSDEF  = `dirname $BINDIR`
    # set ETCDIR   = ${GEOSDEF}/etc

    # # Test if GEOSgcm.x is here which means you are in install directory
    # if (! -x ${BINDIR}/GEOSgcm.x) then
    #    echo "You are trying to run $0 in the Applications/GEOSgcm_App directory"
    #    echo "This is no longer supported. Please run from the bin/ directory"
    #    echo "in your installation"
    #    exit 1
    # endif

    # #######################################################################
    # #                   Test for Command Line Flags
    # #######################################################################

    # # Set default behavior of switches
    # set LINKX = FALSE
    # set EXE_VERB = "copied"
    # set USING_SINGULARITY = FALSE

    # while ( $#argv > 0 )
    #    set arg = $argv[1]
    #    shift argv
    #    switch ( $arg )
    #       # Set our colors
    #       case -[Cc]:
    #       case --[Cc][Oo][Ll][Oo][Rr]:
    #          goto SETCOLOR

    #       # Symlink GEOSgcm.x
    #       case --link:
    #          set LINKX = TRUE
    #          set EXE_VERB = "linked"
    #          breaksw

    #       # Using Singularity
    #       case --singularity:
    #          set USING_SINGULARITY = TRUE
    #          breaksw

    #       # Here any string not above will trigger USAGE
    #       case -[Hh]:
    #       case --[Hh][Ee][Ll][Pp]:
    #       default:
    #          goto USAGE
    #    endsw
    # end

    # #######################################################################
    # #                        Determine site
    # #######################################################################

    # setenv NODE `uname -n`
    # setenv ARCH `uname -s`
    # setenv SITE `awk '{print $2}' $ETCDIR/SITE.rc`

    # #######################################################################
    # #                 Test for Compiler and MPI Setup
    # #######################################################################

    # setenv BASEDIR `awk '{print $2}' $ETCDIR/BASEDIR.rc`

    #      if ( `echo $BASEDIR | grep -i mvapich2` != '') then
    #    set MPI = mvapich2
    # else if ( `echo $BASEDIR | grep -i mpich`    != '') then
    #    set MPI = mpich
    # else if ( `echo $BASEDIR | grep -i openmpi`  != '') then
    #    set MPI = openmpi
    # else if ( `echo $BASEDIR | grep -i hpcx`     != '') then
    #    set MPI = openmpi
    # else if ( `echo $BASEDIR | grep -i impi`     != '') then
    #    set MPI = intelmpi
    # else if ( `echo $BASEDIR | grep -i intelmpi` != '') then
    #    set MPI = intelmpi
    # else if ( `echo $BASEDIR | grep -i mpt`      != '') then
    #    set MPI = mpt
    # else
    #    # Assume default is Intel MPI in case of older baselibs
    #    set MPI = intelmpi
    # endif

    # #######################################################################
    # #               Enter Experiment Specific Run Parameters
    # #######################################################################

    # echo
    # echo "Enter the ${C1}Experiment ID${CN}:"
    # set EXPID = $<

    # DESC:
    # echo "Enter a 1-line ${C1}Experiment Description${CN}:"
    # set EXPTMP =  `echo $<`
    # if( $#EXPTMP == 0 ) goto DESC
    # set EXPDSC =  $EXPTMP[1]
    # foreach WORD ($EXPTMP)
    # if( $WORD !=  $EXPDSC ) set EXPDSC = `echo ${EXPDSC}_${WORD}`
    # end

    # GEOSTAG:
    # set GEOSTAG = `cat ${ETCDIR}/.AGCM_VERSION`

    # #######################################################################
    # #            Test to see if you want to CLONE old experiment
    # #######################################################################

    # # Check for CLONE
    # # ---------------
    # ASKCLONE:
    # echo "Do you wish to ${C1}CLONE${CN} an old experiment? (Default: ${C2}NO${CN} or ${C2}FALSE${CN})"
    # set   KLONE  = $<
    # if( .$KLONE == . ) then
    #    set   KLONE  = FALSE
    # else
    #    set   KLONE  = `echo   $KLONE | tr "[:lower:]" "[:upper:]"`
    #    if(  $KLONE == "Y"     | \
    #         $KLONE == "YES"   | \
    #         $KLONE == "T"     | \
    #         $KLONE == "TRUE"  ) set KLONE = TRUE
    #    if(  $KLONE == "N"     | \
    #         $KLONE == "NO"    | \
    #         $KLONE == "F"     | \
    #         $KLONE == "FALSE" ) set KLONE = FALSE

    #    if( $KLONE != "TRUE" & $KLONE != "FALSE" ) then
    #       echo
    #       echo "${C1}CLONE${CN} must be set equal to ${C2}TRUE${CN} or ${C2}FALSE${CN}!"
    #       goto ASKCLONE
    #    else if ( $KLONE == "TRUE" ) then
    #       goto DOCLONE
    #    endif
    # endif

    # #######################################################################
    # #          Continue to enter in experiment parameters
    # #######################################################################

    # HRCODE:
    # echo "Enter the ${C1}Atmospheric Horizontal Resolution${CN} code:"
    # echo "--------------------------------------"
    # echo "            Cubed-Sphere"
    # echo "--------------------------------------"
    # echo "   ${C2}c12  --  8   deg ${CN}"
    # echo "   ${C2}c24  --  4   deg ${CN}"
    # echo "   ${C2}c48  --  2   deg ${CN}"
    # echo "   ${C2}c90  --  1   deg ${CN}"
    # echo "   ${C2}c180 -- 1/2  deg (${C1}56-km${C2}) ${CN}"
    # echo "   ${C2}c360 -- 1/4  deg (${C1}28-km${C2}) ${CN} "
    # echo "   ${C2}c720 -- 1/8  deg (${C1}14-km${C2}) ${CN}"
    # echo "   ${C2}c1440 - 1/16 deg (${C1} 7-km${C2}) ${CN}"
    # echo "   ${C2}          DYAMOND Grids                 "
    # echo "   ${C2}c768 -- 1/8  deg (${C1}12-km${C2}) ${CN}"
    # echo "   ${C2}c1536 - 1/16 deg (${C1} 6-km${C2}) ${CN}"
    # echo "   ${C2}c3072 - 1/32 deg (${C1} 3-km${C2}) ${CN}"
    # echo " "
    # set HRCODE = `echo $<`
    # set HRCODE = `echo $HRCODE | tr "[:upper:]" "[:lower:]"`

    # if( $HRCODE != 'c12'   & \
    #     $HRCODE != 'c24'   & \
    #     $HRCODE != 'c48'   & \
    #     $HRCODE != 'c90'   & \
    #     $HRCODE != 'c180'  & \
    #     $HRCODE != 'c360'  & \
    #     $HRCODE != 'c720'  & \
    #     $HRCODE != 'c1440' & \
    #     $HRCODE != 'c768'  & \
    #     $HRCODE != 'c1536' & \
    #     $HRCODE != 'c3072' ) goto HRCODE

    # set Resolution = $HRCODE

    # set DYCORE  = FV3
    # set AGCM_NF = 6
    # set GRID_TYPE = "Cubed-Sphere"
    # if (`echo $Resolution[1] | cut -b1` == "c" ) then
    #    set  AGCM_IM = $Resolution[1]
    # else
    #    set  AGCM_IM = "c$Resolution[1]"
    # endif

    # # We make a variable here so we can easily discriminate for CS ocean support below
    # if ( $AGCM_IM == 'c12' || $AGCM_IM == 'c24' || $AGCM_IM == 'c48' ) then
    #    set LOW_ATM_RES = TRUE
    # else
    #    set LOW_ATM_RES = FALSE
    # endif

    # # These are superfluous for GCM, but needed for SCM (considered latlon)
    # set LATLON_AGCM = "#DELETE"
    # set CUBE_AGCM = ""

    # echo "Enter the Atmospheric Model ${C1}Vertical Resolution${CN}: ${C2}LM${CN} (Default: 72)"
    # set   AGCM_LM = $<
    # if( .$AGCM_LM == . ) then
    #   set AGCM_LM = 72
    # endif


    # #######################################################################
    # #                 Choose microphysics option
    # #######################################################################

    # ASKMP:

    #  echo "Enter Choice for Atmospheric Model ${C1}Microphysics${CN}: (Default: 1MOM)"
    #  echo "   ${C2}1MOM  --  3-phase 1-moment Bacmeister et al${CN}"
    #  echo "   ${C2}GFDL  --  6-phase 1-moment Geophysical Fluid Dynamics Laboratory${CN}"
    #  echo "   ${C2}MG1   --  3-phase 2-moment Morrison & Gettleman${CN}"
    # #echo "   ${C2}MG2   --  5-phase 2-moment Morrison & Gettleman${CN}"
    # #echo "   ${C2}MG3   --  6-phase 2-moment Morrison & Gettleman${CN}"
    #  set   AGCM_MP = $<
    # if( .$AGCM_MP == . ) then
    #    set AGCM_MP = "1MOM"
    # else
    #    set AGCM_MP = `echo $AGCM_MP | tr "[:lower:]" "[:upper:]"`
    #    if( "$AGCM_MP" != "1MOM" & \
    #        "$AGCM_MP" != "GFDL" & \
    #        "$AGCM_MP" != "MG1"  ) then
    #        echo
    #        echo "${C1}Microphysics${CN} must be one of the options below!"
    #        goto ASKMP
    #    endif
    # endif


    # #######################################################################
    # #   Test to see if using hydrostatic or non-hydrostatic atmosphere
    # #######################################################################

    # ASKHYDRO:

# MAKEFILE Injects NAME HERE
    # set DEFAULT_HYDROSTATIC = @CFG_HYDROSTATIC@

    # echo "Use ${C1}Hydrostatic Atmosphere${CN}? (Default: ${C2}${DEFAULT_HYDROSTATIC}${CN})"
    # set   USE_HYDROSTATIC = $<

    # if( .$USE_HYDROSTATIC == . ) then
    #    set USE_HYDROSTATIC = $DEFAULT_HYDROSTATIC
    # else
    #    set USE_HYDROSTATIC = `echo $USE_HYDROSTATIC | tr "[:lower:]" "[:upper:]"`
    #    if(  $USE_HYDROSTATIC == "Y"     | \
    #         $USE_HYDROSTATIC == "YES"   | \
    #         $USE_HYDROSTATIC == "T"     | \
    #         $USE_HYDROSTATIC == "TRUE"  ) set USE_HYDROSTATIC = TRUE
    #    if(  $USE_HYDROSTATIC == "N"     | \
    #         $USE_HYDROSTATIC == "NO"    | \
    #         $USE_HYDROSTATIC == "F"     | \
    #         $USE_HYDROSTATIC == "FALSE" ) set USE_HYDROSTATIC = FALSE

    #    if( "$USE_HYDROSTATIC" != "TRUE" & "$USE_HYDROSTATIC" != "FALSE" ) then
    #       echo
    #       echo "Use ${C1}Hydrostatic Atmosphere${CN} must be set equal to ${C2}TRUE/YES${CN} or ${C2}FALSE/NO${CN}!"
    #       goto ASKHYDRO
    #    endif
    # endif


    # #######################################################################
    # #            Test to see if you want to use ioserver
    # #######################################################################

    # ASKIOS:

    # if( $HRCODE == 'c180'  | \
    #     $HRCODE == 'c360'  | \
    #     $HRCODE == 'c720'  | \
    #     $HRCODE == 'c1440' | \
    #     $HRCODE == 'c768'  | \
    #     $HRCODE == 'c1536' | \
    #     $HRCODE == 'c3072' ) then

    #     set DEFAULT_DO_IOS = TRUE
    #     echo "Do you wish to ${C1}IOSERVER${CN}? (Default: ${C2}YES${CN} or ${C2}TRUE${CN})"
    # # MVAPICH2 requires ioserver for history (issue with MPI_Put and MAPL)
    # else if( $MPI == mvapich2 ) then
    #     set DEFAULT_DO_IOS = TRUE
    #     echo "Do you wish to ${C1}IOSERVER${CN}? (Default: ${C2}YES${CN} or ${C2}TRUE${CN})"
    # else
    #     set DEFAULT_DO_IOS = FALSE
    #     echo "Do you wish to ${C1}IOSERVER${CN}? (Default: ${C2}NO${CN} or ${C2}FALSE${CN})"
    # endif

    # set   DO_IOS  = $<
    # if( .$DO_IOS == . ) then
    #    set   DO_IOS  = $DEFAULT_DO_IOS
    # else
    #    set   DO_IOS  = `echo   $DO_IOS | tr "[:lower:]" "[:upper:]"`
    #    if(  $DO_IOS == "Y"     | \
    #         $DO_IOS == "YES"   | \
    #         $DO_IOS == "T"     | \
    #         $DO_IOS == "TRUE"  ) set DO_IOS = TRUE
    #    if(  $DO_IOS == "N"     | \
    #         $DO_IOS == "NO"    | \
    #         $DO_IOS == "F"     | \
    #         $DO_IOS == "FALSE" ) set DO_IOS = FALSE

    #    if( $DO_IOS != "TRUE" & $DO_IOS != "FALSE" ) then
    #       echo
    #       echo "${C1}IOSERVER${CN} must be set equal to ${C2}TRUE/YES${CN} or ${C2}FALSE/NO${CN}!"
    #       goto ASKIOS
    #    endif
    # endif

    # if ( $DO_IOS == "TRUE" ) then
    #    set USE_IOSERVER = 1
    # else
    #    set USE_IOSERVER = 0
    # endif

    # #######################################################################
    # #                 What Processor Should We Run On?
    # #######################################################################

    # ASKPROC:

    # if ( $SITE == 'NCCS' ) then
    #    echo "Enter the ${C1}Processor Type${CN} you wish to run on:"
    #    echo "   ${C2}hasw (Haswell)${CN}"
    #    echo "   ${C2}sky  (Skylake)${CN} (default)"
    #    echo "   ${C2}cas  (Cascade Lake)${CN}"
    #    echo " "
    #    set MODEL = `echo $<`
    #    set MODEL = `echo $MODEL | tr "[:upper:]" "[:lower:]"`
    #    if ( .$MODEL == .) then
    #       set MODEL = 'sky'
    #    endif

    #    if( $MODEL != 'hasw' & \
    #        $MODEL != 'sky'  & \
    #        $MODEL != 'cas' ) goto ASKPROC

    #    if (     $MODEL == 'hasw') then
    #       set NCPUS_PER_NODE = 28
    #    else if ($MODEL == 'sky') then
    #       set NCPUS_PER_NODE = 40
    #    else if ($MODEL == 'cas') then
    #       # NCCS currently recommends that users do not run with
    #       # 48 cores per node on SCU16 due to OS issues and
    #       # recommends that CPU-intensive works run with 46 or less
    #       # cores. As 45 is a multiple of 3, it's the best value
    #       # that doesn't waste too much
    #       #set NCPUS_PER_NODE = 48
    #       set NCPUS_PER_NODE = 45
    #    endif

    # else if ( $SITE == 'NAS' ) then

    #    echo "Enter the ${C1}Processor Type${CN} you wish to run on:"
    #    echo "   ${C2}has (Haswell)${CN}"
    #    echo "   ${C2}bro (Broadwell)${CN}"
    #    echo "   ${C2}sky (Skylake)${CN} (default)"
    #    echo "   ${C2}cas (Cascade Lake)${CN}"
    #    echo "   ${C2}rom (AMD Rome)${CN}"
    #    echo " "
    #    echo " NOTE 1: Due to how FV3 is compiled by default, Sandy Bridge"
    #    echo "         and Ivy Bridge are not supported by current GEOS"
    #    echo " "
    #    echo " NOTE 2: GEOS is non-zero-diff when running on AMD Rome"
    #    echo "         compared to the other Intel nodes."
    #    echo " "
    #    set MODEL = `echo $<`
    #    set MODEL = `echo $MODEL | tr "[:upper:]" "[:lower:]"`
    #    if ( .$MODEL == .) then
    #       set MODEL = 'sky'
    #    endif

    #    if( $MODEL != 'has' & \
    #        $MODEL != 'bro' & \
    #        $MODEL != 'sky' & \
    #        $MODEL != 'cas' & \
    #        $MODEL != 'rom' ) goto ASKPROC

    #    # Some processors have weird names at NAS
    #    # ---------------------------------------

    #    if ($MODEL == sky) then
    #       set MODEL = 'sky_ele'
    #    else if ($MODEL == cas) then
    #       set MODEL = 'cas_ait'
    #    else if ($MODEL == rom) then
    #       set MODEL = 'rom_ait'
    #    endif

    #    if (     $MODEL == 'san') then
    #       set NCPUS_PER_NODE = 16
    #    else if ($MODEL == 'ivy') then
    #       set NCPUS_PER_NODE = 20
    #    else if ($MODEL == 'has') then
    #       set NCPUS_PER_NODE = 24
    #    else if ($MODEL == 'bro') then
    #       set NCPUS_PER_NODE = 28
    #    else if ($MODEL == 'sky_ele') then
    #       set NCPUS_PER_NODE = 40
    #    else if ($MODEL == 'cas_ait') then
    #       set NCPUS_PER_NODE = 40
    #    else if ($MODEL == 'rom_ait') then
    #       set NCPUS_PER_NODE = 128
    #    endif

    # else
    #    set MODEL = 'UNKNOWN'
    # endif

    # #######################################################################
    # #                         Check for COUPLED Ocean
    # #######################################################################

    # OGCM:
    # echo "Do you wish to run the ${C1}COUPLED${CN} Ocean/Sea-Ice Model? (Default: ${C2}NO${CN} or ${C2}FALSE${CN})"
    # set   OGCM  = $<
    # if( .$OGCM == . ) then
    # set   OGCM  = FALSE
    # else
    # set   OGCM  = `echo   $OGCM | tr "[:lower:]" "[:upper:]"`
    # if(  $OGCM == "Y"     | \
    #      $OGCM == "YES"   | \
    #      $OGCM == "T"     | \
    #      $OGCM == "TRUE"  ) set OGCM = TRUE
    # if(  $OGCM == "N"     | \
    #      $OGCM == "NO"    | \
    #      $OGCM == "F"     | \
    #      $OGCM == "FALSE" ) set OGCM = FALSE

    # if( $OGCM != TRUE & $OGCM != FALSE ) then
    # echo
    # echo "${C1}COUPLED${CN} must be set equal to TRUE or FALSE!"
    # goto  OGCM
    # else
    # echo
    # endif
    # endif

    # set CLDMICRO = "1MOMENT"
    # set MPT_SHEPHERD = ""
    # if( $OGCM == TRUE ) then

    #     set COUPLED   = ""
    #     set OGRIDTYP  = "TM"
    #     set DATAOCEAN = "#DELETE"
    #     set OCEAN_NAME = ""
    #     set OCEAN_PRELOAD = ""
    #     set CLDMICRO = "2MOMENT"

    #     # Ocean Model
    #     # -----------
    #     OCNMODEL:
    #     echo "Choose an ${C1}Ocean Model${CN}: (Default: ${C2}MOM5${CN})"
    #     echo "   ${C2}MOM5${CN}"
    #     echo "   ${C2}MOM6${CN}"

    #     set OCNMODEL  = $<
    #     if ( .$OCNMODEL == . ) then
    #        set OCNMODEL  = "MOM5"
    #     else
    #        set OCNMODEL = `echo $OCNMODEL | tr "[:lower:]" "[:upper:]"`

    #        if ( $OCNMODEL != "MOM5" & $OCNMODEL != "MOM6" ) then
    #           echo
    #           echo "${C1}Ocean Model${CN} must be either MOM5 or MOM6!"
    #           goto OCNMODEL
    #        else
    #           echo
    #        endif
    #     endif

    #     # NOTE: We use a CMake variable here because the shared library
    #     # suffix is different on Linux and macOS. This is set by configure_file()
    #     if ( $OCNMODEL == "MOM5" ) then
    #        set OCEAN_NAME="MOM"
    #        set OCEAN_PRELOAD = 'env LD_PRELOAD=$GEOSDIR/lib/libmom@CMAKE_SHARED_LIBRARY_SUFFIX@'
    #        set MOM5=""
    #        set MOM6 = "#DELETE"
    #        set DEFAULT_HISTORY_TEMPLATE="HISTORY.AOGCM-MOM5.rc.tmpl"

    #        set mom5_warning=">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\nYou (user) have chosen to set up a coupled model experiment with MOM5.\nBe aware that such a set up is _known_ to have problems. See following for more details.\nhttps://github.com/GEOS-ESM/MOM5/issues/19\nIf your intent is to help _fix_ above issue, your help is much appreciated. Thank you and good luck!\nOtherwise, until this above issue is _fixed_ you are on your own with above choice.\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
    #        echo "\033[31;5m"${mom5_warning}"\033[0m"
    #     else if ( $OCNMODEL == "MOM6" ) then
    #        set OCEAN_NAME="MOM6"
    #        set OCEAN_PRELOAD = 'env LD_PRELOAD=$GEOSDIR/lib/libmom6@CMAKE_SHARED_LIBRARY_SUFFIX@'
    #        set MOM6=""
    #        set MOM5 = "#DELETE"
    #        set DEFAULT_HISTORY_TEMPLATE="HISTORY.AOGCM.rc.tmpl"
    #     endif

    #     # Coupled Ocean Resolution
    #     # ------------------------
    #     CORSLV:

    #     if ( $OCEAN_NAME == "MOM" ) then
    #        set Resolution = `echo 360 200`
    #     else if ( $OCEAN_NAME == "MOM6" ) then
    #        # c12 MOM6 is default 72x36
    #        if( $AGCM_IM ==  "c12" ) then
    #           set Resolution = `echo 72 36`
    #        else
    #           set Resolution = `echo 360 210`
    #        endif
    #     endif

    #     echo "Enter the Ocean Lat/Lon ${C1}Horizontal Resolution${CN}: ${C2}IM JM${CN} (Default:" $Resolution")"
    #     set Resolution = `echo $<`
    #     set num = $#Resolution
    #     if( $num == 2 ) then
    #         set OGCM_IM = $Resolution[1]
    #         set OGCM_JM = $Resolution[2]
    #     else
    #        if ( $OCEAN_NAME == "MOM" ) then
    #           set Resolution = `echo 360 200`
    #        else if ( $OCEAN_NAME == "MOM6" ) then
    #           # c12 MOM6 is default 72x36
    #           if( $AGCM_IM ==  "c12" ) then
    #              set Resolution = `echo 72 36`
    #           else
    #              set Resolution = `echo 360 210`
    #           endif
    #        endif

    #        if( $num == 0 ) then
    #           set OGCM_IM = $Resolution[1]
    #           set OGCM_JM = $Resolution[2]
    #        else
    #           goto CORSLV
    #        endif
    #     endif

    #     set IMO = ${OGCM_IM}
    #     if( $IMO < 10 ) then
    #        set IMO = 000$IMO
    #     else if($IMO < 100) then
    #        set IMO = 00$IMO
    #     else if($IMO < 1000) then
    #        set IMO = 0$IMO
    #     endif

    #     set JMO = ${OGCM_JM}
    #     if( $JMO < 10 ) then
    #        set JMO = 000$JMO
    #     else if($JMO < 100) then
    #        set JMO = 00$JMO
    #     else if($JMO < 1000) then
    #        set JMO = 0$JMO
    #     endif

    #     set OCEAN_RES = TM${IMO}xTM${JMO}
    #     set OCEAN_TAG  = Reynolds
    #     set SSTNAME  = "#DELETE"
    #     set SSTFILE  = "#DELETE"
    #     set ICEFILE  = "#DELETE"
    #     set KPARFILE = SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM}
    #     set OSTIA    = "#DELETE"
    #     set OCEANOUT = "#DELETE"

    #     echo "Enter the Ocean Model ${C1}Vertical Resolution${CN}: ${C2}LM${CN} (Default: 50)"
    #     set   OGCM_LM = $<
    #     if( .$OGCM_LM == . ) then
    #     set   OGCM_LM = 50
    #     endif

    #     # At the moment c12 MOM6 is default 3x2 ocean (and 1x6 atmos)
    #     if( $AGCM_IM ==  "c12" ) then
    #        set OGCM_NX = 3
    #        set OGCM_NY = 2
    #     else
    #        set OGCM_NX = 36
    #        set OGCM_NY = 10
    #     endif
    #     @   OGCM_NPROCS = $OGCM_NX * $OGCM_NY
    #     set OGCM_GRID_TYPE = Tripolar
    #     set LATLON_OGCM = ""
    #     set CUBE_OGCM = "#DELETE"
    #     set DATAOCEAN = "#DELETE"
    #     set OGCM_NF = 1

    # else

    #     # OGCM = FALSE (Data Ocean Resolution)
    #     # ------------------------------------
    #     DORSLV:
    #     echo "Enter the ${C1}Data_Ocean Horizontal Resolution ${CN}code: ${C2}o1${CN} (1  -deg,  360x180  Reynolds) Default"
    #     echo "                                                 ${C2}o2${CN} (1/4-deg, 1440x720  MERRA-2)"
    #     echo "                                                 ${C2}o3${CN} (1/8-deg, 2880x1440 OSTIA)"
    #     echo "                                                 ${C2}CS${CN} (Cubed-Sphere OSTIA)"
    #     set   HRCODE = `echo $<`
    #     if( .$HRCODE == . ) set HRCODE = o1
    #     set   HRCODE = `echo $HRCODE | tr "[:upper:]" "[:lower:]"`

    #     if( $HRCODE != 'o1' & \
    #         $HRCODE != 'o2' & \
    #         $HRCODE != 'o3' & \
    #         $HRCODE != 'cs' ) goto DORSLV
    #     if( $HRCODE == 'o1' ) then
    #         set Resolution = `echo  360  180`
    #         set OGCM_IM  = $Resolution[1]
    #         set OGCM_JM  = $Resolution[2]
    #         set OGCM_GRID_TYPE = LatLon
    #         set OGCM_NF = 1
    #         set OCEAN_TAG = Reynolds
    #         set SSTNAME  = SST
    #         set OCEANOUT = c
    #         set SSTFILE  = dataoceanfile_MERRA_sst_1971-current.${OGCM_IM}x${OGCM_JM}.LE
    #         set ICEFILE  = dataoceanfile_MERRA_fraci_1971-current.${OGCM_IM}x${OGCM_JM}.LE
    #         set KPARFILE = SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM}
    #         set OGRIDTYP = "DE"
    #         set LATLON_OGCM = ""
    #         set CUBE_OGCM = "#DELETE"
    #         set OSTIA    = "#DELETE"
    #         set DATAOCEAN = ""
    #     endif
    #     if( $HRCODE == 'o2' ) then
    #         set Resolution = `echo 1440 720`
    #         set OGCM_IM  = $Resolution[1]
    #         set OGCM_JM  = $Resolution[2]
    #         set OGCM_GRID_TYPE = LatLon
    #         set OGCM_NF = 1
    #         set OCEAN_TAG = MERRA-2
    #         set SSTNAME  = MERRA2
    #         set OCEANOUT = e
    #         set SSTFILE  = dataoceanfile_MERRA2_SST.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #         set ICEFILE  = dataoceanfile_MERRA2_ICE.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #         set KPARFILE = SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM}
    #         set OGRIDTYP = "DE"
    #         set LATLON_OGCM = ""
    #         set CUBE_OGCM = "#DELETE"
    #         set OSTIA    = ""
    #         set DATAOCEAN = ""
    #     endif
    #     if( $HRCODE == 'o3' ) then
    #         set Resolution = `echo 2880 1440`
    #         set OGCM_IM  = $Resolution[1]
    #         set OGCM_JM  = $Resolution[2]
    #         set OGCM_GRID_TYPE = LatLon
    #         set OGCM_NF = 1
    #         set LATLON_OGCM = ""
    #         set CUBE_OGCM = "#DELETE"
    #         set OCEAN_TAG = Ostia
    #         set SSTNAME  = OSTIA_REYNOLDS
    #         set OCEANOUT = f
    #         set SSTFILE  = dataoceanfile_OSTIA_REYNOLDS_SST.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #         set ICEFILE  = dataoceanfile_OSTIA_REYNOLDS_ICE.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #         set KPARFILE = SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM}
    #         set OGRIDTYP = "DE"
    #         set OSTIA    = ""
    #         set DATAOCEAN = ""
    #     endif
    #     if( $HRCODE == 'cs' ) then
    #         if( $LOW_ATM_RES == 'FALSE') then
    #              set OGCM_IM  = `echo $AGCM_IM | cut -b2-`
    #              set OGCM_JM  = `expr $OGCM_IM \* 6`
    #              set Resolution = `echo $OGCM_IM $OGCM_JM`
    #              set OGCM_IM  = $Resolution[1]
    #              set OGCM_JM  = $Resolution[2]
    #              set OGCM_GRID_TYPE = Cubed-Sphere
    #              set OGCM_NF = 6
    #              set LATLON_OGCM = "#DELETE"
    #              set CUBE_OGCM = ""
    #              set DATAOCEAN = "#DELETE"

    #              set OCEAN_TAG = Ostia
    #              set SSTNAME  = OSTIA_REYNOLDS
    #              set OCEANOUT = f
    #              set SSTFILE  = dataoceanfile_OSTIA_REYNOLDS_SST.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #              set ICEFILE  = dataoceanfile_OSTIA_REYNOLDS_ICE.${OGCM_IM}x${OGCM_JM}.\${YEAR}.data
    #              set KPARFILE = SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM}
    #              set OGRIDTYP = "CF"
    #              set OSTIA    = ""
    #         else
    #              echo "Error: Cubed-Sphere Ocean with ${AGCM_IM} not currently supported. Must be c90 or higher"
    #              exit 7
    #         endif
    #     endif

    #     set IMO = ${OGCM_IM}
    #     set JMO = ${OGCM_JM}

    #     if( $IMO < 10 ) then
    #          set IMO = 000$IMO
    #     else if($IMO < 100) then
    #          set IMO = 00$IMO
    #     else if($IMO < 1000) then
    #          set IMO = 0$IMO
    #     endif

    #     if( $JMO < 10 ) then
    #          set JMO = 000$JMO
    #     else if($JMO < 100) then
    #          set JMO = 00$JMO
    #     else if($JMO < 1000) then
    #          set JMO = 0$JMO
    #     endif

    #     if( $HRCODE == 'cs' ) then
    #         set OCEAN_RES =  CF${IMO}x6C
    #     else
    #         set OCEAN_RES =  DE${IMO}xPE${JMO}
    #     endif

    #     set DATAOCEAN = ""
    #     set OCEAN_NAME = ""
    #     set OCEAN_PRELOAD = ""
    #     set OGCM_LM   =  34
    #     set COUPLED   = "#DELETE"
    #     set MOM5      = "#DELETE"
    #     set MOM6      = "#DELETE"
    #     set OCNMODEL  = "Data Ocean (${HRCODE})"
    #     set DEFAULT_HISTORY_TEMPLATE="HISTORY.AGCM.rc.tmpl"

    #     set OGCM_NX       = ""
    #     set OGCM_NY       = ""
    #     set OGCM_NPROCS   = ""
    # endif

    # # Set DEFAULT SHMEM Parameter
    # # ---------------------------
    #      set USE_SHMEM = 0

    # # Set IAU-Forcing and Bias Correction OFF
    # # ---------------------------------------
    #      set FORCEDAS = "#"
    #      set FORCEGCM = "#"

    # # Set Default Convert Parameters
    # # ------------------------------
    #      set CNV_NX = 2
    #      set CNV_NY = 24     # Best set to number of cores per node

    # # Set Default Readers and Writers
    # # -------------------------------
    #      set NUM_READERS = 1
    #      set NUM_WRITERS = 1

    # # Default Run Parameters
    # # ----------------------
    # if( $AGCM_IM ==  "c12" ) then
    #      set       DT = 900
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 12
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      # C12 MOM6 should be 1x6 to match the default 3x2 ocean layout
    #      if ( $OCEAN_NAME == "MOM6") then
    #      set       NX = 1
    #      else
    #      set       NX = 2
    #      endif
    #      set       NY = `expr $NX \* 6`
    #      set   HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set JOB_SGMT = 00000015
    #      set NUM_SGMT = 20
    #      set ATMOS_RES = CF0012x6C
    #      set POST_NDS = 4
    #      set   CNV_NX = 1
    #      set   CNV_NY = 6
    # endif
    # if( $AGCM_IM ==  "c24" ) then
    #      set       DT = 900
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 24
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 4
    #      set       NY = `expr $NX \* 6`
    #      set   HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set JOB_SGMT = 00000015
    #      set NUM_SGMT = 20
    #      set ATMOS_RES = CF0024x6C
    #      set POST_NDS = 4
    #      set   CNV_NX = 1
    #      set   CNV_NY = 6
    # endif
    # if( $AGCM_IM ==  "c48" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 48
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 4
    #      set       NY = `expr $NX \* 6`
    #      set   HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = 180
    #      set HIST_JM  =  91
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set JOB_SGMT = 00000015
    #      set NUM_SGMT = 20
    #      set ATMOS_RES = CF0048x6C
    #      set POST_NDS = 4
    # endif
    # if( $AGCM_IM ==  "c90" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set AGCM_IM  = 90
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      if( $OGCM == TRUE ) then
    #           set  NX = $OGCM_NY
    #           set  NY = $OGCM_NX
    #      set OCEAN_DT = $DT
    #      else
    #           set  NX = 4
    #           set  NY = `expr $NX \* 6`
    #      set OCEAN_DT = $IRRAD_DT
    #      endif
    #      set  CHEM_DT = $DT
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set JOB_SGMT = 00000032
    #      set NUM_SGMT = 4
    #      set ATMOS_RES = CF0090x6C
    #      set POST_NDS = 8
    # endif
    # if( $AGCM_IM ==  "c180" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 180
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      if( $OGCM == TRUE ) then
    #           set  NX = $OGCM_NY
    #           set  NY = $OGCM_NX
    #      set OCEAN_DT = $DT
    #      else
    #           set  NX = 6
    #           set  NY = `expr $NX \* 6`
    #      set OCEAN_DT = $IRRAD_DT
    #      endif
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 2
    #      set JOB_SGMT = 00000016
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF0180x6C
    #      set POST_NDS = 8
    # endif
    # if( $AGCM_IM == "c360" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 360
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 12
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 4
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF0360x6C
    #      set   CNV_NX = 4
    #      set POST_NDS = 12
    # endif
    # if( $AGCM_IM == "c500" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 500
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 12
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 4
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF0500x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 12
    #      set USE_SHMEM = 1
    # endif
    # if( $AGCM_IM == "c720" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 720
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 16
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 6
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF0720x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 16
    #      set USE_SHMEM = 1
    # endif
    # if( $AGCM_IM == "c1440" ) then
    #      set       DT = 450
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = $DT
    #      set AGCM_IM  = 1440
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 30
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 6
    #      set JOB_SGMT = 00000001
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF1440x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 32
    #      set USE_SHMEM = 1
    # endif
    # # DYAMOND Resolutions
    # if( $AGCM_IM == "c768" ) then
    #      set       DT = 180
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = 900
    #      set AGCM_IM  = 768
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 32
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 6
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF0768x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 16
    #      set USE_SHMEM = 1
    # endif
    # if( $AGCM_IM == "c1536" ) then
    #      set       DT = 90
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = 900
    #      set AGCM_IM  = 1536
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 64
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 6
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF1536x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 16
    #      set USE_SHMEM = 1
    # endif
    # if( $AGCM_IM == "c3072" ) then
    #      set       DT = 45
    #      set SOLAR_DT = 3600
    #      set IRRAD_DT = 3600
    #      set OCEAN_DT = $IRRAD_DT
    #      set  CHEM_DT = 900
    #      set AGCM_IM  = 3072
    #      set AGCM_JM  = `expr $AGCM_IM \* 6`
    #      set       NX = 64
    #      set       NY = `expr $NX \* 6`
    #      set HYDROSTATIC = $USE_HYDROSTATIC
    #      set HIST_IM  = `expr $AGCM_IM \* 4`
    #      set HIST_JM  = `expr $AGCM_IM \* 2 + 1`
    #      set GRID_FILE = "Gnomonic_c${AGCM_IM}.dat"
    #      set NUM_READERS = 6
    #      set JOB_SGMT = 00000005
    #      set NUM_SGMT = 1
    #      set ATMOS_RES = CF3072x6C
    #      set   CNV_NX = 8
    #      set POST_NDS = 16
    #      set USE_SHMEM = 1
    # endif

    # set IS_FCST = 0
    # set FVCUBED         = ""
    # set AGCM_GRIDNAME   = "PE${AGCM_IM}x${AGCM_JM}-CF"
    # if( ${OGRIDTYP} == 'CF' ) then
    #    set OGCM_GRIDNAME   = "OC${OGCM_IM}x${OGCM_JM}-${OGRIDTYP}"
    # else
    #    set OGCM_GRIDNAME   = "PE${OGCM_IM}x${OGCM_JM}-${OGRIDTYP}"
    # endif
    # set BCSRES          = "${ATMOS_RES}_${OCEAN_RES}"
    # set RES_DATELINE    = '${AGCM_IM}x${AGCM_JM}'
    # set TILEDATA        = "${ATMOS_RES}_${OCEAN_RES}_Pfafstetter.til"
    # set TILEBIN         = "${ATMOS_RES}_${OCEAN_RES}_Pfafstetter.TIL"
    # if( $HIST_IM >= $OGCM_IM ) then
    #    set INTERPOLATE_SST = 1
    # else
    #    set INTERPOLATE_SST = 0
    # endif


    # set BEG_DATE =  '18910301 000000'
    # set END_DATE =  '29990302 210000'
    # set JOB_SGMT = "$JOB_SGMT 000000"


    # #######################################################################
    # #           Setting for Microphysics & Hydrostatic Mode               #
    # #######################################################################

    # # For @-lines in AGCM.rc.tmpl
    # set GFDL_HYDRO = FALSE
    # if ( "$AGCM_MP" == "GFDL" ) then
    #    set MP_GFDL = ""
    # else
    #    set MP_GFDL = "# "
    # endif
    # if ( "$AGCM_MP" == "MG1" ) then
    #    set MP_MG1 = ""
    # else
    #    set MP_MG1 = "# "
    # endif

    # # 1MOM and GFDL do not use WSUB_NATURE, so this eliminates a regrid
    # if ( "$AGCM_MP" == "1MOM" | "$AGCM_MP" == "GFDL" ) then
    #    set MP_NO_USE_WSUB = ""
    # else
    #    set MP_NO_USE_WSUB = "#DELETE#"
    # endif

    # # Settings for fvcore_layout.rc
    # set FV_NWAT    = ""
    # set FV_ZTRACER = "z_tracer    = .T."
    # if ( "$USE_HYDROSTATIC" == "TRUE" ) then
    #    set FV_MAKENH     = "Make_NH     = .F."
    #    set FV_HYDRO      = "hydrostatic = .T."
    #    set FV_SATADJ     = "do_sat_adj  = .F."
    # else
    #    # Logic for NH runs based on build type
    #    if ( "$DEFAULT_HYDROSTATIC" == "TRUE") then
    #       # If you built for hydrostatic, but want an NH run, run make_nh
    #       set FV_MAKENH     = "Make_NH     = .T."
    #    else
    #       # If you built for non-hydrostatic and want an NH run, do not run
    #       # NOTE: If you run regrid.pl it will make restarts that should trigger
    #       #       make_nh automatically in FV3
    #       set FV_MAKENH     = "Make_NH     = .F."
    #    endif
    #    set FV_HYDRO      = "hydrostatic = .F."
    #    if ( "$AGCM_MP" == "1MOM" | "$AGCM_MP" == "MG1" ) then
    #       set FV_SATADJ  = "do_sat_adj = .F."
    #       set FV_NWAT    = "nwat       =  3 "
    #    else if ( "$AGCM_MP" == "GFDL" ) then
    #       set FV_SATADJ  = "do_sat_adj = .T."
    #       set FV_NWAT    = "nwat       =  6 "
    #       set GFDL_HYDRO = TRUE
    #    endif
    # endif


    # #######################################################################
    # #           Check for land surface and runoff routing  models         #
    # #######################################################################

    # # Check for Old vs New Land Surface Model Boundary Conditions
    # # -----------------------------------------------------------
    # LSM_BCS:
    # echo "Enter the choice of ${C1} Land Surface Boundary Conditions${CN} using: ${C2}1${CN} (Icarus), ${C2}2${CN} (Default: Icarus-NLv3)"
    # set   LSM_BCS  = $<
    # if( .$LSM_BCS == . ) set LSM_BCS  = 2
    # if(  $LSM_BCS != 1   &  $LSM_BCS != 2 ) then
    #      echo
    #      echo "${C1} Land Surface Boundary Conditions${CN} must be set equal to ${C2}1 (Icarus){CN} or ${C2}2 (Icarus-NLv3)${CN}!"
    #      goto LSM_BCS
    # else
    #      echo
    # endif

    #     if( $LSM_BCS == 1 ) then
    #                         set LSM_BCS = "Icarus"
    #                         set LSM_PARMS = "#DELETE"
    #                         set EMIP_BCS_IN = "Ganymed-4_0"
    #                         set EMIP_OLDLAND = ""
    #                         set EMIP_NEWLAND = "#DELETE"
    #                         set EMIP_MERRA2  = "MERRA2"
    #     endif
    #     if( $LSM_BCS == 2 ) then
    #                         set LSM_BCS = "Icarus-NLv3"
    #                         set LSM_PARMS = ""
    #                         set EMIP_BCS_IN = "Icarus-NLv3"
    #                         set EMIP_OLDLAND = "#DELETE"
    #                         set EMIP_NEWLAND = ""
    #                         set EMIP_MERRA2  = "MERRA2_NewLand"
    #     endif

    # # Check for Catch-Carbon
    # # ----------------------
    # if( $LSM_BCS == "Icarus-NLv3" ) then
    #     LSM_CHOICE:
    #     echo "Enter the choice of ${C1} Land Surface Model${CN} using: ${C2}1${CN} (Default: Catchment), ${C2}2${CN} (CatchmentCN-CLM4.0 (CN_CLM40)), ${C2}3${CN} (CatchmentCN-CLM4.5 (CN_CLM45))"
    #     set   LSM_CHOICE = $<
    #     if( .$LSM_CHOICE == . ) set LSM_CHOICE  = 1
    #     if(  $LSM_CHOICE != 1   &  $LSM_CHOICE != 2 &  $LSM_CHOICE != 3 ) then
    #         echo
    #         echo "${C1} Catchment Model${CN} must be set equal to ${C2}1{CN} , ${C2}2${CN} or ${C2}3${CN}!"
    #         goto LSM_CHOICE
    #     else
    #         echo
    #     endif
    #     if( $LSM_CHOICE == 1 ) set HIST_CATCHCN = "#DELETE"
    #     if( $LSM_CHOICE == 2 ) set HIST_CATCHCN = ""
    #     if( $LSM_CHOICE == 3 ) set HIST_CATCHCN = ""
    #     if( $LSM_CHOICE == 1 ) set GCMRUN_CATCHCN = "#DELETE"
    #     if( $LSM_CHOICE == 2 ) set GCMRUN_CATCHCN = ""
    #     if( $LSM_CHOICE == 3 ) set GCMRUN_CATCHCN = ""
    #     if( $LSM_CHOICE == 2 ) then
    #       echo "IMPORTANT: please set LAND_PARAMS: to CN_CLM40 in RC/GEOS_SurfaceGridComp.rc in the experiment directory."
    #     else if ( $LSM_CHOICE == 3 ) then
    #       echo "IMPORTANT: please set LAND_PARAMS: to CN_CLM45 in RC/GEOS_SurfaceGridComp.rc in the experiment directory."
    #     endif
    # else
    #     set LSM_CHOICE = 1
    #     set HIST_CATCHCN = "#DELETE"
    #     set GCMRUN_CATCHCN = "#DELETE"
    # endif

    # #######################################################################
    # #                   Check for AEROSOL/Chemistry Models
    # #######################################################################

    # # Check for GOCART
    # # ----------------
    # GOCART:
    # echo "Do you wish to run ${C1}GOCART${CN} with ${C2}Actual${CN} or ${C2}Climatological${CN} Aerosols? (Enter: ${C2}A${CN} (Default) or ${C2}C${CN})"
    # set   GOKART  = $<
    # if( .$GOKART == . ) set  GOKART  = A
    # set   GOKART  = `echo   $GOKART | tr "[:lower:]" "[:upper:]"`
    # if(  $GOKART != 'A'   & $GOKART != 'C' ) then
    #       echo
    #       echo "${C1}GOCART${CN} must be set equal to ${C2}A${CN} or ${C2}C${CN}!"
    #       goto GOCART
    # else
    #       echo
    # endif

    # if(  $GOKART == "A" ) set GOKART = TRUE    # Use Actual         Aerosols
    # if(  $GOKART == "C" ) set GOKART = FALSE   # Use Climatological Aerosols

    # # Default setup for linkbcs emissions
    # set OPS_SPECIES      = "#"
    # set CMIP_SPECIES     = "#"
    # set MERRA2OX_SPECIES = "#"
    # set PCHEM_CLIM_YEARS = ""

    # if( $GOKART == TRUE ) then

    # set GOCART          = ""
    # set GOCART_INTERNAL = "GOCART_INTERNAL"
    # set HIST_GOCART     = ""

    # # Emission Files
    # # --------------
    # EMISSIONS:
    #      set   EMISSIONS = ''
    #      echo "Enter the GOCART ${C1}Emission Files${CN} to use: ${C2}AMIP${CN} (Default) or ${C2}OPS${CN}:"
    #      set   EMISSIONS = $<
    #      if( .$EMISSIONS == . ) then
    #      set   EMISSIONS = AMIP
    #      else
    #      set   EMISSIONS = `echo   $EMISSIONS | tr "[:lower:]" "[:upper:]"`
    #      if(  $EMISSIONS != OPS & $EMISSIONS != AMIP) then
    #      echo
    #      echo "${C1}Emission Files${CN} must be equal to:  ${C2}AMIP${CN} or ${C2}OPS${CN}!"
    #      goto EMISSIONS
    #      else
    #      echo
    #      endif
    #      endif

    #      set EMISSIONS = ${EMISSIONS}_EMISSIONS

    #      # Fix up the linkbcs species.data
    #      # NOTE: If OPS is selected, EMISSIONS is blanked
    #      if(  $EMISSIONS == OPS_EMISSIONS ) then
    #         set OPS_SPECIES = ''
    #         set PCHEM_CLIM_YEARS = 1
    #      else
    #         set MERRA2OX_SPECIES = ''
    #         set PCHEM_CLIM_YEARS = 39
    #      endif

    # # AERO Provider
    # # -------------
    #      set AERO_PROVIDER = GOCART2G
    # #AERO:
    # #echo "Enter the ${C1}AERO_PROVIDER${CN}: ${C2}GOCART${CN} (Default) or ${C2}GOCART.data${CN}:"
    # #      set AERO_PROVIDER  = $<
    # #         if( .$AERO_PROVIDER == . ) then
    # #          set  AERO_PROVIDER = GOCART
    # #      else
    # #         if(  $AERO_PROVIDER != GOCART.data & $AERO_PROVIDER != GOCART ) then
    # #            echo
    # #            echo "${C1}AERO_PROVIDER${CN} must be equal to ${C2}GOCART${CN} or ${C2}GOCART.data${CN}!"
    # #            goto AERO
    # #         else
    # #            echo
    # #         endif
    # #      endif

    # else

    # # GOKART = FALSE
    # # --------------
    # set  AERO_PROVIDER   = GOCART.data
    # set  EMISSIONS       = ""
    # set  GOCART          = "#"
    # set  HIST_GOCART     = "#DELETE"

    # # GOCART.data uses MERRA2OX currently
    # set MERRA2OX_SPECIES = ""
    # set PCHEM_CLIM_YEARS = 39

    # endif

    # # RATS Provider
    # # -------------
    # set  RATS_PROVIDER   = PCHEM

    # #######################################################################
    # #                    Create Desired HISTORY template
    # #######################################################################

    #     set check =  FALSE
    # while( $check == FALSE )

    #   echo "Enter the filename of the ${C1}HISTORY template${CN} to use"
    #   echo "-----------------------------------------------------------"
    #   echo "As ${OCNMODEL} was chosen, the default HISTORY Template is ${C2}${DEFAULT_HISTORY_TEMPLATE}${CN}"
    #   echo "  1. Hit ENTER to use the default HISTORY template (${C2}${DEFAULT_HISTORY_TEMPLATE}${CN}) or"
    #   echo "  2. Type the full path of the HISTORY template to use"

    #   set   HISTORY_TEMPLATE  = $<
    #   if( .$HISTORY_TEMPLATE == . ) then
    #      set HISTORY_TEMPLATE = ${DEFAULT_HISTORY_TEMPLATE}
    #   endif

    #   set TMPHIST = `mktemp`

    #   # Check to see if the HISTORY template exists in the $ETCDIR
    #   if( -f $ETCDIR/$HISTORY_TEMPLATE ) then
    #      set check = TRUE
    #      set TMPHIST1 = `mktemp`
    #      /bin/cp -f $ETCDIR/$HISTORY_TEMPLATE $TMPHIST1

    #      set  EXPID_old = `grep  "EXPID:" $TMPHIST1 | cut -d: -f2`
    #      set EXPDSC_old = `grep "EXPDSC:" $TMPHIST1 | cut -d: -f2`

    #      set  TMPCMD = `mktemp`
    #      set  string = "EXPID:"
    #      echo cat $TMPHIST1 \| awk \'\{if \( \$1 \~ \"${string}\" \) \
    #            \{sub \( \"${EXPID_old}\" , \"${EXPID}\"  \)\;print\} else print\}\' \> $TMPHIST > $TMPCMD
    #      chmod +x   $TMPCMD
    #                 $TMPCMD
    #      /bin/rm -f $TMPCMD
    #      /bin/mv -f $TMPHIST $TMPHIST1
    #      cat $TMPHIST1 | sed -e "s|${EXPDSC_old}|${EXPDSC}|g" > $TMPHIST
    #      /bin/rm -f $TMPHIST1

    #   # else check if the file exists (assuming they sent in full path)
    #   else if( -e $HISTORY_TEMPLATE ) then
    #      set check = TRUE
    #      /bin/cp -f $HISTORY_TEMPLATE $TMPHIST
    #   else
    #      echo "Error: Could not find ${RED}${HISTORY_TEMPLATE}${RESET}"
    #      echo ""
    #      set check = FALSE
    #   endif
    # end

    # echo ""
    # echo "Using ${C2}${HISTORY_TEMPLATE}${CN}"

    # #######################################################################
    # #                  Architecture Specific Variables
    # #######################################################################

    # @ MODEL_NPES = $NX * $NY

    # # Calculate OSERVER nodes based on recommended algorithm
    # if ( $DO_IOS == TRUE ) then

    #    # In the calculations below, the weird bc-awk command is to round up the floating point calcs

    #    # First we calculate the number of model nodes
    #    set NUM_MODEL_NODES=`echo "scale=6;($MODEL_NPES / $NCPUS_PER_NODE)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

    #    # Next the number of frontend PEs is 10% of the model PEs
    #    set NUM_FRONTEND_PES=`echo "scale=6;($MODEL_NPES * 0.1)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

    #    # Now we roughly figure out the number of collections in the HISTORY.rc (this is not perfect, but is close to right)
    #    set NUM_HIST_COLLECTIONS=`cat $TMPHIST | sed -n '/^COLLECTIONS:/,/^ *::$/{p;/^ *::$/q}' | grep -v '^ *#' | wc -l`

    #    # And the total number of oserver PEs is frontend PEs plus number of history collections
    #    @ NUM_OSERVER_PES=$NUM_FRONTEND_PES + $NUM_HIST_COLLECTIONS

    #    # Now calculate the number of oserver nodes
    #    set NUM_OSERVER_NODES=`echo "scale=6;($NUM_OSERVER_PES / $NCPUS_PER_NODE)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

    #    # And then the number of backend PEs is the number of history collections divided by the number of oserver nodes
    #    set NUM_BACKEND_PES=`echo "scale=6;($NUM_HIST_COLLECTIONS / $NUM_OSERVER_NODES)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

    #    # multigroup requires at least two backend pes
    #    if ($NUM_BACKEND_PES < 2) set NUM_BACKEND_PES = 2

    #    # Calculate the total number of nodes to request from batch
    #    @ NODES=$NUM_MODEL_NODES + $NUM_OSERVER_NODES

    # else
    #    # Calculate the number of model nodes
    #    set NODES=`echo "scale=6;($MODEL_NPES / $NCPUS_PER_NODE)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

    #    set NUM_OSERVER_NODES = 0
    #    set NUM_BACKEND_PES   = 0
    # endif

    # @ CNV_NPES = $CNV_NX * $CNV_NY
    # # Here we need to convert POST_NDS to total tasks. Using 16 cores
    # # per task as a good default
    # @ POST_NPES = $POST_NDS * 16

    # # Longer job names are now supported with SLURM and PBS. Limits seem to be 1024 characters with SLURM
    # # and 230 with PBS. To be safe, we will limit to 200

    #               setenv     RUN_N  `echo $EXPID | cut -b1-200`_RUN                         # RUN      Job Name
    #               setenv     RUN_FN `echo $EXPID | cut -b1-200`_FCST                        # Forecast Job Name
    #               setenv    POST_N  `echo $EXPID | cut -b1-199`_POST                        # POST     Job Name
    #               setenv    PLOT_N  `echo $EXPID | cut -b1-200`_PLT                         # PLOT     Job Name
    #               setenv    MOVE_N  `echo $EXPID | cut -b1-200`_MOVE                        # MOVE     Job Name
    #               setenv ARCHIVE_N  `echo $EXPID | cut -b1-199`_ARCH                        # ARCHIVE  Job Name
    #               setenv REGRESS_N  `echo $EXPID | cut -b1-199`_RGRS                        # REGRESS  Job Name
    #               setenv CONVERT_N  `echo $EXPID | cut -b1-200`_CNV                         # CONVERT  Job Name

    # # Default converter time
    #               setenv CONVERT_T  "0:15:00"                                              # Wallclock Time   for gcm_convert.j

    # if( $SITE == 'NAS' ) then
    #               setenv BATCH_CMD "qsub"                 # PBS Batch command
    #               setenv BATCH_GROUP "PBS -W group_list=" # PBS Syntax for GROUP
    #               setenv BATCH_TIME "PBS -l walltime="    # PBS Syntax for walltime
    #               setenv BATCH_JOBNAME "PBS -N "          # PBS Syntax for job name
    #               setenv BATCH_OUTPUTNAME "PBS -o "       # PBS Syntax for job output name
    #               setenv BATCH_JOINOUTERR "PBS -j oe -k oed"    # PBS Syntax for joining output and error
    #               setenv     RUN_FT "6:00:00"             # Wallclock Time   for gcm_forecast.j
    #               setenv     RUN_T  "8:00:00"             # Wallclock Time   for gcm_run.j
    #               setenv    POST_T  "8:00:00"             # Wallclock Time   for gcm_post.j
    #               setenv    PLOT_T  "8:00:00"             # Wallclock Time   for gcm_plot.j
    #               setenv ARCHIVE_T  "8:00:00"             # Wallclock Time   for gcm_archive.j
    #               set QTYPE = "normal"                    # Queue to use

    #               @ NPCUS = `echo "($POST_NPES + $NCPUS_PER_NODE - 1)/$NCPUS_PER_NODE" | bc`

    #               setenv     RUN_Q  "PBS -q ${QTYPE}"                                                                              # batch queue name for gcm_run.j
    #               setenv     RUN_P  "PBS -l select=${NODES}:ncpus=${NCPUS_PER_NODE}:mpiprocs=${NCPUS_PER_NODE}:model=${MODEL}"     # PE Configuration for gcm_run.j
    #               setenv    RUN_FP  "PBS -l select=24:ncpus=${NCPUS_PER_NODE}:mpiprocs=${NCPUS_PER_NODE}:model=${MODEL}"           # PE Configuration for gcm_forecast.j
    #               setenv    POST_Q  "PBS -q normal"                                                                                # batch queue name for gcm_post.j
    #               setenv    PLOT_Q  "PBS -q normal"                                                                                # batch queue name for gcm_plot.j
    #               setenv    MOVE_Q  "PBS -q normal"                                                                                # batch queue name for gcm_moveplot.j
    #               setenv ARCHIVE_Q  "PBS -q normal"                                                                                # batch queue name for gcm_archive.j
    #               setenv    POST_P  "PBS -l select=${NPCUS}:ncpus=${NCPUS_PER_NODE}:mpiprocs=${NCPUS_PER_NODE}:model=${MODEL}"     # PE Configuration for gcm_post.j
    #               setenv    PLOT_P  "PBS -l select=1:ncpus=${NCPUS_PER_NODE}:mpiprocs=1:model=${MODEL}"                            # PE Configuration for gcm_plot.j
    #               setenv ARCHIVE_P  "PBS -l select=1:ncpus=${NCPUS_PER_NODE}:mpiprocs=${NCPUS_PER_NODE}:model=${MODEL}"            # PE Configuration for gcm_archive.j
    #               setenv CONVERT_P  "PBS -l select=${CNV_NX}:ncpus=${NCPUS_PER_NODE}:mpiprocs=${NCPUS_PER_NODE}:model=${MODEL}"    # PE Configuration for gcm_convert.j
    #               setenv    MOVE_P  "PBS -l select=1:ncpus=1"                                                                      # PE Configuration for gcm_moveplot.j

    #               setenv BCSDIR     /nobackup/gmao_SIteam/ModelData/bcs/${LSM_BCS}/${LSM_BCS}_${OCEAN_TAG} # location of Boundary Conditions
    #               setenv REPLAY_ANA_EXPID    ONLY_MERRA2_SUPPORTED                                         # Default Analysis Experiment for REPLAY
    #               setenv REPLAY_ANA_LOCATION ONLY_MERRA2_SUPPORTED                                         # Default Analysis Location   for REPLAY
    #               setenv M2_REPLAY_ANA_LOCATION /nobackup/gmao_SIteam/ModelData/merra2/data                # Default Analysis Location   for M2 REPLAY

    #               if( ${OGCM_IM}x${OGCM_JM} == "1440x720" ) then
    #                    setenv SSTDIR     /nobackup/gmao_SIteam/ModelData/fvInput/g5gcm/bcs/SST/${OGCM_IM}x${OGCM_JM}  # location of SST Boundary Conditions
    #               else
    #                    setenv SSTDIR     /nobackup/gmao_SIteam/ModelData/fvInput/g5gcm/bcs/realtime/@SSTNAME/${OGCM_IM}x${OGCM_JM}  # location of SST Boundary Conditions
    #               endif
    #               setenv CHMDIR     /nobackup/gmao_SIteam/ModelData/fvInput_nc3            # locations of Aerosol Chemistry BCs
    #               setenv WRKDIR     /nobackup/$LOGNAME                                     # user work directory
    #               setenv COUPLEDIR  /nobackup/gmao_SIteam/ModelData/aogcm                  # Coupled Ocean/Atmos Forcing
    # else if( $SITE == 'NCCS' ) then
    #               setenv BATCH_CMD "sbatch"                                                # SLURM Batch command
    #               setenv BATCH_GROUP "SBATCH --account="                                   # SLURM Syntax for account name
    #               setenv BATCH_TIME "SBATCH --time="                                       # SLURM Syntax for walltime
    #               setenv BATCH_JOBNAME "SBATCH --job-name="                                # SLURM Syntax for job name
    #               setenv BATCH_OUTPUTNAME "SBATCH --output="                               # SLURM Syntax for job output name
    #               setenv BATCH_JOINOUTERR "DELETE"                                         # SLURM joins out and err by default
    #               setenv     RUN_FT "06:00:00"                                             # Wallclock Time   for gcm_forecast.j
    #               setenv     RUN_T  "12:00:00"                                             # Wallclock Time   for gcm_run.j
    #               setenv    POST_T  "8:00:00"                                              # Wallclock Time   for gcm_post.j
    #               setenv    PLOT_T  "12:00:00"                                             # Wallclock Time   for gcm_plot.j
    #               setenv ARCHIVE_T  "2:00:00"                                              # Wallclock Time   for gcm_archive.j

    #               @ NPCUS = `echo "($POST_NPES + $NCPUS_PER_NODE - 1)/$NCPUS_PER_NODE" | bc`

    #               setenv    RUN_Q   "SBATCH --constraint=${MODEL}"                                # batch queue name for gcm_run.j
    #               setenv    RUN_P   "SBATCH --nodes=${NODES} --ntasks-per-node=${NCPUS_PER_NODE}" # PE Configuration for gcm_run.j
    #               setenv    RUN_FP  "SBATCH --nodes=${NODES} --ntasks-per-node=${NCPUS_PER_NODE}" # PE Configuration for gcm_forecast.j
    #               setenv    POST_Q  "SBATCH --constraint=${MODEL}"                                # batch queue name for gcm_post.j
    #               setenv    PLOT_Q  "SBATCH --constraint=${MODEL}"                                # batch queue name for gcm_plot.j
    #               setenv    MOVE_Q  "SBATCH --partition=datamove"                                 # batch queue name for gcm_moveplot.j
    #               setenv ARCHIVE_Q  "SBATCH --partition=datamove"                                 # batch queue name for gcm_archive.j
    #               setenv    POST_P  "SBATCH --nodes=${NPCUS} --ntasks-per-node=${NCPUS_PER_NODE}" # PE Configuration for gcm_post.j
    #               setenv    PLOT_P  "SBATCH --nodes=4 --ntasks=4"                                 # PE Configuration for gcm_plot.j
    #               setenv ARCHIVE_P  "SBATCH --ntasks=1"                                           # PE Configuration for gcm_archive.j
    #               setenv CONVERT_P  "SBATCH --ntasks=${CNV_NPES}"                                 # PE Configuration for gcm_convert.j
    #               setenv    MOVE_P  "SBATCH --ntasks=1"                                           # PE Configuration for gcm_moveplot.j

    #               setenv BCSDIR  /discover/nobackup/ltakacs/bcs/${LSM_BCS}/${LSM_BCS}_${OCEAN_TAG}  # location of Boundary Conditions
    #               setenv REPLAY_ANA_EXPID x0039                                                     # Default Analysis Experiment for REPLAY
    #               setenv REPLAY_ANA_LOCATION /discover/nobackup/projects/gmao/g6dev/ltakacs/x0039   # Default Analysis Location   for REPLAY
    #               setenv M2_REPLAY_ANA_LOCATION /discover/nobackup/projects/gmao/merra2/data        # Default Analysis Location   for M2 REPLAY

    #               if( ${OGCM_IM}x${OGCM_JM} == "1440x720" ) then
    #                    setenv SSTDIR     $SHARE/gmao_ops/fvInput/g5gcm/bcs/SST/${OGCM_IM}x${OGCM_JM} # location of SST Boundary Conditions
    #               else
    #                    setenv SSTDIR     $SHARE/gmao_ops/fvInput/g5gcm/bcs/realtime/@SSTNAME/${OGCM_IM}x${OGCM_JM} # location of SST Boundary Conditions
    #               endif
    #               setenv CHMDIR     $SHARE/gmao_ops/fvInput_nc3                            # locations of Aerosol Chemistry BCs
    #               setenv WRKDIR     /discover/nobackup/$LOGNAME                            # user work directory
    #               setenv COUPLEDIR  /discover/nobackup/projects/gmao/ssd/aogcm             # Coupled Ocean/Atmos Forcing
    # else if( $SITE == 'AWS' ) then
    #               setenv BATCH_CMD "sbatch"                                                # SLURM Batch command
    #               setenv BATCH_GROUP DELETE                                                # SLURM Syntax for account name
    #               setenv BATCH_TIME "SBATCH --time="                                       # SLURM Syntax for walltime
    #               setenv BATCH_JOBNAME "SBATCH --job-name="                                # SLURM Syntax for job name
    #               setenv BATCH_OUTPUTNAME "SBATCH --output="                               # SLURM Syntax for job output name
    #               setenv BATCH_JOINOUTERR "DELETE"                                         # SLURM joins out and err by default
    #               setenv     RUN_FT "06:00:00"                                             # Wallclock Time   for gcm_forecast.j
    #               setenv     RUN_T  "12:00:00"                                             # Wallclock Time   for gcm_run.j
    #               setenv    POST_T  "8:00:00"                                              # Wallclock Time   for gcm_post.j
    #               setenv    PLOT_T  "12:00:00"                                             # Wallclock Time   for gcm_plot.j
    #               setenv ARCHIVE_T  "1:00:00"                                              # Wallclock Time   for gcm_archive.j
    #               setenv  RUN_Q     DELETE                                                 # batch queue name for gcm_run.j
    #               setenv  RUN_P   "SBATCH --ntasks=${MODEL_NPES}"                          # PE Configuration for gcm_run.j
    #               setenv  RUN_FP  "SBATCH --ntasks=${MODEL_NPES}"                          # PE Configuration for gcm_forecast.j
    #               setenv    POST_Q  NULL                                                   # batch queue name for gcm_post.j
    #               setenv    PLOT_Q  NULL                                                   # batch queue name for gcm_plot.j
    #               setenv    MOVE_Q  NULL                                                   # batch queue name for gcm_moveplot.j
    #               setenv ARCHIVE_Q  NULL                                                   # batch queue name for gcm_archive.j
    #               setenv    POST_P  "SBATCH --ntasks=${POST_NPES}"                         # PE Configuration for gcm_post.j
    #               setenv    PLOT_P  "SBATCH --nodes=4 --ntasks=4"                          # PE Configuration for gcm_plot.j
    #               setenv ARCHIVE_P  "SBATCH --ntasks=1"                                    # PE Configuration for gcm_archive.j
    #               setenv CONVERT_P  "SBATCH --ntasks=${CNV_NPES}"                          # PE Configuration for gcm_convert.j
    #               setenv    MOVE_P  "SBATCH --ntasks=1"                                    # PE Configuration for gcm_moveplot.j

    #               setenv BCSDIR     /ford1/share/gmao_SIteam/ModelData/bcs/${LSM_BCS}/${LSM_BCS}_${OCEAN_TAG}  # location of Boundary Conditions
    #               setenv REPLAY_ANA_EXPID    REPLAY_UNSUPPORTED                                                # Default Analysis Experiment for REPLAY
    #               setenv REPLAY_ANA_LOCATION REPLAY_UNSUPPORTED                                                # Default Analysis Location   for REPLAY
    #               setenv M2_REPLAY_ANA_LOCATION REPLAY_UNSUPPORTED                                             # Default Analysis Location   for M2 REPLAY

    #               setenv SSTDIR     /ford1/share/gmao_SIteam/ModelData/@SSTNAME/${OGCM_IM}x${OGCM_JM}  # location of SST Boundary Conditions
    #               setenv CHMDIR     /ford1/share/gmao_SIteam/ModelData/fvInput_nc3         # locations of Aerosol Chemistry BCs
    #               setenv WRKDIR     /home/$LOGNAME                                         # user work directory
    #               setenv COUPLEDIR  /ford1/share/gmao_SIteam/ModelData/aogcm               # Coupled Ocean/Atmos Forcing

    #               # By default on AWS, just ignore IOSERVER for now until testing
    #               set USE_IOSERVER = 0
    #               set NUM_OSERVER_NODES = 0
    #               set NUM_BACKEND_PES = 0
    #               set NCPUS_PER_NODE = 0
    # else
    # # These are defaults for the desktop
    #               setenv BATCH_CMD "sbatch"                                                # SLURM Batch command
    #               setenv BATCH_GROUP "SBATCH --account="                                    # SLURM Syntax for account name
    #               setenv BATCH_TIME "SBATCH --time="                                       # SLURM Syntax for walltime
    #               setenv BATCH_JOBNAME "SBATCH --job-name="                                # SLURM Syntax for job name
    #               setenv BATCH_OUTPUTNAME "SBATCH --output="                               # SLURM Syntax for job output name
    #               setenv BATCH_JOINOUTERR "DELETE"                                         # SLURM joins out and err by default
    #               setenv     RUN_FT "06:00:00"                                             # Wallclock Time   for gcm_forecast.j
    #               setenv     RUN_T  "12:00:00"                                             # Wallclock Time   for gcm_run.j
    #               setenv    POST_T  "8:00:00"                                              # Wallclock Time   for gcm_post.j
    #               setenv    PLOT_T  "12:00:00"                                             # Wallclock Time   for gcm_plot.j
    #               setenv ARCHIVE_T  "1:00:00"                                              # Wallclock Time   for gcm_archive.j
    #               setenv  RUN_Q     NULL                                                   # batch queue name for gcm_run.j
    #               setenv  RUN_P     NULL                                                   # PE Configuration for gcm_run.j
    #               setenv  RUN_FP    NULL                                                   # PE Configuration for gcm_forecast.j
    #               setenv    POST_Q  NULL                                                   # batch queue name for gcm_post.j
    #               setenv    PLOT_Q  NULL                                                   # batch queue name for gcm_plot.j
    #               setenv    MOVE_Q  NULL                                                   # batch queue name for gcm_moveplot.j
    #               setenv ARCHIVE_Q  NULL                                                   # batch queue name for gcm_archive.j
    #               setenv    POST_P  NULL                                                   # PE Configuration for gcm_run.j
    #               setenv    PLOT_P  NULL                                                   # PE Configuration for gcm_post.j
    #               setenv ARCHIVE_P  NULL                                                   # PE Configuration for gcm_archive.j
    #               setenv CONVERT_P  NULL                                                   # PE Configuration for gcm_convert.j
    #               setenv    MOVE_P  NULL                                                   # PE Configuration for gcm_moveplot.j

    #               setenv BCSDIR     /ford1/share/gmao_SIteam/ModelData/bcs/${LSM_BCS}/${LSM_BCS}_${OCEAN_TAG}  # location of Boundary Conditions
    #               setenv REPLAY_ANA_EXPID    REPLAY_UNSUPPORTED                                                # Default Analysis Experiment for REPLAY
    #               setenv REPLAY_ANA_LOCATION REPLAY_UNSUPPORTED                                                # Default Analysis Location   for REPLAY
    #               setenv M2_REPLAY_ANA_LOCATION REPLAY_UNSUPPORTED                                             # Default Analysis Location   for M2 REPLAY

    #               setenv SSTDIR     /ford1/share/gmao_SIteam/ModelData/@SSTNAME/${OGCM_IM}x${OGCM_JM}  # location of SST Boundary Conditions
    #               setenv CHMDIR     /ford1/share/gmao_SIteam/ModelData/fvInput_nc3         # locations of Aerosol Chemistry BCs
    #               setenv WRKDIR     /home/$LOGNAME                                         # user work directory
    #               setenv COUPLEDIR  /ford1/share/gmao_SIteam/ModelData/aogcm               # Coupled Ocean/Atmos Forcing
    #               set NX = 1
    #               set NY = 6
    #               set CNV_NX = ${NX}
    #               set CNV_NY = ${NY}
    #               # By default on desktop, just ignore IOSERVER for now
    #               set USE_IOSERVER = 0
    #               set NUM_OSERVER_NODES = 0
    #               set NUM_BACKEND_PES = 0
    #               set NCPUS_PER_NODE = 0
    # endif

    # #######################################################################
    # #                 Create Paths for HOME and EXP Directories
    # #         Note: Default Path will be kept in $HOME/.HOMDIRroot
    # #######################################################################

    # # HOME Directory (for scripts and RC files)
    # # -----------------------------------------
    #     set check =  FALSE
    # while( $check == FALSE )
    #   if( -e $HOME/.HOMDIRroot ) then
    #          set HOMDIRroot = `cat $HOME/.HOMDIRroot`
    #   else
    #          set HOMDIRroot = $HOME/geos5
    #   endif

    #   setenv HOMDIR_def $HOMDIRroot/$EXPID
    #   echo " "
    #   echo "Enter Desired Location for the ${C1}HOME${CN} Directory (to contain scripts and RC files)"
    #   echo "Hit ENTER to use Default Location:"
    #   echo "----------------------------------"
    #   echo "Default:  ${C2}${HOMDIR_def}${CN}"
    #   set   NUHOMDIR  = $<
    #   if( .$NUHOMDIR != . ) then
    #        setenv HOMDIR     $NUHOMDIR
    #        setenv HOMDIR_def $NUHOMDIR
    #   else
    #        setenv HOMDIR  $HOMDIR_def
    #   endif

    #   if( "$EXPID" != `basename $HOMDIR` ) then
    #        echo "\!\! The ${C1}HOME${CN} Directory MUST point to the ${C1}EXPID${CN}: ${C2}${EXPID}${CN}"
    #        echo " "
    #   else
    #       set check = TRUE
    #   endif
    # end
    # mkdir -p $HOMDIR


    # # EXP Directory (for Output Data and Restarts)
    # # --------------------------------------------
    #     set check =  FALSE
    # while( $check == FALSE )
    #   if( -e $HOME/.EXPDIRroot ) then
    #          set EXPDIRroot = `cat $HOME/.EXPDIRroot`
    #   else
    #          set EXPDIRroot = $WRKDIR
    #   endif
    #   setenv EXPDIR_def $EXPDIRroot/$EXPID
    #   echo ""
    #   echo "Enter Desired Location for the ${C1}EXPERIMENT${CN} Directory (to contain model output and restart files)"
    #   echo "Hit ENTER to use Default Location:"
    #   echo "----------------------------------"
    #   echo "Default:  ${C2}${EXPDIR_def}${CN}"
    #   set   NUEXPDIR  = $<
    #   if( .$NUEXPDIR != . ) then
    #        setenv EXPDIR     $NUEXPDIR
    #        setenv EXPDIR_def $NUEXPDIR
    #   else
    #        setenv EXPDIR  $EXPDIR_def
    #   endif

    #   if( "$EXPID" != `basename $EXPDIR` ) then
    #        echo "\!\! The ${C1}EXPERIMENT${CN} Directory MUST point to the ${C1}EXPID${CN}: ${C2}${EXPID}${CN}"
    #        echo " "
    #   else
    #       set check = TRUE
    #   endif
    # end
    # mkdir -p $EXPDIR



    # # Build HOME Root Directory
    # # -------------------------
    # @ n = 1
    # set root = `echo $HOMDIR | cut -d / -f$n`
    # while( .$root == . )
    # @ n = $n + 1
    # set root = `echo $HOMDIR | cut -d / -f$n`
    # end

    # set HOMDIRroot = ''
    # while( $root != $EXPID )
    # set HOMDIRroot = `echo ${HOMDIRroot}/${root}`
    # @ n = $n + 1
    # set root = `echo $HOMDIR | cut -d / -f$n`
    # end
    # if( -e $HOME/.HOMDIRroot ) /bin/rm $HOME/.HOMDIRroot
    # echo $HOMDIRroot > $HOME/.HOMDIRroot


    # # Build EXP Root Directory
    # # ------------------------
    # @ n = 1
    # set root = `echo $EXPDIR | cut -d / -f$n`
    # while( .$root == . )
    # @ n = $n + 1
    # set root = `echo $EXPDIR | cut -d / -f$n`
    # end

    # set EXPDIRroot = ''
    # while( $root != $EXPID )
    # set EXPDIRroot = `echo ${EXPDIRroot}/${root}`
    # @ n = $n + 1
    # set root = `echo $EXPDIR | cut -d / -f$n`
    # end
    # if( -e $HOME/.EXPDIRroot ) /bin/rm $HOME/.EXPDIRroot
    # echo $EXPDIRroot > $HOME/.EXPDIRroot

    # # Set CNVDIR alias
    # # ----------------

    # setenv CNVDIR $EXPDIR/convert

    # #######################################################################
    # #                       Locate Build Directory
    # #######################################################################

    # # GEOSagcm Build Directory
    # # ------------------------

    #   echo "Enter Location for ${C1}Build${CN} directory containing:  bin/ etc/ include/ etc..."
    #   echo "Hit ENTER to use Default Location:"
    #   echo "----------------------------------"
    #   echo "Default:  ${C2}${GEOSDEF}${CN}"

    #   set     GEOSDIR  = $GEOSDEF
    #   set   NUGEOSDIR  = $<
    #   if( .$NUGEOSDIR != . ) then
    #        set GEOSDIR = $NUGEOSDIR
    #   endif

    #   set check =  FALSE

    # GEOSDIR:
    # while( $check == FALSE )

    #   if(! (-e $GEOSDIR/bin ) ) then
    #        echo " "
    #        echo "\!\! ${C1}WARNING${CN} \!\!"
    #        echo "\!\! This Sandbox has not been built"
    #        echo "\!\! Please enter a valid location for the ${C1}GEOSagcm Build${CN}"
    #        echo "\!\! or ... "
    #        echo "\!\! Enter ${C1}C${CN} to continue without a Build"
    #        echo " "

    #        set        NUGEOSDIR  = $<
    #        if(      .$NUGEOSDIR == . ) then
    #               set   GEOSDIR  = $GEOSDEF

    #        else if( .$NUGEOSDIR == .C | .$NUGEOSDIR == .c ) then
    #                 set GEOSDIR  = $GEOSDEF
    #                 set check    = TRUE
    #        else
    #                 set GEOSDIR = $NUGEOSDIR
    #        endif

    #        goto GEOSDIR
    #   else
    #        set check = TRUE
    #   endif
    # end

    # #
    # # GEOSBIN does point to the bin/ directory in each
    # #
    # setenv GEOSSRC  ${GEOSDIR}
    # setenv GEOSBIN  ${GEOSDIR}/bin
    # setenv GEOSETC  ${GEOSDIR}/etc

    # setenv GEOSUTIL ${GEOSSRC}
    # setenv GCMVER   `cat ${GEOSETC}/.AGCM_VERSION`

    # #######################################################################
    # #                    Check for Group ID Sponsor Code
    # #         Note: Default GROUP will be kept in $HOME/.GROUProot
    # #######################################################################

    #        set GROUPS    = `groups`
    # if( -e $HOME/.GROUProot ) then
    #        set GROUProot = `cat $HOME/.GROUProot`
    # else
    #        set GROUProot = $GROUPS[1]
    # endif

    # echo " "
    # echo "Current GROUPS: ${GROUPS}"
    # echo "Enter your ${C1}GROUP ID${CN} for Current EXP: (Default: ${C2}${GROUProot}${CN})"
    # echo "-----------------------------------"
    # set   NUGROUP  = $<
    # if( .$NUGROUP != . ) then
    #      setenv GROUP $NUGROUP
    # else
    #      setenv GROUP $GROUProot
    # endif

    # if( -e $HOME/.GROUProot ) /bin/rm $HOME/.GROUProot
    # echo $GROUP > $HOME/.GROUProot

    # #######################################################################
    # #      Copy Model Executable and RC Files to Experiment Directory
    # #######################################################################

    # mkdir -p  $EXPDIR/RC

    # # Copy over all files and subdirs in install/etc, keeping symlinks
    # cp -RP $GEOSDIR/etc/* $EXPDIR/RC

    # # Remove templated files in RC as they are unneeded and confusing for experiments
    # rm -f $EXPDIR/RC/*.tmpl
    # rm -f $EXPDIR/RC/fvcore_layout.rc

    # # Copy or link GEOSgcm.x if USING_SINGULARITY is FALSE
    # if ( $USING_SINGULARITY == FALSE ) then
    #    if ( $LINKX == "TRUE" ) then
    #       if ( -e $GEOSBIN/GEOSgcm.x ) ln -s $GEOSBIN/GEOSgcm.x $EXPDIR
    #    else
    #       if ( -e $GEOSBIN/GEOSgcm.x ) cp $GEOSBIN/GEOSgcm.x $EXPDIR
    #    endif

    #    # Set a couple variables for sed'ing out bits of the run script
    #    set SINGULARITY_BUILD = "#DELETE"
    #    set NATIVE_BUILD = ""
    # else
    #    # Set a couple variables for sed'ing out bits of the run script
    #    set SINGULARITY_BUILD = ""
    #    set NATIVE_BUILD = "#DELETE"
    # endif

    # #######################################################################
    # #               Set Recommended MPI Stack Settings
    # #######################################################################

    # # By default do not write restarts by oserver
    # set RESTART_BY_OSERVER = NO

    # /bin/rm -f $HOMDIR/SETENV.commands

    # # If we are using singularity, we can advise on MPI stack settings

    # if( $USING_SINGULARITY == TRUE ) then

    # cat > $HOMDIR/SETENV.commands << EOF
    # # These are recommended settings for MPI stacks

    # # Intel MPI
    # # ---------
    # #setenv I_MPI_ADJUST_ALLREDUCE 12
    # #setenv I_MPI_ADJUST_GATHERV 3
    # # This flag prints out the Intel MPI state. Uncomment if needed
    # #setenv I_MPI_DEBUG 9

    # # OpenMPI
    # # -------
    # # Open MPI and GEOS has had issues with restart writing. Having the
    # # oserver write them can be orders of magnitude faster. To enable this
    # # set WRITE_RESTART_BY_OSERVER to YES in AGCM.rc

    # # This turns off an annoying warning when running
    # # Open MPI on a system where TMPDIRs are on a networked
    # # file system
    # #setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0

    # # HPE MPT
    # # -------
    # #setenv MPI_COLL_REPRODUCIBLE
    # #setenv SLURM_DISTRIBUTION block
    # #unsetenv MPI_MEMMAP_OFF
    # #unsetenv MPI_NUM_MEMORY_REGIONS
    # #setenv MPI_XPMEM_ENABLED yes
    # #unsetenv SUPPRESS_XPMEM_TRIM_THRESH
    # #setenv MPI_LAUNCH_TIMEOUT 40
    # # Testing at NAS shows that coupled runs *require* MPI_SHEPHERD=true
    # # to run. We believe this is due to LD_PRELOAD. For now we only set
    # # this for coupled runs.
    # #setenv MPI_SHEPHERD true
    # EOF

    # else

    # if( $MPI == openmpi ) then

    # # Open MPI and GEOS has issues with restart writing. Having the
    # # oserver write them can be orders of magnitude faster
    # set RESTART_BY_OSERVER = YES

    # # This turns off an annoying warning when running
    # # Open MPI on a system where TMPDIRs are on a networked
    # # file system

    # cat > $HOMDIR/SETENV.commands << EOF
    #    setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0
    # EOF

    # # The below settings seem to be recommended for hybrid
    # # systems using MVAPICH2 but could change

    # else if( $MPI == mvapich2 ) then

    # cat > $HOMDIR/SETENV.commands << EOF
    #    setenv MV2_ENABLE_AFFINITY     0
    #    setenv SLURM_DISTRIBUTION block
    #    setenv MV2_MPIRUN_TIMEOUT 100
    #    setenv MV2_GATHERV_SSEND_THRESHOLD 256
    # EOF

    # else if( $MPI == mpt ) then

    # cat > $HOMDIR/SETENV.commands << EOF

    #    setenv MPI_COLL_REPRODUCIBLE
    #    setenv SLURM_DISTRIBUTION block

    #    #setenv MPI_DISPLAY_SETTINGS 1
    #    #setenv MPI_VERBOSE 1

    #    unsetenv MPI_MEMMAP_OFF
    #    unsetenv MPI_NUM_MEMORY_REGIONS
    #    setenv MPI_XPMEM_ENABLED yes
    #    unsetenv SUPPRESS_XPMEM_TRIM_THRESH

    #    setenv MPI_LAUNCH_TIMEOUT 40

    #    # For some reason, PMI_RANK is randomly set and interferes
    #    # with binarytile.x and other executables.
    #    unsetenv PMI_RANK

    #    # Often when debugging on MPT, the traceback from Intel Fortran
    #    # is "absorbed" and only MPT's errors are displayed. To allow the
    #    # compiler's traceback to be displayed, uncomment this environment
    #    # variable
    #    #setenv FOR_IGNORE_EXCEPTIONS false

    # EOF

    # # Testing at NAS shows that coupled runs *require* MPI_SHEPHERD=true
    # # to run. We believe this is due to LD_PRELOAD. For now we only set
    # # this for coupled runs.
    # if( $OGCM == TRUE ) then
    #    set MPT_SHEPHERD = "setenv MPI_SHEPHERD true"
    # endif

    # else if( $MPI == intelmpi ) then

    # cat > $HOMDIR/SETENV.commands << EOF
    # setenv I_MPI_ADJUST_ALLREDUCE 12
    # setenv I_MPI_ADJUST_GATHERV 3

    # # This flag prints out the Intel MPI state. Uncomment if needed
    # #setenv I_MPI_DEBUG 9
    # EOF

    # # These are options determined to be useful at NCCS
    # # Not setting generally as they are more fabric/cluster
    # # specific compared to the above adjustments
    # if ( $SITE == 'NCCS' ) then

    # cat >> $HOMDIR/SETENV.commands << EOF
    # setenv I_MPI_SHM_HEAP_VSIZE 512
    # setenv PSM2_MEMORY large
    # setenv I_MPI_EXTRA_FILESYSTEM 1
    # setenv I_MPI_EXTRA_FILESYSTEM_FORCE gpfs
    # EOF

    # endif # if NCCS

    # endif # if mpi

    # endif # if singularity

    # #######################################################################
    # #               Create Local Scripts and Resource Files
    # #######################################################################

    # cat >      $HOMDIR/sedfile << EOF
    # /@SETENVS/ {
    # t success
    # : success
    # r $HOMDIR/SETENV.commands
    # d
    # }

    # s?@GCMVER?$GCMVER?g
    # s?@EXPSRC?$GEOSTAG?g
    # s?@EXPID?$EXPID?g
    # s?@RUN_N?$RUN_N?g
    # s?@RUN_FN?$RUN_FN?g
    # s?@RUN_FT?$RUN_FT?g
    # s?@RUN_T?$RUN_T?g
    # s?@RUN_P?$RUN_P?g
    # s?@RUN_FP?$RUN_FP?g
    # s?@RUN_Q?$RUN_Q?g
    # s?@POST_N?$POST_N?g
    # s?@POST_T?$POST_T?g
    # s?@POST_P?$POST_P?g
    # s?@POST_Q?$POST_Q?g
    # s?@MOVE_N?$MOVE_N?g
    # s?@PLOT_N?$PLOT_N?g
    # s?@PLOT_T?$PLOT_T?g
    # s?@PLOT_P?$PLOT_P?g
    # s?@PLOT_Q?$PLOT_Q?g
    # s?@MOVE_Q?$MOVE_Q?g
    # s?@MOVE_P?$MOVE_P?g
    # s?@ARCHIVE_N?$ARCHIVE_N?g
    # s?@ARCHIVE_T?$ARCHIVE_T?g
    # s?@ARCHIVE_P?$ARCHIVE_P?g
    # s?@ARCHIVE_Q?$ARCHIVE_Q?g
    # s?@REGRESS_N?$REGRESS_N?g
    # s?@CONVERT_N?$CONVERT_N?g
    # s?@CONVERT_P?$CONVERT_P?g
    # s?@CONVERT_T?$CONVERT_T?g
    # s?@CNV_NX?$CNV_NX?g
    # s?@CNV_NY?$CNV_NY?g
    # s?@BCSDIR?$BCSDIR?g
    # s?@SSTDIR?$SSTDIR?g
    # s?@SSTNAME?$SSTNAME?g
    # s?@OCEANOUT?$OCEANOUT?g
    # s?@LSMBCS?$LSM_BCS?g
    # s?@EMIP_BCS_IN?$EMIP_BCS_IN?g
    # s?@EMIP_MERRA2?$EMIP_MERRA2?g
    # s?@BCSTAG?$OCEAN_TAG?g
    # s?@SSTFILE?$SSTFILE?g
    # s?@ICEFILE?$ICEFILE?g
    # s?@KPARFILE?$KPARFILE?g
    # s?@CHMDIR?$CHMDIR?g
    # s?@COUPLEDIR?$COUPLEDIR?g
    # s?@EXPDIR?$EXPDIR?g
    # s?@EXPDSC?$EXPDSC?g
    # s?@HOMDIR?$HOMDIR?g
    # s?@CNVDIR?$CNVDIR?g
    # s?@BATCH_GROUP?${BATCH_GROUP}${GROUP}?g
    # s?@BATCH_TIME?$BATCH_TIME?g
    # s?@BATCH_CMD?$BATCH_CMD?g
    # s?@BATCH_JOBNAME?$BATCH_JOBNAME?g
    # s?@BATCH_OUTPUTNAME?$BATCH_OUTPUTNAME?g
    # s?@BATCH_JOINOUTERR?$BATCH_JOINOUTERR?g
    # s?@SITE?$SITE?g
    # s?@GEOSDIR?$GEOSDIR?g
    # s?@GEOSSRC?$GEOSSRC?g
    # s?@GEOSBIN?$GEOSBIN?g
    # s?@GEOSETC?$GEOSETC?g
    # s?@GEOSUTIL?$GEOSUTIL?g
    # s?@SINGULARITY_BUILD?$SINGULARITY_BUILD?g
    # s?@NATIVE_BUILD?$NATIVE_BUILD?g
    # s?@MPT_SHEPHERD?$MPT_SHEPHERD?g

    # s?@CHECKPOINT_TYPE?default?g

    # s?@OGCM_NX?$OGCM_NX?g
    # s?@OGCM_NY?$OGCM_NY?g
    # s?@OGCM_NPROCS?$OGCM_NPROCS?g

    # s?@OBSERVER_FRQ?0?g
    # s?RECORD_?#RECORD_?g

    # s?@DASTUNING?#?g

    # s?>>>FORCEDAS<<<?$FORCEDAS?g
    # s?>>>FORCEGCM<<<?$FORCEGCM?g
    # s?@COUPLED?$COUPLED?g
    # s?@CLDMICRO?$CLDMICRO?g
    # s?@MOM5?$MOM5?g
    # s?@MOM6?$MOM6?g
    # s?@DATAOCEAN?$DATAOCEAN?g
    # s?>>>GOCART<<<?$GOCART?g
    # s?@OPS_SPECIES?$OPS_SPECIES?g
    # s?@CMIP_SPECIES?$CMIP_SPECIES?g
    # s?@MERRA2OX_SPECIES?$MERRA2OX_SPECIES?g
    # s?>>>FVCUBED<<<?$FVCUBED?g
    # s?>>>HIST_GOCART<<<?$HIST_GOCART?g
    # s?>>>OSTIA<<<?$OSTIA?g
    # s?>>>HIST_CATCHCN<<<?$HIST_CATCHCN?g
    # s?>>>GCMRUN_CATCHCN<<<?$GCMRUN_CATCHCN?g
    # s?>>>EMIP_OLDLAND<<<?$EMIP_OLDLAND?g
    # s?>>>EMIP_NEWLAND<<<?$EMIP_NEWLAND?g
    # s?@LSM_PARMS?$LSM_PARMS?g
    # s?@OCEAN_NAME?$OCEAN_NAME?g
    # s?@OCEAN_PRELOAD?$OCEAN_PRELOAD?g

    # s?>>>4DIAUDAS<<<?#DELETE?g
    # s?>>>REGULAR_REPLAY<<<?#?g
    # s?>>>REGULAR_REPLAY_GMAO<<<?#?g
    # s?>>>REGULAR_REPLAY_NCEP<<<?#DELETE?g
    # s?>>>REGULAR_REPLAY_ECMWF<<<?#DELETE?g
    # s?ana4replay.eta.%y4%m2%d2_%h2z.nc4?/discover/nobackup/projects/gmao/merra2/data/ana/MERRA2_all/Y%y4/M%m2/MERRA2.ana.eta.%y4%m2%d2_%h2z.nc4?g

    # s?@REPLAY_ANA_EXPID?$REPLAY_ANA_EXPID?g
    # s?@REPLAY_ANA_LOCATION?$REPLAY_ANA_LOCATION?g
    # s?@M2_REPLAY_ANA_LOCATION?$M2_REPLAY_ANA_LOCATION?g

    # s?@OX_RELAXTIME?259200.?g
    # s?@PCHEM_CLIM_YEARS?$PCHEM_CLIM_YEARS?g

    # s?@RATS_PROVIDER?$RATS_PROVIDER?g
    # s?@AERO_PROVIDER?$AERO_PROVIDER?g
    # s?@OANA_PROVIDER?PCHEM?g
    # s?@EMISSIONS?$EMISSIONS?g

    # s^@DYCORE^$DYCORE^g
    # s^@AGCM_GRIDNAME^$AGCM_GRIDNAME^g
    # s^@OGCM_GRIDNAME^$OGCM_GRIDNAME^g

    # s?@IS_FCST?$IS_FCST?g
    # s^@BOOT^YES^g
    # s^@BCSRES^$BCSRES^g
    # s^@OCEANtag^$OCEAN_RES^g
    # s^@ATMOStag^$ATMOS_RES^g
    # s^@RES_DATELINE^$RES_DATELINE^g
    # s^@TILEDATA^$TILEDATA^g
    # s^@TILEBIN^$TILEBIN^g
    # s/@DT/$DT/g
    # s/@SOLAR_DT/$SOLAR_DT/g
    # s/@IRRAD_DT/$IRRAD_DT/g
    # s/@OCEAN_DT/$OCEAN_DT/g
    # s/@CHEM_DT/$CHEM_DT/g
    # s/@NX/$NX/g
    # s/@NY/$NY/g
    # s/@USE_SHMEM/$USE_SHMEM/g
    # s/@USE_IOSERVER/$USE_IOSERVER/g
    # s/@NUM_OSERVER_NODES/$NUM_OSERVER_NODES/g
    # s/@NUM_BACKEND_PES/$NUM_BACKEND_PES/g
    # s/@RESTART_BY_OSERVER/$RESTART_BY_OSERVER/g
    # s/@NCPUS_PER_NODE/$NCPUS_PER_NODE/g
    # s/@NUM_READERS/$NUM_READERS/g
    # s/@NUM_WRITERS/$NUM_WRITERS/g
    # s/@LATLON_AGCM/$LATLON_AGCM/g
    # s?@LATLON_OGCM?$LATLON_OGCM?g
    # s/@CUBE_AGCM/$CUBE_AGCM/g
    # s?@CUBE_OGCM?$CUBE_OGCM?g
    # s/@GRID_TYPE/$GRID_TYPE/g
    # s/@AGCM_NF/$AGCM_NF/g
    # s/@AGCM_IM/$AGCM_IM/g
    # s/@AGCM_JM/$AGCM_JM/g
    # s/@AGCM_LM/$AGCM_LM/g
    # s/@OGCM_IM/$OGCM_IM/g
    # s/@OGCM_JM/$OGCM_JM/g
    # s/@OGCM_LM/$OGCM_LM/g
    # s/@OGCM_NF/$OGCM_NF/g
    # s/@OGCM_GRID_TYPE/$OGCM_GRID_TYPE/g
    # s/@BEG_DATE/${BEG_DATE}/g
    # s/@END_DATE/${END_DATE}/g
    # s/@JOB_SGMT/${JOB_SGMT}/g
    # s/@NUM_SGMT/${NUM_SGMT}/g

    # s/@INTERPOLATE_SST/$INTERPOLATE_SST/g
    # s/@HIST_IM/$HIST_IM/g
    # s/@HIST_JM/$HIST_JM/g

    # s/@ISCCP_SATSIM/1/g
    # s/@MODIS_SATSIM/0/g
    # s/@RADAR_SATSIM/0/g
    # s/@LIDAR_SATSIM/0/g
    # s/@MISR_SATSIM/0/g
    # s/@SATSIM/0/g

    # s/@USE_SKIN_LAYER/1/g
    # s/@ANALYZE_TS/0/g

    # s/@LSM_CHOICE/$LSM_CHOICE/g

    # s/@MP_MG1/$MP_MG1/g
    # s/@MP_GFDL/$MP_GFDL/g
    # s/@MP_NO_USE_WSUB/$MP_NO_USE_WSUB/g
    # s?@GFDL_HYDRO?$GFDL_HYDRO?g
    # s?@FV_NWAT?$FV_NWAT?g
    # s?@FV_ZTRACER?$FV_ZTRACER?g
    # s?@FV_MAKENH?$FV_MAKENH?g
    # s?@FV_HYDRO?$FV_HYDRO?g
    # s?@FV_SATADJ?$FV_SATADJ?g

    # EOF

    # # Added FV3 Specific Parameters
    # # -----------------------------

    # cat >> $HOMDIR/sedfile << EOF

    # s^@HYDROSTATIC^$HYDROSTATIC^g
    # s/@GRID_FILE/$GRID_FILE/g

    # EOF

    # set FILES = "gcm_run.j          \
    #              gcm_post.j         \
    #              gcm_archive.j      \
    #              gcm_regress.j      \
    #              gcm_convert.j      \
    #              gcm_plot.tmpl      \
    #              gcm_quickplot.csh  \
    #              gcm_moveplot.j     \
    #              gcm_forecast.tmpl  \
    #              gcm_forecast.setup \
    #              gcm_emip.setup     \
    #              CAP.rc.tmpl        \
    #              AGCM.rc.tmpl       \
    #              HISTORY.rc.tmpl    \
    #              logging.yaml       "
    # set FILES = `echo $FILES`

    # if( $OGCM == TRUE ) then
    #    if ( $OCEAN_NAME == "MOM" ) then
    #       /bin/cp ${ETCDIR}/MOM5/geos5/${OGCM_IM}x${OGCM_JM}/INPUT/input.nml .
    #       /bin/cp ${ETCDIR}/MOM5/geos5/${OGCM_IM}x${OGCM_JM}/INPUT/*table .
    #       set FILES = "$FILES field_table "
    #    else if ( $OCEAN_NAME == "MOM6" ) then
    #       /bin/cp ${ETCDIR}/MOM6/mom6_app/${OGCM_IM}x${OGCM_JM}/MOM_input .
    #       /bin/cp ${ETCDIR}/MOM6/mom6_app/${OGCM_IM}x${OGCM_JM}/MOM_override .
    #       /bin/cp ${ETCDIR}/MOM6/mom6_app/${OGCM_IM}x${OGCM_JM}/input.nml .
    #       /bin/cp ${ETCDIR}/MOM6/mom6_app/${OGCM_IM}x${OGCM_JM}/*table .
    #       set FILES = "$FILES MOM_input MOM_override data_table "
    #    endif

    #    /bin/cp ${GEOSDEF}/coupled_diagnostics/g5lib/plotocn.j .
    #    /bin/cp ${GEOSDEF}/coupled_diagnostics/g5lib/confocn.py __init__.py

    # set FILES = "$FILES      \
    #              input.nml   \
    #              diag_table  \
    #              plotocn.j   \
    #              __init__.py"

    # set FILES = `echo $FILES`
    # endif

    # set FILES = "$FILES \
    #              fvcore_layout.rc"
    # set FILES = `echo $FILES`

    # echo " "

    # # Operate on files in ETCDIR

    # foreach FILE ($FILES)

    #    /bin/rm -f $HOMDIR/tmpfile
    #    /bin/rm -f $HOMDIR/$FILE

    #    if ( $FILE == "HISTORY.rc.tmpl" ) then
    #       cat            $TMPHIST > $HOMDIR/tmpfile
    #    else if ( -e $BINDIR/$FILE ) then
    #       cat       $BINDIR/$FILE > $HOMDIR/tmpfile
    #    else if ( -e $ETCDIR/$FILE ) then
    #       cat       $ETCDIR/$FILE > $HOMDIR/tmpfile
    #    else
    #       echo "ERROR! Cannot find $FILE in $BINDIR or $ETCDIR!"
    #       exit 2
    #    endif

    #    sed -f $HOMDIR/sedfile $HOMDIR/tmpfile > $HOMDIR/$FILE

    #    echo "Creating ${C1}${FILE}${CN} for Experiment: $EXPID "
    #    chmod 755 $HOMDIR/$FILE

    # end

    # /bin/rm -f $HOMDIR/SETENV.commands

    # /bin/rm -f $TMPHIST

    # echo $HOMDIR > $EXPDIR/.HOMDIR
    # echo " "

    # #######################################################################
    # #                 Produce Final script and .rc files
    # #######################################################################

    # # Comment or UN-Comment RESTARTS based on EXP Configuration
    # # ---------------------------------------------------------
    # set LH2O       = FALSE
    # set LMAM       = FALSE
    # set LCARMA     = FALSE
    # set LGMICHEM   = FALSE
    # set LSTRATCHEM = FALSE

    # set RSNAMES = "LH2O LMAM LCARMA LGMICHEM LSTRATCHEM"
    # set RSTYPES = "INTERNAL IMPORT"

    # set FILE = AGCM.rc.tmpl

    # if( -e $HOMDIR/$FILE ) set LOCDIR = $HOMDIR
    # if( -e $EXPDIR/$FILE ) set LOCDIR = $EXPDIR

    # foreach rsname ($RSNAMES)
    #    set  name = `echo $rsname | cut -b2-`
    #    set  test = `eval echo \$$rsname`
    #    if( $test == FALSE ) then
    #        foreach type ($RSTYPES)
    #           set  TMPCMD = `mktemp`
    #           set  string = ${name}_${type}
    #           /bin/rm -f $LOCDIR/$FILE.tmp
    #           /bin/mv -f $LOCDIR/$FILE $LOCDIR/$FILE.tmp
    #           echo cat   $LOCDIR/$FILE.tmp \| awk \'\{if \( \$1 \~ \"${string}\" \) \
    #                \{sub \( \/${string}\/ ,\"\#${string}\"  \)\;print\} else print\}\' \> $LOCDIR/$FILE > $TMPCMD
    #           chmod +x   $TMPCMD
    #                      $TMPCMD
    #           /bin/rm -f $TMPCMD
    #        end
    #    endif
    # end
    # /bin/rm -f $LOCDIR/$FILE.tmp


    # # Delete or Enable EXP Configuration Variables
    # # --------------------------------------------
    #     set FILES = "AGCM.rc.tmpl gcm_run.j gcm_forecast.tmpl gcm_forecast.setup HISTORY.rc.tmpl gcm_convert.j gcm_regress.j gcm_post.j gcm_emip.setup gcm_plot.tmpl"
    # foreach FILE ($FILES)

    # if( -e $HOMDIR/$FILE ) set LOCDIR = $HOMDIR
    # if( -e $EXPDIR/$FILE ) set LOCDIR = $EXPDIR

    # /bin/rm -f $LOCDIR/$FILE.tmp
    # /bin/mv -f $LOCDIR/$FILE $LOCDIR/$FILE.tmp
    #     cat    $LOCDIR/$FILE.tmp | awk '{ if ( $1  !~ "#DELETE") { print } }' > $LOCDIR/$FILE
    # /bin/rm -f $LOCDIR/$FILE.tmp

    # end

    # chmod   +x $HOMDIR/gcm_run.j
    # chmod   +x $HOMDIR/gcm_convert.j
    # chmod   +x $HOMDIR/gcm_forecast.setup
    # chmod   +x $HOMDIR/gcm_regress.j
    # chmod   +x $HOMDIR/gcm_post.j
    # chmod   +x $HOMDIR/gcm_emip.setup

    # #######################################################################
    # #         Finalizing Experiment Directories and Chem Registry
    # #######################################################################

    # # Check for Experiment Sub-Directories
    # # ------------------------------------
    # if(! -e $EXPDIR/post      ) mkdir -p $EXPDIR/post
    # if(! -e $EXPDIR/plot      ) mkdir -p $EXPDIR/plot
    # if(! -e $EXPDIR/archive   ) mkdir -p $EXPDIR/archive
    # if(! -e $EXPDIR/regress   ) mkdir -p $EXPDIR/regress
    # if(! -e $EXPDIR/forecasts ) mkdir -p $EXPDIR/forecasts
    # if(! -e $EXPDIR/convert   ) mkdir -p $EXPDIR/convert

    # if( $OGCM == TRUE ) if(! -e $EXPDIR/RESTART ) mkdir -p $EXPDIR/RESTART


    # # Rename and Move Files to Relevant Experiment Work Sub-Directories
    # # -----------------------------------------------------------------
    # /bin/mv $HOMDIR/gcm_post.j          $EXPDIR/post
    # /bin/mv $HOMDIR/gcm_plot.tmpl       $EXPDIR/plot
    # /bin/mv $HOMDIR/gcm_quickplot.csh   $EXPDIR/plot
    # /bin/mv $HOMDIR/gcm_moveplot.j      $EXPDIR/plot
    # /bin/mv $HOMDIR/gcm_archive.j       $EXPDIR/archive
    # /bin/mv $HOMDIR/gcm_regress.j       $EXPDIR/regress
    # /bin/mv $HOMDIR/gcm_convert.j       $EXPDIR/convert
    # /bin/mv $HOMDIR/gcm_forecast.tmpl   $EXPDIR/forecasts
    # /bin/mv $HOMDIR/gcm_forecast.setup  $EXPDIR/forecasts
    # /bin/cp $GEOSUTIL/post/plot.rc      $EXPDIR/plot
    # /bin/cp $GEOSUTIL/post/post.rc      $EXPDIR/post

    # /bin/mv $HOMDIR/CAP.rc.tmpl         $HOMDIR/CAP.rc
    # /bin/mv $HOMDIR/AGCM.rc.tmpl        $HOMDIR/AGCM.rc
    # /bin/mv $HOMDIR/HISTORY.rc.tmpl     $HOMDIR/HISTORY.rc

    # if( $OGCM == TRUE ) /bin/mv $HOMDIR/plotocn.j       $EXPDIR/plot

    # #######################################################################
    # #       Modify RC Directory for LM and GOCART.data/GOCART Options
    # #######################################################################

    # # Modify RC Files for LM
    # # ----------------------
    # if( $AGCM_LM != 72 ) then
    #     set files = `ls -1 $EXPDIR/RC/*.rc $EXPDIR/RC/*.yaml`
    #     foreach file ($files)
    #        /bin/rm -f    $EXPDIR/RC/dummy
    #        /bin/mv $file $EXPDIR/RC/dummy
    #        cat $EXPDIR/RC/dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" \
    #                             | sed -e "s|z72|z${AGCM_LM}|g"     \
    #                             | sed -e "s|_72_|_${AGCM_LM}_|g"   > $file
    #      end
    # endif


    # # Turn on PCHEM
    # # -------------
    # if( $RATS_PROVIDER == PCHEM ) then
    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_PCHEM:") { sub(/FALSE/,"TRUE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc
    # endif

    # # Turn on GOCART.data
    # # -------------------
    # if( $AERO_PROVIDER == GOCART.data ) then
    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_GOCART_DATA:") { sub(/FALSE/,"TRUE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc
    # endif

    # # Turn on TR GridComp
    # # -------------------
    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_TR:") { sub(/FALSE/,"TRUE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc

    # # Update LAND_PARAMS Choices
    # # ------------------------------------------------------------------
    # if( $LSM_BCS == "Icarus-NLv3" ) then
    #     /bin/mv $EXPDIR/RC/GEOS_SurfaceGridComp.rc $EXPDIR/RC/GEOS_SurfaceGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_SurfaceGridComp.tmp | sed -e 's?# GEOSagcm=>?            ?g' > $EXPDIR/RC/GEOS_SurfaceGridComp.rc
    # else
    #     /bin/mv $EXPDIR/RC/GEOS_SurfaceGridComp.rc $EXPDIR/RC/GEOS_SurfaceGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_SurfaceGridComp.tmp | sed -e 's?# GEOSagcm=>?            ?g' > $EXPDIR/RC/GEOS_SurfaceGridComp.rc

    #     /bin/mv $EXPDIR/RC/GEOS_SurfaceGridComp.rc $EXPDIR/RC/GEOS_SurfaceGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_SurfaceGridComp.tmp | \
    #     awk '{ if ($1~"LAND_PARAMS:") { sub(/NRv7.2/,"Icarus") }; print }' > $EXPDIR/RC/GEOS_SurfaceGridComp.rc

    #     /bin/mv $EXPDIR/RC/GEOS_SurfaceGridComp.rc $EXPDIR/RC/GEOS_SurfaceGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_SurfaceGridComp.tmp | \
    #     awk '{ if ($1~"Z0_FORMULATION:") { sub(/4/,"2") }; print }' > $EXPDIR/RC/GEOS_SurfaceGridComp.rc
    # endif

    # # Turn on GOCART
    # # --------------
    # if( $GOKART == TRUE ) then
    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_GOCART:") { sub(/FALSE/,"TRUE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc

    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_GOCART_DATA:") { sub(/TRUE/,"FALSE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc
    # else
    #     /bin/mv $EXPDIR/RC/GEOS_ChemGridComp.rc $EXPDIR/RC/GEOS_ChemGridComp.tmp
    #     cat $EXPDIR/RC/GEOS_ChemGridComp.tmp | \
    #     awk '{ if ($1~"ENABLE_GOCART:") { sub(/TRUE/,"FALSE") }; print }' > $EXPDIR/RC/GEOS_ChemGridComp.rc

    #     /bin/mv $EXPDIR/RC/Chem_Registry.rc $EXPDIR/RC/Chem_Registry.tmp
    #     cat $EXPDIR/RC/Chem_Registry.tmp | \
    #     awk '{ if ( $1 ~ "doing" ) { sub(/yes/, "no" ) }; print }' > $EXPDIR/RC/Chem_Registry.rc
    # endif

    # # Remove FVWORK/EXPID syntax from GAAS_GridComp.rc for use in REPLAY
    # # ------------------------------------------------------------------
    #     /bin/mv $EXPDIR/RC/GAAS_GridComp.rc $EXPDIR/RC/GAAS_GridComp.tmp
    #     cat $EXPDIR/RC/GAAS_GridComp.tmp | sed -e 's?${FVWORK}/${EXPID}.?aod/Y%y4/M%m2/@ANA_EXPID.?g' > $EXPDIR/RC/GAAS_GridComp.rc


    # # Turn on RATS_PROVIDER
    # # ---------------------
    # if( $RATS_PROVIDER == PCHEM ) then
    #     /bin/mv $EXPDIR/RC/Chem_Registry.rc $EXPDIR/RC/Chem_Registry.tmp
    #     cat $EXPDIR/RC/Chem_Registry.tmp | \
    #     awk '{if ( $1 ~ "doing") { if ( $1 ~ "PC") sub(/no/, "yes" ); print;} else print }' > $EXPDIR/RC/Chem_Registry.rc
    # endif

    # # Turn on TR GridComp
    # # ---------------------
    #     /bin/mv $EXPDIR/RC/Chem_Registry.rc $EXPDIR/RC/Chem_Registry.tmp
    #     cat $EXPDIR/RC/Chem_Registry.tmp | \
    #     awk '{if ( $1 ~ "doing") { if ( $1 ~ "TR") sub(/no/, "yes" ); print;} else print }' > $EXPDIR/RC/Chem_Registry.rc



    # #######################################################################
    # #                       Echo Settings and Messages
    # #######################################################################

    # echo "Done!"
    # echo "-----"
    # echo " "
    # echo "Build Directory: ${C2}${GEOSDIR}${CN}"
    # echo "----------------"
    # echo " "
    # if ( $USING_SINGULARITY == TRUE ) then
    #    echo "Note: As --singularity was passed in, we will run "
    #    echo "      GEOSgcm.x from the installation bin directory."
    # else
    #    echo " "
    #    echo "The following executable has been ${EXE_VERB} to your Experiment Directory:"
    #    echo "----------------------------------------------------------------------"
    #    echo "${C2}$GEOSBIN/GEOSgcm.x${CN}"
    #    echo " "
    # endif
    # echo " "
    # echo "You must now copy your ${C1}Initial Conditions${CN} into: "
    # echo "----------------------------------------------- "
    # echo "${C2}${EXPDIR}${CN}"
    # echo ""
    # echo ""

    # #######################################################################
    # #                              Clean-Up
    # #######################################################################

    # if( -e $HOMDIR/tmpfile ) /bin/rm $HOMDIR/tmpfile
    # if( -e $HOMDIR/sedfile ) /bin/rm $HOMDIR/sedfile

    # #######################################################################
    # #                     Copy over Source Tarfile
    # #######################################################################

    # # NOTE: This variable is set at CMake time depending on
    # #       how the build was configured.
    # set INSTALL_TARFILE = @CFG_INSTALL_SOURCE_TARFILE@
    # set TARFILE_NAME = "@CMAKE_PROJECT_NAME@.tar.gz"

    # if ( $INSTALL_TARFILE == "TRUE" ) then

    #    # Make a src directory under EXPDIR to hold current Experiment files
    #    # ------------------------------------------------------------------
    #    /bin/rm -rf ${EXPDIR}/src
    #    mkdir   -p  ${EXPDIR}/src

    #    echo "Copying Build Source Code into ${C2}${EXPDIR}/src${CN}"
    #    # -----------------------------------------------------------
    #    if (-e ${GEOSDEF}/src/${TARFILE_NAME}) then
    #       cp ${GEOSDEF}/src/${TARFILE_NAME} ${EXPDIR}/src
    #    else
    #       echo "${GEOSDEF}/src/${TARFILE_NAME} not found yet CMake was asked to make and install a tarfile."
    #       echo "Something went wrong."
    #       exit 7
    #    endif
    #    echo ""

    # endif

    # #######################################################################

    # exit

    # #######################################################################
    # #                         Clone old Experiment
    # #######################################################################

    # DOCLONE:

    # #######################################################################
    # #                    Enter Clone ID and Description
    # #######################################################################

    # OLDEXP:
    # echo
    # echo "Enter the ${C1}location${CN} of the experiment to clone (where gcm_run.j is located):"
    # set CLONEDIR = $<

    # if ( $CLONEDIR == "") then
    #    goto OLDEXP
    # else if ( ! -d $CLONEDIR ) then
    #    echo
    #    echo "Could not find ${CLONEDIR}"
    #    goto OLDEXP
    # endif

    # # ------------------------------------------------------
    # # To setup the clone, we need to look in a couple files,
    # # so make sure they are readable
    # # ------------------------------------------------------

    # if ( ! -r $CLONEDIR/gcm_run.j ) then
    #    echo
    #    echo "$CLONEDIR/gcm_run.j is not readable. Please check permissions."
    #    exit 1
    # endif

    # if ( ! -r $CLONEDIR/HISTORY.rc ) then
    #    echo
    #    echo "$CLONEDIR/HISTORY.rc is not readable. Please check permissions."
    #    exit 1
    # endif

    # # -----------------------------------------
    # # Grab the old EXPID, and other information
    # # -----------------------------------------

    # set  OLDEXPID=`awk '/^EXPID/ {print $2}' $CLONEDIR/HISTORY.rc`
    # set OLDHOMDIR=`awk '/^setenv +HOMDIR/ {print $3}' $CLONEDIR/gcm_run.j`
    # set OLDEXPDIR=`awk '/^setenv +EXPDIR/ {print $3}' $CLONEDIR/gcm_run.j`
    # set   OLDUSER=`/bin/ls -l $CLONEDIR/gcm_run.j | awk '{print $3}'`

    # setenv ARCH      `uname`
    # setenv GEOSDIR  /`grep "setenv GEOSDIR" $CLONEDIR/gcm_run.j | cut -d'/' -f2-`
    # setenv GEOSSRC  ${GEOSDIR}
    # setenv GEOSBIN  ${GEOSDIR}/bin
    # setenv GEOSETC  ${GEOSDIR}/etc
    # setenv GEOSUTIL ${GEOSDIR}
    # setenv GCMVER   `cat ${GEOSETC}/.AGCM_VERSION`

    # # -------------------------------------------------
    # # Figure out how this person usually runs the model
    # # -------------------------------------------------

    # if ( ! -e $HOME/.HOMDIRroot || ! -e $HOME/.EXPDIRroot ) then
    #    if ( -e $HOME/.HOMDIRroot && ! -e $HOME/.EXPDIRroot ) then
    #       echo "$HOME/.EXPDIRroot was not found."
    #       echo "Please run gcm_setup in non-clone mode at least once to use this script."
    #       exit 1
    #    else if ( ! -e $HOME/.HOMDIRroot && -e $HOME/.EXPDIRroot ) then
    #       echo "$HOME/.HOMDIRroot was not found."
    #       echo "Please run gcm_setup in non-clone mode at least once to use this script."
    #       exit 1
    #    else
    #       echo "$HOME/.HOMDIRroot and $HOME/.EXPDIRroot were not found."
    #       echo "Please run gcm_setup in non-clone mode at least once to use this script."
    #       exit 1
    #    endif
    # endif

    # # MAT There are two thoughts here. You can either place the clone in
    # # the .HOMDIRroot/.EXPDIRroot, but that is only nice if you have one
    # # single place you put all your experiments. What if you have lots of
    # # directories? Instead, let us default to the directory root of the
    # # cloned experiment if the cloned experiment is yours. If it isn't your
    # # experiment you are cloning, then default to the values in the dotfile

    # if ( $OLDUSER == $LOGNAME) then
    #    set HOMDIRroot=`dirname $OLDHOMDIR`
    #    set EXPDIRroot=`dirname $OLDEXPDIR`
    # else
    #    set HOMDIRroot=`cat $HOME/.HOMDIRroot`
    #    set EXPDIRroot=`cat $HOME/.EXPDIRroot`
    # endif

    # echo "Setting HOMDIR to $HOMDIRroot"
    # echo "Setting EXPDIR to $EXPDIRroot"

    # if ( -e $HOME/.GROUProot ) then
    #    set GROUProot=`cat $HOME/.GROUProot`
    #    echo "Using account $GROUProot"
    # else
    #    echo "$HOME/.GROUProot not found."
    #    set GROUProot=`groups | awk '{print $1}'`
    #    echo "Based off of groups, setting account to $GROUProot"
    # endif

    # # -----------------------------------------------
    # # Find out if we are running the cube and/or OGCM
    # # -----------------------------------------------

    # # First we need to find out if we are running in coupled mode
    # # To do this, we look for "OCEAN_NAME" in AGCM.rc:

    # set OGCM = `grep "OCEAN_NAME" ${OLDHOMDIR}/AGCM.rc | wc -l`

    # if ( $OGCM == 1 ) then
    #    set OGCM = TRUE
    #    # Now we need to find out if we are running MOM or MOM6 by looking
    #    # at OCEAN_NAME: val and checking if it is MOM or MOM6
    #    set OCEAN_MODEL = `grep "OCEAN_NAME" ${OLDHOMDIR}/AGCM.rc | cut -d: -f2 | tr -d ' '`
    # else if ( $OGCM == 0 ) then
    #    set OGCM = FALSE
    # else
    #    echo "Found more than one OCEAN_NAME in ${OLDHOMDIR}/AGCM.rc"
    #    echo "This is not allowed. Please fix this and try again."
    #    exit 1
    # endif

    # # ------------------------------------------------
    # # Set the new EXPDIR and HOMDIR based on the roots
    # # ------------------------------------------------
    # set  NEWEXPID=$EXPID
    # set NEWEXPDIR=$EXPDIRroot/$NEWEXPID
    # set NEWHOMDIR=$HOMDIRroot/$NEWEXPID

    # # -----------------------------------------
    # # If the new EXPDIR and HOMDIR exist, exit!
    # # -----------------------------------------

    # if ( -d $NEWEXPDIR ) then
    #    echo "$NEWEXPDIR already exists! Exiting!"
    #    exit 2
    # endif

    # if ( -d $NEWHOMDIR ) then
    #    echo "$NEWHOMDIR already exists! Exiting!"
    #    exit 3
    # endif

    # # -----------------------------------
    # # Make all our needed temporary files
    # # -----------------------------------

    # onintr TRAP

    # set FILES_TO_PROCESS=`mktemp`
    # set OLDEXPFILES=`mktemp`
    # set NEWEXPFILES=`mktemp`
    # set COPYSCRIPT=`mktemp`
    # set SEDFILE=`mktemp`


    # # --------------------------
    # # Setup the files to process
    # # --------------------------

    # cat > $FILES_TO_PROCESS << EOF
    # EXPDIR/post/gcm_post.j
    # EXPDIR/plot/gcm_plot.tmpl
    # EXPDIR/plot/gcm_quickplot.csh
    # EXPDIR/plot/gcm_moveplot.j
    # EXPDIR/archive/gcm_archive.j
    # EXPDIR/regress/gcm_regress.j
    # EXPDIR/convert/gcm_convert.j
    # EXPDIR/forecasts/gcm_forecast.tmpl
    # EXPDIR/forecasts/gcm_forecast.setup
    # EXPDIR/plot/plot.rc
    # EXPDIR/post/post.rc
    # HOMDIR/CAP.rc
    # HOMDIR/AGCM.rc
    # HOMDIR/HISTORY.rc
    # HOMDIR/gcm_run.j
    # HOMDIR/gcm_emip.setup
    # HOMDIR/logging.yaml
    # EOF

    # if( $OGCM == TRUE ) then

    # # Some files are common to both MOM and MOM6
    # cat >> $FILES_TO_PROCESS << EOF
    # HOMDIR/input.nml
    # HOMDIR/diag_table
    # HOMDIR/__init__.py
    # EXPDIR/plot/plotocn.j
    # EOF

    # # Some files are specific to MOM
    # if ( $OCEAN_MODEL == "MOM" ) then
    # cat >> $FILES_TO_PROCESS << EOF
    # HOMDIR/field_table
    # EOF
    # endif

    # # Some files are specific to MOM6
    # if ( $OCEAN_MODEL == "MOM6" ) then
    # cat >> $FILES_TO_PROCESS << EOF
    # HOMDIR/MOM_override
    # HOMDIR/MOM_input
    # HOMDIR/data_table
    # EOF
    # endif

    # endif

    # cat >> $FILES_TO_PROCESS << EOF
    # HOMDIR/fvcore_layout.rc
    # EOF

    # # ------------------------------------------------
    # # Create two sets of files so we can copy from one
    # # directory to another.
    # # ------------------------------------------------

    # # Then alter them
    # # ---------------

    # sed -e "/^EXPDIR/ s#EXPDIR#$OLDEXPDIR#" \
    #     -e "/^HOMDIR/ s#HOMDIR#$OLDHOMDIR#"   $FILES_TO_PROCESS > $OLDEXPFILES

    # sed -e "/^EXPDIR/ s#EXPDIR#$NEWEXPDIR#" \
    #     -e "/^HOMDIR/ s#HOMDIR#$NEWHOMDIR#"   $FILES_TO_PROCESS > $NEWEXPFILES

    # # -----------------------------------------
    # # Now, use paste to join these two files...
    # # ...add a cp in front of the lines.
    # # -----------------------------------------

    # paste $OLDEXPFILES $NEWEXPFILES | sed -e "s/.*/cp -a &/" > $COPYSCRIPT

    # # ------------------------
    # # Make the new directories
    # # ------------------------

    # foreach file (`cat $NEWEXPFILES`)
    #    set dir=`dirname $file`
    #    /bin/mkdir -p $dir
    # end

    # # -------------------
    # # Run the copy script
    # # -------------------

    # sh $COPYSCRIPT

    # # ----------------------------------------------------
    # # Create or copy over files that don't need processing
    # # ----------------------------------------------------

    # echo "$NEWHOMDIR" >> $NEWEXPDIR/.HOMDIR
    # /bin/cp $OLDEXPDIR/GEOSgcm.x $NEWEXPDIR
    # /bin/cp -a $OLDEXPDIR/RC $NEWEXPDIR/RC

    # # -----------------------------------------------------
    # # Now actually change the various environment variables
    # # -----------------------------------------------------

    # cat > $SEDFILE << EOF
    # /^setenv \+EXPDIR/ s#$OLDEXPDIR#$NEWEXPDIR#
    # /^setenv \+HOMDIR/ s#$OLDHOMDIR#$NEWHOMDIR#
    # /^setenv \+CNVDIR/ s#$OLDHOMDIR#$NEWHOMDIR#
    # /^setenv \+EXPID/  s#$OLDEXPID#$NEWEXPID#
    # /^set \+EXPDIR/ s#$OLDEXPDIR#$NEWEXPDIR#
    # /^set \+HOMDIR/ s#$OLDHOMDIR#$NEWHOMDIR#
    # /^set \+EXPID/  s#$OLDEXPID#$NEWEXPID#
    # /^EXPID:/  s#$OLDEXPID#$NEWEXPID#
    # /GEOSUTIL\/post\/gcmpost.script/ s#$OLDEXPDIR#$NEWEXPDIR#
    # /group_list/ s#\(group_list=\)\(.*\)#\1$GROUProot#
    # /^#SBATCH -A/ s#\(SBATCH -A \)\(.*\)#\1$GROUProot#
    # /^#SBATCH --account=/ s#\(SBATCH --account=\)\(.*\)#\1$GROUProot#
    # EOF

    # foreach file (`cat $NEWEXPFILES`)
    #    sed -i -f $SEDFILE $file
    # end

    # # ------------------------------------------
    # # Change the EXPDSC in HISTORY.rc to reflect
    # # the fact this experiment was cloned
    # # ------------------------------------------

    # #sed -i -e "/^EXPDSC:/ s#\(EXPDSC: \)\(.*\)#\1${NEWEXPID}_clonedfrom_${OLDEXPID}_by_${OLDUSER}#" $NEWHOMDIR/HISTORY.rc
    #  sed -i -e "/^EXPDSC:/ s#\(EXPDSC: \)\(.*\)#\1${EXPDSC}#" $NEWHOMDIR/HISTORY.rc
    #  sed -i -e "/^EXPID:/ s#\(EXPID: \)\(.*\)#\1${NEWEXPID}#" $NEWHOMDIR/HISTORY.rc

    # # Change OLDEXPID to NEWEXPID in __init__.py if it exists
    # # -------------------------------------------------------
    # if ( -e $NEWHOMDIR/__init__.py ) then
    #    sed -i -e "/$OLDEXPID/ s#$OLDEXPID#$NEWEXPID#" $NEWHOMDIR/__init__.py
    # endif

    # # -------------------------
    # # Construct the new job ids
    # # -------------------------

    #     set RUN_N=`echo $NEWEXPID | cut -b1-200`_RUN
    #    set RUN_FN=`echo $NEWEXPID | cut -b1-200`_FCST
    #    set POST_N=`echo $NEWEXPID | cut -b1-199`_POST
    #    set PLOT_N=`echo $NEWEXPID | cut -b1-200`_PLT
    #    set MOVE_N=`echo $NEWEXPID | cut -b1-200`_MOVE
    # set ARCHIVE_N=`echo $NEWEXPID | cut -b1-199`_ARCH
    # set REGRESS_N=`echo $NEWEXPID | cut -b1-199`_RGRS
    # set CONVERT_N=`echo $NEWEXPID | cut -b1-200`_CNV

    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$RUN_N#"     \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$RUN_N#"     $NEWHOMDIR/gcm_run.j
    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$RUN_FN#"    \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$RUN_FN#"    $NEWEXPDIR/forecasts/gcm_forecast.tmpl
    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$POST_N#"    \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$POST_N#" \
    #        -e "/^setenv BATCHNAME/ s#\(setenv BATCHNAME *\)\(.*\)#\1 $POST_N#"      $NEWEXPDIR/post/gcm_post.j
    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$PLOT_N#"    \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$PLOT_N#"    $NEWEXPDIR/plot/gcm_plot.tmpl

    # if ( -e $NEWEXPDIR/plot/gcm_moveplot.j ) then
    #    sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$MOVE_N#"    \
    #           -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$MOVE_N#" $NEWEXPDIR/plot/gcm_moveplot.j
    # endif

    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$ARCHIVE_N#" \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$ARCHIVE_N#" $NEWEXPDIR/archive/gcm_archive.j
    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$REGRESS_N#" \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$REGRESS_N#" $NEWEXPDIR/regress/gcm_regress.j
    # sed -i -e "/^#PBS -N/ s#\(PBS -N \)\(.*\)#\1$CONVERT_N#" \
    #        -e "/^#SBATCH --job-name=/ s#\(SBATCH --job-name=\)\(.*\)#\1$CONVERT_N#" $NEWEXPDIR/convert/gcm_convert.j

    # # --------------------------
    # # Echo Settings and Messages
    # # --------------------------

    # echo "Done with cloning!"
    # echo "------------------"
    # echo " "
    # echo "Original Experiment Directory: ${C2}${OLDEXPDIR}${CN}"
    # echo "------------------------------"
    # echo " "
    # echo "You must now copy your ${C1}Initial Conditions${CN} into: "
    # echo "----------------------------------------------- "
    # echo "${C2}${NEWEXPDIR}${CN}"
    # echo ""
    # echo ""

    # # -------------------------
    # # Clean up the mktemp files
    # # -------------------------

    # /bin/rm $FILES_TO_PROCESS
    # /bin/rm $OLDEXPFILES
    # /bin/rm $NEWEXPFILES
    # /bin/rm $COPYSCRIPT
    # /bin/rm $SEDFILE

    # # ------------------------
    # # Cloned Experiment Source
    # # ------------------------

    # # NOTE: This variable is set at CMake time depending on
    # #       how the build was configured.
    # set INSTALL_TARFILE = @CFG_INSTALL_SOURCE_TARFILE@
    # set TARFILE_NAME = "@CMAKE_PROJECT_NAME@.tar.gz"

    # if ( $INSTALL_TARFILE == "TRUE" ) then

    #    # Make a src directory under EXPDIR to hold current Experiment files
    #    # ------------------------------------------------------------------
    #    /bin/rm -rf ${NEWEXPDIR}/src
    #    mkdir   -p  ${NEWEXPDIR}/src

    #    echo "Copying Build Source Code into ${C2}${NEWEXPDIR}/src${CN}"
    #    # --------------------------------------------------------------
    #    if (-e ${GEOSDEF}/src/${TARFILE_NAME}) then
    #       cp ${GEOSDEF}/src/${TARFILE_NAME} ${NEWEXPDIR}/src
    #    else
    #       echo "${GEOSDEF}/src/${TARFILE_NAME} not found yet CMake was asked to make and install a tarfile."
    #       echo "Something went wrong."
    #       exit 7
    #    endif
    #    echo ""

    # endif

    # exit

    # # ------------------------------------------
    # # Set a trap to remove the tempfiles on EXIT
    # # ------------------------------------------
    # TRAP:
    #    echo "Interrupt received, cleaning up temporary files"
    #    /bin/rm $FILES_TO_PROCESS $OLDEXPFILES $NEWEXPFILES $COPYSCRIPT $SEDFILE
    #    exit 1

    # #######################################################################
    # #                      Usage and Error Outputs
    # #######################################################################

    # SETCOLOR:
    # echo
    # echo "\033[1;4mGCM Setup Utility${RESET}"
    # echo
    # echo "Enter Desired Color Codes for ${BOLD}Highlighted${RESET} and ${BOLD}Default${RESET} text:"
    # echo
    # echo "${BOLD}Highlighted${RESET} Text Color: ${BLACK}0 BLACK${RESET}"
    # echo "                 Color: ${RED}1 RED${RESET}"
    # echo "                 Color: ${GREEN}2 GREEN${RESET}"
    # echo "                 Color: ${YELLOW}3 YELLOW${RESET}"
    # echo "                 Color: ${BLUE}4 BLUE${RESET}"
    # echo "                 Color: ${MAGENTA}5 MAGENTA${RESET}"
    # echo "                 Color: ${CYAN}6 CYAN${RESET}"
    # echo "                 Color: ${WHITE}7 WHITE${RESET}"
    # echo "                 Color: ${RESET}8 No Color"
    # set C1 = $<
    #   @ C1 = $C1 + 1
    # echo
    # echo "    ${BOLD}Default${RESET} Text Color: ${BLACK}0 BLACK${RESET}"
    # echo "                 Color: ${RED}1 RED${RESET}"
    # echo "                 Color: ${GREEN}2 GREEN${RESET}"
    # echo "                 Color: ${YELLOW}3 YELLOW${RESET}"
    # echo "                 Color: ${BLUE}4 BLUE${RESET}"
    # echo "                 Color: ${MAGENTA}5 MAGENTA${RESET}"
    # echo "                 Color: ${CYAN}6 CYAN${RESET}"
    # echo "                 Color: ${WHITE}7 WHITE${RESET}"
    # echo "                 Color: ${RESET}8 No Color"
    # set C2 = $<
    #   @ C2 = $C2 + 1
    # if( -e $HOME/.GCMSETUP ) /bin/rm -f $HOME/.GCMSETUP
    # touch $HOME/.GCMSETUP
    # echo $COLORS[$C1] >> $HOME/.GCMSETUP
    # echo $COLORS[$C2] >> $HOME/.GCMSETUP
    # exit 1

    # USAGE:
    # cat <<EOF
    # gcm_setup, a setup script for the GEOS GCM

    #    Usage: $0:t [optional flag]

    #    -c --color        Set the colors for $0:t
    #       --link         Link GEOSgcm.x into experiment directory
    #       --singularity  Setup Singularity experiment
    #    -h --help         Show usage

    #    If invoked alone, the script runs as normal.

    #    For more information, please contact Matt Thompson or Scott Rabenhorst

    # EOF
    # exit 1