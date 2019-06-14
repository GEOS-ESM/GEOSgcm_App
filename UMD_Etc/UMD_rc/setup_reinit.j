#!/bin/csh -vf

# Set up ODAS for a new experiment after running Yuri's gcm_setup
#set EXPID       = $1   # S2S-2_1_REINIT_TEST_SETUP
#set EXP_DIR     = $2   # /home/aborovik/exp/
#set SANDBOX     = $2   # yuri-S2S-2_1_UNSTABLE
#set SANDBOX_DIR = $3   # /home/aborovik/esma/

echo "Enter Experiment Name"
set EXPID = $<

echo "Enter Path to Experiment"
set EXP_DIR = $<

echo "Enter Sandbox Name"
set SANDBOX = $<

echo "Enter Path to Sandbox"
set SANDBOX_DIR = $<

# Copy Files from UMD_rc
set HOMDIR = ${EXP_DIR}/${EXPID}
if (! -d ${HOMDIR} ) then
        mkdir ${HOMDIR}
endif

cd ${HOMDIR}

#######################################################################
#                         System Settings 
#######################################################################

umask 022
limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             NCCS
setenv GEOSDIR          ${SANDBOX_DIR}/${SANDBOX}/GEOSodas 
setenv GEOSBIN          ${SANDBOX_DIR}/${SANDBOX}//GEOSodas/Linux/bin 
setenv RUN_CMD         "mpirun -np "
setenv GCMVER           Heracles-5_4_p3

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    HOMDIR  ${EXP_DIR}/${EXPID}
setenv    SCRDIR_TMPLT  $HOMDIR/scratch_tmplt

cd $HOMDIR
#######################################################################
#                   Set Processors Layout and OLETF stuff
#######################################################################

cp ${GEOSDIR}/src/Applications//UMD_Etc/scripts/ocean_das* . 
cp ${GEOSDIR}/src/Applications//UMD_Etc/UMD_rc/diag_table .
cp ${GEOSDIR}/src/Applications//UMD_Etc/UMD_rc/input.nml .
cp ${GEOSDIR}/src/Applications//UMD_Etc/UMD_rc/AGCM.rc .
cp ${GEOSDIR}/src/Applications//UMD_Etc/UMD_rc/CAP.rc .

#======================================================================

# uncomment this once the MOtoM2O.csh is updated in the tag
cp $GEOSDIR/src/Applications/UMD_Etc/scripts/MOtoM2O.csh .

cp $GEOSDIR/src/Applications/UMD_Etc/scripts/ocean_das* .
cp $GEOSDIR/src/Applications/UMD_Etc/scripts/oda_run.j .
cp $GEOSDIR/src/Applications/UMD_Etc/scripts/MOtoM2O.py .

#cp /home/aborovik/exp/ab016/HISTORY.rc .  # need to get this as a template from the tag, like this
cp ${GEOSDIR}/src/Applications//UMD_Etc/UMD_rc/HISTORY_REINIT.rc .
mv HISTORY_REINIT.rc HISTORY.rc

mkdir $SCRDIR_TMPLT 
cp HISTORY.rc $SCRDIR_TMPLT

#cp ocean_das_config tmp_config
sed "s|SANDBOX_DIR|${GEOSDIR}|g" ocean_das_config > tmp
mv tmp ocean_das_config

sed "s|setenv ODAS_do_reinit       False|setenv ODAS_do_reinit       True|g" ocean_das_config > tmp
mv tmp ocean_das_config 

sed "s|setenv ODAS_OUTPUT_OMA      True|setenv ODAS_OUTPUT_OMA      False|g" ocean_das_config > tmp
mv tmp ocean_das_config 

#cp MOtoM2O.csh tmp_MOtoM2O
sed "s|SANDBOX_DIR|${GEOSDIR}|g"  MOtoM2O.csh > tmp
mv tmp MOtoM2O.csh 
sed "s|MY_HOME_DIR|${HOMDIR}|g"  MOtoM2O.csh > tmp
mv tmp MOtoM2O.csh 

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep           NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep           NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM  = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM  = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM  = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM  = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM  = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set END_DATE  = `grep     END_DATE:  $HOMDIR/CAP.rc | cut -d':' -f2`
set NUM_SGMT  = `grep     NUM_SGMT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set FSEGMENT  = `grep FCST_SEGMENT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set USE_SHMEM = `grep    USE_SHMEM:  $HOMDIR/CAP.rc | cut -d':' -f2`

set  OGCM_LM  = `grep OGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NX = `grep  OGCM_NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY = `grep  OGCM_NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  HIST_IM = 720
set  HIST_JM = 361

# Check for Over-Specification of CPU Resources
# ---------------------------------------------
  if ($?PBS_NODEFILE) then
     set  NCPUS = `cat $PBS_NODEFILE | wc -l`
     @    NPES  = $NX * $NY
        if( $NPES > $NCPUS ) then
             echo "CPU Resources are Over-Specified"
             echo "--------------------------------"
             echo "Allotted NCPUs: $NCPUS"
             echo "Specified  NX : $NX"
             echo "Specified  NY : $NY"
             exit
        endif
     endif
  endif

# Set ATMOS and OCEAN Horizontal Resolution Tags
# ----------------------------------------------
set AGCM_IM_Tag = `echo $AGCM_IM | awk '{printf "%4.4i", $1}'`
set AGCM_JM_Tag = `echo $AGCM_JM | awk '{printf "%4.4i", $1}'`
set OGCM_IM_Tag = `echo $OGCM_IM | awk '{printf "%4.4i", $1}'`
set OGCM_JM_Tag = `echo $OGCM_JM | awk '{printf "%4.4i", $1}'`
set HIST_IM_Tag = `echo $HIST_IM | awk '{printf "%4.4i", $1}'`
set HIST_JM_Tag = `echo $HIST_JM | awk '{printf "%4.4i", $1}'`

set ATMOStag = CF${AGCM_IM_Tag}x6C
set OCEANtag = TM${OGCM_IM_Tag}xTM${OGCM_JM_Tag}
set HISTtag = DC${HIST_IM_Tag}xPC${HIST_JM_Tag}

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR_TMPLT 

/bin/rm -rf *
/bin/ln -sf $HOMDIR/RC/* .
/bin/cp -f  $HOMDIR/*.rc .
/bin/cp -f  $HOMDIR/*.nml .

#######################################################################
#         Create Strip Utility to Remove Multiple Blank Spaces
#######################################################################

set      FILE = strip
/bin/rm $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

#######################################################################
#                        Link Boundary Datasets
#######################################################################

setenv BCSDIR    /discover/nobackup/ltakacs/bcs/Ganymed-4_0/Ganymed-4_0_Reynolds
setenv CHMDIR    /discover/nobackup/projects/gmao/share/dao_ops/fvInput_nc3
setenv BCRSLV    ${ATMOStag}_DE0360xPE0180
setenv DATELINE  DC
setenv EMISSIONS MERRA2

setenv GRIDDIR  /discover/nobackup/yvikhlia/coupled/Forcings/a${AGCM_IM}x${AGCM_JM}_o${OGCM_IM}x${OGCM_JM}
setenv BCTAG `basename $GRIDDIR`

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -vf

cd $SCRDIR_TMPLT
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

/bin/ln -sf $GRIDDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
/bin/ln -sf $GRIDDIR/${ATMOStag}_${OCEANtag}-Pfafstetter.til   tile.data
/bin/ln -sf $GRIDDIR/${ATMOStag}_${OCEANtag}-Pfafstetter.TRN   runoff.bin
/bin/ln -sf $GRIDDIR/tripolar_${OGCM_IM}x${OGCM_JM}.ascii .
/bin/ln -sf $GRIDDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii

/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data
/bin/ln -sf $BCSDIR/Shared/*bin .

/bin/ln -sf $GRIDDIR/visdf.dat visdf.dat
/bin/ln -sf $GRIDDIR/nirdf.dat nirdf.dat
#/bin/ln -sf $GRIDDIR/vegdyn.data vegdyn.data
/bin/ln -sf/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OCEAN_DAS_RC/RST/rst-720x410x40-tri/vegdyn.data $SCRDIR_TMPLT

/bin/ln -sf $GRIDDIR/lai.dat lai.data
/bin/ln -sf $GRIDDIR/green.dat green.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data

if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
/bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
endif

/bin/cp $HOMDIR/input.nml .
/bin/cp $HOMDIR/*_table .
/bin/ln -sf $GRIDDIR/cice/kmt_cice.bin .
/bin/ln -sf $GRIDDIR/cice/grid_cice.bin .

/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP 

cd ..

_EOF_

chmod +x linkbcs
/bin/cp  linkbcs $HOMDIR

cd $HOMDIR
./linkbcs

cd $SCRDIR_TMPLT 

#######################################################################
#          Get C2L History weights/index file for Cubed-Sphere
#######################################################################

set C_NPX = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set C_NPY = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set H_NPX = `echo 720 | awk '{printf "%5.5i", $1}'`
set H_NPY = `echo 361 | awk '{printf "%5.5i", $1}'`

set c2l_file = "${C_NPX}x${C_NPY}_c2l_${H_NPX}x${H_NPY}.bin"

if (-e $BCSDIR/$BCRSLV/${c2l_file}) /bin/ln -s $BCSDIR/$BCRSLV/${c2l_file} .

#######################################################################
#                    Get Executable 
#######################################################################

/bin/cp $HOMDIR/GEOSgcm.x .


# For MERRA-2 and OSTIA, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# ---------------------------------------------------------------------------------------------------
if( ${OCEANtag} == DE1440xPE0720 | \
    ${OCEANtag} == DE2880xPE1440 ) then
    if( $yearf > $yearc ) then
       @ yearf = $yearc + 1
       @ nymdf = $yearf * 10000 + 0101
        set oldstring = `cat CAP.rc | grep END_DATE:`
        set newstring = "END_DATE: $nymdf $nhmsf"
        /bin/mv CAP.rc CAP.tmp
        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
    endif
endif

/bin/ln -s /discover/nobackup/yvikhlia/coupled/Forcings/a720x361_o720x410/DC0720xPC0361_TM0720xTM0410-Pfafstetter.til tile_hist_720x361.data
/bin/ln -s /discover/nobackup/yvikhlia/coupled/Forcings/a${HIST_IM}x${HIST_JM}_o${OGCM_IM}x${OGCM_JM}/${HISTtag}_${OCEANtag}-Pfafstetter.til tile_hist.data
/bin/ln -sf $GRIDDIR/${ATMOStag}_${OCEANtag}-Pfafstetter.til   tile.data

if (! -e tile.bin) then
$GEOSBIN/binarytile.x tile.data tile.bin
$GEOSBIN/binarytile.x tile_hist.data tile_hist.bin
$GEOSBIN/binarytile.x tile_hist_720x361.data tile_hist_720x361.bin
endif

# Environment variables for MPI, etc
# ----------------------------------

setenv I_MPI_DAPL_UD enable




