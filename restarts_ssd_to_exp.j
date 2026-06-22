#!/bin/csh -f
#SBATCH --job-name=SSD_to_EXP
#SBATCH --output=%x.o%j
#SBATCH --ntasks=120 --ntasks-per-node=120
#SBATCH --time=06:00:00

setenv GEOSBIN /discover/nobackup/projects/gmao/osse2/GIT/LM_v12-rc19/GEOSgcm/install/bin

set HOMDIR = `grep -m 1 "HOMDIR" gcm_run.j | awk '{print $NF}'`
set SSDDIR = `grep -m 1 "SSDDIR" gcm_run.j | awk '{print $NF}'`

set SSD_RSTDIR = "${SSDDIR}/restarts"
set HOM_RSTDIR = "${HOMDIR}/restarts"

# current date
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set hhc   = `cat cap_restart | cut -c10-11`
echo $nymdc
echo $nhmsc
echo $hhc
echo 

echo  $SSD_RSTDIR
ls -l $SSD_RSTDIR
echo  $HOM_RSTDIR
ls -l $HOM_RSTDIR
echo

cd ${SSD_RSTDIR} 
set rsts = `find . -maxdepth 1 -type f | grep -v ${nymdc}_${hhc}`

if ( $#rsts > 0 ) then

@ start_time = `date +%s`
foreach rst ($rsts)
  set file = `basename $rst`
  echo $file
  set edate = `echo $file | sed 's/.*\.e\([0-9]\{8\}\)\_\([0-9]\{2\}z\).*/e\1_\2/'`
  set tar_file = "${HOM_RSTDIR}/restarts.${edate}.tar"
  # Add file to tar archive (create if first file, append if exists)
  if ( ! -f $tar_file ) then
     tar cf $tar_file $file
  else
     tar uf $tar_file $file
  endif
end
wait

#foreach rst ($rsts)
#  /bin/rm $rst &
#end
wait

@ end_time = `date +%s`
@ diff = $end_time - $start_time
echo "It took $diff seconds"

endif

exit 0

