#!/bin/csh -f
#SBATCH --job-name=CUBE_BCS_CONUS02KM
##SBATCH --output=%x.o%j
#SBATCH --output=/dev/null
#SBATCH --ntasks=1200 --ntasks-per-node=120
#SBATCH --partition=geosgms --constraint=mil --account=s1062
#SBATCH --time=1:00:00

set bcsdir = /discover/nobackup/projects/gmao/share/dao_ops/fvInput/g5gcm/bcs/realtime/OSTIA_REYNOLDS/2880x1440
set tiledir = /discover/nobackup/ltakacs/bcs/Icarus/Shared

if ($?YEAR == 0) set YEAR = `date +"%Y"`
set yyyy = $YEAR
set outdir = /discover/nobackup/projects/gmao/osse2/HWT/CONUS02KM/Feature-c2160_L137
echo $yyyy

umask 022
limit stacksize unlimited
setenv ARCH `uname`
setenv SITE             NCCS
setenv GEOSDIR          /discover/nobackup/projects/gmao/osse2/GIT/KM_v11.5.1/GEOSgcm/install-IntelMPI
setenv GEOSBIN          /discover/nobackup/projects/gmao/osse2/GIT/KM_v11.5.1/GEOSgcm/install-IntelMPI/bin
setenv GEOSETC          /discover/nobackup/projects/gmao/osse2/GIT/KM_v11.5.1/GEOSgcm/install-IntelMPI/etc
setenv GEOSUTIL         /discover/nobackup/projects/gmao/osse2/GIT/KM_v11.5.1/GEOSgcm/install-IntelMPI
echo   $GEOSBIN/g5_modules
source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib

set mpi_type = "openmpi"
if ($?I_MPI_ROOT ) then
  set mpi_type = "intel"
endif

if ($mpi_type =~ "openmpi") then
setenv RUN_CMD "$GEOSBIN/esma_mpirun --map-by core --bind-to core -np "
setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0
# pre-connect MPI procs on mpi_init
setenv OMPI_MCA_mpi_preconnect_all 1
# options fo bcast: 0|ignore, 1|basic_linear, 2|chain, 3|pipeline, 4|split_binary_tree, 5|binary_tree, 6|binomial, 7|knomial, 8|scatter_allgather, 9|scatter_all
setenv OMPI_MCA_coll_tuned_bcast_algorithm 7
# options for scatter: 0|ignore, 1|basic_linear, 2|binomial, 3|linear_nb
setenv OMPI_MCA_coll_tuned_scatter_algorithm 2
# options for reduce_scatter: 0|ignore, 1|non-overlapping, 2|recursive_halving, 3|ring, 4|butterfly
setenv OMPI_MCA_coll_tuned_reduce_scatter_algorithm 3
# options for allreduce: 0|ignore, 1|basic_linear, 2|nonoverlapping, 3|recursive_doubling, 4|ring, 5|segmented_ring, 6|rabenseifner
setenv OMPI_MCA_coll_tuned_allreduce_algorithm 3
# options for allgather: 0|ignore, 1|linear, 2|bruck, 3|recursive_doubling, 4|ring, 5|neighbor, 6|two_proc, 7|sparbit
setenv OMPI_MCA_coll_tuned_allgather_algorithm 4
# options for allgatherv: 0|ignore, 1|default, 2|bruck, 3|ring, 4|neighbor, 5|two_proc, 6|sparbit
setenv OMPI_MCA_coll_tuned_allgatherv_algorithm 3
# options for gather: 0 ignore, 1 basic linear, 2 binomial, 3 linear with synchronization
setenv OMPI_MCA_coll_tuned_gather_algorithm 1
# options for barrier: 0|ignore, 1|linear, 2|double_ring, 3|recursive_doubling, 4|bruck, 5|two_proc, 6|tree
setenv OMPI_MCA_coll_tuned_barrier_algorithm 0
# required for a tuned flag to be effective
setenv OMPI_MCA_coll_tuned_use_dynamic_rules 1
# disable file locks
setenv OMPI_MCA_sharedfp "^lockedfile,individual"
else
setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "
setenv I_MPI_FABRICS ofi
setenv I_MPI_OFI_PROVIDER psm3
setenv I_MPI_ADJUST_SCATTER 2
setenv I_MPI_ADJUST_SCATTERV 2
setenv I_MPI_ADJUST_GATHER 2
setenv I_MPI_ADJUST_GATHERV 3
setenv I_MPI_ADJUST_ALLGATHER 3
setenv I_MPI_ADJUST_ALLGATHERV 3
setenv I_MPI_ADJUST_ALLREDUCE 12
setenv I_MPI_ADJUST_REDUCE 10
setenv I_MPI_ADJUST_BCAST 11
setenv I_MPI_ADJUST_REDUCE_SCATTER 4
setenv I_MPI_ADJUST_BARRIER 9
endif

set workarea = $outdir/cube_BCs
set workdir = $workarea/bcswork.$yyyy.$$

if (-d $workdir) \rm -r $workdir
mkdir -p $workdir
cd $workdir

@ errcnt = 0
set output_list = ()

foreach res (C2160)

   set resX = `echo $res | cut -c2-`
   @ resY = $resX * 6
   set resX3 = $resX
   if ($resX3 < 100) set resX3 = 0$resX3
   set resX4 = 0$resX3

   set out_im = $resX
   set out_gridname = PE${resX}x${resY}-CF
   set out_tiling_file = DE2880xPE1440_CF${resX4}x6C.bin

## foreach var (ICE SST SEAWIFS_KPAR_mon_clim)
   foreach var (ICE SST)
      if ($var == SEAWIFS_KPAR_mon_clim) then
        set input_bcs  = SEAWIFS_KPAR_mon_clim.2880x1440
        set output     = SEAWIFS_KPAR_mon_clim.${resX}x${resY}
      else
        set input_bcs  = dataoceanfile_OSTIA_REYNOLDS_$var.2880x1440.$yyyy.data
        set output     = dataoceanfile_OSTIA_REYNOLDS_$var.${resX}x${resY}.$yyyy.data
      endif

                       set FIX_FRACTION = '.false.'
      if ($var == ICE) set FIX_FRACTION = '.true.'

      if (! -e $input_bcs) ln -s $bcsdir/$input_bcs .

      set rcfile = REGRID_FORCING_${res}_${var}.rc

      alias rcwrite "echo \!* >> $rcfile"
      rcwrite RUN_DT: 1800
      rcwrite
      rcwrite NX: 10
      rcwrite NY: 120
      rcwrite 
      rcwrite INPUT_GRIDNAME: LatLonGrid
      rcwrite LatLonGrid.NX: 10
      rcwrite LatLonGrid.NY: 120
      rcwrite LatLonGrid.GRID_TYPE: LatLon
      rcwrite LatLonGrid.IM_WORLD: 2880
      rcwrite LatLonGrid.JM_WORLD: 1440
      rcwrite LatLonGrid.POLE: 'PE'
      rcwrite LatLonGrid.DATELINE: 'DE'
      rcwrite      
      rcwrite OUTPUT_GRIDNAME: CubedGrid
      rcwrite CubedGrid.GRID_TYPE: Cubed-Sphere
      rcwrite CubedGrid.NX: 10
      rcwrite CubedGrid.NY: 20
      rcwrite CubedGrid.IM_WORLD: $out_im
      rcwrite CubedGrid.STRETCH_FACTOR: 2.5
      rcwrite CubedGrid.TARGET_LON: -98.35
      rcwrite CubedGrid.TARGET_LAT: 39.5
      rcwrite 
      rcwrite INPUT_FILE:  $input_bcs
      rcwrite OUTPUT_FILE: $output
      rcwrite
      rcwrite FIX_FRACTION: $FIX_FRACTION

      echo
      echo  Converting $var for year $yyyy to output resolution: $resX $resY
      echo "-------------------------------------------------------"
      set output_list = ( $output_list $output )

      if (-e $output) \rm $output
      if (-e REGRID_FORCING.rc) \rm REGRID_FORCING.rc

      ln -s $rcfile REGRID_FORCING.rc
      $RUN_CMD 1200 $GEOSBIN/regrid_forcing_esmf.x

      if ($status) @ errcnt++
      if (! -e $output) @ errcnt++
      if (-z $output) @ errcnt++
   end
end

if ($errcnt) then
else
   foreach file ( $output_list )
      set outfile = $outdir/$file
      set previous = $outfile.previous
      if (-e $outfile) then
      if (-e $previous) \rm $previous
         \mv $outfile $previous
      endif
      \mv $file $outdir
   end
   cd $outdir
   \rm -r $workdir
endif
