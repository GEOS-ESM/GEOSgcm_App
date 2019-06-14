program moments
!!$ /////////////////////////////////////////////////////////////////////////
!!$ /**
!!$ * Title: ocean_moments.f90
!!$ *
!!$ * Description: Computes first and second moments of the background and analysis. Filter out un-physical values. Impose minmax limits
!!$ *              on increment.
!!$ *     
!!$ * Options:  
!!$ *              -do_ild : Boolean. If true, use the isothermal layer depth from the background to filter the temperature increment.
!!$ *                                Default: .false.
!!$ *              -do_temp_incr  : Boolean. If true, apply the temp increment.
!!$ *                                Default: .true.
!!$ *              -do_salt_incr  : Boolean. If true, apply the temp increment.
!!$ *                                Default: .true.
!!$ * Input:              
!!$ *              Ensemble Backgrounds: ./bkg/
!!$ *              Ensemble Analysis: ./ana/
!!$ *              Analysis grid: ./grid_spec.nc
!!$ * Output:              
!!$ *              Ensemble Increments: ./incr/
!!$ *              Ensemble of ReAnalysed field: ./ana/
!!$ *              First and second moments of bkg, ana and incr: ./mean_ana_restart/             
!!$ * Example:             
!!$ *             

  use mpi
  use netcdf
  use ocnice_utils


  implicit none

  integer, parameter :: real_kind = selected_real_kind(6)
  integer :: i, j, n, nt, k, argc, fid_in, arg_index, Nargs
  character*1028 :: bkg_fname
  character*1028 :: ana_fname
  character*1028 :: incr_fname
  character*1028 :: incr_dir
  character*1028 :: mean_fname
  character*1028 :: outfile  = 'checked_ocean_temp_salt.res.nc'
  logical        :: do_ild = .false.
  logical        :: do_temp_incr = .true.
  logical        :: do_salt_incr = .true.
  logical        :: do_ice_incr = .true.

  real   :: alfa_T = 0.0, alfa_S = 0.0

  character*128 :: fname
  integer :: varid, varidvi, varidlon, varidlat
  character*128 :: varname

  real*4, allocatable, dimension(:,  :, :) ::   Tb, Sb, ICEb               ! Background 
  real*4, allocatable, dimension(:,  :, :) ::   Ta, Sa, ICEa               ! Analysis
  real*4, allocatable, dimension(:,  :, :) ::   Tincr, Sincr, ICEincr      ! Increment

  real*4, allocatable, dimension(:,  :) ::   ILD, SLVb, SLVa, SLVincr

  real*4, allocatable, dimension(:,  :, : )    :: DUMVAR
  real*4, allocatable, dimension(:,  :)    :: DUMVAR2d

  real*4    :: Tf, efold, dt
  real*4    :: D0 = 1500.0     ! e-folding scale, used to limit increment size at depth
  real*4    :: Sincr_max = 5.0 ! Max absolute value for the salinity increment  
  real*4    :: Tincr_max = 10.0 ! Max absolute value for the temperature increment  

  character*300                          :: buffer, command, arg1, arg2
  integer :: ncid, dimids2d(2), cat_dimid, catp1_dimid, tile_dimid, dimids1d(1)

  type (ModelGrid)                      :: ogrid     ! Ocean grid, tripolar

  logical                              ::  append

  !MPI vars
  integer                                        :: ierror, myrank, numprocs, Ns, N2d
  integer, parameter                             :: root=0
  real, allocatable, dimension (:)               :: meanT, meanS, meanICE, meanSLV, sigT, sigS, sigICE, SUMX, sigTb, sigSb
  real, allocatable, dimension (:)               :: meanTi, meanSi, meanICEi, meanSLVi

  call MPI_INIT(ierror)                                  ! Initialize MPI
  call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierror)   ! Find number of workers
  call MPI_COMM_RANK(MPI_COMM_WORLD, myrank  , ierror)   ! Find rank

  !PROCESS COMMAND LINE
  !====================
  Nargs = iargc()
  do arg_index = 1, Nargs, 2
     call get_command_argument(arg_index, arg1)
     select case(arg1)
     case('-do_ild')
        call get_command_argument(arg_index+1, arg2)
        do_ild = str2bool(arg2)
     case('-do_temp_incr')
        call get_command_argument(arg_index+1, arg2)
        do_temp_incr = str2bool(arg2)
     case('-do_salt_incr')
        call get_command_argument(arg_index+1, arg2)
        do_salt_incr = str2bool(arg2)
     case('-do_ice_incr')
        call get_command_argument(arg_index+1, arg2)
        do_ice_incr = str2bool(arg2)
     end select
  end do
  if (do_ild) then
     D0 = 20.0
  end if

  if (do_temp_incr) then 
     alfa_T = 1.0 
  else
     alfa_T = 0.0 
  end if

  if (do_salt_incr) then 
     alfa_S = 1.0 
  else
     alfa_S = 0.0 
  end if

  !OCEAN GRID
  !==========
  call read_grid(ogrid)

  !READ BACKGROUND
  !===============
  ! Temp & Salt
  write(bkg_fname(1:30),'(A4,I3.3,A23)') 'bkg/',myrank+1,'/ocean_temp_salt.res.nc'
  allocate(Tb(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'temp'
  call readoceanrst(bkg_fname, varname, ogrid, Tb)
  allocate(Sb(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'salt'
  call readoceanrst(bkg_fname, varname, ogrid, Sb)

  ! SLV
  write(bkg_fname(1:31),'(A4,I3.3,A24)') 'bkg/',myrank+1,'/ocean_barotropic.res.nc'
  allocate(SLVb(ogrid%Nx, ogrid%Ny))
  varname = 'eta_t'
  call readrst2d(bkg_fname, varname, SLVb)

  ! AICE, HICE, HSNO, ... stored in ocean_velocity.res.nc! 
  bkg_fname=''
  write(bkg_fname(1:29),'(A4,I3.3,A22)') 'bkg/',myrank+1,'/ocean_velocity.res.nc'
  allocate(ICEb(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'u'
  call readoceanrst(bkg_fname, varname, ogrid, ICEb)

  !READ ANALYSIS
  !=============
  ! Temp & Salt
  write(ana_fname(1:30),'(A4,I3.3,A23)') 'ana/',myrank+1,'/ocean_temp_salt.res.nc'
  allocate(Ta(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'temp'
  call readoceanrst(ana_fname, varname, ogrid, Ta)
  allocate(Sa(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'salt'
  call readoceanrst(ana_fname, varname, ogrid, Sa)

  ! SLV
  ana_fname=''  
  write(ana_fname(1:31),'(A4,I3.3,A24)') 'ana/',myrank+1,'/ocean_barotropic.res.nc'
  allocate(SLVa(ogrid%Nx, ogrid%Ny))
  varname = 'eta_t'
  call readrst2d(ana_fname, varname, SLVa)

  ! AICE, HICE, HSNO, ...
  ana_fname=''
  write(ana_fname(1:29),'(A4,I3.3,A22)') 'ana/',myrank+1,'/ocean_velocity.res.nc'
  allocate(ICEa(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  varname = 'u'
  call readoceanrst(ana_fname, varname, ogrid, ICEa)

  !CHECK OCEAN TEMP SALT STATE
  !===========================
  call clean_ocean_state(ogrid, Tb, Sb)
  call clean_ocean_state(ogrid, Ta, Sa)

  !COMPUTE INCREMENT
  !=================
  allocate(Tincr(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  allocate(Sincr(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  allocate(ICEincr(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  allocate(SLVincr(ogrid%Nx, ogrid%Ny))

  if (myrank == root) then 
     command = "mkdir -p incr"
     call execute_command_line (command, exitstat=i)
  end if
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  write(incr_fname(1:31),'(A5,I3.3,A23)') 'incr/',myrank+1,'/ocean_temp_salt.res.nc'
  write(incr_dir(1:8),'(A5,I3.3)') 'incr/',myrank+1
  command = "mkdir -p "//trim(incr_dir)
  call execute_command_line (command, exitstat=i)  
  Tincr = Ta - Tb
  Sincr = Sa - Sb
  ICEincr = ICEa - ICEb
  SLVincr = SLVa - SLVb

  do k=1, ogrid%Nz
     efold = exp( - (ogrid%depth(k)/D0)**2 )
     Tincr(:,:,k) = alfa_T * Tincr(:,:,k) * efold
     Sincr(:,:,k) = alfa_S * Sincr(:,:,k) * efold
  end do

  !RE-ANALYZE
  !==========
  Tincr = Tincr_max * tanh( Tincr/Tincr_max ) ! limit the increment to ]-Tincr_max, Tincr_max[
  Ta = Tb + Tincr

  Sincr = Sincr_max * tanh( Sincr/Sincr_max ) ! Constrain the increment smoothly in ]-Sincr_max, Sincr_max[ 
                                              ! Probably should do the same for temperature
  Sa = Sb + Sincr

  !SAVE ICREMENT
  !=============
  varname='temp'
  append=.false.
  call write_ocean_state(incr_fname, varname, Tincr, append, ogrid)  
  varname='salt'
  append=.true.
  call write_ocean_state(incr_fname, varname, Sincr, append, ogrid)
  
  varname='slv'
  append=.true.
  call write_ocean_state2d(incr_fname, varname, SLVincr, append, ogrid)

  !incr_fname=''
  !write(incr_fname(1:18),'(A5,I3.3,A11)') 'incr/',myrank+1,'/sea_ice.nc'
  varname='ice'
  append=.true.
  call write_ocean_state(incr_fname, varname, ICEincr, append, ogrid)

  !SAVE ANALYSIS
  !=============
  write(ana_fname(1:30),'(A4,I3.3,A23)') 'ana/',myrank+1,'/ocean_temp_salt.res.nc'
  varname='temp'
  append=.false.
  call write_ocean_state(ana_fname, varname, Ta, append, ogrid)
  varname='salt'
  append=.true.
  call write_ocean_state(ana_fname, varname, Sa, append, ogrid)  
  
  ana_fname=''
  write(ana_fname(1:18),'(A4,I3.3,A11)') 'ana/',myrank+1,'/sea-ice.nc'
  varname='ice'
  append=.false.
  call write_ocean_state(ana_fname, varname, ICEa, append, ogrid)

  call mpi_barrier(MPI_COMM_WORLD,ierror)

  !FIRST AND SECOND MOMENT OF ANALYSIS/BACKGROUND <----------- NEED TO RECODE THAT PART !!!!!!!
  !==============================================
  Ns = ogrid%Nx*ogrid%Ny*ogrid%Nz
  N2d = ogrid%Nx*ogrid%Ny
  allocate(meanT(Ns), meanS(Ns), meanICE(Ns), meanSLV(N2d))
  call mpi_allreduce(reshape(Ta,(/Ns/)),meanT,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanT=meanT/real(numprocs)
  call mpi_allreduce(reshape(Sa,(/Ns/)),meanS,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanS=meanS/real(numprocs)
  call mpi_allreduce(reshape(ICEa,(/Ns/)),meanICE,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanICE=meanICE/real(numprocs)
  call mpi_allreduce(reshape(SLVa,(/N2d/)),meanSLV,N2d,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanSLV=meanSLV/real(numprocs)

  allocate(meanTi(Ns), meanSi(Ns), meanICEi(Ns), meanSLVi(N2d))
  call mpi_allreduce(reshape(Tincr,(/Ns/)),meanTi,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanTi=meanTi/real(numprocs)
  call mpi_allreduce(reshape(Sincr,(/Ns/)),meanSi,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanSi=meanSi/real(numprocs)
  call mpi_allreduce(reshape(ICEincr,(/Ns/)),meanICEi,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanICEi=meanICEi/real(numprocs)
  call mpi_allreduce(reshape(SLVincr,(/N2d/)),meanSLVi,N2d,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanSLVi=meanSLVi/real(numprocs)

  if (myrank==root) then
     command = "mkdir mean_ana_restart"
     call execute_command_line (command, exitstat=i)       
     mean_fname='mean_ana_restart/ocean_temp_salt.res.nc'
     varname='temp'
     append=.false.
     allocate(DUMVAR(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     DUMVAR=reshape(meanT,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='salt'
     append=.true.
     DUMVAR=reshape(meanS,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  

     mean_fname='mean_ana_restart/slv.nc'
     varname='SLVa'
     append=.false.
     allocate(DUMVAR2d(ogrid%Nx, ogrid%Ny))     
     DUMVAR2d=reshape(meanSLV,(/ogrid%Nx, ogrid%Ny/))
     call write_ocean_state2d(mean_fname, varname, DUMVAR2d, append, ogrid)  
     varname='SLVb'
     append=.true.
     DUMVAR2d=reshape(meanSLV-meanSLVi,(/ogrid%Nx, ogrid%Ny/))
     call write_ocean_state2d(mean_fname, varname, DUMVAR2d, append, ogrid)  

     mean_fname='mean_ana_restart/sea-ice.nc'
     varname='ice'
     append=.false.
     DUMVAR=reshape(meanICE,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  

     mean_fname='mean_ana_restart/incr.nc'
     varname='temp'
     append=.false.
     DUMVAR=reshape(meanTi,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='salt'
     append=.true.
     DUMVAR=reshape(meanSi,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='ice'
     append=.true.
     DUMVAR=reshape(meanICEi,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='SLV'
     append=.true.
     DUMVAR2d=reshape(meanSLVi,(/ogrid%Nx, ogrid%Ny/))
     call write_ocean_state2d(mean_fname, varname, DUMVAR2d, append, ogrid)  
  end if

  ! Compute ana std 
  !============================
  call mpi_barrier(MPI_COMM_WORLD,ierror)
  allocate(sigT(Ns), sigS(Ns), SUMX(Ns))
  sigT=(reshape(Ta, (/Ns/))-meanT)**2
  call mpi_allreduce(sigT,SUMX,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  sigT=(SUMX/(real(numprocs)-1.0))**0.5
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  sigS=(reshape(Sa, (/Ns/))-meanS)**2
  call mpi_allreduce(sigS,SUMX,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  sigS=(SUMX/(real(numprocs)-1.0))**0.5
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  ! Compute background mean/std
  !============================
  call mpi_allreduce(reshape(Tb,(/Ns/)),meanT,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanT=meanT/real(numprocs)
  call mpi_allreduce(reshape(Sb,(/Ns/)),meanS,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  meanS=meanS/real(numprocs)

  call mpi_barrier(MPI_COMM_WORLD,ierror)
  allocate(sigTb(Ns), sigSb(Ns))
  sigTb=(reshape(Tb, (/Ns/))-meanT)**2
  call mpi_allreduce(sigTb,SUMX,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  sigTb=(SUMX/(real(numprocs)-1.0))**0.5
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  sigSb=(reshape(Sb, (/Ns/))-meanS)**2
  call mpi_allreduce(sigSb,SUMX,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
  sigSb=(SUMX/(real(numprocs)-1.0))**0.5
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  if (myrank==root) then
     mean_fname='mean_ana_restart/std_ocean_temp_salt.res.nc'
     varname='temp_ana'
     append=.false.
     DUMVAR=reshape(sigT,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='salt_ana'
     append=.true.
     DUMVAR=reshape(sigS,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  

     varname='temp_bkg'
     append=.true.
     DUMVAR=reshape(sigTb,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
     varname='salt_bkg'
     append=.true.
     DUMVAR=reshape(sigSb,(/ogrid%Nx, ogrid%Ny, ogrid%Nz/))
     call write_ocean_state(mean_fname, varname, DUMVAR, append, ogrid)  
  end if

  call MPI_FINALIZE(ierror)

contains


end program moments
