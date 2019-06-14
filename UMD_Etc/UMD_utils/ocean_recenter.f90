program ocean_recenter
  ! 
  !  INPUT:
  !
  !  OUTPUT:

  use mpi
  use ocean_recenter_utils

  implicit none
  !integer, parameter :: r_size=kind(0.0)
  character (len = 400)                          :: fname, varname, fname_center
  integer                                        :: Ne                       ! Ensemble size
  integer                                        :: Ns                       ! Number of element in Prognostic state variable
  !real(r_size), allocatable, dimension (:)               :: Xec, Xe  ,meanX, sigX                  ! Center state
  real, allocatable, dimension (:)               :: Xec, Xe  ,meanX, sigX                  ! Center state
  integer                                        :: Nx, Ny, Nz, Ncat, index

  !mpi
  !===
  integer                                        :: ierror, myrank, numprocs
  integer, parameter                             :: root=0

  type (obs_space)                    :: obs
  integer                             :: obs_index

  real                                   :: L, L_obs, scaling, inflation
  integer                                :: i,j,k,No, level_index, num_levels
  type (ModelGrid)                       :: state_grid
  character (len = 400)                  :: Xe_path, HX_path, HX_varname, X_varname, solver, obs_varname, X_varshape
  logical                                :: correlation, append
  character (len=8)                      :: yyyymmdd
  character*300                          :: BUFFER  
  character (len = 400)                  :: grid_fname, lon_name, lat_name, lev_name, mask_name, topo_name

  
  real, parameter :: undef=9.99e6

  namelist /recenter_grid_size/Nx, Ny, Nz, Ncat, grid_fname, lon_name, lat_name, lev_name, mask_name, topo_name
  namelist /recenter_HX/HX_path, HX_varname, obs_varname
  namelist /recenter_X/Xe_path, X_varname, num_levels, X_varshape
  namelist /recenter_correlation/correlation
  namelist /recenter_localization/L
  namelist /recenter_solver/solver 

  CALL MPI_INIT(ierror)                                  ! Initialize MPI
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierror)   ! Find number of workers
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank  , ierror)   ! Find rank

  call getarg(1,yyyymmdd)
  call getarg(2,BUFFER)
  read(BUFFER,*)append
  call getarg(3,fname_center)
  call getarg(4,BUFFER)
  read(BUFFER,*)scaling
  call getarg(5,BUFFER)
  read(BUFFER,*)inflation


  !if (myrank==root) then
  !   print *,yyyymmdd, append
  !end if

  open(7,file='recenter.nml')
  read(7,NML=recenter_HX)
  read(7,NML=recenter_X)
  read(7,NML=recenter_correlation)
  read(7,NML=recenter_localization)
  read(7,NML=recenter_solver)
  close(7)

  !Initialize ocean grid
  !=====================
  !print *,'Initialize ocean grid'
  call init_grid(state_grid)
  Ns=state_grid%Nx*state_grid%Ny*num_levels
  Ne=numprocs
  allocate(Xe(1:Ns), meanX(1:Ns), sigX(1:Ns))

  !print *,'Ns=',Ns
  !Xe=0.0

  call mpi_barrier(MPI_COMM_WORLD,ierror)
  call get_Xe(Xe,myrank,yyyymmdd,state_grid%Nx,state_grid%Ny,num_levels,Xe_path,X_varname,X_varshape) 
  call mpi_barrier(MPI_COMM_WORLD,ierror)

  Xe=real(Xe)
  where (abs(Xe)>undef)
     Xe=0.0
  end where

  !do level_index = 1, num_levels
  !   VAR(:,:,level_index)=VAR(:,:,level_index)*state_grid%wet
  !end do
  !Xe=reshape(VAR,(/Ns/))
  !deallocate(VAR)
  !where (Xe>Xetop)
  !   Xe=0.
  !end where

  
  !do index = 1,Ns
     !print *,Xe(index)
  !   if (abs(Xe(index))>Xe(1)) then
  !      print *,index, Xe(index), Ns
        !Xe(index)=0.0d0
  !   end if
  !end do
  
  call mpi_demean(Xe, meanX, Ns, Ne, inflation, ierror)         ! Compute mean and demean
  call mpi_barrier(MPI_COMM_WORLD, ierror)   
  call mpi_std(Xe, sigX, Ns, Ne, ierror)             ! Compute standard deviatio
  call mpi_barrier(MPI_COMM_WORLD, ierror)   

  !SAVE FIRST AND SECOND MOMENTS OF ENSEMBLE
  !=========================================
  if (myrank==root) then
     select case(X_varshape)
     case('2d')
        fname='./moments2d-'
     case('3d')
        fname='./moments3d-'
     end select
     fname=trim(fname)//yyyymmdd
     fname=trim(fname)//'.nc'
     varname=trim('mean_'//X_varname)
     call write23d(fname, varname, reshape(meanX,(/state_grid%Nx, state_grid%Ny, num_levels/)), state_grid%Nx, state_grid%Ny,append, num_levels, X_varshape)
     varname=trim('std_'//X_varname)
     call write23d(fname, varname, reshape(sigX,(/state_grid%Nx, state_grid%Ny, num_levels/)), state_grid%Nx, state_grid%Ny, .true., num_levels, X_varshape)
  end if
  call mpi_barrier(MPI_COMM_WORLD,ierror) 

  !SAVE RECENTERED ENSEMBLE
  !========================
  allocate(Xec(Ns))
  call get_center_Xe(Xec, fname_center, myrank,yyyymmdd,state_grid%Nx,state_grid%Ny,num_levels,Xe_path,X_varname,X_varshape)
  call mpi_barrier(MPI_COMM_WORLD,ierror)  

  where (isnan(Xec))
     Xec=-scaling
  end where

  select case(X_varshape)
  case('2d')
     fname='./recenter2d-'
  case('3d')
     fname='./recenter3d-'
  end select

  fname=trim(fname)//yyyymmdd
  fname=trim(fname)//'.'
  !fname=trim(fname)//int2str(myrank)
  fname=trim(fname)//int2str(myrank+1)
  fname=trim(fname)//'.nc'  

!!$  !fname='./recentered/state.'
!!$  !fname='state.'
!!$  fname=trim(fname)//int2str(myrank+1)
!!$  fname=trim(fname)//'.'
!!$  fname=trim(fname)//yyyymmdd
!!$  fname=trim(fname)//'.nc'
  varname=trim(X_varname)

  Xec=Xec+scaling
  call write23d(fname, varname, reshape(Xec+Xe,(/state_grid%Nx, state_grid%Ny, num_levels/)), state_grid%Nx, state_grid%Ny,append, num_levels, X_varshape)
  !call write23d(fname, varname, reshape(Xe,(/state_grid%Nx, state_grid%Ny, num_levels/)), state_grid%Nx, state_grid%Ny,append, num_levels, X_varshape)
  call mpi_barrier(MPI_COMM_WORLD,ierror) 
  
  deallocate(Xec, Xe)
  deallocate(meanX, sigX)

  call MPI_FINALIZE(ierror)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------
  subroutine write23d(fname, varname, VAR, Nx, Ny, append, nlevels, var_shape)
    !-----------------------
    use netcdf
    implicit none

    integer, parameter :: NDIMS = 3
    integer :: ncid, varid, dimids2d(3), dimids3d(4)
    integer :: x_dimid, y_dimid, z_dimid, t_dimid
    integer, intent(in)  ::  Nx, Ny, nlevels
    integer              ::  Nt
    logical, intent(in)  ::  append
    character (len = 400), intent(in)   :: fname, varname, var_shape
    character (len = 400) :: mom_varname
    real, dimension(Nx,Ny,nlevels), intent(in)  :: VAR
    real, dimension(Nx,Ny,nlevels,1)  :: VARtmp

    Nt=1
    VARtmp(:,:,:,1)=VAR
    select case(var_shape)
    case('2d')
       !print *,'Writing 2D field'
       if (append) then
          call check( nf90_open(fname, NF90_WRITE, ncid) )
          call check( nf90_inq_dimid(ncid, "xaxis_1", x_dimid) )
          call check( nf90_inq_dimid(ncid, "yaxis_1", y_dimid) )
          call check( nf90_inq_dimid(ncid, "Time", t_dimid) )
          call check( nf90_redef(ncid) )
       else
          !print *,'Creating file ',trim(fname)
          call check( nf90_create(fname, NF90_CLOBBER, ncid) )
          call check( nf90_def_dim(ncid, "xaxis_1", Nx, x_dimid) )
          call check( nf90_def_dim(ncid, "yaxis_1", Ny, y_dimid) )
          call check( nf90_def_dim(ncid, "Time", Nt, t_dimid) )
       end if
       dimids2d =  (/ x_dimid, y_dimid, t_dimid /)

       !print *,'mom_varname:',trim(mom_varname)
       !print *,'varname:',trim(varname)

       select case(trim(varname))
       case('mean_SLV')
          mom_varname='mean_eta_t'
       case('mean_SSH')
          mom_varname='mean_eta_t'
       case('std_SLV')
          mom_varname='std_eta_t'
       case('std_SSH')
          mom_varname='std_eta_t'
       case('SLV')
          mom_varname='eta_t'
       case('SSH')
          mom_varname='eta_t'
       case default
          mom_varname=trim(varname)
       end select

       !print *,'mom_varname:',trim(mom_varname)
       !print *,'varname:',trim(varname)
       !print *,'Writing ',trim(mom_varname),' with shape',shape(VAR),' ',trim(varname)

       !stop

       call check( nf90_def_var(ncid, mom_varname, NF90_REAL, dimids2d, varid) )
       !call check( nf90_def_var(ncid, mom_varname, NF90_DOUBLE, dimids2d, varid) )
       call check( nf90_enddef(ncid) )
       call check(nf90_inq_varid(ncid,mom_varname,varid))    
       call check( nf90_put_var(ncid, varid, VAR(:,:,1)) )
       call check( nf90_close(ncid) )

    case('3d')
       !print *,'Writing 3D field'
       if (append) then
          call check( nf90_open(fname, NF90_WRITE, ncid) )
          call check( nf90_inq_dimid(ncid, "xaxis_1", x_dimid) )
          call check( nf90_inq_dimid(ncid, "yaxis_1", y_dimid) )
          call check( nf90_inq_dimid(ncid, "zaxis_1", z_dimid) )
          call check( nf90_inq_dimid(ncid, "Time", t_dimid) )
          call check( nf90_redef(ncid) )
       else
          call check( nf90_create(fname, NF90_CLOBBER, ncid) )
          call check( nf90_def_dim(ncid, "xaxis_1", Nx, x_dimid) )
          call check( nf90_def_dim(ncid, "yaxis_1", Ny, y_dimid) )
          call check( nf90_def_dim(ncid, "zaxis_1", nlevels, z_dimid) )
          call check( nf90_def_dim(ncid, "Time", Nt, t_dimid) )
       end if
       dimids3d =  (/ x_dimid, y_dimid, z_dimid, t_dimid /)

       !print *,'varname=',trim(varname)
       select case(trim(varname))
       case('T')
          mom_varname='temp'
       case('S')
          mom_varname='salt'
          where ( VARtmp<=0.0 )
             VARtmp = 0.0
          end where
       case('U')
          mom_varname='u'
       case('V')
          mom_varname='v'          
       case default
          mom_varname=trim(varname)
       end select

       !print *,'mom_varname=',trim(mom_varname)
       call check( nf90_def_var(ncid, mom_varname, NF90_REAL, dimids3d, varid) )
       !call check( nf90_def_var(ncid, mom_varname, NF90_DOUBLE, dimids3d, varid) )
       call check( nf90_enddef(ncid) )
       call check(nf90_inq_varid(ncid,mom_varname,varid))    
       call check( nf90_put_var(ncid, varid, VARtmp) )
       call check( nf90_close(ncid) )

    end select
  end subroutine write23d

  !-----------------------
  subroutine write1d(fname, varname, VAR1d, No, append)
    !-----------------------
    use netcdf
    implicit none

    integer, parameter :: NDIMS = 1
    integer :: ncid, varid, dimids1d(1)
    integer :: x_dimid, N
    integer, intent(in)  ::  No
    logical, intent(in)  ::  append
    character (len = 400), intent(in)   :: fname, varname
    real, dimension(No), intent(in)  :: VAR1d

    if (append) then
       call check( nf90_open(fname, NF90_WRITE, ncid) )
       call check( nf90_inq_dimid(ncid, "N_obs", x_dimid) )
       call check( nf90_redef(ncid) )
    else
       call check( nf90_create(fname, NF90_CLOBBER, ncid) )
       call check( nf90_def_dim(ncid, "N_obs", No, x_dimid) )
    end if
    dimids1d =  (/ x_dimid /)
    call check( nf90_def_var(ncid, varname, NF90_REAL, dimids1d, varid) )
    call check( nf90_enddef(ncid) )
    call check( nf90_inq_varid(ncid,varname,varid) )    
    call check( nf90_put_var(ncid, varid, VAR1d) )
    call check( nf90_close(ncid) )

  end subroutine write1d

  !-----------------------
  subroutine get_center_Xe(Xe,fname,myrank,yyyymmdd,Nx,Ny,num_levels,Xe_path, X_varname, X_varshape)
    !-----------------------

    use netcdf

    implicit none

    !real, allocatable, dimension(:,:,:), intent(out)  :: Xe     !Allocated before call
    real, allocatable, dimension(:), intent(inout)  :: Xe     !Allocated before call
    integer, intent(in)                               :: myrank
    character (len=8), intent(in)                     :: yyyymmdd
    character (len = 400), intent(in)                :: Xe_path
    character (len = 400), intent(in)                :: X_varname, X_varshape
    real, allocatable, dimension(:,:,:,:)              :: VAR
    integer                                          :: varid
    integer                                          :: ncid
    character (len = 400), intent(in)                 :: fname
    character (len = 400)                 :: varname
    integer                                          :: Nx, Ny, Nz, Ncat, num_levels, lev_index
    !character (len = 400)                            :: Xe_path, X_varname
    !namelist /recenter_grid_size/Nx, Ny, Nz, Ncat
    !namelist /recenter_X/Xe_path, X_varname, num_levels

    !open(7,file='recenter.nml')
    !read(7,NML=recenter_X)
    !read(7,NML=recenter_grid_size)
    !close(7)

    
    !select case(X_varname)
    !case('T','S')
       !allocate(VAR(Nx, Ny, 40,1))
    !   allocate(VAR(Nx, Ny, Nz,1))
    !case default
    allocate(VAR(Nx, Ny, num_levels,1))
    !end select
    !allocate(Xe(Nx, Ny, num_levels))

    !print *,'cpu#', myrank, fname, shape(VAR), fname

    call check(nf90_open(fname,nf90_nowrite,ncid))
    call check(nf90_inq_varid(ncid,X_varname,varid))
    call check(nf90_get_var(ncid,varid,VAR))
    call check(nf90_close(ncid))

    !Xe(:,:,1)=VAR(:,:,1)
    !Xe=VAR(:,:,:,1)

    Xe=reshape(VAR,(/Nx*Ny*num_levels/))

    where (abs(Xe)>undef)
       Xe=0.0
    end where

    !print *,'cpu#',myrank,' getting out of get_Xe'
    
    !do lev_index = 1, num_levels
    !   Xe(:,:,lev_index)=VAR(:,:,lev_index)
    !end do
    deallocate(VAR)

  end subroutine get_center_Xe



  !-----------------------    
  subroutine mpi_demean(Xe,meanX,Ns,Ne,scale,ierror)
    !-----------------------

    !mpi
    !===
    integer                                        :: ierror
    integer                             :: root=0

    real, allocatable, dimension (:), intent(inout)               :: Xe, meanX
    real, intent(in)               :: scale

    integer :: Ne, Ns

    !Compute mean and demean
    !=======================

    call mpi_barrier(MPI_COMM_WORLD,ierror)
    call mpi_allreduce(Xe,meanX,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
    call mpi_barrier(MPI_COMM_WORLD,ierror)
    meanX=meanX/real(Ne)
    Xe=scale*(Xe-meanX)
    where (isnan(Xe))
       Xe=0.0
    end where

  end subroutine mpi_demean

  !-----------------------
  subroutine mpi_std(Xe,sigX,Ns,Ne,ierror)
    !-----------------------
    !mpi
    !===
    implicit none

    integer                             :: ierror
    integer                             :: root=0

    real, allocatable, dimension (:), intent(inout)     :: Xe, sigX
    real, allocatable, dimension (:)      :: sumX2

    integer :: Ne, Ns

    allocate(sumX2(Ns))
    sigX=Xe**2
    call mpi_barrier(MPI_COMM_WORLD,ierror)
    call mpi_allreduce(sigX,sumX2,Ns,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierror)
    call mpi_barrier(MPI_COMM_WORLD,ierror)
    sigX=(sumX2/(Ne-1))**0.5
    
    deallocate(sumX2)
    
  end subroutine mpi_std

end program ocean_recenter
