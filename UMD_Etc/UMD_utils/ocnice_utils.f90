module ocnice_utils

  use netcdf

  implicit none

  type ModelGrid
     character (len = 400)              :: fname = 'grid_spec.nc'
     integer                            :: Nx, Ny, Nz
     real, dimension(:,:), allocatable  :: x, y, wet, area
     real, dimension(:), allocatable    :: depth
  end type ModelGrid

  type OceanRaster
     character (len = 400)              :: fname = 'tile.data'
     integer                            :: Ntiles
     real, dimension(:), allocatable    :: lons, lats, areas, wghts
     !real, dimension(:), allocatable    :: lons, lats, areas
     integer, dimension(:), allocatable :: GIs, GJs
  end type OceanRaster

  type OceanBounds
     character (len = 400)              :: fname = 'input.nml'
     real*4                             :: t_min_limit, t_max_limit, s_min_limit, s_max_limit
  end type OceanBounds

  type OceanDates
     !Time origine
     character (len = 4)              :: yyyy = '2012'
     character (len = 2)              :: mm = '01'
     character (len = 2)              :: dd = '01'
     character (len = 2)              :: hh = '12'
     real*4                           :: ndays !days past yyyymmddhh
  end type OceanDates

 public readrst2d
 public readrst3d


  interface readrst2d 
     module procedure readrst2dr4
     module procedure readrst2dr8
  end interface 

  interface readrst3d 
     module procedure readrst3dr4
     module procedure readrst3dr8
  end interface 

contains

  !---------------------------------------------------------------------------------
  subroutine init_bounds(ocn_bnds)
    !For MOM's namelist
    type (OceanBounds), intent(inout) :: ocn_bnds 
    logical           :: debug_this_module
    character*300     :: temperature_variable
    logical           :: pottemp_2nd_iteration
    logical           :: pottemp_equal_contemp
    real*4            :: t_min, t_max, s_min, s_max
    real*4            :: t_min_limit, t_max_limit, s_min_limit, s_max_limit
    namelist /ocean_tempsalt_nml/debug_this_module, temperature_variable, pottemp_2nd_iteration, pottemp_equal_contemp, t_min, t_max, s_min, s_max, t_min_limit, t_max_limit, s_min_limit, s_max_limit
    open(7,file='input.nml')
    read(7,NML=ocean_tempsalt_nml)
    close(7)
    ocn_bnds%t_min_limit = t_min_limit
    ocn_bnds%t_max_limit = t_max_limit
    ocn_bnds%s_min_limit = s_min_limit
    ocn_bnds%s_max_limit = s_max_limit
  
  end subroutine init_bounds

  !---------------------------------------------------------------------------------
  subroutine read_grid(grid)

    use netcdf

    type (ModelGrid), intent(inout)        :: grid
    integer                                :: varid, ncid, londimid, latdimid, levdimid

    call check(nf90_open('grid_spec.nc', nf90_nowrite,ncid))

    !Get the size of the horizontal grid
    call check(nf90_inq_dimid(ncid, 'grid_x_T', londimid))
    call check(nf90_inquire_dimension(ncid, londimid, len = grid%Nx))

    call check(nf90_inq_dimid(ncid, 'grid_y_T', latdimid))
    call check(nf90_inquire_dimension(ncid, latdimid, len = grid%Ny))

    !Get number of levels
    call check(nf90_inq_dimid(ncid, 'zt', levdimid))
    call check(nf90_inquire_dimension(ncid, levdimid, len = grid%Nz))

    allocate( grid%x(grid%Nx, grid%Ny), grid%y(grid%Nx, grid%Ny), grid%wet(grid%Nx, grid%Ny), grid%area(grid%Nx, grid%Ny) )
    allocate( grid%depth(grid%Nz) )

    call check(nf90_inq_varid(ncid,'x_T',varid))
    call check(nf90_get_var(ncid,varid,grid%x))
    call check(nf90_inq_varid(ncid,'y_T',varid))
    call check(nf90_get_var(ncid,varid,grid%y))
    call check(nf90_inq_varid(ncid,'wet',varid))
    call check(nf90_get_var(ncid,varid,grid%wet))
    call check(nf90_inq_varid(ncid,'area_T',varid))
    call check(nf90_get_var(ncid,varid,grid%area))
    call check(nf90_inq_varid(ncid,'zt',varid))
    call check(nf90_get_var(ncid,varid,grid%depth))

    call check(nf90_close(ncid))

  end subroutine read_grid


  !---------------------------------------------------------------------------------
  subroutine readoceanrst(filename, varname, ogrid, VARout)

    implicit none

    character*1028, intent(in)                                  :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    type (ModelGrid), intent(in)                                :: ogrid      ! Ocean grid, tripolar
    real, allocatable, dimension(:, :, :), intent(inout)        :: VARout      ! VAR2d needs to be allocated before call
    integer                                                     :: varid, fid_in
    real, allocatable, dimension(:,:,:,:)                       :: VAR

    allocate(VAR(1:ogrid%Nx, 1:ogrid%Ny, 1:ogrid%Nz,1))
    call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
    call check(nf90_inq_varid(fid_in, varname, varid))
    call check(nf90_get_var(fid_in, varid, VAR))
    call check(nf90_close(fid_in))

    VARout(:,:,:)=VAR(:,:,:,1)

    deallocate(VAR)

  end subroutine readoceanrst

 !---------------------------------------------------------------------------------
  subroutine readrst2dr4(filename, varname, VAR2d)
    ! Read 2-d variables from raster file
    implicit none

    character*1028, intent(in)                                   :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    real, allocatable, dimension(:, :), intent(inout)   :: VAR2d      ! VAR2d needs to be allocated before calling readrst
    integer                                                     :: varid, fid_in

    print *,'Reading ',trim(varname)
    call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
    call check(nf90_inq_varid(fid_in, varname, varid))
    call check(nf90_get_var(fid_in, varid, VAR2d))
    call check(nf90_close(fid_in))  

  end subroutine readrst2dr4

  subroutine readrst2dr8(filename, varname, VAR2d)
    ! Read 2-d variables from raster file
    implicit none

    character*1028, intent(in)                                   :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    real*8, allocatable, dimension(:, :), intent(inout)   :: VAR2d      ! VAR2d needs to be allocated before calling readrst
    integer                                                     :: varid, fid_in

    print *,'Reading ',trim(varname)
    call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
    call check(nf90_inq_varid(fid_in, varname, varid))
    call check(nf90_get_var(fid_in, varid, VAR2d))
    call check(nf90_close(fid_in))  

  end subroutine readrst2dr8

  !---------------------------------------------------------------------------------
  subroutine readrst3dr4(filename, varname, VAR3d)
    ! Read 3-d variables from raster file
    implicit none

    character*1028, intent(in)                                   :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    real, allocatable, dimension(:, :, :), intent(inout)        :: VAR3d      ! VAR3d needs to be allocated before calling readrst
    integer                                                     :: varid, fid_in

    print *,'Reading ',trim(varname)
    call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
    call check(nf90_inq_varid(fid_in, varname, varid))
    call check(nf90_get_var(fid_in, varid, VAR3d))
    call check(nf90_close(fid_in))  

  end subroutine readrst3dr4

  subroutine readrst3dr8(filename, varname, VAR3d)
    ! Read 3-d variables from raster file
    implicit none

    character*1028, intent(in)                                   :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    real*8, allocatable, dimension(:, :, :), intent(inout)        :: VAR3d      ! VAR3d needs to be allocated before calling readrst
    integer                                                     :: varid, fid_in

    print *,'Reading ',trim(varname)
    call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
    call check(nf90_inq_varid(fid_in, varname, varid))
    call check(nf90_get_var(fid_in, varid, VAR3d))
    call check(nf90_close(fid_in))  

  end subroutine readrst3dr8

  !---------------------------------------------------------------------------------
  subroutine readseaicerst(filename, varname, ogrid, ana_fmt, VAR2d)
    ! Read 2-d sea-ice variables stored in ocean velocity restart!
    implicit none

    character*1028, intent(in)                                   :: filename
    character*128, intent(in)                                   :: varname    ! Name of variable in filename
    type (ModelGrid), intent(in)                                :: ogrid      ! Ocean grid, tripolar
    character*128, intent(in)                                   :: ana_fmt    ! 'oletkf' or 'regular'
    real, allocatable, dimension(:, :), intent(inout)           :: VAR2d      ! VAR2d needs to be allocated before call
    integer                                                     :: varid, fid_in
    real, allocatable, dimension(:,:,:,:)                       :: VAR

    print *,'Reading ',trim(varname)
    print *,'File format:',trim(ana_fmt)
    if ( ana_fmt == 'regular' ) then
       call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
       call check(nf90_inq_varid(fid_in, varname, varid))
       call check(nf90_get_var(fid_in, varid, VAR2d))
       call check(nf90_close(fid_in))
    else   
       allocate(VAR(1:ogrid%Nx, 1:ogrid%Ny, 1:ogrid%Nz,1))
       call check(nf90_open(filename, NF90_NOWRITE, fid_in) )
       !call check(nf90_inq_varid(fid_in, 'u', varid))
       call check(nf90_inq_varid(fid_in, 'ice', varid))
       call check(nf90_get_var(fid_in, varid, VAR))
       call check(nf90_close(fid_in))  

       select case(varname)
       case('AICE')
          VAR2d = VAR(:,:,1,1)
          !where (abs(VAR2d)>20.0)
          !   VAR2d=(abs(VAR2d)/VAR2d)*20.0
          !end where
          !VAR2d = exp(VAR2d)/(1.0 + exp(VAR2d)) ! Assumes analysis file contains logit(AICE)
       case('HICE')
          VAR2d = VAR(:,:,2,1)
          where(VAR2d<=0.0)
             VAR2d=0.0
          end where
       case('HSNO')
          VAR2d = VAR(:,:,3,1)
       end select
       deallocate(VAR)
    end if
  end subroutine readseaicerst

  !---------------------------------------------------------------------------------
  subroutine readraster(oraster)
    implicit none

    type (OceanRaster), intent(out)                      :: oraster   ! Tile info
    integer                  :: ntiles
    integer                  :: mark
    character*128            :: dumstr
    real                     :: dum, rdum0, rdum1, rdum2, rdum3
    integer                  :: idum0, idum1, nt, n
    ! Get the number of ocean tiles

    print *,'raster file name:',trim(oraster%fname)

    open(10, file=oraster%fname, form="formatted",status='old')
    read(10, fmt=*) nt
    read(10, fmt=*) dum
    read(10, fmt=*) dumstr
    read(10, fmt=*) dum
    read(10, fmt=*) dum
    read(10, fmt=*) dumstr
    read(10, fmt=*) dum
    read(10, fmt=*) dum
    ntiles = 0
    do n=1,nt
       read(10, fmt=*) mark
       if(mark .eq. 0) ntiles = ntiles + 1
    enddo
    close(10)
    oraster%Ntiles = ntiles

    ! Allocate 
    allocate( oraster%lons(ntiles), oraster%lats(ntiles), oraster%areas(ntiles) )
    allocate( oraster%wghts(ntiles)) 
    allocate( oraster%GIs(ntiles), oraster%GJs(ntiles) )

    open(10, file=oraster%fname, form="formatted",status='old')

    read(10, fmt=*) nt
    read(10, fmt=*) dum
    read(10, fmt=*) dumstr
    read(10, fmt=*) dum
    read(10, fmt=*) dum
    read(10, fmt=*) dumstr
    read(10, fmt=*) dum
    read(10, fmt=*) dum
    do
       read(10, fmt=*) mark
       if(mark .eq. 0) exit
    end do
    backspace(10)

    do n=1,ntiles
       !read(10, fmt=*) mark, rdum0, rdum1, rdum2, dum, dum, dum, dum, idum0, idum1
       read(10, fmt=*) mark, rdum0, rdum1, rdum2, dum, dum, dum, dum, idum0, idum1, rdum3
       if(mark .eq. 0) then
          oraster%areas(n) = rdum0*40589641.0 ! dum*R_earth^2
          oraster%lons(n)  = rdum1
          oraster%lats(n)  = rdum2
          oraster%GIs(n)   = idum0
          oraster%GJs(n)   = idum1
          oraster%wghts(n) = rdum3
       endif
    enddo
    close(10)
    
  end subroutine readraster


  !---------------------------------------------------------------------------------
  subroutine check(status)

    use netcdf

    integer, intent (in) :: status

    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if

  end subroutine check

  !---------------------------------------------------------------------------------
  function str2bool(str_in) result(bool_out)

    implicit none
    logical                              :: bool_out
    character (len = 200)                :: str_in

    if ( str_in == '.true.') then
       bool_out = .true.
    else
       bool_out = .false.
    end if

  end function str2bool

  !---------------------------------------------------------------------------------
  ! Should use gsw, but in the meantime ...
  function Tfreeze(S, p)

    implicit none
    real                              :: S       ! [psu]
    real                              :: p       ! [dbar]
    real                              :: Tfreeze ! [C] Potential temperature freezing point

    ! UNESCO (1983): Algorithms for computation of fundamental properties of seawater. UNESCO technical papers in marine science 44:1-55.
    Tfreeze = (-0.0575 + 1.710523E-3 * Sqrt (Abs (S)) - 2.154996E-4 * S) * S - 7.53E-4 * p 
    Tfreeze = T2potT(Tfreeze, S, p)

  end function Tfreeze

  function T2potT(T, S, p)
    implicit none
    real                              :: S        ! [psu]
    real                              :: p        ! [dbar]
    real                              :: T        ! [C] Temperature
    real                              :: T2potT   ! [C] Potential Temperature    
    real                              :: s_rel
    real  :: a_a1 = 3.6504E-4, a_a2 = 8.3198E-5, a_a3 = 5.4065E-7, a_a4 = 4.0274E-9
    real  :: a_b1 = 1.7439E-5, a_b2 = 2.9778E-7
    real  :: a_c1 = 8.9309E-7, a_c2 = 3.1628E-8, a_c3 = 2.1987E-10
    real  :: a_d = 4.1057E-9, a_e1 = 1.6056E-10, a_e2 = 5.0484E-12
    real  :: aa, bb, cc, cc1, dd

    s_rel = S - 35.0;

    aa = (a_a1+ T *(a_a2 - T*(a_a3 - a_a4*T)));
    bb = s_rel*(a_b1 -a_b2*T)     ;
    cc = (a_c1 + T*(-a_c2 + a_c3*T));
    cc1 = a_d*s_rel;
    dd = (-a_e1 + a_e2*T);

    T2potT = T - p * ( aa + bb + p * (cc - cc1 + p * dd) )

  end function T2potT

  !---------------------------------------------------------------------------------
  subroutine clean_ocean_state(ogrid, T, S)

    implicit none

    type (ModelGrid), intent(in)                                :: ogrid      ! Ocean grid
    real, allocatable, dimension(:, :, :), intent(inout)        :: T, S       ! Need to be allocated before call
    type (OceanBounds)                                          :: ocn_bnds 
    integer                                                     :: i, j, k
    real                                                        :: Tf, Tcrit=35.0, Scrit=40.0

    call init_bounds(ocn_bnds)
    
    where (S>999.9)
       S = 0.0
    end where    
    where (S<0.0)
       S = 0.0
    end where
    where (S>=Scrit)
       S = Scrit + 0.5*(S-Scrit) 
    end where

    where (T>999.9)
       T = 0.0
    end where    
    where (T>=Tcrit)
       T = Tcrit + 0.5*(T-Tcrit) 
    end where

    !Set Tmin to the freeze point
    !

    do k=1,ogrid%Nz
       do j=1,ogrid%Ny
          do i=1,ogrid%Nx
             Tf = Tfreeze(S(i,j,k), ogrid%depth(k))   ! Assumes 1dbar/m
             if (T(i,j,k)<Tf) T(i,j,k)=Tf
          end do
       end do
    end do

    where (S<ocn_bnds%s_min_limit) ! MOVE THAT STUFF IN clean_ocean_state
       S=ocn_bnds%s_min_limit
    end where
    where (S>ocn_bnds%s_max_limit)
       S=ocn_bnds%s_max_limit
    end where
    
    where (T<ocn_bnds%t_min_limit)
       T=ocn_bnds%t_min_limit
    end where
    where (T>ocn_bnds%t_max_limit)
       T=ocn_bnds%t_max_limit
    end where

  end subroutine clean_ocean_state

  !---------------------------------------------------------------------------------
  subroutine write_ocean_state2d(fname, varname, VAR, append, ogrid)

    use netcdf
    implicit none

    integer, parameter :: NDIMS = 3
    integer :: ncid, varid, dimids2d(3), dimids3d(4)
    integer :: x_dimid, y_dimid, z_dimid, t_dimid
    integer              ::  Nt = 1
    type (ModelGrid), intent(in)                     :: ogrid      ! Ocean grid
    logical, intent(in)                              ::  append
    character*1028, intent(in)                :: fname
    character*128, intent(in)                :: varname
    real, dimension(:,:), allocatable, intent(in)  :: VAR
    real, dimension(:,:,:), allocatable            :: VARtmp


    Nt=1

    allocate(VARtmp(ogrid%Nx, ogrid%Ny, Nt))

    VARtmp(:,:,1)=VAR

    if (append) then
       call check( nf90_open(fname, NF90_WRITE, ncid) )
       call check( nf90_inq_dimid(ncid, "xaxis_1", x_dimid) )
       call check( nf90_inq_dimid(ncid, "yaxis_1", y_dimid) )
       call check( nf90_inq_dimid(ncid, "Time", t_dimid) )
       call check( nf90_redef(ncid) )
    else
       call check( nf90_create(fname, NF90_CLOBBER, ncid) )
       call check( nf90_def_dim(ncid, "xaxis_1", ogrid%Nx, x_dimid) )
       call check( nf90_def_dim(ncid, "yaxis_1", ogrid%Ny, y_dimid) )
       call check( nf90_def_dim(ncid, "Time", Nt, t_dimid) )
    end if
    dimids2d =  (/ x_dimid, y_dimid, t_dimid /)


    call check( nf90_def_var(ncid, varname, NF90_DOUBLE, dimids2d, varid) )
    call check( nf90_enddef(ncid) )
    call check(nf90_inq_varid(ncid,varname,varid))    
    call check( nf90_put_var(ncid, varid, VARtmp) )
    call check( nf90_close(ncid) )

  end subroutine write_ocean_state2d

  !---------------------------------------------------------------------------------
  subroutine write_ocean_state(fname, varname, VAR, append, ogrid)

    use netcdf
    implicit none

    integer, parameter :: NDIMS = 3
    integer :: ncid, varid, dimids2d(3), dimids3d(4)
    integer :: x_dimid, y_dimid, z_dimid, t_dimid
    integer              ::  Nt = 1
    type (ModelGrid), intent(in)                     :: ogrid      ! Ocean grid
    logical, intent(in)                              ::  append
    character*1028, intent(in)                :: fname
    character*128, intent(in)                :: varname
    real, dimension(:,:,:), allocatable, intent(in)  :: VAR
    real, dimension(:,:,:,:), allocatable            :: VARtmp


    Nt=1

    allocate(VARtmp(ogrid%Nx, ogrid%Ny, ogrid%Nz, Nt))

    VARtmp(:,:,:,1)=VAR

    if (append) then
       call check( nf90_open(fname, NF90_WRITE, ncid) )
       call check( nf90_inq_dimid(ncid, "xaxis_1", x_dimid) )
       call check( nf90_inq_dimid(ncid, "yaxis_1", y_dimid) )
       call check( nf90_inq_dimid(ncid, "zaxis_1", z_dimid) )
       call check( nf90_inq_dimid(ncid, "Time", t_dimid) )
       call check( nf90_redef(ncid) )
    else
       call check( nf90_create(fname, NF90_CLOBBER, ncid) )
       call check( nf90_def_dim(ncid, "xaxis_1", ogrid%Nx, x_dimid) )
       call check( nf90_def_dim(ncid, "yaxis_1", ogrid%Ny, y_dimid) )
       call check( nf90_def_dim(ncid, "zaxis_1", ogrid%Nz, z_dimid) )
       call check( nf90_def_dim(ncid, "Time", Nt, t_dimid) )
    end if
    dimids3d =  (/ x_dimid, y_dimid, z_dimid, t_dimid /)


    call check( nf90_def_var(ncid, varname, NF90_DOUBLE, dimids3d, varid) )
    call check( nf90_enddef(ncid) )
    call check(nf90_inq_varid(ncid,varname,varid))    
    call check( nf90_put_var(ncid, varid, VARtmp) )
    call check( nf90_close(ncid) )

  end subroutine write_ocean_state


  !---------------------------------------------------------------------------------
  subroutine write_mom_increment(fname, varname, VAR, ogrid)

    use netcdf
    implicit none

    integer, parameter :: NDIMS = 3
    integer :: ncid, varid, dimids1d(1), dimids3d(4)
    integer :: x_dimid, y_dimid, z_dimid, t_dimid
    integer              ::  Nt = 1
    type (ModelGrid), intent(in)                     :: ogrid      ! Ocean grid
    character*1028, intent(in)                :: fname
    character*128, intent(in)                :: varname
    real, dimension(:,:,:), allocatable, intent(in)  :: VAR
    real, dimension(:,:,:,:), allocatable            :: VARtmp
    real, dimension(:), allocatable :: Time, X, Y, Z
    integer :: index

    Nt=1

    allocate(VARtmp(ogrid%Nx, ogrid%Ny, ogrid%Nz, Nt), Time(Nt), X(ogrid%Nx), Y(ogrid%Nx), Z(ogrid%Nx) )

    do index = 1, ogrid%Nx
       X(index)=real(index)
    end do
    do index = 1, ogrid%Ny
       Y(index)=real(index)
    end do
    do index = 1, ogrid%Nz
       Z(index)=real(index)
    end do    

    VARtmp(:,:,:,1)=VAR

    call check( nf90_create(fname, NF90_CLOBBER, ncid) )
    call check( nf90_def_dim(ncid, "xaxis_1", ogrid%Nx, x_dimid) )
    call check( nf90_def_dim(ncid, "yaxis_1", ogrid%Ny, y_dimid) )
    call check( nf90_def_dim(ncid, "zaxis_1", ogrid%Nz, z_dimid) )
    call check( nf90_def_dim(ncid, "Time", nf90_unlimited, t_dimid) )

    dimids3d =  (/ x_dimid, y_dimid, z_dimid, t_dimid/)
    call check( nf90_def_var(ncid, varname, NF90_DOUBLE, dimids3d, varid) )

    dimids1d =  (/ x_dimid/)
    call check( nf90_def_var(ncid, 'xaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "X") )

    dimids1d =  (/ y_dimid/)
    call check( nf90_def_var(ncid, 'yaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "Y") )

    dimids1d =  (/ z_dimid/)
    call check( nf90_def_var(ncid, 'zaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "Z") )

    dimids1d =  (/ t_dimid/)
    call check( nf90_def_var(ncid, 'Time', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "T") )
    call check( nf90_put_att(ncid, varid, "units", "days since 2013-04-30 00:00:00") )
    call check( nf90_put_att(ncid, varid, "calendar_type", "365_day") )
    call check( nf90_enddef(ncid) )


    Time =  (/1.0/)!, 2.0/)
    call check(nf90_inq_varid(ncid,'Time',varid))    
    call check( nf90_put_var(ncid, varid, Time) )
    call check(nf90_inq_varid(ncid,varname,varid))    
    call check( nf90_put_var(ncid, varid, VARtmp) )

    call check( nf90_close(ncid) )

  end subroutine write_mom_increment

  !---------------------------------------------------------------------------------
  subroutine write_mass_increment(mass_incr, ogrid)

    use netcdf
    implicit none

    integer, parameter                        :: NDIMS = 3
    integer                                   :: ncid, varid, dimids1d(1), dimids3d(3)
    integer                                   :: x_dimid, y_dimid, z_dimid, t_dimid
    integer                                   :: Nt = 1
    type (ModelGrid), intent(in)              :: ogrid      ! Ocean grid
    real, intent(in)                          :: mass_incr
    character*1028                :: fname
    character*128                 :: varname
    real, dimension(:,:,:), allocatable     :: VARtmp
    real, dimension(:), allocatable :: Time, X, Y, Z
    integer :: index

    Nt=1
    fname   = 'eta_increment.nc'
    varname = 'eta_inc'

    allocate(VARtmp(ogrid%Nx, ogrid%Ny, Nt), Time(Nt), X(ogrid%Nx), Y(ogrid%Nx) )

    do index = 1, ogrid%Nx
       X(index)=real(index)
    end do
    do index = 1, ogrid%Ny
       Y(index)=real(index)
    end do

    VARtmp(:,:,1)=mass_incr

    call check( nf90_create(fname, NF90_CLOBBER, ncid) )
    call check( nf90_def_dim(ncid, "xaxis_1", ogrid%Nx, x_dimid) )
    call check( nf90_def_dim(ncid, "yaxis_1", ogrid%Ny, y_dimid) )
    call check( nf90_def_dim(ncid, "Time", nf90_unlimited, t_dimid) )

    dimids3d =  (/ x_dimid, y_dimid, t_dimid/)
    call check( nf90_def_var(ncid, varname, NF90_DOUBLE, dimids3d, varid) )

    dimids1d =  (/ x_dimid/)
    call check( nf90_def_var(ncid, 'xaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "X") )

    dimids1d =  (/ y_dimid/)
    call check( nf90_def_var(ncid, 'yaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "Y") )

    dimids1d =  (/ t_dimid/)
    call check( nf90_def_var(ncid, 'Time', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "T") )
    call check( nf90_put_att(ncid, varid, "units", "days since 2013-04-30 00:00:00") )
    call check( nf90_put_att(ncid, varid, "calendar_type", "365_day") )
    call check( nf90_enddef(ncid) )

    Time =  (/ 1.0 /)
    call check(nf90_inq_varid(ncid,'Time',varid))    
    call check( nf90_put_var(ncid, varid, Time) )
    call check(nf90_inq_varid(ncid,varname,varid))    
    call check( nf90_put_var(ncid, varid, VARtmp) )

    call check( nf90_close(ncid) )

  end subroutine write_mass_increment

  !---------------------------------------------------------------------------------
  subroutine write_mom_sponge(fname, varname, ogrid, dt)

    !INPUT/temp_sponge_coeff.nc
    !INPUT/salt_sponge_coeff.nc
    !INPUT/age_global_sponge_coeff.nc not

    use netcdf
    implicit none

    integer, parameter :: NDIMS = 3
    integer :: ncid, varid, dimids1d(1), dimids3d(3), dimids4d(4)
    integer :: x_dimid, y_dimid, z_dimid, t_dimid
    integer              ::  Nt = 1
    type (ModelGrid),  intent(in)                     :: ogrid      ! Ocean grid
    character*1028, intent(in)                        :: fname
    character*128, intent(in)                         :: varname
    real, intent(in)                                  :: dt    ! Restoring time scale in hours 
    real, dimension(:,:,:), allocatable               :: COEFF !sponge coeff
    real, dimension(:,:), allocatable                 :: VAR2d !sponge coeff
    real, dimension(:,:), allocatable                 :: VAR2d2 !sponge coeff
    real, dimension(:,:,:,:), allocatable             :: VARtmp
    real, dimension(:), allocatable                   :: Time, X, Y, Z
    integer :: index, sst_time_index

    character*1028                                    :: data_filename
    character*128                                     :: data_varname    ! Name of variable in filename

    character*128                                     :: date_origin, date_units

    open(10, file='ocean_sst.txt')
    read(10, fmt="(A30)") date_units
    read(10, fmt="(A20)") date_origin
    read(10, fmt=*) Nt    

    allocate(VARtmp(ogrid%Nx, ogrid%Ny, ogrid%Nz, Nt), Time(Nt), X(ogrid%Nx), Y(ogrid%Nx), Z(ogrid%Nx), COEFF(ogrid%Nx, ogrid%Ny, ogrid%Nz), VAR2d(ogrid%Nx, ogrid%Ny), VAR2d2(ogrid%Nx, ogrid%Ny) )

    do index = 1, ogrid%Nx
       X(index)=real(index)
    end do
    do index = 1, ogrid%Ny
       Y(index)=real(index)
    end do
    do index = 1, ogrid%Nz
       Z(index)=real(index)
    end do    

    data_varname='sst'
    do sst_time_index=0,Nt-1
       read(10, fmt=*)Time(sst_time_index+1)
       read(10, fmt=*)data_filename
       call readrst2d(data_filename, data_varname, VAR2d)
       VARtmp(:,:,1,sst_time_index+1)=VAR2d
    end do
    close(10)

!  now same for SIC
    open(10, file='ocean_sic.txt')
    read(10, fmt="(A30)") date_units
    read(10, fmt="(A20)") date_origin
    read(10, fmt=*) Nt    

!   data_varname='sic'
    data_varname='AICE'
    do sst_time_index=0,Nt-1
       read(10, fmt=*)Time(sst_time_index+1)
       read(10, fmt=*)data_filename
       call readrst2d(data_filename, data_varname, VAR2d2)
    end do
    close(10)

    COEFF = 0.0
    !COEFF(:,:,1) = ogrid%wet/(0.5*3600.0*24.0)  !12hrs restoring time scale
    !COEFF(:,:,1) = ogrid%wet/(3600.0*2.0)  ! 2hrs restoring time scale
    !COEFF(:,:,1) = ogrid%wet/(3600.0*0.5)  ! 2hrs restoring time scale
    COEFF(:,:,1) = ogrid%wet/(3600.0*dt)  ! 24hrs restoring time scale
 
!   mask out sic > 0.1 
    where (VAR2d2>0.1)
        COEFF(:,:,1)=0.0
    end where

    call check( nf90_create(fname, NF90_CLOBBER, ncid) )
    call check( nf90_def_dim(ncid, "xaxis_1", ogrid%Nx, x_dimid) )
    call check( nf90_def_dim(ncid, "yaxis_1", ogrid%Ny, y_dimid) )
    call check( nf90_def_dim(ncid, "zaxis_1", ogrid%Nz, z_dimid) )
    call check( nf90_def_dim(ncid, "Time", nf90_unlimited, t_dimid) )

    dimids4d =  (/ x_dimid, y_dimid, z_dimid, t_dimid/)
    call check( nf90_def_var(ncid, varname, NF90_DOUBLE, dimids4d, varid) )
    dimids3d =  (/ x_dimid, y_dimid, z_dimid/)
    call check( nf90_def_var(ncid, 'coeff', NF90_DOUBLE, dimids3d, varid) )

    dimids1d =  (/ x_dimid/)
    call check( nf90_def_var(ncid, 'xaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "X") )

    dimids1d =  (/ y_dimid/)
    call check( nf90_def_var(ncid, 'yaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "Y") )

    dimids1d =  (/ z_dimid/)
    call check( nf90_def_var(ncid, 'zaxis_1', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", "Z") )

    dimids1d =  (/ t_dimid/)
    call check( nf90_def_var(ncid, 'Time', NF90_DOUBLE, dimids1d, varid) )
    call check( nf90_put_att(ncid, varid, "units", date_units) )
    call check( nf90_put_att(ncid, varid, "time_origin", trim(date_origin)) )
    call check( nf90_put_att(ncid, varid, "calendar_type", "julian") )
    call check( nf90_enddef(ncid) )

    call check(nf90_inq_varid(ncid,'Time',varid))    
    call check( nf90_put_var(ncid, varid, Time) )

    call check(nf90_inq_varid(ncid,varname,varid))    
    call check( nf90_put_var(ncid, varid, VARtmp) )

    call check(nf90_inq_varid(ncid,'coeff',varid))    
    call check( nf90_put_var(ncid, varid, COEFF) )

    call check( nf90_close(ncid) )

  end subroutine write_mom_sponge

end module ocnice_utils
