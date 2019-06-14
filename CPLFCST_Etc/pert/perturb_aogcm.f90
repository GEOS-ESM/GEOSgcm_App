program perturb_aogcm

  use netcdf
  !use odas_resolution
  !use odas_types
  !use my_stats

  implicit none

  real, allocatable, dimension(:,:,:)                   :: CVAR3D,PVAR3D,VAR3D
  real, allocatable, dimension(:,:)                     :: CVAR2D,PVAR2D,VAR2D
  real, allocatable, dimension(:)                       :: CVAR1D,PVAR1D,VAR1D

  real                                                  :: meanT
  real                                                  :: meanS

  !type(ocean)                                           :: X
  !type(ocean)                                           :: sigX
  !type(ocean)                                           :: dX

  integer                                               :: sign_of_bv

  character (len = 200)                                 :: RPFILE_NAME         ! Name of the data file to be written
  character (len = 200)                                 :: RNFILE_NAME         ! Name of the data file to be written
  character (len = 200)                                 :: CFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: NFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: PFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: VAR_NAME            ! Descriptive name of the variable
  character (len = 200), dimension(2)                   :: VAR_NAME_TEMP
  character (len = 200), dimension(2)                   :: VAR_NAME_CUR
  character (len = 200), dimension(6)                   :: VAR_NAME_SBC
  character (len = 200), dimension(7)                   :: VAR_NAME_ICE
  character (len = 200), dimension(6)                   :: VAR_NAME_SLT
  character (len = 200), dimension(3)                   :: VAR_NAME_FVC
  character (len = 200), dimension(1)                   :: VAR_NAME_MST

  character (len = 200)                                 :: EOF_BASEDIR           ! Name of the data file to be read
  character (len = 200)                                 :: EOF_FNAME           ! Name of the data file to be read

  !Random number
  !=============
  integer, parameter                                    :: Ne=10              ! # of EOF to read
  integer                                               :: iseed, i
  real, dimension(Ne)                                   :: beta

  !Dummies
  !=======
  integer                                               :: index

  !I/O stuff
  !=========
  integer  :: ioerr

  !COMMAND LINE ARGUMENT STUFF
  !===========================
  character*300                      :: BUFFER  
  real                               :: resl

  !VARIABLE NAMES 
  !===========================
  data VAR_NAME_TEMP /"temp"  ,"salt"   /
  data VAR_NAME_CUR  /"u"     ,"v"      /
  data VAR_NAME_SBC  /"t_surf" ,"s_surf", "u_surf", "v_surf", "sea_lev", "frazil"  /
  data VAR_NAME_ICE /"DIVU","ICEUMASK","SHEAR","STRENGTH","STRESSCOMP","UVEL","VVEL" /
  data VAR_NAME_SLT /"HSKINI","HSKINW","SSKINI","SSKINW","TSKINI","TSKINW" /
  data VAR_NAME_FVC /"PT","U","V"/
  data VAR_NAME_MST /"Q"/

  !Read Temp and Salt from MOM restart files
  !-----------------------------------------
  CFILE_NAME="./cdata/aocean_temp_salt.res.nc"
  NFILE_NAME="./ndata/uocean_temp_salt.res.nc"
  PFILE_NAME="./pdata/uocean_temp_salt.res.nc"
  RNFILE_NAME="./ndata/ocean_temp_salt.res.nc"
  RPFILE_NAME="./pdata/ocean_temp_salt.res.nc"

  !CALCULATE RESCALING MAGNITUDE
  open (99, file='coef.dat')
  read (99,*) resl
  close(99)
  print*,'Rescaling factor read:',resl

  DO I=1,2
  !DO I=1,1
  VAR_NAME=VAR_NAME_TEMP(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  !RNBV=CTR-(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  ENDDO

  print*,'OCN MOM RESTARTS GENERATED ocean_temp_salt - temp salt'

  !Read Zonal and Meridional Currents from MOM restart files
  !-----------------------------------------
  CFILE_NAME="./cdata/aocean_velocity.res.nc"
  NFILE_NAME="./ndata/uocean_velocity.res.nc"
  PFILE_NAME="./pdata/uocean_velocity.res.nc"
  RNFILE_NAME="./ndata/ocean_velocity.res.nc"
  RPFILE_NAME="./pdata/ocean_velocity.res.nc"

  DO I=1,2
  VAR_NAME=VAR_NAME_CUR(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  !RNBV=CTR-(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  ENDDO

  print*,'OCN MOM RESTARTS GENERATED ocean_velocity - u v'

  !Read Surface values from MOM restart files
  !----------------------------------------
  CFILE_NAME="./cdata/aocean_sbc.res.nc"
  NFILE_NAME="./ndata/uocean_sbc.res.nc"
  PFILE_NAME="./pdata/uocean_sbc.res.nc"
  RNFILE_NAME="./ndata/ocean_sbc.res.nc"
  RPFILE_NAME="./pdata/ocean_sbc.res.nc"
  DO I=1,6
  VAR_NAME=VAR_NAME_SBC(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_2D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RNFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  sign_of_bv= 1
  call read_2D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RPFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  ENDDO

  print*,'OCN MOM RESTARTS GENERATED ocean_sbc - t_surf s_surf u_surf v_surf sea_lev frazil'

  !Read Seaice  values from seaice restart files
  !----------------------------------------
  CFILE_NAME="./cdata/aseaice_internal_rst"
  NFILE_NAME="./ndata/useaice_internal_rst"
  PFILE_NAME="./pdata/useaice_internal_rst"
  RNFILE_NAME="./ndata/seaice_internal_rst"
  RPFILE_NAME="./pdata/seaice_internal_rst"

  DO I=1,4
  VAR_NAME=VAR_NAME_ICE(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_2D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RNFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_2D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RPFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  ENDDO

  I=5
  VAR_NAME=VAR_NAME_ICE(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)

  DO I=6,7
  VAR_NAME=VAR_NAME_ICE(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_2D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RNFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_2D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR2D, VAR_NAME, resl,sign_of_bv)
  call write_2D(RPFILE_NAME, VAR2D, VAR_NAME)
  deallocate(VAR2D)
  ENDDO

  print*,'SEAICE PERT RESTARTS GENERATED seaice - DIVU ICEUMASK SHEAR STRENGTH STRESSCOMP UVEL VVEL'

  !Read Saltwater values from restart files
  !----------------------------------------
  CFILE_NAME="./cdata/asaltwater_internal_rst"
  NFILE_NAME="./ndata/usaltwater_internal_rst"
  PFILE_NAME="./pdata/usaltwater_internal_rst"
  RNFILE_NAME="./ndata/saltwater_internal_rst"
  RPFILE_NAME="./pdata/saltwater_internal_rst"
  DO I=1,6
  VAR_NAME=VAR_NAME_SLT(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_1D(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR1D, VAR_NAME, resl,sign_of_bv)
  call write_1D(RNFILE_NAME, VAR1D, VAR_NAME)
  deallocate(VAR1D)
  sign_of_bv= 1
  call read_1D(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR1D, VAR_NAME, resl,sign_of_bv)
  call write_1D(RPFILE_NAME, VAR1D, VAR_NAME)
  deallocate(VAR1D)
  ENDDO

  print*,'SALTWATER PERT RESTARTS ON TILES GENERATED saltwater - HSKINI HSKINW SSKINI SSKINW TSKINI TSKINW'

  !Read fvcore  values from fvcore restart files
  !----------------------------------------
  CFILE_NAME="./cdata/afvcore_internal_rst"
  NFILE_NAME="./ndata/ufvcore_internal_rst"
  PFILE_NAME="./pdata/ufvcore_internal_rst"
  RNFILE_NAME="./ndata/fvcore_internal_rst"
  RPFILE_NAME="./pdata/fvcore_internal_rst"

  DO I=1,3
  VAR_NAME=VAR_NAME_FVC(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  ENDDO

  print*,'ATM PERT RESTARTS GENERATED fvcore - U V PT'

 !Read moist   values from moist  restart files
  !----------------------------------------
  CFILE_NAME="./cdata/amoist_internal_rst"
  NFILE_NAME="./ndata/umoist_internal_rst"
  PFILE_NAME="./pdata/umoist_internal_rst"
  RNFILE_NAME="./ndata/moist_internal_rst"
  RPFILE_NAME="./pdata/moist_internal_rst"

  I=1
  VAR_NAME=VAR_NAME_MST(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D7(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)

  print*,'ATM PERT RESTARTS GENERATED moist - Q'

!========================================================================
!========================================================================

contains
  !-----------------------
  subroutine check(status)
  !-----------------------
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if

  end subroutine check

  !------------------------------------------
  subroutine read_3D3(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,NVAR,PVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV),NVAR(NLON,NLAT,NLEV),PVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling 
    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl/2.
    print *,'VAR 180 90 ;min/max=',VAR(180,90,1),minval(VAR),maxval(VAR)
    print *,'CVAR 180 90;min/max ',CVAR(180,90,1),minval(CVAR),maxval(CVAR)
    print *,'NVAR 180 90;min/max ',NVAR(180,90,1),minval(NVAR),maxval(NVAR)
    print *,'PVAR 180 90;min/max ',PVAR(180,90,1),minval(PVAR),maxval(PVAR)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading un-nbv  file ", NFILE_NAME
    print *,"*** SUCCESS reading un-pbv  file ", PFILE_NAME
    if (VAR_NAME .eq.'temp') then
     if ( minval(VAR).lt.-5.   ) print*,' Min PERT ErrVAR< -5:',minval(VAR)
     if ( minval(CVAR).lt.-5.  ) print*,' Min ASSI ErrVAR< -5:',minval(CVAR)
     if ( minval(NVAR).lt.-5   ) print*,' Min BV   ErrVAR< -5:',minval(NVAR)
     if ( minval(PVAR).lt.-5.  ) print*,' Min BV   ErrVAR< -5:',minval(PVAR)
     if ( maxval(VAR).gt.55.   ) print*,' Max PERT ErrVAR> 55:',maxval(VAR)
     if ( maxval(CVAR).gt.55.  ) print*,' Max ASSI ErrVAR> 55:',maxval(CVAR)
     if ( maxval(NVAR).gt.55   ) print*,' Max BV   ErrVAR> 55:',maxval(NVAR)
     if ( maxval(PVAR).gt.55.  ) print*,' Max BV   ErrVAR> 55:',maxval(PVAR)
    endif
    if (VAR_NAME .eq.'salt') then
     if ( minval(VAR).lt.-1.   ) print*,' Min PERT ErrVAR< -1:',minval(VAR)
     if ( minval(CVAR).lt.-1.  ) print*,' Min ASSI ErrVAR< -1:',minval(CVAR)
     if ( minval(NVAR).lt.-1   ) print*,' Min BV   ErrVAR< -1:',minval(NVAR)
     if ( minval(PVAR).lt.-1.  ) print*,' Min BV   ErrVAR< -1:',minval(PVAR)
     if ( maxval(VAR).gt.65.   ) print*,' Max PERT ErrVAR> 65:',maxval(VAR)
     if ( maxval(CVAR).gt.65.  ) print*,' Max ASSI ErrVAR> 65:',maxval(CVAR)
     if ( maxval(NVAR).gt.65   ) print*,' Max BV   ErrVAR> 65:',maxval(NVAR)
     if ( maxval(PVAR).gt.65.  ) print*,' Max BV   ErrVAR> 65:',maxval(PVAR)
    endif

    
    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_3D3

  !------------------------------------------
  subroutine read_3D7(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,NVAR,PVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'lon', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'lat', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'lev', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV),NVAR(NLON,NLAT,NLEV),PVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling 
    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl/2.
    print *,'VAR 180 400;min/max =',VAR(180,400,1),minval(VAR),maxval(VAR)
    print *,'CVAR 180 400;min/max ',CVAR(180,400,1),minval(CVAR),maxval(CVAR)
    print *,'NVAR 180 400;min/max ',NVAR(180,400,1),minval(NVAR),maxval(NVAR)
    print *,'PVAR 180 400;min/max ',PVAR(180,400,1),minval(PVAR),maxval(PVAR)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading un-nbv  file ", NFILE_NAME
    print *,"*** SUCCESS reading un-pbv  file ", PFILE_NAME

    
    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_3D7



  !------------------------------------------
  subroutine read_3DP(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,NVAR,PVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV),NVAR(NLON,NLAT,NLEV),PVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling 
    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl/2.
    print *,'VAR=',VAR(180,90,1)
    print *,'CVAR',CVAR(180,90,1)
    print *,'NVAR',NVAR(180,90,1)
    print *,'PVAR',PVAR(180,90,1)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading un-nbv  file ", NFILE_NAME
    print *,"*** SUCCESS reading un-pbv  file ", PFILE_NAME
    
    deallocate(CVAR,PVAR)

  end subroutine read_3DP

  !------------------------------------------
  subroutine read_3D(CFILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: CFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid
    integer                                               :: cvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )

    ! saving
    VAR = CVAR
    print *,'VAR=',VAR(180,90,1)
    print *,'CVAR',CVAR(180,90,1)
    print *,"*** SUCCESS reading file ", CFILE_NAME
    
    deallocate(CVAR)

  end subroutine read_3D

  !------------------------------------------
  subroutine write_3D(FILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: FILE_NAME           ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: VAR              ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: ncid
    integer                                               :: varid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Create netcdf file
    call check( nf90_open(FILE_NAME, NF90_WRITE, ncid) )

    
    ! Define the dimensions.
    !NLON=size(VAR,1)
    !NLAT=size(VAR,2)
    !NLEV=size(VAR,3)
    !call check( nf90_def_dim(ncid, 'lat', NLAT, lat_dimid) )
    !call check( nf90_def_dim(ncid, 'lon', NLON, lon_dimid) )
    !call check( nf90_def_dim(ncid, 'lev', NLEV, lev_dimid) )
    !state_dimids = (/ lon_dimid, lat_dimid, lev_dimid/)
    !call check( nf90_def_var(ncid, VAR_NAME, NF90_REAL, state_dimids, varid) )
    !call check( nf90_enddef(ncid) )
    call check( nf90_inq_varid(ncid, VAR_NAME, varid) )
    call check( nf90_put_var(ncid, varid, VAR))
    call check( nf90_close(ncid) )

    print *,"*** SUCCESS writing file ", FILE_NAME

  end subroutine write_3D

  !------------------------------------------ 
  subroutine read_2D3(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none
    
    
    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points 
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:)                   :: CVAR,NVAR,PVAR,VAR   ! 2D array of stuff
  
    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 2
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )

    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )

    allocate(VAR(NLON,NLAT),CVAR(NLON,NLAT),NVAR(NLON,NLAT),PVAR(NLON,NLAT))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling
    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl*0.5
    print *,'VAR=',VAR(180,90),minval(VAR),maxval(VAR)
    print *,'CVAR',CVAR(180,90),minval(CVAR),maxval(CVAR)
    print *,'NVAR',NVAR(180,90),minval(NVAR),maxval(NVAR)
    print *,'PVAR',PVAR(180,90),minval(PVAR),maxval(PVAR)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading neg pert.file ", NFILE_NAME
    print *,"*** SUCCESS reading pos pert.file ", PFILE_NAME

    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_2D3

  !------------------------------------------ 
  subroutine read_2D7(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none
    
    
    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points 
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:)                   :: CVAR,NVAR,PVAR,VAR,VTMP   ! 2D array of stuff
  
    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 2
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )

    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'lon', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )

    call check( nf90_inq_dimid(cncid, 'lat', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )

    allocate(VAR(NLON,NLAT),CVAR(NLON,NLAT),NVAR(NLON,NLAT),PVAR(NLON,NLAT),VTMP(NLON,NLAT))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling
    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl*0.5
    VTMP = VAR
    if (VAR_NAME .eq.'ICEUMASK') then
     where(VTMP.lt.0.) VAR=0.
     where(VTMP.gt.1.) VAR=1.
     if ( minval(VAR).lt.0.   ) print*,' Min PERT ErrVAR< 0:',minval(VAR)
     if ( maxval(VAR).gt.1.   ) print*,' Max PERT ErrVAR> 1:',maxval(VAR)
    endif
    if (VAR_NAME .eq.'STRENGTH' ) then
     where(VTMP .lt.0. ) VAR=0.
     if ( minval(VAR).lt.0.   ) print*,' Min PERT ErrVAR< 0:',minval(VAR)
    endif
    if (VAR_NAME .eq.'SHEAR' ) then
     where(VTMP .lt.0. ) VAR=0.
     if ( minval(VAR).lt.0.   ) print*,' Min PERT ErrVAR< 0:',minval(VAR)
    endif
    if (VAR_NAME .eq.'VVEL') then
     if ( minval(VAR).lt.-2.   ) print*,' Min PERT ErrVAR< -2:',minval(VAR)
     if ( minval(CVAR).lt.-2.  ) print*,' Min ASSI ErrVAR< -2:',minval(CVAR)
     if ( maxval(VAR).gt.2.   ) print*,' Max PERT ErrVAR> 2:',maxval(VAR)
     if ( maxval(CVAR).gt.2.  ) print*,' Max BV   ErrVAR> 2:',maxval(CVAR)
    endif
    if (VAR_NAME .eq.'UVEL') then
     if ( minval(VAR).lt.-2.   ) print*,' Min PERT ErrVAR< -2:',minval(VAR)
     if ( minval(CVAR).lt.-2.  ) print*,' Min ASSI ErrVAR< -2:',minval(CVAR)
     if ( maxval(VAR).gt.2.   ) print*,' Max PERT ErrVAR> 2:',maxval(VAR)
     if ( maxval(CVAR).gt.2.  ) print*,' Max ASSI ErrVAR> 2:',maxval(CVAR)
    endif
    print *,'VAR=',VAR(180,400),minval(VAR),maxval(VAR)
    print *,'CVAR',CVAR(180,400),minval(CVAR),maxval(CVAR)
    print *,'NVAR',NVAR(180,400),minval(NVAR),maxval(NVAR)
    print *,'PVAR',PVAR(180,400),minval(PVAR),maxval(PVAR)

    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading neg pert.file ", NFILE_NAME
    print *,"*** SUCCESS reading pos pert.file ", PFILE_NAME

    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_2D7



  !------------------------------------------
  subroutine write_2D(FILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none

    character (len = 200), intent(in)                     :: FILE_NAME           ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable

    ! VARIABLES
    real, allocatable, dimension(:,:)                   :: VAR              ! 2D array of stuff

    ! netCDF stuff
    integer                                               :: ncid
    integer                                               :: varid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid
    integer, parameter                                    :: NDIMS = 2
    integer                                               :: state_dimids(NDIMS)

    ! Create netcdf file
    call check( nf90_open(FILE_NAME, NF90_WRITE, ncid) )

    ! Define the dimensions.
    call check( nf90_inq_varid(ncid, VAR_NAME, varid) )
    call check( nf90_put_var(ncid, varid, VAR))
    call check( nf90_close(ncid) )

    print *,"*** SUCCESS writing file ", FILE_NAME

  end subroutine write_2D

  !------------------------------------------
  subroutine write_1D(FILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none

    character (len = 200), intent(in)                     :: FILE_NAME           ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable

    ! VARIABLES
    real, allocatable, dimension(:)                   :: VAR              ! 1D array of stuff

    ! netCDF stuff
    integer                                               :: ncid
    integer                                               :: varid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid
    integer, parameter                                    :: NDIMS = 1
    integer                                               :: state_dimids(NDIMS)

    ! Create netcdf file
    call check( nf90_open(FILE_NAME, NF90_WRITE, ncid) )

    ! Define the dimensions.
    call check( nf90_inq_varid(ncid, VAR_NAME, varid) )
    call check( nf90_put_var(ncid, varid, VAR))
    call check( nf90_close(ncid) )

    print *,"*** SUCCESS writing file ", FILE_NAME

  end subroutine write_1D

  !------------------------------------------ 
  subroutine read_1D(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv)
  !------------------------------------------

    use netcdf
    implicit none

    integer,parameter :: NTIL=667010
    
    
    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points 
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:)                   :: CVAR,NVAR,PVAR,VAR   ! 1D array of stuff
  
    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 1
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )

    !Read dimensions.
    !call check( nf90_inq_dimid(cncid, 'SLMASK', nDimensions) )
    !call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,"*** TILES NUMBER IS",NTIL

    allocate(VAR(NTIL),CVAR(NTIL),NVAR(NTIL),PVAR(NTIL))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    ! rescaling
    print *,'resl',resl
    print *,'sign_of_bv',sign_of_bv
    print *,'CVAR',CVAR(180),minval(CVAR),maxval(CVAR)
    print *,'NVAR',NVAR(180),minval(NVAR),maxval(NVAR)
    print *,'PVAR',PVAR(180),minval(PVAR),maxval(PVAR)

    VAR = CVAR + sign_of_bv*(PVAR-NVAR)*resl*0.5
    print *,'VAR=',VAR(180),minval(VAR),maxval(VAR)
    print *,'CVAR',CVAR(180),minval(CVAR),maxval(CVAR)
    print *,'NVAR',NVAR(180),minval(NVAR),maxval(NVAR)
    print *,'PVAR',PVAR(180),minval(PVAR),maxval(PVAR)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading neg pert.file ", NFILE_NAME
    print *,"*** SUCCESS reading pos pert.file ", PFILE_NAME

    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_1D

end program perturb_aogcm

