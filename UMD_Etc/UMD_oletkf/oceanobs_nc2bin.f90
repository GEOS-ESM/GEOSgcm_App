PROGRAM oceanobs_nc2bin
  !===============================================================================
  ! This program converts netcdf profiles of temperature and salinity to 
  ! a format readable by letkf. Eventually, the obsop.f90 should read these
  ! in directly.
  !
  ! Observation errors are computed here using Dave Behringer's technique based
  ! on the temperature gradients, or read from an observation error file
  ! (e.g. when doing adaptive obs error)
  !
  ! Either the in situ temperatures will have to be converted to potential temperature,
  ! or the obs operator will have to transform the model potential temperature to
  ! in situ equivalent. The latter will be easier because there is no guarantee
  ! that temperature and salinity are observed simultaneously.
  !
  ! Example: ./gmao2ncep.x -y 2013 -m 01 -d 01 -indir1 ../utils/gmao-obs- -outdir .
  !===============================================================================
  use netcdf
  USE common,       ONLY: r_sngl, r_size, slen
  USE params_model, ONLY: nlev
  USE params_obs,   ONLY: id_t_obs, id_s_obs

  IMPLICIT NONE

  INTEGER :: nobs, nobs0
  INTEGER :: year,month,day,hour
  INTEGER :: start_year, end_year
  INTEGER :: start_day, end_day
  INTEGER :: start_month, end_month
  INTEGER :: start_hour, end_hour
  CHARACTER(4) :: YYYY
  CHARACTER(2) :: MM, DD, HH
  CHARACTER(slen) :: indir, outdir, indir1, indir2
  CHARACTER(3) :: obtype = 'tmp' !tmp, sal, alt

  INTEGER :: i,j,k,n
  CHARACTER(slen) :: infile, outfile
  INTEGER :: typ
  !REAL(r_size) :: wk(6)
  REAL(r_sngl) :: wk(6)
  INTEGER, PARAMETER :: fid = 90

  TYPE OceanObs
     integer, allocatable, dimension(:)       :: typ
     real(r_size), allocatable, dimension(:)  :: lon
     real(r_size), allocatable, dimension(:)  :: lat
     real(r_size), allocatable, dimension(:)  :: depth
     real(r_size), allocatable, dimension(:)  :: value
     real(r_size), allocatable, dimension(:)  :: oerr
  END TYPE OceanObs

  type (OceanObs)                             :: obs_data
  integer                                     :: varid
  integer                                     :: ncid, nodimid

  CALL process_command_line

  ! Read temp data
  !infile = '20110101.tmpa.mom' !STEVE: sample
  infile = trim(indir1)//YYYY//MM//DD//'.nc' !STEVE: sample
  print *,infile

  nobs = 0
  call check(nf90_open(infile,nf90_nowrite,ncid))
  call check(nf90_inq_dimid(ncid,'nobs',NoDimID))
  call check(nf90_inquire_dimension(ncid, NoDimID, len = nobs))
  allocate(obs_data%typ(nobs), obs_data%lon(nobs), obs_data%lat(nobs), obs_data%depth(nobs), obs_data%value(nobs), obs_data%oerr(nobs))    

  print *,'nobs=',nobs

  call check(nf90_inq_varid(ncid,'typ',varid))
  call check(nf90_get_var(ncid,varid,obs_data%typ))

  call check(nf90_inq_varid(ncid,'lon',varid))
  call check(nf90_get_var(ncid,varid,obs_data%lon))

  call check(nf90_inq_varid(ncid,'lat',varid))
  call check(nf90_get_var(ncid,varid,obs_data%lat))
  
  call check(nf90_inq_varid(ncid,'depth',varid))
  call check(nf90_get_var(ncid,varid,obs_data%depth))

  call check(nf90_inq_varid(ncid,'value',varid))
  call check(nf90_get_var(ncid,varid,obs_data%value))

  call check(nf90_inq_varid(ncid,'oerr',varid))
  call check(nf90_get_var(ncid,varid,obs_data%oerr))

  call check(nf90_close(ncid))

  outfile = trim(outdir)//'/'//YYYY//MM//DD//'.dat' !STEVE: sample
  OPEN(fid,FILE=outfile,FORM='unformatted',ACCESS='sequential')

  ! Write letkf file
  do i=1,nobs
     !STEVE: the following are required for miyoshi's letkf observation input:
     !1 = obelm
     !2 = lon
     !3 = lat
     !4 = lev
     !5 = value
     !6 = oberr
     
     wk(1) = obs_data%typ(i)
     wk(2) = obs_data%lon(i)
     wk(3) = obs_data%lat(i)
     wk(4) = obs_data%depth(i)
     wk(5) = obs_data%value(i)
     wk(6) = obs_data%oerr(i)

     WRITE(fid) wk
  enddo

  ! Close letkf file
  CLOSE(fid)

CONTAINS

  !-----------------------
  subroutine check(status)
    !-----------------------
    use netcdf

    integer, intent (in) :: status

    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
    
  end subroutine check

  SUBROUTINE process_command_line
    !===============================================================================
    ! Proces the command line arguments
    !===============================================================================

    IMPLICIT NONE

    CHARACTER(slen) :: arg1,arg2
    INTEGER :: ierr
    INTEGER, DIMENSION(3) :: values

    ! STEVE: add input error handling!^M
    ! inputs are in the format "-x xxx"^M
    DO i=1,COMMAND_ARGUMENT_COUNT(),2
       CALL GET_COMMAND_ARGUMENT(i,arg1)
       PRINT *, "Argument ", i, " = ",TRIM(arg1)

       select case (arg1)
       case ('-ob')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          obtype = arg2
          print *, "obtype = ", obtype
       case ('-y')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) year
          YYYY = arg2
          print *, "year = ", arg2
       case('-m')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) month
          MM = arg2
          print *, "month = ", arg2
       case('-d')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) day
          DD = arg2
          print *, "day = ", arg2
       case('-h')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) hour
          HH = arg2
          print *, "hour = ", arg2
       case ('-sy')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) start_year
          print *, "start_year = ", start_year
       case('-sm')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) start_month
          print *, "start_month = ", start_month
       case('-sd')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) start_day
          print *, "start_day = ", start_day
       case('-sh')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) start_hour
          print *, "start_hour = ", start_hour
       case('-ey')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) end_year
          print *, "end_year = ", end_year
       case('-em')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) end_month
          print *, "end_month = ", end_month
       case('-ed')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) end_day
          print *, "end_day = ", end_day
       case('-eh')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          READ(arg2,*) end_hour
          print *, "end_hour = ", end_hour
       case('-indir')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          indir1 = trim(arg2)
       case('-indir1')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          indir1 = trim(arg2)
       case('-indir2')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          indir2 = trim(arg2)
       case('-outdir')
          CALL GET_COMMAND_ARGUMENT(i+1,arg2)
          PRINT *, "Argument ", i+1, " = ",TRIM(arg2)
          outdir = trim(arg2)
       end select
    END DO

  END SUBROUTINE process_command_line

END PROGRAM oceanobs_nc2bin
