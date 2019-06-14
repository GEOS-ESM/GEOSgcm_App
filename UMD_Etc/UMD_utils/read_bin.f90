!
! to create a python usable module:
! f2py -c -m <module_name> <file_name>.f90--->	f2py -c -m read_merra2_bcs read_bin.f90
! to create a signature file:
! f2py -h <module_name>.pyf <file_name>.f90-->  f2py -h read_merra2_bcs.pyf read_bin.f90
! .....................................................................
   SUBROUTINE read_bin_data( fileName, nymd_in, nymd_out, NLON, NLAT, LON, LAT, bcs_field)

   IMPLICIT NONE

   CHARACTER (LEN = 200), INTENT(IN)  :: fileName
   INTEGER,               INTENT(IN)  :: nymd_in

   INTEGER,               INTENT(OUT) :: nymd_out
   INTEGER,               INTENT(OUT) :: NLON
   INTEGER,               INTENT(OUT) :: NLAT

   REAL,                  INTENT(OUT) :: LON(1440)
   REAL,                  INTENT(OUT) :: LAT(720)
   REAL,                  INTENT(OUT) :: bcs_field(720, 1440)

! LOCAL VARS
   real    year1,month1,day1,hour1,min1,sec1
   real    year2,month2,day2,hour2,min2,sec2
   real    dum1,dum2
   real    dLat,dLon
   real    field(1440,720)

   integer nymd1,nhms1
   integer nymd2,nhms2
   
   integer rc, ix
!  ....................................................................

!     print *, 'nymd_in=', nymd_in

      open (10,file=fileName,form='unformatted',access='sequential', STATUS = 'old')
!     ....................................................................
      rc = 0
      do while (rc.eq.0)
         ! read header
         read (10,iostat=rc)  year1,month1,day1,hour1,min1,sec1,                            &
                              year2,month2,day2,hour2,min2,sec2,dum1,dum2
         if( rc.eq.0 ) then
           read (10,iostat=rc)  field

           NLON  = nint(dum1)
           NLAT  = nint(dum2)
           IF( (NLON /= 1440) .or. (NLAT /= 720)) THEN
              PRINT *, 'ERROR in LAT/LON dimension in file: ', fileName, 'NLON=', NLON, 'NLAT=', NLAT
              EXIT
           END IF
           dLat = 180./NLAT
           dLon = 360./NLON

           ! start date
           nymd1 = nint( year1*10000 )  + nint (month1*100)  +  nint( day1 )
           nhms1 = nint( hour1*10000 )  + nint (  min1*100)  +  nint( sec1 )

           ! end date
           nymd2 = nint( year2*10000 )  + nint (month2*100)  +  nint( day2 )
           nhms2 = nint( hour2*10000 )  + nint (  min2*100)  +  nint( sec2 )
         end if

         if (nymd1 .eq. nymd_in) then
            nymd_out = nymd1

!           print *, 'nymd_out=', nymd_out
!!          print *, bcs_field

            LAT(1) = -90. + dLat/2.
            DO ix = 2, NLAT
              LAT(ix) = LAT(ix-1) + dLat
            END DO

            LON(1) = -180. + dLon/2.
            DO ix = 2, NLON
              LON(ix) = LON(ix-1) + dLon
            END DO

            bcs_field = TRANSPOSE(field)
            close(10)
            RETURN 
         end if
      end do 
!     ....................................................................
      close(10)

!     print *, year1,month1,day1,hour1,min1,sec1 
!     print *, year2,month2,day2,hour2,min2,sec2,dum1,dum2
!     print *, 'rc = ', rc             
!---------------------------------------------------------------------------
    END SUBROUTINE read_bin_data
! .....................................................................
!

