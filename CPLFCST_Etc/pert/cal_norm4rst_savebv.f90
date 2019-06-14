 !------------------------------------------
program cal_norm4rst_savebv
  
  use netcdf
  implicit none

  !------------------------------------------
  
  integer                             :: nx, ny, nz, ncid, dimid
  real*8,allocatable :: lon(:,:),lat(:,:),dep(:),area(:,:), wet(:,:), kmt(:,:)
  real  ,allocatable :: bv(:,:,:)
  real  ,allocatable :: tot_dep(:,:)
  real :: norm, tot_area
  real                                :: longvar,pct,res_coef
  character*100                       :: varname
  integer                             :: i,j,k
  real                                :: avgz,slon,elon,slat,elat
  
  call check(nf90_open("grid_spec.nc",NF90_NOWRITE,ncid))

  call check( nf90_inq_dimid(ncid, 'grid_x_T', dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len=nx))

  call check( nf90_inq_dimid(ncid, 'grid_y_T', dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len=ny))

  call check( nf90_inq_dimid(ncid, 'zt', dimid) )
  call check( nf90_inquire_dimension(ncid, dimid, len=nz))

  call check(nf90_close(ncid))

  print*, "Dimentions ",nx,ny,nz

  namelist /variable_nml/  varname
  namelist /region_nml/    avgz   , slon   , elon     , slat    , elat
  namelist /recl_nml/      longvar, pct
  
  open (99, file='bv.nml')
  read (99, nml=variable_nml)
  read (99, nml=region_nml)   
  read (99, nml=recl_nml)   
  
  close(99)

  print*, 'Variable: ', varname

  allocate(bv(nx,ny,nz),tot_dep(nx,ny))
  allocate(lon(nx,ny),lat(nx,ny),dep(nz),area(nx,ny),wet(nx,ny),kmt(nx,ny))

  ! Read dimentions and data here
  call read_grid(lon,lat,dep,area,wet,kmt)
  call read_bv4rst(varname, bv)

  ! Integrate vertically
  tot_dep=0.0
  do j=1,ny
     do i=1,nx
        do k=1,kmt(i,j)
           if (dep(k) .le. avgz) then
              tot_dep(i,j)=dep(k)
           end if           
        end do
        bv(i,j,1)=bv(i,j,1)*dep(1)
        do k=2,kmt(i,j)
           if (dep(k) .le. avgz) then
              bv(i,j,1)=bv(i,j,1)+bv(i,j,k)*(dep(k)-dep(k-1))
           end if
        end do
     end do
  end do

  where(wet .eq. 1.d0)
     bv(:,:,1)=bv(:,:,1)/tot_dep
  end where

  ! Save BV to a file
  !open (98, file='bv4rst.dat')
  !write(98,*) wet
  !write(98,*) bv
  !close(98)

  ! Integrate over area
  area=area*wet
  bv(:,:,1)=area*bv(:,:,1)**2
  norm=0.; tot_area=0.0
  do j=1,ny
     do i=1,nx
        if ( (lon(i,j) .ge. slon) .and. (lon(i,j) .le. elon) &
             .and. (lat(i,j) .ge. slat) .and. (lat(i,j) .le. elat)) then
           norm=norm+bv(i,j,1)
           tot_area=tot_area+area(i,j)
        end if
     end do
  end do
  
  norm=sqrt(norm/tot_area)

  print*, 'Norm: ', norm

  res_coef=longvar*pct/norm

  print*, 'Rescaling factor: ', res_coef

  deallocate(bv,tot_dep)
  deallocate(lon,lat,dep,area,wet)
  
   open (99, file='coef.dat')
   write(99,*) res_coef
   close(99)
contains
  
  subroutine read_grid(lon,lat,dep,area,wet,kmt)
    real*8, intent(inout) :: lon(:,:), lat(:,:), dep(:), area(:,:), wet(:,:),kmt(:,:)
    integer :: varid,ncid

    call check(nf90_open("grid_spec.nc",NF90_NOWRITE,ncid))

    call check(nf90_inq_varid(ncid,'x_T',varid))
    call check(nf90_get_var(ncid,varid,lon))
    
    call check(nf90_inq_varid(ncid,'y_T',varid))
    call check(nf90_get_var(ncid,varid,lat))

    call check(nf90_inq_varid(ncid,'zb',varid))
    call check(nf90_get_var(ncid,varid,dep))

    call check(nf90_inq_varid(ncid,'area_T',varid))
    call check(nf90_get_var(ncid,varid,area))
    
    call check(nf90_inq_varid(ncid,'wet',varid))
    call check(nf90_get_var(ncid,varid,wet))

    call check(nf90_inq_varid(ncid,'num_levels',varid))
    call check(nf90_get_var(ncid,varid,kmt))

    call check(nf90_close(ncid))

  end subroutine read_grid

  subroutine read_bv4rst(varname,bv)
     character*100 :: varname
     real  ,intent(inout) :: bv(:,:,:)
     real  ,allocatable :: tmp(:,:,:,:)
     integer :: ncid,varid, nx,ny,nz, sh(3)

     sh=shape(bv); nx=sh(1); ny=sh(2); nz=sh(3)

     allocate(tmp(nx,ny,nz,1))

     bv=0.0

     call check(nf90_open("exp1.nc",NF90_NOWRITE,ncid))
     call check(nf90_inq_varid(ncid,varname,varid))
     call check(nf90_get_var(ncid,varid,tmp))
     call check(nf90_close(ncid))

     bv=tmp(:,:,:,1)

     call check(nf90_open("exp2.nc",NF90_NOWRITE,ncid))
     call check(nf90_inq_varid(ncid,varname,varid))
     call check(nf90_get_var(ncid,varid,tmp))
     call check(nf90_close(ncid))

     bv=(bv-tmp(:,:,:,1))*0.5

     deallocate(tmp)
  end subroutine read_bv4rst

  subroutine read_bv(varname,bv)
     character*100 :: varname
     real,intent(inout) :: bv(:,:,:)
     real*4, allocatable :: tmp(:,:,:,:)
     integer :: ncid,varid, nx,ny,nz, sh(3)

     sh=shape(bv); nx=sh(1); ny=sh(2); nz=sh(3)

     allocate(tmp(nx,ny,nz,1))

     bv=0.0

     call check(nf90_open("exp1.nc4",NF90_NOWRITE,ncid))
     call check(nf90_inq_varid(ncid,varname,varid))
     call check(nf90_get_var(ncid,varid,tmp))
     call check(nf90_close(ncid))

     bv=tmp(:,:,:,1)

     call check(nf90_open("exp2.nc4",NF90_NOWRITE,ncid))
     call check(nf90_inq_varid(ncid,varname,varid))
     call check(nf90_get_var(ncid,varid,tmp))
     call check(nf90_close(ncid))

     bv=(bv-tmp(:,:,:,1))*0.5

     deallocate(tmp)
  end subroutine read_bv
  
  
  SUBROUTINE check(STATUS)
    INTEGER,intent(in) :: STATUS
    IF (STATUS .NE. NF90_NOERR) THEN
       PRINT *, NF90_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF
  END SUBROUTINE check
end program cal_norm4rst_savebv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
