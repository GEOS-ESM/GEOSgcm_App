module ocean_recenter_utils
  implicit none

  type ModelGrid
     character (len = 400)              :: fname
     integer                            :: Nx, Ny, Nz, Ncat
     real, dimension(:,:), allocatable  :: x, y, wet, depth  
     real, dimension(:), allocatable    :: z  
  end type ModelGrid

  type obs_space
     integer                            :: No
     real, dimension(:), allocatable    :: lon, lat, HXe, Yo
  end type obs_space
contains

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

  !-----------------------
  subroutine init_grid(grid)
    !-----------------------
    use netcdf
        
    type (ModelGrid), intent(inout)        :: grid
    integer                                :: varid, ncid
    integer                                :: Nx, Ny, Nz, Ncat
    character (len = 400)                  :: grid_fname, lon_name, lat_name, lev_name, mask_name, topo_name
    namelist /recenter_grid_size/Nx, Ny, Nz, Ncat, grid_fname, lon_name, lat_name, lev_name, mask_name, topo_name

    open(7,file='recenter.nml')
    read(7,NML=recenter_grid_size)
    close(7)

    grid%Nx=Nx
    grid%Ny=Ny
    grid%Nz=Nz
    grid%Ncat=Ncat
    grid%fname=trim(grid_fname)


    allocate( grid%x(grid%Nx, grid%Ny), grid%y(grid%Nx, grid%Ny), grid%wet(grid%Nx, grid%Ny), grid%depth(grid%Nx, grid%Ny), grid%z(grid%Nz) )

    print *,'grid%fname:',grid%fname

    call check(nf90_open(grid%fname, nf90_nowrite,ncid))
    call check(nf90_inq_varid(ncid,trim(lon_name),varid))
    call check(nf90_get_var(ncid,varid,grid%x))
    call check(nf90_inq_varid(ncid,trim(lat_name),varid))
    call check(nf90_get_var(ncid,varid,grid%y))
    call check(nf90_close(ncid))

  end subroutine init_grid

  !-----------------------
  subroutine get_Xe(Xe,myrank,yyyymmdd,Nx,Ny,num_levels,Xe_path, X_varname, X_varshape)
    !-----------------------

    use netcdf

   implicit none
    !integer, parameter :: r_size=kind(0.0)
    !real, allocatable, dimension(:,:,:), intent(out)  :: Xe     !Allocated before call
    real, allocatable, dimension(:), intent(inout)  :: Xe     !Allocated before call
    real*8, allocatable, dimension(:,:,:)  :: Xe_d     !Allocated before call
    integer, intent(in)                               :: myrank
    character (len=8), intent(in)                     :: yyyymmdd
    character (len = 400), intent(in)                 :: Xe_path
    character (len = 400), intent(in)                 :: X_varname, X_varshape
    !real, allocatable, dimension(:,:,:,:)           :: VAR
    integer                                           :: varid
    integer                                           :: ncid
    character (len = 400)                             :: fname
    integer                                           :: Nx, Ny, Nz, Ncat, num_levels, lev_index
    integer,parameter :: single = selected_real_kind(p=6,r=37)
    
    !select case(X_varname)
    !case('T','S')
    !   print *,'var shape:',Nx, Ny, 40,1
    !   allocate(VAR(Nx, Ny, num_levels,1))
    !case default
    !   print *,'var shape:',Nx, Ny, num_levels,1
    !   allocate(VAR(Nx, Ny, num_levels,1))
    !end select
    !allocate(VAR(Nx, Ny, num_levels,1))
    !allocate(Xe(Nx, Ny, num_levels),Xe_d(Nx, Ny, num_levels))
    !allocate(Xe(1:Nx*Ny*num_levels),Xe_d(1:Nx, 1:Ny, 1:num_levels))
    allocate(Xe_d(1:Nx, 1:Ny, 1:num_levels))

    fname=trim(Xe_path)//'state.'
    fname=trim(fname)//int2str(myrank+1)
    fname=trim(fname)//'.'
    fname=trim(fname)//yyyymmdd
    fname=trim(fname)//'.nc'

    !print *,'cpu#', myrank, fname, shape(Xe), fname
    !print *,Nx,Ny,num_levels

    call check(nf90_open(fname,nf90_nowrite,ncid))
    call check(nf90_inq_varid(ncid,X_varname,varid))
    call check(nf90_get_var(ncid,varid,Xe_d))
    Xe=reshape(real(Xe_d),(/Nx*Ny*num_levels/))
    call check(nf90_close(ncid))

    !Xe(:,:,1)=VAR(:,:,1)
    !Xe=VAR(:,:,:,1)

    !print *,'cpu#',myrank,' getting out of get_Xe'
    
    !do lev_index = 1, num_levels
    !   Xe(:,:,lev_index)=VAR(:,:,lev_index)
    !end do
    deallocate(Xe_d)

  end subroutine get_Xe     

  function int2str(int) result(str)

    implicit none
    integer                              :: int
    character (len = 200)                :: str

    open(572,status='scratch')
    write(572,*)int
    rewind(572)
    read(572,*)str
    close(572)

  end function int2str

end module ocean_recenter_utils
