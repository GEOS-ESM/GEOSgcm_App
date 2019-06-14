program mk_iau_increments
!!$ /////////////////////////////////////////////////////////////////////////
!!$ /**
!!$ * Title: mk_iau_incr
!!$ * ARGUMENT:
!!$ *        -DO_ANA_INCR                     : compute iau increment and sst relaxation
!!$ *        -DO_SPONGE dt_restore_sst        : nudge to sst with restoring time scale hh hours
!!$ *
!!$ * INPUT FILES:
!!$ *        MOM's namelist      : input.nml  
!!$ *        Model's native grid : grid_spec.nc
!!$ *        Background          : ocean_temp_salt.res.nc
!!$ *        Analysis            : ANA-ocean_temp_salt.res.nc
!!$ *        Mass increment      : seaice_mass_incr.txt
!!$ * OUTPUT
!!$ *        Temp-Salt restarts for MOM's IAU : temp_increment.nc, salt_increment.nc, eta_increment.nc
!!$ *
!!$ * Stuff to do: Save the date of the increment in the file, pass the date as argument ...

  use netcdf
  use ocnice_utils

  implicit none

  integer, parameter :: real_kind = selected_real_kind(6)
  !integer :: i, j, n, k
  character*1028 :: bkg_fname
  character*1028 :: ana_fname
  character*1028 :: incr_fname
  character*1028 :: outfile

  character*128 :: varname

  real*4, allocatable, dimension(:,  :, :) ::   Tb, Sb         ! Background Temp, Salt 
  real*4, allocatable, dimension(:,  :, :) ::   Ta, Sa         ! Analysis Temp, Salt
  real*4, allocatable, dimension(:,  :, :) ::   Tincr, Sincr   ! Increment Temp, Salt
  type (ModelGrid)                         :: ogrid            ! Ocean grid

  real*4    :: Sincr_max = 5.0 ! Max absolute value for the salinity increment  
  real*4    :: Tincr_max = 5.0 ! Max absolute value for the temperature increment  
  real*4    :: mass_incr       ! Mass increment from sea-ice assimilation
  

  logical       :: DO_ANA_INCR = .false.
  logical       :: DO_MASS_INCR = .false.
  logical       :: DO_SPONGE = .false.
  real*4        :: dt_restore_sst
  integer       :: Nargs, arg_index
  character*300 :: arg1, arg2

  !PROCESS COMMAND LINE
  !====================
  Nargs = iargc()
  do arg_index = 1, Nargs
     call get_command_argument(arg_index, arg1)
     select case(arg1)
     case('-DO_ANA_INCR')
        DO_ANA_INCR = .true.
     case('-DO_MASS_INCR')
        print *,'adding mass incr to ocean ...'
        DO_MASS_INCR = .true.
     case('-DO_SPONGE')
        DO_SPONGE = .true.
        call get_command_argument(arg_index+1, arg2)
        read(arg2,*)dt_restore_sst
     end select
  enddo
  print *,DO_ANA_INCR

  !OCEAN GRID
  !==========
  call read_grid(ogrid)

  if (DO_ANA_INCR) then
     !READ BACKGROUND
     !===============
     bkg_fname='ocean_temp_salt.res.nc'
     allocate(Tb(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     varname = 'temp'
     call readoceanrst(bkg_fname, varname, ogrid, Tb)
     allocate(Sb(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     varname = 'salt'
     call readoceanrst(bkg_fname, varname, ogrid, Sb)

     !READ ANALYSIS
     !===============
     ana_fname='ANA-ocean_temp_salt.res.nc'
     allocate(Ta(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     varname = 'temp'
     call readoceanrst(ana_fname, varname, ogrid, Ta)
     allocate(Sa(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     varname = 'salt'
     call readoceanrst(ana_fname, varname, ogrid, Sa)

     !CHECK OCEAN TEMP SALT STATE
     !===========================
     call clean_ocean_state(ogrid, Tb, Sb)
     call clean_ocean_state(ogrid, Ta, Sa)

     !INCREMENT
     !=========
     allocate(Tincr(ogrid%Nx, ogrid%Ny, ogrid%Nz))
     allocate(Sincr(ogrid%Nx, ogrid%Ny, ogrid%Nz))

     Tincr = Ta - Tb
     Tincr = Tincr_max * tanh( Tincr/Tincr_max ) 

     Sincr = Sa - Sb
     Sincr = Sincr_max * tanh( Sincr/Sincr_max )

     print *,'Saving temp-salt increment restart'
     outfile='temp_increment.nc'
     varname='temp'
     call write_mom_increment(outfile, varname, Tincr, ogrid)  
     
     outfile='salt_increment.nc'
     varname='salt'
     call write_mom_increment(outfile, varname, Sincr, ogrid)
  end if

  if (DO_MASS_INCR) then
     open(10, file='seaice_mass_incr.txt')
     read(10, fmt=*) mass_incr
     close(10)
     call write_mass_increment(mass_incr, ogrid)
  end if

  if (DO_SPONGE) then
     print *,'Saving temp_salt sponge'
     outfile='temp_salt_sponge.nc'
     varname='temp'
     call write_mom_sponge(outfile, varname, ogrid, dt_restore_sst)  
  end if

end program mk_iau_increments
