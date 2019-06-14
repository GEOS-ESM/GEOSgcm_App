program anaice2rst 
!!$ /////////////////////////////////////////////////////////////////////////
!!$ /**
!!$ * Title: anaice2rst.f90
!!$ *
!!$ * Description: Based on Bin Zhao's code that converts PIOMASS 12 itd into CICE 5 itd. 
!!$ *              The core philosophy is to keep the background itd and conserve the Enthalpy per unit volume.
!!$ *     
!!$ * Options:  
!!$ *              -rst     : File name for saltwater restart. 
!!$ *                         Default: Salt_fname  = 'saltwater_internal_rst.nc4' 
!!$ *              -orst    : File name for ocean temp salt restart. 
!!$ *                         Default: Ocean_fname  = 'ocean_temp_salt.res.nc' 
!!$ *              -ana     : File name containing analysis, or sea-ice state on ocean grid.
!!$ *                         variables in the file should be AICE and/or HICE and/or HSNO on the ocean native grid.
!!$ *                         Default: Ana_fname  = 'ana.nc4'    
!!$ *              -ana_fmt : Format of Ana_fname, 'oletkf' or 'regular'
!!$ *                         Default: ana_fmt='regular'
!!$ *              -do_aice : DO_AICE_ANA, Read AICE, .true or .false.  !!!!! Assumes analysis file contains logit(AICE)
!!$ *              -do_hice : DO_HICE_ANA,   "  HICE,   "
!!$ *              -do_hsno : DO_HSNO_ANA,   "  HSNO,   "
!!$ *              -do_all_1st : DO_ALL_1ST, .true or .false. !! apply increment starting from 1st category; Default(.false.) 
!!$ *               NOTE: if true, this scheme creates some biases because ice may not be present in the 1st category when removed  
!!$ *              -hi_new : real, new ice thickness when DO_ALL_1ST is .true.; 
!!$ *                        Default(0.5m)  
!!$ *              
!!$ * Output:              
!!$ *              saltwater_internal_rst_OUT.nc4
!!$ * Example:             
!!$ *             
!!$ *             saltwater_internal_rst_OUT is a copy of saltwater_internal_rst
!!$ *             ./anaice2rst.x saltwater_internal_rst_OUT saltwater_internal_rst ana-20030407.nc .T. .F. .F.
!!$ *                 will read AICE and rebalance CICE's restart, result written in saltwater_internal_rst_OUT
!!$ *

  use netcdf
  use ocnice_utils

  implicit none

  integer, parameter :: real_kind = selected_real_kind(6)
  integer, parameter :: dbl_kind = selected_real_kind(13)
  integer :: i, j, n, nt, k, argc, fid_in, arg_index, Nargs
  character*1028 :: Salt_fname  = 'saltwater_internal_rst.nc4'
  character*1028 :: Ocean_fname  = 'ocean_temp_salt.res.nc'
  character*1028 :: Ana_fname  = 'ana.nc4'
  character*1028 :: Outfile  = 'saltwater_internal_rst_OUT.nc4'
  character*1028 :: OceanOutfile  = 'ocean_temp_salt.res.OUT.nc'
  character*400  :: tilefile_fname  = 'tile.data'
  logical       :: DO_AICE_ANA = .true.
  logical       :: DO_HICE_ANA = .true.
  logical       :: DO_HSNO_ANA = .true.
  logical       :: DO_SKIN_FLUSH = .false.
  logical       :: DO_ALL_1ST  = .false.
  logical       :: DO_ADHOC_TW = .false. 
  logical       :: DO_CONS_VOL = .false. 
  logical       :: oob
  character*128 :: ana_fmt = 'regular' ! 'oletkf' or 'regular'

  character*128 :: fname
  integer :: varid, varidvi, varidlon, varidlat
  character*128 :: varname

  real*8, allocatable, dimension(:)    :: tmpx 
  real*8, allocatable, dimension(:, :) :: aicen, vicen ,vsnon
  real*4, allocatable, dimension(:,:)    :: Tw  ! Skin temperature (under ice) [K]
  real*4, allocatable, dimension(:,:)    :: Sw  ! Skin salinity (under ice)    [psu]
  real*4, allocatable, dimension(:,:)    :: Slmask  

  real*8, allocatable, dimension(:, :, :) ::   eicen    ! energy of melting for each ice layer  (J/m^2)
  real*8, allocatable, dimension(:, :, :) ::   esnon    ! energy of melting for each snow layer (J/m^2)

  real*4, allocatable, dimension(:,  :)    :: sic_ana, sit_ana, snd_ana
  real*8, allocatable, dimension(:,  :)    :: sst_incr       ! Re-analyzed sst increment
  real*4, allocatable, dimension(:,  :, :) :: Tocean      ! 3d ocean potential temperature [C]
  real*4, allocatable, dimension(:,  :, :) :: Socean      ! 3d ocean salinity [PSU]
  real*4, allocatable, dimension(:,  :, : , :)    :: DUMVAR

  real*4, allocatable, dimension(:) :: madjice 
  real*4, allocatable, dimension(:, :) :: madjiceij, Toij


  real*4            :: beta                    ! Scaling of the background field 
  real*4            :: mass_incr               ! Sea-ice mass increment 
  real*4            :: sic_bkg                 ! Background ice Fraction
  real*4            :: sit_bkg                 ! Background ice thickness
  real*4            :: snd_bkg                 ! Background snow depth
  real*8            :: sic_incr                ! Ice Fraction increment
  real*4            :: sic_incr_max = 0.9      ! Max ice Fraction increment
  real*4            :: Tliquid                 ! Background Temperature of liquid ocean 
  real*4            :: Tf                      ! Freeze temp
  real*4            :: Tincr                   ! Tw increment
  real*4, parameter :: Tincr_max = 1.0         ! Max Tw increment
  real*4, parameter :: rho_ice = 917.0         ! Sea-ice density [kg/m3]
  real*4, parameter :: rho_sno = 330.0         ! Snow density [kg/m3]
  real*4, parameter :: tiny = 1.0e-7           !
  real*4, parameter :: puny = 1.0e-11          !
  real*4, parameter :: beta_max = 2.0         !
  real*4, parameter :: rho_lwe = 1003.091    ! Surface water density. S=4 psu, T=-0.216 C (freeze point) [kg/m3]


  real (kind=dbl_kind), parameter :: &
         p5   = 0.5_dbl_kind, &
         c0   = 0.0_dbl_kind, &
         c1   = 1.0_dbl_kind, &
         c1p5 = 1.5_dbl_kind, &
         c2   = 2.0_dbl_kind, &
         c3   = 3.0_dbl_kind, &
         c15  = 15.0_dbl_kind, &
         pi = 3.14159265358979323846_dbl_kind ! pi

  real(kind=dbl_kind), parameter  :: &
                         rhoi      = 917.0_dbl_kind , &
                         rhow      = 1026.0_dbl_kind, &
                         rhos      = 330.0_dbl_kind , &
                         cp_ice    = 2106._dbl_kind   ,&! specific heat of fresh ice (J/kg/K)
                         cp_ocn    = 4218._dbl_kind  ,& ! specific heat of ocn    (J/kg/K)
                         depressT  = 0.054_dbl_kind   ,&! Tf:brine salinity ratio (C/ppt)
                         saltmax   = 3.2_dbl_kind,  & ! max salinity at ice base (ppt)
                         nsal      = 0.407_dbl_kind, &
                         msal      = 0.573_dbl_kind, &
                         Lsub      = 2.835e6_dbl_kind ,&! latent heat, sublimation freshwater (J/kg)
                         Lvap      = 2.501e6_dbl_kind ,&! latent heat, vaporization freshwater (J/kg)
                         Lfresh    = Lsub-Lvap        ! latent heat of melting of fresh ice (J/kg)




  character*128        :: Usage="Example: anaice2rst.x saltwater_internal_rst_OUT saltwater_internal_rst_IN ANALYSIS.nc .T. .T. .T."
  integer, parameter   :: ncat = 5
  integer, parameter   :: nilyr = 4
  integer, parameter   :: nslyr = 1
  integer, allocatable :: GIs(:), GJs(:)
  integer              :: targt = 0
  integer              :: tilemax

  real*4               :: hinew = 0.5             ! new ice thickness generated
  real*8               :: hi, hs, qi0, incr_remain 
  real*4               :: mass1, mass2 
  real*8               :: qin_save(nilyr), qsn_save(nslyr)
  real (kind=real_kind), dimension(nilyr+1) :: &
                       salin       , & ! salinity (ppt)   
                       Tmlt            ! melting temp, -depressT * salinity
                         ! nilyr + 1 index is for bottom surface
  real (kind=dbl_kind)    :: zn       ! normalized ice thickness
  real (kind=dbl_kind)    :: cc1, cc2, cc3, x1, rncat, hin
  real (kind=dbl_kind) :: &
         hin_max(0:ncat) ! category limits (m)
  real (kind=dbl_kind) :: &
         hsn

  integer ::  ilyr1 (ncat), & ! array position of top ice layer in each cat
              ilyrn (ncat), & ! array position of bottom ice layer in each cat
              slyr1 (ncat), & ! array position of top snow layer in each cat
              slyrn (ncat)    ! array position of bottom snow layer in each cat


  character*300                          :: buffer, command, arg1, arg2
  integer :: ncid, dimids2d(2), cat_dimid, catp1_dimid, tile_dimid, dimids1d(1)

  type (ModelGrid)                      :: ogrid     ! Ocean grid, tripolar
  type (OceanRaster)                    :: oraster   ! Ocean tiles

  !PROCESS COMMAND LINE
  !====================
  Nargs = iargc()
  do arg_index = 1, Nargs, 2
     call get_command_argument(arg_index, arg1)
     select case(arg1)
     case('-rst')
        call get_command_argument(arg_index+1, arg2)
        Salt_fname = arg2
     case('-orst')
        call get_command_argument(arg_index+1, arg2)
        Ocean_fname = arg2
     case('-ana')
        call get_command_argument(arg_index+1, arg2)
        Ana_fname = arg2
     case('-tilefile')
        call get_command_argument(arg_index+1, arg2)
        tilefile_fname = arg2
     case('-do_aice')
        call get_command_argument(arg_index+1, arg2)
        DO_AICE_ANA = str2bool(arg2)
     case('-do_hice')
        call get_command_argument(arg_index+1, arg2)
        DO_HICE_ANA = str2bool(arg2)
     case('-do_hsno')
        call get_command_argument(arg_index+1, arg2)
        DO_HSNO_ANA = str2bool(arg2)
     case('-do_all_1st')
        call get_command_argument(arg_index+1, arg2)
        DO_ALL_1ST = str2bool(arg2)
     case('-do_skin_flush')
        call get_command_argument(arg_index+1, arg2)
        DO_SKIN_FLUSH = str2bool(arg2)
     case('-do_conserve_volume')
        call get_command_argument(arg_index+1, arg2)
        DO_CONS_VOL = str2bool(arg2)
     case('-hi_new')
        call get_command_argument(arg_index+1, arg2)
        read(arg2, *) hinew
     case('-target_tile')
        call get_command_argument(arg_index+1, arg2)
        read(arg2, *) targt
     case('-do_adhoc_tw_adjustment')
        call get_command_argument(arg_index+1, arg2)
        DO_ADHOC_TW = str2bool(arg2)
     case('-ana_fmt')
        call get_command_argument(arg_index+1, arg2)
        ana_fmt = arg2
     end select
  end do

  command = "cp "//trim(Salt_fname)//" "//trim(Outfile)
  call execute_command_line (command, exitstat=i)

  command = "cp "//trim(Ocean_fname)//" "//trim(OceanOutfile)
  call execute_command_line (command, exitstat=i)

  !RASTER INFO
  !===========
  call readraster(oraster)

  !OCEAN GRID
  !==========
  call read_grid(ogrid)

  print *,'ocean tiles area = ',sum(oraster%areas)
  print *,'ocean grid area = ',sum(ogrid%wet*ogrid%area)/(1000.0**2)
  
  !-----------------------------------------------------------------
  ! vectors identifying first and last layer in each category
  !-----------------------------------------------------------------
  ilyr1(1) = 1                       ! if nilyr  = 4
  ilyrn(1) = nilyr                   !   ilyr1 = { 1,5,9 }
  do n = 2, ncat                     !   ilyrn = { 4,8,12} etc
       ilyr1(n) = ilyrn(n-1) + 1
       ilyrn(n) = ilyrn(n-1) + nilyr
  enddo

  slyr1(1) = 1
  slyrn(1) = nslyr
  do n = 2, ncat
       slyr1(n) = slyrn(n-1) + 1
       slyrn(n) = slyrn(n-1) + nslyr
  enddo

  do k = 1, nilyr
      zn = (real(k,kind=dbl_kind)-p5) /  &
            real(nilyr,kind=dbl_kind)
      salin(k)=(saltmax/c2)*(c1-cos(pi*zn**(nsal/(msal+zn))))
!            salin(k)=saltmax ! for isosaline ice
      Tmlt(k) = -salin(k)*depressT
   enddo
   salin(nilyr+1) = saltmax
   Tmlt(nilyr+1) = -salin(nilyr+1)*depressT

    ! linear remapping itd category limits
    rncat = real(ncat, kind=dbl_kind)
    cc1 = c3/rncat
    cc2 = c15*cc1
    cc3 = c3
    hin_max(0) = c0     ! minimum ice thickness, m
    do n = 1, ncat
        x1 = real(n-1,kind=dbl_kind) / rncat
        hin_max(n) = hin_max(n-1) &
                    + cc1 + cc2*(c1 + tanh(cc3*(x1-c1)))
    enddo


  !READ ANALYSIS
  !=============
  if (DO_AICE_ANA) then
     print *,'Rebalancing state for sea-ice fraction'
     allocate(sic_ana(ogrid%Nx, ogrid%Ny))
     varname = 'AICE'
     call readseaicerst(Ana_fname, varname, ogrid, ana_fmt, sic_ana)
     where (sic_ana<0.0)
        sic_ana=0.0
     end where     
     where (sic_ana>=1.0-tiny)
        sic_ana=1.0-tiny
     end where     
  end if
  if (DO_HICE_ANA) then     
     print *,'Rebalancing state for sea-ice thickness'
     allocate(sit_ana(ogrid%Nx, ogrid%Ny))
     varname = 'HICE'
     call readseaicerst(Ana_fname, varname, ogrid, ana_fmt, sit_ana)
     where (sit_ana<0.0)
        sit_ana=0.0
     end where     
  end if
  if (DO_HSNO_ANA) then     
     print *,'Rebalancing state for snow depth over sea-ice'
     allocate(snd_ana(ogrid%Nx, ogrid%Ny))
     varname = 'HSNO'
     call readseaicerst(Ana_fname, varname, ogrid, ana_fmt, snd_ana)
     where (snd_ana<0.0)
        snd_ana=0.0
     end where     
  end if

  !READ SALTWATER RESTART
  !======================
  allocate( aicen(oraster%Ntiles,ncat+1) )
  varname = 'FR'
  call readrst2d(Salt_fname, varname, aicen)

  allocate(Tw(oraster%Ntiles, 1))
  varname = 'TSKINW'
  call readrst2d(Salt_fname, varname, Tw)

  allocate(Sw(oraster%Ntiles, 1))
  varname = 'SSKINW'
  call readrst2d(Salt_fname, varname, Sw)

  allocate(vicen(oraster%Ntiles,ncat))
  varname = 'VOLICE'
  call readrst2d(Salt_fname, varname, vicen)

  allocate(vsnon(oraster%Ntiles,ncat))
  varname = 'VOLSNO'
  call readrst2d(Salt_fname, varname, vsnon)

  allocate(eicen(oraster%Ntiles,nilyr,ncat))
  varname = 'ERGICE'
  call readrst3d(Salt_fname, varname, eicen)

  allocate(esnon(oraster%Ntiles,nslyr,ncat))
  varname = 'ERGSNO'
  call readrst3d(Salt_fname, varname, esnon)

  allocate(Slmask(oraster%Ntiles, 1))
  varname = 'SLMASK'
  call readrst2d(Salt_fname, varname, Slmask)


  allocate(madjice(oraster%Ntiles))
  allocate(madjiceij(ogrid%Nx, ogrid%Ny))
  allocate(Toij(ogrid%Nx, ogrid%Ny))
  madjice   = 0.0
  madjiceij = 0.0
  Toij      = 0.0

  !READ OCEAN TEMP SALT RESTART
  !============================
  varname =  'temp'
  allocate(Tocean(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  call readoceanrst(Ocean_fname, varname, ogrid, Tocean)
  
  varname =  'salt'
  allocate(Socean(ogrid%Nx, ogrid%Ny, ogrid%Nz))
  call readoceanrst(Ocean_fname, varname, ogrid, Socean)
  
  !ANALYSIS UPDATE
  !===============
  ! The following conserves the background ice thickness distribution 
  ! and rebalances the ice/snow internal energy so that Enthalpy/unit volume 
  ! is conserved. 
  ! "Craming" the analysis in the background restart is done incrementally:
  !    1 - Update aicen                  => balance ice and snow internal E
  !    2 - If ice thickness was analyzed => balance ice E
  !    3 - If snow depth was analyzed    => balance snow E
  !    

  allocate(sst_incr(ogrid%Nx, ogrid%Ny))
  do i=1,oraster%Ntiles
     if(i==targt) then
         print*, 'lon, lat: ', oraster%lons(i), oraster%lats(i)
         print*, 'initial aice0: ', aicen(i,1)
         print*, 'initial aicen: ', (aicen(i,n), n=2,ncat+1)
         print*, 'initial vicen: ', (vicen(i,n), n=1,ncat)
     endif
  enddo

  if(DO_SKIN_FLUSH) then
     do i=1,oraster%Ntiles
     ! first flush skin layer with MOM top layer T & S to avoid
     ! initial shock 
       if(i==targt) then
             print*, 'tripole i,j: ', oraster%GIs(i), oraster%GJs(i)
             print*, 'before flushing: ', Tw(i,1), Tocean(oraster%GIs(i), oraster%GJs(i),1) 
             print*, 'before flushing: ', Sw(i,1)
       endif
       if(SLMASK(i,1) < 0.5) then ! only flush when it is a MOM tile
          SW(i,1) = Socean(oraster%GIs(i), oraster%GJs(i),1) 
          TW(i,1) = Tocean(oraster%GIs(i), oraster%GJs(i),1)+273.15 
       endif  
       if(i==targt) then
             print*, 'after flushing: ', Tw(i,1), Tocean(oraster%GIs(i), oraster%GJs(i),1) 
             print*, 'after flushing: ', Sw(i,1)
       endif
     enddo
  endif  

  mass_incr = 0.0
  mass1     = 0.0
  do i=1,oraster%Ntiles
      if(SLMASK(i,1) > 0.5) cycle ! no blending for salt lakes 
      if(SLMASK(i,1) < 0.5) then ! only count MOM tiles 
        mass1 = mass1 + oraster%areas(i)*(rho_ice*sum(vicen(i,:))+rho_sno*sum(vsnon(i,:)))
      endif   

     ! SEA-ICE FRACTION
     !-----------------
     if (DO_AICE_ANA) then
        sic_bkg = sum( aicen(i,2:) )
        sic_incr = sic_ana(oraster%GIs(i), oraster%GJs(i))-sic_bkg
        !if(sic_bkg > 0.9 .and. sic_ana(oraster%GIs(i), oraster%GJs(i)) > 0.9) cycle
        if(sic_bkg > puny .and. sic_ana(oraster%GIs(i), oraster%GJs(i)) > puny .and. &
           abs(sic_bkg-sic_ana(oraster%GIs(i), oraster%GJs(i))) < 0.01) cycle
        if (DO_ALL_1ST) then
           if(sic_ana(oraster%GIs(i), oraster%GJs(i)) < -puny .or. &
              sic_ana(oraster%GIs(i), oraster%GJs(i)) > 1.0+puny) cycle
           !safety check
           oob = .false.
           do n=1, ncat 
               if(aicen(i,n+1) > puny) then
                 hi = vicen(i,n) / aicen(i,n+1)
                 if(hi > hin_max(n) .or. hi < hin_max(n-1)) then
                    oob = .true.   
                    exit 
                 endif 
               endif
           enddo 
           if(oob) cycle
           Tf = -depressT * Sw(i,1) ! Linear liquidus approximation (Bitz and Lipscomb, 1999)
           !if(sic_bkg>0.0) then
              if(sic_incr > 0.0) then  ! generate ice
                if(aicen(i,2) > puny) then
                   call  save_old_state(aicen(i,:),vicen(i,:),vsnon(i,:),&
                                        eicen(i,:,:),esnon(i,:,:), &
                                        nilyr, nslyr, 2, 1, &
                                        hi, hs, qin_save, qsn_save)  
                   call  restore_ice_state(aicen(i,:),vicen(i,:),vsnon(i,:),&
                                          eicen(i,:,:),esnon(i,:,:), &
                                          nilyr, nslyr, 2, 1, sic_incr, &
                                          hi, hs, qin_save, qsn_save, himin=hinew)  
                else
                   aicen(i,2) = aicen(i,2) + sic_incr
                   vicen(i,1) = aicen(i,2) * hinew
                   ! new ice enthalpy
                   do k=1,nilyr
                        qi0 = -rhoi * (cp_ice*(Tmlt(k)-Tf)   &
                           + Lfresh*(1.-Tmlt(k)/Tf) - cp_ocn*Tmlt(k))
                        eicen(i,k,1)                          &
                              = qi0 * vicen(i,1)                  &
                               / real(nilyr,kind=real_kind)
                   enddo
                endif
              else ! remove ice
                  incr_remain = sic_incr 
                  do n=2, ncat+1 
                     !if(incr_remain < 0.0) then
                        if(aicen(i,n) > puny) then
                          call  save_old_state(aicen(i,:),vicen(i,:),vsnon(i,:),&
                                               eicen(i,:,:),esnon(i,:,:), &
                                               nilyr, nslyr, n, n-1, &
                                               hi, hs, qin_save, qsn_save)  
                           incr_remain = aicen(i,n) + sic_incr
                           if(i==targt) then
                             print*, 'during processing: '   
                             print*, i, n,' incr = ', sic_incr 
                             print*, i, n,' remain incr = ', incr_remain 
                           endif  
                           if(incr_remain > puny) then
                              call  restore_ice_state(aicen(i,:),vicen(i,:),vsnon(i,:),&
                                                      eicen(i,:,:),esnon(i,:,:), &
                                                      nilyr, nslyr, n, n-1, sic_incr, &
                                                      hi, hs, qin_save, qsn_save)  
                              exit
                           else
                              ! all ice in category n is gone
                              aicen(i,n)   = 0.0
                              vicen(i,n-1) = 0.0
                              vsnon(i,n-1) = 0.0
                              eicen(i,:,n-1) = 0.0
                              esnon(i,:,n-1) = 0.0
                           endif 
                        endif 
                        sic_incr = incr_remain 
                  enddo ! n
                endif ! generate ice or not
           !endif 
           aicen(i,1) = 1.0 - sum( aicen(i,2:) )       ! Re-compute ocean fraction
           if(i==targt) then
              print*, 'after processing: '  
              print*, i, oraster%lons(i), oraster%lats(i)
              print*, aicen(i,1), aicen(i,2), aicen(i,3)
              print*, aicen(i,4), aicen(i,5), aicen(i,6)
           endif  
        else  ! .not. DO_ALL_1ST
        if (sic_bkg>0.0) then
           beta = real( (sic_ana(oraster%GIs(i), oraster%GJs(i))+tiny)/sic_bkg ,kind=real_kind)
           beta = min(beta, beta_max)
           !print*, beta
!           if(beta <= beta_max) then 
              if(i==targt) then
                 print*, 'beta : ', beta
                 print*, 'before scaling vicen: ', (vicen(i,n), n=1,ncat)
              endif
              aicen(i,:) = beta * aicen(i,:)         ! Re-adjust background fraction to match analysis
              mass_incr = mass_incr + (beta-1.0)*oraster%areas(i)*( rho_ice*sum( vicen(i,:) )  + rho_sno*sum( vsnon(i,:) ) ) ! Update sea-ice mass increment
                                                                                                                       ! due to sea-ice fraction analysis
                  vicen(i,:)   = beta * vicen(i,:)       ! Adjust volume of ice 
                  vsnon(i,:)   = beta * vsnon(i,:)       ! and snow accordingly.
                  eicen(i,:,:) = beta * eicen(i,:,:)     ! This way of rebalancing the ice 
                  esnon(i,:,:) = beta * esnon(i,:,:)     ! and snow internal energy conserves the enthalpy per unit volume
              if(i==targt) then
                 print*, 'after scaling vicen: ', (vicen(i,n), n=1,ncat)
              endif
        elseif (sic_ana(oraster%GIs(i), oraster%GJs(i)) > puny) then
           !beta=1.0 
           if(i==targt) then
                 print*, 'sic_ana : ', sic_ana(oraster%GIs(i), oraster%GJs(i))
               print*, 'in step C before vicen: ', (vicen(i,n), n=1,ncat)
           endif
           Tf = -depressT * Sw(i,1) ! Linear liquidus approximation (Bitz and Lipscomb, 1999)
           aicen(i,2) = aicen(i,2) + sic_ana(oraster%GIs(i), oraster%GJs(i))
           vicen(i,1) = aicen(i,2) * hinew
           ! new ice enthalpy
           do k=1,nilyr
              qi0 = -rhoi * (cp_ice*(Tmlt(k)-Tf)   &
                   + Lfresh*(1.-Tmlt(k)/Tf) - cp_ocn*Tmlt(k))
              eicen(i,k,1)                          &
                   = qi0 * vicen(i,1)                  &
                   / real(nilyr,kind=real_kind)
           enddo
           if(i==targt) then
               print*, 'in step C after vicen: ', (vicen(i,n), n=1,ncat)
           endif
        end if
        aicen(i,1) = 1.0 - sum( aicen(i,2:) )       ! Re-compute ocean fraction

        end if ! DO_ALL_1ST       


        Tf = -0.054 * Sw(i,1) + 273.15

        if( DO_ADHOC_TW ) then
           if ( sum( aicen(i,2:) ) > 0.01 .and. sic_bkg < 0.01 ) then
              Tw(i,1) = Tf 
              madjice(i) = oraster%wghts(i)
              madjiceij(oraster%GIs(i), oraster%GJs(i)) = &
                        madjiceij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)
              Toij(oraster%GIs(i), oraster%GJs(i)) = &
                        Toij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)*Tw(i,1)
           endif 
           if ( sum( aicen(i,2:) ) < 0.01 .and. sic_bkg > 0.01 ) then
              Tw(i,1) = Tf + 0.5 
              madjice(i) = oraster%wghts(i)
              madjiceij(oraster%GIs(i), oraster%GJs(i)) = &
                        madjiceij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)
              Toij(oraster%GIs(i), oraster%GJs(i)) = &
                        Toij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)*Tw(i,1)
           endif 
        else
        ! the following adjustment should be done regardless
        if ( (sum( aicen(i,2:) ) > 0.01) .and. (sic_bkg>0.01) .and. (sum( aicen(i,2:))<0.99) .and. (sic_bkg<0.99) ) then
           Tliquid= ( Tw(i,1) - sic_bkg*Tf )/(1.0-sic_bkg)            ! Keep liquid water temp unchanged
           if(i==targt) then
              print*, Tf, Tliquid, Tw(i,1), sic_bkg
           endif
           Tincr = sum( aicen(i,2:) ) * Tf + ( 1.0 - sum( aicen(i,2:) ) ) * Tliquid - Tw(i,1) 
           Tincr = Tincr_max * tanh( Tincr/Tincr_max ) ! limit the increment to ]-Tincr_max, Tincr_max[
           Tw(i,1) = Tw(i,1) + Tincr
           !Tw(i,1) = sum( aicen(i,2:) ) * Tf + ( 1.0 - sum( aicen(i,2:) ) ) * Tliquid
           if(i==targt) then
              print*,  sum(aicen(i,2:)), (1.0 - sum( aicen(i,2:))), Tw(i,1), Tincr
           endif
           madjice(i) = oraster%wghts(i)
           madjiceij(oraster%GIs(i), oraster%GJs(i)) = &
                        madjiceij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)
           Toij(oraster%GIs(i), oraster%GJs(i)) = &
                        Toij(oraster%GIs(i), oraster%GJs(i)) + madjice(i)*Tw(i,1)
           !sst_incr(oraster%GIs(i), oraster%GJs(i)) = Tincr    ! Save to diagnoze
           !Tocean(oraster%GIs(i), oraster%GJs(i), 1) = Tw(i,1) - 273.15 ! Zero out ice-ocean fluxes where sea-ice is present, investigate propagating the 
                                                                        ! increment further down the water column

           !Tw(i,1) = sum( aicen(i,2:) ) * Tf + ( 1.0 - sum( aicen(i,2:) ) ) * Tliquid  ! Adjust skin temperature below sea-ice
           !write(55,*)Tw(i,1)-273.15, Tliquid - 273.15, Tf - 273.15, sic_bkg, Tincr, sum( aicen(i,2:) )
        end if        
        end if        
     end if

     ! SEA-ICE THICKNESS
     !------------------
     if (DO_HICE_ANA) then   
     if(i==targt) then
         print*, 'lon, lat: ', oraster%lons(i), oraster%lats(i)
         print*, 'before ana_hice aice0: ', aicen(i,1)
         print*, 'before ana_hice aicen: ', (aicen(i,n), n=2,ncat+1)
         print*, 'before ana_hice vicen: ', (vicen(i,n), n=1,ncat)
     endif
        sit_bkg = sum( vicen(i,:) )
        if (sit_bkg>0.0) then
           beta=real( (sit_ana(oraster%GIs(i), oraster%GJs(i))+tiny)/sit_bkg ,kind=real_kind)
        else           
           beta=1.0           
        end if
        mass_incr = mass_incr + (beta-1.0)*oraster%areas(i)*rho_ice*sum( vicen(i,:) )   ! Update sea-ice mass increment
                                                                                        ! due to sea-ice thickness analysis

        vicen(i,:)   = beta * vicen(i,:)            ! Readjust background volume
        eicen(i,:,:) = beta * eicen(i,:,:)          ! and ice energy 
     if(i==targt) then
         print*, 'lon, lat: ', oraster%lons(i), oraster%lats(i)
         print*, 'after ana_hice aice0: ', aicen(i,1)
         print*, 'after ana_hice aicen: ', (aicen(i,n), n=2,ncat+1)
         print*, 'after ana_hice vicen: ', (vicen(i,n), n=1,ncat)
     endif
     end if

     ! SNOW DEPTH
     !-----------
     if (DO_HSNO_ANA) then   
        snd_bkg = sum( vsnon(i,:) )
        if (snd_bkg>0.0) then
           beta=real( (snd_ana(oraster%GIs(i), oraster%GJs(i))+tiny)/snd_bkg ,kind=real_kind)
        else           
           beta=1.0           
        end if

        mass_incr = mass_incr + (beta-1.0)*oraster%areas(i)*rho_sno*sum( vsnon(i,:) )   ! Update sea-ice mass increment
                                                                                        ! due to sea-ice thickness analysis

        vsnon(i,:)   = beta * vsnon(i,:)            ! Readjust background snow volume
        esnon(i,:,:) = beta * esnon(i,:,:)          ! and snow energy 
     end if
  enddo


  if (DO_AICE_ANA .and. DO_ADHOC_TW) then

  do j=1,ogrid%Ny
  do i=1,ogrid%Nx
     if(madjiceij(i,j) /= 0.0) then 
        Tocean(i,j,1) = Toij(i,j)/madjiceij(i,j) - 273.15 !
     endif
  enddo 
  enddo

  endif 

  mass2 = 0.0
  do i=1,oraster%Ntiles
      if(SLMASK(i,1) < 0.5) then ! only count MOM tiles 
        mass2 = mass2 + oraster%areas(i)*(rho_ice*sum(vicen(i,:))+rho_sno*sum(vsnon(i,:)))
      endif   
  enddo 
  
  !allocate(tmpx(oraster%Ntiles))
  !tmpx = sum(aicen(:,2:), dim=2)
  !tilemax = maxloc(tmpx,dim=1)
  !print*, 'max at tile: ', tilemax
  !do n=1,ncat
  !  print*, aicen(tilemax,n+1), vicen(tilemax,n) 
  !enddo
  !do i=1,oraster%Ntiles
  !  if(abs(oraster%lons(i)) < 0.5 .and.  abs(oraster%lats(i)-(-60.0)) < 0.5) then
  !    print*, i, oraster%lons(i), oraster%lats(i), tmpx(i)
  !  endif
  !enddo
  !deallocate(tmpx)

 ! if(DO_ALL_1ST) then
     print *, 'before mass : ', mass1 
     print *, 'after  mass : ', mass2
     print *,'Sea-ice mass increment = ',mass2-mass1,' kg'
     mass_incr = mass2-mass1  
 ! else
 !    print *,'Sea-ice mass increment = ',mass_incr,' kg'
     print *,'Sea-ice mass increment/m^2 = ',mass_incr/(sum(ogrid%wet*ogrid%area)),' kg/m2'
     print *,'Sea-ice mass increment in lwe = ',1000.0*mass_incr/(sum(ogrid%wet*ogrid%area)),' m'

   mass_incr = mass_incr/(rho_lwe*(sum(ogrid%wet*ogrid%area))) !Compute Eustatic sea-level increment
   open(10, file='seaice_mass_incr.txt',status='replace')
   write(10,*) mass_incr
   close(10)

  !endif

  !SAVE IN OCEAN TEMP SALT RESTART
  !===============================

  varname = 'temp'
  allocate( DUMVAR(ogrid%Nx, ogrid%Ny, ogrid%Nz, 1) )
  DUMVAR(:,:,:,1) = Tocean
  call check(nf90_open(OceanOutFile, NF90_WRITE, fid_in) )
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, DUMVAR))
  call check(nf90_close(fid_in))

  !SAVE IN SALTWATER RESTART
  !=========================

  varname = 'FR'
  call check(nf90_open(OutFile, NF90_WRITE, fid_in) )
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, aicen))

  varname = 'TSKINW'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, Tw(:,1)))

  varname = 'SSKINW'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, Sw(:,1)))

  varname = 'VOLICE'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, vicen))

  varname = 'VOLSNO'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, vsnon))

  varname = 'ERGICE'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, eicen))

  varname = 'ERGSNO'
  call check(nf90_inq_varid(fid_in, varname, varid))
  call check(nf90_put_var(fid_in, varid, esnon))
  call check(nf90_close(fid_in))  


  !SAVE TILE LOCATION
  !==================
  fname='tileloc.nc'
  call check( nf90_create(fname, NF90_CLOBBER, ncid) )
  call check( nf90_def_dim(ncid, "ncats", ncat, cat_dimid) )
  call check( nf90_def_dim(ncid, "ntiles", oraster%Ntiles, tile_dimid) )
  dimids1d =  (/ tile_dimid /)

  varname = 'lon'  
  call check( nf90_def_var(ncid, varname, NF90_REAL, dimids1d, varidlon) )
  varname = 'lat'  
  call check( nf90_def_var(ncid, varname, NF90_REAL, dimids1d, varidlat) )
  call check( nf90_enddef(ncid) )

  varname = 'lon'
  call check( nf90_inq_varid(ncid,varname,varidlon) )
  call check( nf90_put_var(ncid, varidlon, oraster%lons) )

  varname = 'lat'
  call check( nf90_inq_varid(ncid,varname,varidlat) )   
  call check( nf90_put_var(ncid, varidlat, oraster%lats) )

  call check( nf90_close(ncid) )
  
  ! deallocate stuff here ... 
  if(allocated(sic_ana)) deallocate(sic_ana)
  if(allocated(sit_ana)) deallocate(sit_ana)
  if(allocated(snd_ana)) deallocate(snd_ana)
  if(allocated(sst_incr))deallocate(sst_incr)

  if(allocated(Tw))    deallocate(Tw)
  if(allocated(Sw))    deallocate(Sw)
  if(allocated(Tocean))deallocate(Tocean)

  if(allocated(aicen)) deallocate(aicen)
  if(allocated(vicen)) deallocate(vicen)
  if(allocated(vsnon)) deallocate(vsnon)
  if(allocated(eicen)) deallocate(eicen)
  if(allocated(esnon)) deallocate(esnon)

  if(allocated(DUMVAR)) deallocate(DUMVAR) 

contains
     subroutine save_old_state(aicen,vicen,vsnon,eicen,esnon, &
                               nilyr, nslyr, ia, ib, &
                                hi, hs, qins, qsns)  

  real*8, dimension(:),   intent(in)  :: aicen, vicen, vsnon
  real*8, dimension(:,:), intent(in)  :: eicen, esnon
  integer*4,              intent(in)  :: nilyr, nslyr, ia, ib
  real*8,                 intent(out) :: hi, hs
  real*8, dimension(:),   intent(out) :: qins, qsns 

  integer*4                           :: k

      hi = vicen(ib) / aicen(ia)
      hs = vsnon(ib) / aicen(ia)
      qin_save(:) = 0.
      qsn_save(:) = 0.
      if (vicen(ib) > 0.0) then
           do k=1,nilyr
              qins(k) = eicen(k,ib)         &
                        * real(nilyr,kind=real_kind)             &
                           / vicen(ib)
           enddo
      endif
      if (vsnon(ib) > 0.0) then
          do k=1,nslyr
             qsns(k) = esnon(k,ib)         &
                       * real(nslyr,kind=real_kind)             &
                          / vsnon(ib)
          enddo
      endif
      return
   end subroutine save_old_state

    subroutine restore_ice_state(aicen,vicen,vsnon,eicen,esnon, &
                                nilyr, nslyr, ia, ib, sic_incr, &
                                hi, hs, qins, qsns, himin)  

  real*8, dimension(:),   intent(inout) :: aicen
  real*8, dimension(:),   intent(out)  :: vicen, vsnon
  real*8, dimension(:,:), intent(out)  :: eicen, esnon
  integer*4,              intent(in)  :: nilyr, nslyr, ia, ib
  real*8,                 intent(in) :: sic_incr
  real*8,                 intent(in) :: hi, hs
  real*8, dimension(:),   intent(in) :: qins, qsns 
  real*4, intent(in), optional       :: himin

  integer*4                           :: k

     if(present(himin)) then 
      if(hi >= himin) then 
         aicen(ia) = aicen(ia) + sic_incr
         vicen(ib) = aicen(ia) * hi
      else
         vicen(ib) = aicen(ia) * hi + sic_incr * hinew
         aicen(ia) = aicen(ia) + sic_incr
      endif
     else
       aicen(ia) = aicen(ia) + sic_incr
       vicen(ib) = aicen(ia) * hi
     endif    
       vsnon(ib) = aicen(ia) * hs
       do k=1,nilyr
            eicen(k,ib)                          &
            = qins(k) * vicen(ib)                  &
              / real(nilyr,kind=real_kind)
       enddo
       do k=1,nslyr
            esnon(k,ib)                          &
               = qsns(k) * vsnon(ib)                  &
               / real(nslyr,kind=real_kind)
       enddo

      return
   end subroutine restore_ice_state

end program anaice2rst
