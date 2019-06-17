!  $Id$
!
! idfupd.x - ESMF/MAPL application to perform an IDF update to the RST fields 
!
! 1. Read dynamics, moisture, and pchem restarts.
! 2. Read restarts with DFI fields from two integrations: one from the
!    background; and another from an analysis.
! 3. Calculates the difference between the two DFI states, creating
!    and incremental (IDF) correction that's applied to the fields 
!    of interest.
!
! REMARKS: 
!   a) This program requires an RC file: IDFUPD.rc
!
! Ricardo Todling, August 2012
!............................................................................
!  !REVISION_HISTORY:  
!   01Aug2012  Trayanov initial coding of handle to internal states of GCM
!   31Aug2012  Todling  completion of implementation of IDF
!----------------------------------------------------------------------------

#include "MAPL_Generic.h"

program idfupd

  use ESMF
  use MAPL_Mod

  use FVdycore_GridCompMod,   only : FV_SetServices    => SetServices
  use FVdycoreCubed_GridComp, only : FV3_SetServices   => SetServices
  use GEOS_MoistGridCompMod,  only : Moist_SetServices => SetServices
  use GEOS_PChemGridCompMod,  only : Pchem_SetServices => SetServices
  use DFI_GridCompMod,        only : DFI_SetServices   => SetServices

  implicit none

  integer,  parameter :: r8           = 8
  integer,  parameter :: r4           = 4
  real(r4), parameter :: KAPPA        = MAPL_KAPPA

  logical AmIRoot
  integer RC
  character,parameter :: Name='IDFUPD'

!  Pointers to internal state variables
!  ======================================
   real(r8), pointer                  :: ak(:), bk(:)
   real(r8), pointer                  ::  u(:,:,:)
   real(r8), pointer                  ::  v(:,:,:)
   real(r8), pointer                  ::  pt(:,:,:)
   real(r8), pointer                  ::  pe(:,:,:)
   real(r8), pointer                  ::  pkz(:,:,:)
   real(r4), pointer                  ::  qv(:,:,:)
   real(r4), pointer                  ::  ox(:,:,:)

   real(r8), pointer                  ::  u_dfia (:,:,:)
   real(r8), pointer                  ::  v_dfia (:,:,:)
   real(r8), pointer                  ::  pt_dfia(:,:,:)
   real(r8), pointer                  ::  pe_dfia(:,:,:)
   real(r4), pointer                  ::  qv_dfia(:,:,:)
   real(r4), pointer                  ::  ox_dfia(:,:,:)

   real(r8), pointer                  ::  u_dfib (:,:,:)
   real(r8), pointer                  ::  v_dfib (:,:,:)
   real(r8), pointer                  ::  pt_dfib(:,:,:)
   real(r8), pointer                  ::  pe_dfib(:,:,:)
   real(r4), pointer                  ::  qv_dfib(:,:,:)
   real(r4), pointer                  ::  ox_dfib(:,:,:)

   call MAIN(Name, AmIRoot, RC)
   
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOPI

! !IROUTINE: MAPL_Cap -- Implements generic Cap functionality

! !INTERFACE:

  subroutine MAIN( Name, AmIRoot, RC)

! !ARGUMENTS:

    character*(*), optional, intent(IN ) :: Name
    logical,       optional, intent(OUT) :: AmIRoot
    integer,       optional, intent(OUT) :: rc

!EOPI

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------

   integer                      :: DYN, MOIST, PCHEM, DFIA, DFIB
   character(len=ESMF_MAXSTR)   :: ROOT_NAME

! A MAPL object for the cap
!--------------------------

   type(MAPL_MetaComp)          :: MAPLOBJ

! The children's GCs and IM/Ex states
!------------------------------------

   type(ESMF_GridComp), pointer :: GCS(:)
   type(ESMF_State),    pointer :: IMPORTS(:)
   type(ESMF_State),    pointer :: EXPORTS(:)
   type(ESMF_Grid)              :: GCMgrid   ! GCM Grid

! ESMF stuff
!-----------

   type(ESMF_VM)                :: VM
   type(ESMF_Config)            :: config
   type(ESMF_Config)            :: cf_root
   type(ESMF_Clock)             :: clock
   type(ESMF_Time)              :: CurrTime
   type(ESMF_TimeInterval)      :: Frequency

   
! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="MAPL_Cap"

! Misc locals
!------------

   character(len=ESMF_MAXSTR)   :: ROOT_CF
   character(len=ESMF_MAXSTR)   :: enableTimers
   character(len=ESMF_MAXSTR)   :: enableMemUtils
   character(len=ESMF_MAXSTR)   :: clockname

   character(len=ESMF_MAXSTR)   :: bdfifn
   character(len=ESMF_MAXSTR)   :: adfifn

   logical                      :: done_idf

   integer                      :: HEARTBEAT_DT
   integer                      :: RUN_DT
   integer                      :: I,N,NSTEPS

   type(ESMF_State)                   :: INTERNAL
   type (MAPL_MetaComp), pointer      :: CHILD_MAPL

   character(len=ESMF_MAXSTR)         :: DYCORE

! Begin
!------

!  Initialize ESMF
!-----------------

#if defined(ENABLE_ESMF_ERR_LOGGING)
   call ESMF_Initialize (vm=vm, __RC__)
#else
   call ESMF_Initialize (vm=vm, logKindFlag=ESMF_LOGKIND_NONE, rc=status)
#endif

   if (present(AmIRoot)) then
      AmIRoot = MAPL_Am_I_Root(vm)
   end if

!  Open the CAP's configuration from idfupd.rc
!---------------------------------------------

   config = ESMF_ConfigCreate (                      __RC__ )
   call ESMF_ConfigLoadFile   ( config, 'IDFUPD.rc', __RC__ )

!  CAP's MAPL MetaComp
!---------------------

   if(present(Name)) then
      call MAPL_Set (MAPLOBJ, name= Name, cf=CONFIG, __RC__ )
   else
      call MAPL_Set (MAPLOBJ, name='CAP', cf=CONFIG, __RC__ )
   end if

!  Create Clock. This is a private routine that sets the start and 
!   end times and the time interval of the clock from the configuration.
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!---------------------------------------------------------------------------

   call MAPL_ClockInit ( MAPLOBJ, clock, NSTEPS, __RC__ )

   call ESMF_ConfigGetAttribute(config, value=HEARTBEAT_DT, Label="HEARTBEAT_DT:", __RC__ )
   call ESMF_TimeIntervalSet( Frequency, S=HEARTBEAT_DT, __RC__ )


! Set CLOCK for AGCM
! ------------------


!  Get configurable info to create HIST 
!  and the ROOT of the computational hierarchy
!---------------------------------------------

!BOR

! !xRESOURCE_ITEM: string :: Name of ROOT's config file
   call MAPL_GetResource(MAPLOBJ, ROOT_CF,      "ROOT_CF:", &
                         default="IDFUPD.rc",    __RC__ )

! !xRESOURCE_ITEM: string :: Control Timers 
   call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", &
                         default='NO',             __RC__ )

! !xRESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
   call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", &
                         default='NO',             __RC__ )

   if (enableTimers /= 'YES' .and. enableTimers /= 'yes') then
      call MAPL_ProfDisable( __RC__ )
   end if

  if (enableMemUtils /= 'YES' .and. enableMemUtils /= 'yes') then
     call MAPL_MemUtilsDisable( __RC__ )
  else
     call MAPL_MemUtilsInit( __RC__ )
  end if

! Handle RUN_DT in ROOT_CF
!-------------------------

   cf_root = ESMF_ConfigCreate(__RC__)
   call ESMF_ConfigLoadFile(cf_root, ROOT_CF, __RC__ )

   call ESMF_ConfigGetAttribute(cf_root, value=RUN_DT, Label="RUN_DT:", __RC__ )
   if (heartbeat_dt /= run_dt) then
      if (MAPL_AM_I_Root(VM)) then
         print *, "ERROR: inconsistent values of HEATBEAT_DT and RUN_DT"
      end if
      call ESMF_VMBarrier(VM)
      RETURN_(ESMF_FAILURE)
   end if
   

! Register the children with MAPL
!--------------------------------

   call ESMF_ConfigGetAttribute(cf_root, value=DYCORE, Label="DYCORE:", __RC__ )

!  Create All of the children
!----------------------------
   call MAPL_Set(MAPLOBJ, CF=CF_ROOT, __RC__ )

   if ( trim(DYCORE) == 'FV3' ) then
      DYN = MAPL_AddChild ( MAPLOBJ,     &
           name       = 'DYN',           &
           SS         = FV3_SetServices, &
                                __RC__   )
   else
      DYN = MAPL_AddChild ( MAPLOBJ,     &
           name       = 'DYN',           &
           SS         = FV_SetServices,  &
                                __RC__   )
   endif

   MOIST = MAPL_AddChild ( MAPLOBJ,     &
        name       = 'MOIST',           &
        SS         = MOIST_SetServices, &
                                __RC__  )

   PCHEM = MAPL_AddChild ( MAPLOBJ,     &
        name       = 'PCHEM',           &
        SS         = PCHEM_SetServices, &
                                __RC__  )  

   DFIA = MAPL_AddChild ( MAPLOBJ,     &
        name       = 'DFIA',           &
        SS         = DFI_SetServices,  &
                                __RC__ )  

   DFIB = MAPL_AddChild ( MAPLOBJ,     &
        name       = 'DFIB',           &
        SS         = DFI_SetServices,  &
                                __RC__ )  


!  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
!  -----------------------------------------------------------

   call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS, __RC__ )

   call MAPL_GridCreate  (GCS(DYN),ESMFGRID=GCMgrid, __RC__)
   call ESMF_GridCompGet (GCS(DYN), grid=GCMgrid, __RC__ )
   call ESMF_GridValidate(GCMgrid,__RC__)

   DO I = 1, size(GCS)
      call ESMF_GridCompSet ( GCS(I), grid=GCMgrid, __RC__ )
   END DO

!  Initialize the Computational Hierarchy
!  --------------------------------------
   DO I = 1, size(GCS)
      call ESMF_GridCompInitialize ( GCS(I), importState=IMPORTS(I), &
           exportState=EXPORTS(I), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)
   END DO

! Get pointers to internal state variables
   
! dynamics
   call MAPL_GetObjectFromGC ( GCS(DYN), CHILD_MAPL, __RC__ )
   call MAPL_Get (CHILD_MAPL, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )

   call MAPL_GetPointer(INTERNAL, AK , 'AK' , __RC__ )
   call MAPL_GetPointer(INTERNAL, U  , 'U'  , __RC__ )
   call MAPL_GetPointer(INTERNAL, V  , 'V'  , __RC__ )
   call MAPL_GetPointer(INTERNAL, PT , 'PT' , __RC__ )
   call MAPL_GetPointer(INTERNAL, PE , 'PE' , __RC__ )
   call MAPL_GetPointer(INTERNAL, PKZ, 'PKZ', __RC__ )

! moist
   call MAPL_GetObjectFromGC ( GCS(MOIST), CHILD_MAPL, __RC__ )
   call MAPL_Get (CHILD_MAPL, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )

   call MAPL_GetPointer(INTERNAL, QV, 'Q', __RC__ )

! PCHEM
   call MAPL_GetObjectFromGC ( GCS(PCHEM), CHILD_MAPL, __RC__ )
   call MAPL_Get (CHILD_MAPL, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )

   call MAPL_GetPointer(INTERNAL, OX, 'OX', __RC__ )

!  Filtered background
   call MAPL_GetObjectFromGC ( GCS(DFIB), CHILD_MAPL, __RC__ )
   call MAPL_Get (CHILD_MAPL, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )

   call MAPL_GetPointer(INTERNAL, U_DFIB , 'U_DGRID' , __RC__ )
   call MAPL_GetPointer(INTERNAL, V_DFIB , 'V_DGRID' , __RC__ )
   call MAPL_GetPointer(INTERNAL, PT_DFIB, 'PT'      , __RC__ )
   call MAPL_GetPointer(INTERNAL, PE_DFIB, 'PE'      , __RC__ )
   call MAPL_GetPointer(INTERNAL, QV_DFIB, 'Q'       , __RC__ )
   call MAPL_GetPointer(INTERNAL, OX_DFIB, 'OX'      , __RC__ )

!  Filtered analysis
   call MAPL_GetObjectFromGC ( GCS(DFIA), CHILD_MAPL, __RC__ )
   call MAPL_Get (CHILD_MAPL, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )

   call MAPL_GetPointer(INTERNAL, U_DFIA , 'U_DGRID' , __RC__ )
   call MAPL_GetPointer(INTERNAL, V_DFIA , 'V_DGRID' , __RC__ )
   call MAPL_GetPointer(INTERNAL, PT_DFIA, 'PT'      , __RC__ )
   call MAPL_GetPointer(INTERNAL, PE_DFIA, 'PE'      , __RC__ )
   call MAPL_GetPointer(INTERNAL, QV_DFIA, 'Q'       , __RC__ )
   call MAPL_GetPointer(INTERNAL, OX_DFIA, 'OX'      , __RC__ )

   call ESMF_ConfigGetAttribute(cf_root, value=bdfifn, Label="DFIB_INTERNAL_RESTART_FILE:", default='NULL', __RC__ )
   call ESMF_ConfigGetAttribute(cf_root, value=adfifn, Label="DFIA_INTERNAL_RESTART_FILE:", default='NULL', __RC__ )

   done_idf = do_idfupd_(bdfifn,adfifn)

!  Finalize
!  --------
   DO I = 1, size(GCS)
      call ESMF_GridCompFinalize( GCS(I), importState=IMPORTS(I),&
           exportState=EXPORTS(I), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)
   END DO

   if( done_idf .and. STATUS==0 ) then
      if (AmIRoot) then
         close(99)
         open (99,file='IDFUPD_EGRESS',form='formatted')
         close(99)
      end if
   endif

!  Finalize framework
!  ------------------
   call ESMF_Finalize (__RC__)

   RETURN_(ESMF_SUCCESS)

 end subroutine MAIN

 logical function do_idfupd_(bfname,afname)

   character(len=*), intent(in) :: bfname, afname

   real(r8), allocatable              ::  pe2k(:,:,:)
   real(r8), allocatable              :: dpe2k(:,:,:)
   real(r8), allocatable              :: dpkz (:,:,:)
   real(r8), allocatable              :: dpe  (:,:,:)
   real(r8) num, dnum, den, den2, dden
   integer i,j,k
   integer im,jm,km,kml,kmu

   do_idfupd_=.false.
   if(trim(afname)=='NULL'.or.trim(bfname)=='NULL') return

!  Update GCM initial condition with DFI filtered results
!  ------------------------------------------------------
   u  = u  +  u_dfia -  u_dfib
   v  = v  +  v_dfia -  v_dfib
   pt = pt + pt_dfia - pt_dfib

!  Incrementally reconstruct pkz

   im=size(pt,1)
   jm=size(pt,2)
   km=size(pt,3)
   kml = lbound(pe,3)
   kmu = ubound(pe,3)
   allocate( pe2k(size(pe,1) ,size(pe,2) ,kml:kmu))
   allocate(dpe2k(size(pe,1) ,size(pe,2) ,kml:kmu))
   allocate(dpe  (size(pe,1) ,size(pe,2) ,kml:kmu))
   allocate(dpkz (size(pkz,1),size(pkz,2),km))
   dpkz=0.d0

     dpe =   pe_dfia - pe_dfib
    pe2k =             pe** kappa
   dpe2k = kappa*(pe2k/pe)*dpe
   do k=1,km

      do j=1,jm
         do i=1,im
            num  =  pe2k(i,j,k) -  pe2k(i,j,k-1)
            dnum = dpe2k(i,j,k) - dpe2k(i,j,k-1)
            den  = kappa * ( log(pe(i,j,k)) - log(pe(i,j,k-1)) )
            den2 = den*den
            dden = kappa * ( dpe(i,j,k)/pe(i,j,k) - dpe(i,j,k-1)/pe(i,j,k-1) )
            dpkz(i,j,k) = dnum/den - num*dden/den2
         enddo
      enddo

!     dpkz(:,:,k) = (  ( dpe2k(:,:,k)  - dpe2k(:,:,k-1) )*         &
!                    log(   pe(:,:,k)  /    pe(:,:,k-1) )          &
!                   -  (  pe2k(:,:,k)  - pe2k (:,:,k-1) )*         &
!                      (  dpe (:,:,k)  / pe   (:,:,k)              &
!                      -  dpe (:,:,k-1)/ pe   (:,:,k-1) )          &
!                    )  / (kappa*( log(pe(:,:,k+1)/pe(:,:,k)) )**2)
   enddo
   pe  = pe  + dpe
   pkz = pkz + dpkz

   deallocate(dpkz )
   deallocate(dpe  )
   deallocate(dpe2k)
   deallocate( pe2k)

!  Update tracers avoiding negatives

   qv = max(0.,qv + qv_dfia - qv_dfib)
   ox = max(0.,ox + ox_dfia - ox_dfib)

   do_idfupd_=.true.
 end function do_idfupd_

!BOPI

! !IROUTINE: MAPL_ClockInit -- Sets the clock

! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, nsteps, rc)

! !ARGUMENTS:

     type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
     type(ESMF_Clock),    intent(  out) :: Clock
     integer,             intent(  out) :: nsteps
     integer, optional,   intent(  out) :: rc

!  !DESCRIPTION:

!   This is a private routine that sets the start and 
!   end times and the time interval of the application clock from the configuration.
!   This time interal is the ``heartbeat'' of the application.
!   The Calendar is set to Gregorian by default. 
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!


     type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
     type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
     type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
     type(ESMF_TimeInterval)  :: duration
     type(ESMF_Calendar)      :: cal
     character(ESMF_MAXSTR)   :: CALENDAR

     integer                  :: STATUS
     character(ESMF_MAXSTR)   :: IAM="MAPL_ClockInit"

     integer        :: BEG_YY
     integer        :: BEG_MM
     integer        :: BEG_DD
     integer        :: BEG_H
     integer        :: BEG_M
     integer        :: BEG_S

     integer        :: CUR_YY
     integer        :: CUR_MM
     integer        :: CUR_DD
     integer        :: CUR_H
     integer        :: CUR_M
     integer        :: CUR_S

     integer        :: END_YY
     integer        :: END_MM
     integer        :: END_DD
     integer        :: END_H
     integer        :: END_M
     integer        :: END_S

     integer        :: DUR_YY
     integer        :: DUR_MM
     integer        :: DUR_DD
     integer        :: DUR_H
     integer        :: DUR_M
     integer        :: DUR_S

     integer        :: HEARTBEAT_DT
     integer        :: NUM_DT
     integer        :: DEN_DT

     integer        :: UNIT
     integer        :: datetime(2)

! Begin
!------

! Read Times From Config
! ----------------------

!BOR

     call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
     else

! !RESOURCE_ITEM: year :: Beginning year (integer)
        call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Beginning month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Beginning second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
! -----------------------------------------------------------
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     rc=STATUS )
     if(STATUS/=ESMF_SUCCESS) then
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )
     end if

     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! !RESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
     call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: string :: Calendar type
     call MAPL_GetResource( MAPLOBJ, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
     VERIFY_(STATUS)

!EOR

     ASSERT_(NUM_DT>=0)
     ASSERT_(DEN_DT> 0)
     ASSERT_(HEARTBEAT_DT>=0)
!     ASSERT_(NUM_DT*HEARTBEAT_DT>0)
     ASSERT_(NUM_DT<DEN_DT)

! initialize calendar to be Gregorian type
! ----------------------------------------

     if    (CALENDAR=="GREGORIAN") then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="JULIAN"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="NOLEAP"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
        VERIFY_(STATUS)
     else
        ASSERT_(.false.)
     endif

! initialize start time for Alarm frequencies
! -------------------------------------------

     call ESMF_TimeSet( StartTime, YY = BEG_YY, &
                                   MM = BEG_MM, &
                                   DD = BEG_DD, &
                                    H = BEG_H , &
                                    M = BEG_M , &
                                    S = BEG_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

     call ESMF_TimeSet(   EndTime, YY = END_YY, &
                                   MM = END_MM, &
                                   DD = END_DD, &
                                    H = END_H , &
                                    M = END_M , &
                                    S = END_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)  

! Read CAP Restart File for Current Time
! --------------------------------------

     CUR_YY = BEG_YY
     CUR_MM = BEG_MM
     CUR_DD = BEG_DD
     CUR_H  = BEG_H
     CUR_M  = BEG_M
     CUR_S  = BEG_S

     UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
     VERIFY_(STATUS)

     rewind(UNIT)
     read(UNIT,100,err=999,end=999) datetime
100  format(i8.8,1x,i6.6)

     CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

     if( MAPL_AM_I_ROOT() ) then
         write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2)") 'Read CAP restart properly, Current Date = ', CUR_YY,CUR_MM,CUR_DD
         write(6,"(a,2x,i2.2,':',i2.2,':',i2.2)") '                           Current Time = ', CUR_H ,CUR_M ,CUR_S
         print *
     endif


999  continue  ! Initialize Current time

     call FREE_FILE (UNIT)

     call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
                                  MM = CUR_MM, &
                                  DD = CUR_DD, &
                                   H = CUR_H , &
                                   M = CUR_M , &
                                   S = CUR_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

! initialize final stop time
! --------------------------

     call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
                                   MM = DUR_MM, &
                                    D = DUR_DD, &
                                    H = DUR_H , &
                                    M = DUR_M , &
                                    S = DUR_S , &
                                    startTime = currTime, &
                                    rc = STATUS  )
     VERIFY_(STATUS)

     stopTime = currTime + duration

! initialize model time step
! --------------------------

     call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
     VERIFY_(STATUS)

     nsteps = duration/timestep

! Create Clock and set it to one time step before StartTime.
! After Initialize has created all alarms, we will advance the
! clock to ensure the proper ringing state of all alarms
!-------------------------------------------------------------

     if (endTime < stopTime) then
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=EndTime, rc=STATUS )
     else
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=StopTime, rc=STATUS )
     end if
     VERIFY_(STATUS)

     call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine MAPL_ClockInit

end program idfupd
