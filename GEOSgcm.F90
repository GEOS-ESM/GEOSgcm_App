! $Id$

! *********************************************************************
! *****                      Main Program                          ****
! *****         Finite-Volume Dynamical Core (Lin/Rood)            ****
! *****         Forced by GEOS5 Physics                            ****
! *********************************************************************

!
!
!

#define I_AM_MAIN

#include "MAPL_Generic.h"

program GEOS5_Main
   use MPI
   use MAPL_Mod
!   use GEOS_GcmGridCompMod, only:  ROOT_SetServices => SetServices
   implicit none
!EOP

!EOC

   character(len=*), parameter :: Iam="GEOS5_Main"
   type (MAPL_Cap) :: cap
   type (MAPL_FlapCapOptions) :: cap_options
   integer :: status

! Add to the command line:
!     --shared_obj libGEOSgcm_GridComp.so --proc_name setservices 
   cap_options = MAPL_FlapCapOptions(description = 'GEOS AGCM', &
                                     authors     = 'GMAO')
   cap = MAPL_Cap('GCM', cap_options = cap_options)

   call cap%run(_RC)

end program GEOS5_Main
   
