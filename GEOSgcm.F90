! $Id$

! *********************************************************************
! *****                      Main Program                          ****
! *****         Finite-Volume Dynamical Core (Lin/Rood)            ****
! *****         Forced by GEOS5 Physics                            ****
! *********************************************************************

#define I_AM_MAIN

#include "MAPL_Generic.h"

program GEOS5_Main
   use MPI
   use MAPL
   use GEOS_GcmGridCompMod, only:  ROOT_SetServices => SetServices
   implicit none
!EOP

!EOC

   character(len=*), parameter :: Iam="GEOS5_Main"
   type (MAPL_Cap) :: cap
   type (MAPL_FlapCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FlapCLI(description = 'GEOS AGCM', &
                                     authors     = 'GMAO')
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('GCM', ROOT_SetServices, cap_options = cap_options)

   call cap%run(_RC)

end program GEOS5_Main
   
