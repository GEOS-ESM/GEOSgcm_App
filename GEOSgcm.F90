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
   use MAPL_Mod
!!$   use GEOS_GcsGridCompMod, only:  ROOT_SetServices => SetServices
   use GEOS_GcmGridCompMod, only:  ROOT_SetServices => SetServices

   ! populate necessary modele mpi stuff
   use dist_grid_mod, only: setCommunicator, rank, npes_world

   implicit none

!EOP

!EOC

   character(len=*), parameter :: Iam="GEOS5_Main"
   type (MAPL_Cap) :: cap
   type (MAPL_CapOptions) :: cap_options
   integer :: status


   call MPI_init(status)
   call setCommunicator(MPI_COMM_WORLD)
   call MPI_Comm_Size(MPI_COMM_WORLD, npes_world, status)
   call MPI_Comm_Rank(MPI_COMM_WORLD, rank, status)

   cap_options = MAPL_CapOptions()
   cap = MAPL_Cap('GCM', ROOT_SetServices, cap_options = cap_options)

   call cap%run(_RC)

end program GEOS5_Main
   
