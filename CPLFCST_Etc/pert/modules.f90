!======================================================================================
!======================================================================================
module odas_resolution
!======================================================================================
!======================================================================================

  implicit none
  integer, parameter                                    :: im  = 360
  integer, parameter                                    :: jm  = 200
  integer, parameter                                    :: km  = 50 

  integer, parameter                                    :: atm_im  = 144
  integer, parameter                                    :: atm_jm  = 91
  integer, parameter                                    :: atm_km  = 72
end module odas_resolution

!======================================================================================
!======================================================================================
module odas_types 
!======================================================================================
!======================================================================================
 
  use odas_resolution
  implicit none
  
  type ocean
     real(kind=4), dimension (im, jm, km)                    :: T, S, U, V
     real(kind=4), dimension (im, jm)                        :: SSH, TAUX, TAUY
  end type ocean

end module odas_types


!======================================================================================
!======================================================================================
module my_stats
!======================================================================================
!======================================================================================

contains

  !-----------------------------------------------------------
  subroutine set_random_seed ( iseed )
    !-----------------------------------------------------------
    !
    !*******************************************************************************
    !
    !! SET_RANDOM_SEED initializes the FORTRAN 90 random number generator.
    !
    !
    !  Discussion:
    !
    !    If ISEED is nonzero, then that value is used to construct a seed.
    !
    !    If ISEED is zero, then the seed is determined by calling the date 
    !    and time routine.  Thus, if the code is run at different times, 
    !    different seed values will be set.
    !
    !  Modified:
    !
    !    08 May 2002
    !
    !  Author:
    !
    !    Lili Ju
    !
    !  Parameters:
    !
    !    Input, integer ISEED, is nonzero for a user seed, or 0 if the
    !    seed should be determined by this routine.
    !
    implicit none
    !
    integer date_time(8)
    logical, parameter :: debug = .false.
    integer i
    integer iseed
    integer j
    integer k
    integer, parameter :: myrank = 0
    integer, allocatable :: seed(:)
    real x
    !
    !  Initialize the random seed routine.
    !
    call random_seed
    !
    !  Request the size of a typical seed.
    !  (On the DEC ALPHA, K is returned as 2.)
    !
    call random_seed ( size = k )

    if ( debug ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'SET_RANDOM_SEED:'
       write ( *, '(a,i6)' ) '  Random seed size is K = ', k
    end if
    !
    !  Set up space for a seed vector.
    !
    allocate ( seed(k) )

    if ( iseed /= 0 ) then

       seed(1:k) = iseed

    else
       !
       !  Make up a "random" value based on date and time information.
       !
       call date_and_time ( values = date_time )

       do i = 1, k

          seed(i) = 0

          do j = 1, 8
             seed(i) = seed(i) + ( j + i ) * date_time(j) + myrank * 100
             seed(i) = ishftc ( seed(i), 4 * ( j - 1 ) )
          end do

       end do

    end if

    if  ( debug ) then

       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'SET_RANDOM_SEED:'
       write ( *, '(a)' ) '  The random seed vector:'
       write ( *, '(a)' ) ' '

       do i = 1, k
          write ( *, '(i12)' ) seed(i)
       end do

    end if
    !
    !  Send this random value back to the RANDOM_SEED routine, to be
    !  used as the seed of the random number generator.
    !
    call random_seed ( put = seed(1:k) )

    deallocate ( seed )

    return
  end subroutine set_random_seed

end module my_stats
