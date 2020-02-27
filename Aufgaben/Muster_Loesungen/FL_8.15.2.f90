! Aufgabe: Du weisst schon was ich meine
! F-Loesung

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 8.15.2: Du weisst schon was ich meine         |
!|                                                                      |
!|  Problem: create a generic function                                  |
!|                                                                      |
!|              binomial(a,k) = { a*(a-1)*(a-2*....*(a-k+1) / k!        |
!|                                                                      |
!|           for single and double precision  "a"                       |
!|                                                                      |
!+----------------------------------------------------------------------+

module precision
  integer,parameter,public :: is = selected_int_kind(9)    ! integer single 
  integer,parameter,public :: id = selected_int_kind(18)   ! integer double
  integer,parameter,public :: rs = selected_real_kind(6)   ! real, single
  integer,parameter,public :: rd = selected_real_kind(13)  ! real, double

! or
!integer, parameter ::                              &
!    rs = kind(1.0),                                &
!    rd = selected_real_kind(2*precision(1.0_rs)),  &
!    rq = selected_real_kind(2*precision(1.0_rd))

end module precision

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

module functions 

!--------------------------------
!---> contains all functions <---
!--------------------------------

  use precision

  implicit none

  private
  public :: binomial

  interface binomial
    module procedure binomial_single, binomial_double
  end interface


  contains 

!-----------------------------------------------------

  function binomial_single(a,k)result(res)

  implicit none

  real(kind=rs), intent(in)     :: a        ! input-parameter a
  integer(kind=is), intent(in)  :: k        ! input-parameter k
  real(kind=rs)                 :: res      ! result of function

  real(kind=rs)                 :: aa       ! auxiliary variable 
  integer(kind=rs)              :: kk       ! auxiliary variable
  integer(kind=rs)              :: i        ! loop-variable

!----------------

  if ( k == 0 ) then 
     res = 1.0_rs
  else
   
     aa = 1.0_rs
     kk = abs (k)

     do,i=kk,1,-1
       aa = aa*(a-real(kk-i,rs)) / real(i,rs)
     end do

     res = aa

     if ( k < 0 ) res = 1.0_rs / res

     write(*,*)'in function binomial_single, erg =',res
  end if

  end function binomial_single

!-----------------------------------------------------

  function binomial_double(a,k)result(res)

  implicit none

  real(kind=rd), intent(in)     :: a        ! input-parameter a
  integer(kind=is), intent(in)  :: k        ! input-parameter k
  real(kind=rd)                 :: res      ! result of function

  real(kind=rd)                 :: aa       ! auxiliary variable 
  integer(kind=is)              :: kk       ! auxiliary variable
  integer(kind=is)              :: i        ! loop-variable

!----------------

  if ( k == 0 ) then 
     res = 1.0_rd
  else
     aa = 1.0_rd
     kk = abs (k)

     ! backwards to avoid rounding
     do,i=kk,1,-1
       aa = aa*(a-real(kk-i,rd)) / real(i,rd)
     end do

     res = aa

     if ( k < 0 ) res = 1.0_rd / res 

     write(*,*)'in function binomial_double, erg =',res
  endif

  end function binomial_double

end module functions 

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

program generische_Funktionen

  use precision
  use functions, only    : binomial
  implicit none

  real(kind=rs)          :: binom_s
  real(kind=rd)          :: binom_d

!--------------------------------------------------

  binom_s = binomial(15._rs, 3_is)
  write(*,*)'binom 15 3, single precision called :', binom_s
  binom_s = binomial(99.0_rs, 6_is)
  write(*,*)'binom 99 6, single precision called :', binom_s

  binom_d = binomial(49.0_rd, 6_is)
  write(*,*)'binom 49 6, double precision called :', binom_d
  binom_d = binomial(99.0_rd, 6_is)
  write(*,*)'binom 99 6, double precision called :', binom_d



end program generische_Funktionen
