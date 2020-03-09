
module breuche
  implicit none
  public :: operator(+), operator(-), operator(*), operator(/)

  type, public :: b
     real :: zaehler
     real :: nenner
  end type b

  interface operator(+)
     module procedure b_plus_b, i_plus_b
  end interface operator(+)
  interface operator(-)
     module procedure b_minus_b, i_minus_b
  end interface operator(-)
  interface operator(*)
     module procedure b_mal_b, i_mal_b
  end interface operator(*)
  interface operator(/)
     module procedure b_durch_b, i_durch_b
  end interface operator(/)
contains
  function b_plus_b(x,y) result(z)
    type(b), intent(in) :: x,y
    !    type(b), intent(out) :: z ! ERROR: intetnt(out) muss nicht geschrieben, weil bei Funktionen es einen Ruckgabewert gibt und man braucht keine intent(out) zu schreiben.
    type(b) :: z
    z%zaehler = ((x%zaehler * y%nenner) + (x%nenner * y%zaehler))
    z%nenner = (x%nenner * y%nenner)
  end function b_plus_b

  function b_minus_b(x,y) result(z)
    type(b), intent(in) :: x,y
    type(b) :: z
    !    z%zaehler = ((x%zaehler * y%nenner) - (x%nenner * y%zaehler))
    !    z%nenner = (x%nenner * y%nenner)
    z = b_plus_b(x,b(-y%zaehler,y%nenner))
  end function b_minus_b

  function b_mal_b(x,y) result(z)
    type(b), intent(in) :: x,y
    type(b) :: z
    z%zaehler = (x%zaehler * y%zaehler)
    z%nenner = (x%nenner * y%nenner)
  end function b_mal_b

  function b_durch_b(x,y) result(z)
    type(b), intent(in) :: x,y
    type(b) :: z
    z = b_mal_b(x,b(y%nenner,y%zaehler))
  end function b_durch_b

  function i_plus_b(i,y) result(z)
    type(b), intent(in) :: y
    integer, intent(in) :: i 
    type(b) :: z
    z = b(i,1)+y ! oder:    z = b_plus_b(b(i,1),y)
  end function i_plus_b

  function i_minus_b(i,y) result(z)
    type(b), intent(in) :: y
    integer, intent(in) :: i 
    type(b) :: z
    z = b(i,1)-y ! oder:    z = b_minus_b(b(i,1),y)
  end function i_minus_b
  function i_mal_b(i,y) result(z)
    type(b), intent(in) :: y
    integer, intent(in) :: i 
    type(b) :: z
    z = b(i,1)*y ! oder:    z = b_mal_b(b(i,1),y)
  end function i_mal_b
  function i_durch_b(i,y) result(z)
    type(b), intent(in) :: y
    integer, intent(in) :: i 
    type(b) :: z
    z = b(i,1)/y ! oder:    z = b_durch_b(b(i,1),y)
  end function i_durch_b
end module breuche

program a12_1_1
  use breuche
  implicit none

  type(b) :: erg
  integer,parameter :: dp = selected_real_kind(13)
  real(dp) :: r
  !  erg = 3 + (1/5)
  erg = 3+1/(7+b(1,16))
  !  print*, erg, acos(-1.0_dp)
  print*, erg
end program a12_1_1

