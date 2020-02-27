
module precision
  implicit none
  integer,parameter,public :: rs = selected_real_kind(6)
  integer,parameter,public :: rd = selected_real_kind(13)
end module precision

module functions
  use precision
  implicit none
  private
  public :: binomial
  private :: fakultaet, berechnen

  interface binomial
     module procedure b1
     module procedure b2
  end interface binomial

contains
  pure function fakultaet(x)
    real(kind = rs), intent(in) :: x
    real(kind = rs) :: fakultaet
    integer :: i
    fakultaet = 1
    do i = 0,(x-1)
       fakultaet = fakultaet * (x-i)
    end do
  end function fakultaet
  
  pure function berechnen(a,k) 
    real(kind = rs), intent(in) :: a
    real(kind = rs), intent(out) :: berechnen
    integer(kind = rd), value :: k
    integer :: i
    if(k == 0) then
       berechnen = 1
    else if(k < 0) then
       k = abs(k)
    else
       berechnen = 1
       do i=0,k+1
          berechnen = berechnen * (a-i)
       end do
       berechnen = berechnen/fakultaet(k)
    end if
  end function berechnen
  
  pure function b1(x,y)
    real(kind = rs), intent(in) :: x
    integer, intent(in) :: y
    real(kind = rs) :: b1
    b1 = berechnen(x,y)
  end function b1

  pure function b2(xx,yy)
    real(kind = rs), intent(in) :: xx
    integer, intent(in) :: yy
    real(kind = rs) :: b2
    b2 = berechnen(xx,yy)
  end function b2
end module functions


program a8_15_2
  use precision
  use functions, only : binomial
  implicit none
  real(kind = rs) :: binom_s
  real(kind = rd) :: binom_d
  binom_s = binomial(15._rs, 3_is)
  write(*,*) binom_s
end program a8_15_2
