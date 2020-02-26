
!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 8.4.1: Stichproben                            |
!|                                                                      |
!|  Problem: A subroutine minsuch (fun,a,b,n,xmin) has to approximate   |
!|           the location of a minimum "xmin" of the functio,  "fun"    |
!|           by n-times trying using a random generator.                |
!|                                                                      |
!+----------------------------------------------------------------------+

module genau

!-----------------------------------------
!---> precisions used in this program <---
!-----------------------------------------

  implicit none
  integer,parameter,public :: rp = selected_real_kind(13)
end module genau

module funktionen

!----------------------------------------
!---> functions used in this program <---
!----------------------------------------

!---------------------------------------
!---> declaration of the functions  <---
!---------------------------------------

  use genau, only: rp

  implicit none
  private
  public :: expo,ehyp

 contains

!------------------------------------------------------------------------

  function expo(x) result(y)

!--------------------------------------
!---> function exp**(x/2) + e**(-x) <--
!--------------------------------------

!----------------------
!---> declarations <---
!----------------------

    real(kind=rp),intent(in) :: x
    real(kind=rp)            :: y

!------------------------------------------------------------------------

    y = exp(x/2.0_rp) + exp(-x)

  end function expo


  function ehyp(x) result(y)

!--------------------------------------
!---> function exp**(x/2) + e**(-x) <--
!--------------------------------------

!----------------------
!---> declarations <---
!----------------------

    real(kind=rp),intent(in) :: x
    real(kind=rp)            :: y

!------------------------------------------------------------------------

    y = cosh( x ) 

  end function ehyp

end module funktionen

module min_suche

!---------------------------------------
!---> contains all subroutines used <---
!---------------------------------------

  implicit none
  public :: minsuch

 contains

  subroutine minsuch(fun,a,b,n,xmin)

!---------------------------------------------------------------
!---> subroutine for searching of the minimum of a function <---
!---------------------------------------------------------------

    use genau, only: rp

!--------------------
!---> interfaces <---
!--------------------

    interface
      function fun(x) result(y)
        use genau, only : rp
        real(kind=rp), intent(in) :: x
        real(kind=rp)             :: y
      end function fun
    end interface

!----------------------
!---> declarations <---
!----------------------

    real(kind=rp),intent(in) :: a,b
    integer,intent(in)       :: n
    real(kind=rp),intent(out):: xmin
    integer                  :: i
    real(kind=rp)            :: zuf,wert,wmin,xprobe

!------------------------------------------------------------------------

! y and y value
    wmin = huge(wmin)
    xmin = huge(xmin)

!--->
!---> loop over all random numbers wanted <---
!--->

    do i=1,n
      call random_number(zuf)
      xprobe = a + (b-a)*zuf           ! calculate x-value
      wert = fun(xprobe)               ! calculate correesponding function-value

!---> 
!---> minimum ?
!---> 

      if( wert < wmin ) then
        wmin = wert
        xmin = xprobe
      end if

    end do

  end subroutine minsuch
end module min_suche

program stichproben 

!----------------------
!---> declarations <---
!----------------------

  use genau, only :     rp
  use funktionen, only : expo,ehyp
  use min_suche, only  : minsuch

  real(kind=rp) :: erg1,erg2

!------------------------------------------------------------------------

  call minsuch(expo,-1.0_rp,1.0_rp,1000000,erg1)
  write(unit=*,fmt=*) erg1,expo(erg1)
  call minsuch(ehyp,-1.0_rp,1.0_rp,1000000,erg2)
  write(unit=*,fmt=*) erg2,ehyp(erg2)

end program stichproben 
