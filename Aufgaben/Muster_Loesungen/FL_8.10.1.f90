
!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 8.10.1: Schluesselwort parameter              |
!|                                                                      |
!|  Problem: the area of a triangle can be calculated using different   |
!|           specifications.                                            |
!|                                                                      |
!+----------------------------------------------------------------------+

module F_3eck                 ! etwas vorgegriffen, if's nicht F

!---------------------------------------------------------------------------------
!---> contains a function that calculates the area using the given parameters <---
!---------------------------------------------------------------------------------

!-------------------------------------
!---> declaration of the function <---
!-------------------------------------

  public :: f_ber 

contains 

  function f_ber(a,b,c,ha,hb,hc,alpha,beta,gamma) result(f)

!--------------------------------------------------------
!---> calculates the area using the given parameters <---
!--------------------------------------------------------

    implicit none

!----------------------
!---> declarations <---
!----------------------

    real,intent(in),optional :: a,b,c,ha,hb,hc,alpha,beta,gamma
    real :: f
    real :: s
    real, parameter :: pi=3.1415926

!----------------------------------------------------------------------

!---> 1st case: 3 sides given <---
    if(present(a) .and.present(b) .and. present(c) ) then
       s = (a+b+c)/2.0
       f =sqrt(s*(s-a)*(s-b)*(s-c))

!---> 2nd case: 1 side and the corresponding height given <---

    else if (present(a) .and. present(ha) ) then
       f=a*ha/2.0

    else if (present(b) .and. present(hb) ) then
       f=b*hb/2.0

    else if (present(c) .and. present(hc) ) then
       f=c*hc/2.0

!---> 3rd case: 2 sides and the angle between  given <---

    else if ( present(alpha) .and. present(b) .and. present(c)) then
       f=b*c*sin(alpha)/2.0

    else if ( present(beta) .and. present(a) .and. present(c)) then
       f=a*c*sin(beta)/2.0

    else if ( present(gamma) .and. present(b) .and. present(a)) then
       f=b*a*sin(gamma)/2.0

!---> 4th case: 2angles and a line given <---

    else if ( present(c) .and. present(alpha).and.present(beta)) then
       f=(c*c*sin(alpha)*sin(beta))/(2.0*sin(pi-alpha-beta))

    else if ( present(b) .and.  present(alpha).and.present(gamma)) then
       f=(b*b*sin(alpha)*sin(gamma))/(2.0*sin(pi-alpha-gamma))
       
    else if ( present(a) .and.  present(beta).and.present(gamma)) then
       f=(a*a*sin(beta)*sin(gamma))/(2.0*sin(pi-beta-gamma))
         
    else
       ! for pure function the write should not be done here, but outside
       write(*,*)'===> Error ===> area of the triangle could not be calculated'
       write(*,*)'                probably  not enough or wrong input-parameters'
       f = -1.0
    end if
      
  end function f_ber 

end module F_3eck        

!-----------------------------------------------------------------------

program keyw        

!+----------------------------------------------------------------------+
!|                                                                      |
!|       call function f_ber calculating the area of a triangle         |
!|                                                                      |
!|                    with different parameters                         |
!|                                                                      |
!+----------------------------------------------------------------------+

  use F_3eck,only: f_ber   ! expl. Schnittstelle zu f_ber 


!----------------------
!---> declarations <---
!----------------------

      real :: result  

!-----------------------------------------------------------------------

  result =  f_ber(a=3.0,b=4.0,c=5.0)
  write(*,*) result

  result =  f_ber(b=5.0,c=10.0)
  write(*,*) result

  result =  f_ber(b=4.0,c=5.0,alpha=1.0)
  write(*,*) result

  result =  f_ber(c=5.0,alpha=1.0,beta=1.0)
  write(*,*) result

end program keyw 
