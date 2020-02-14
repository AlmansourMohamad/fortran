program aenderung_der_logik

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 2.1.1: Aenderung der Logik                    |
!|                                                                      |
!|  Problem: show via program the equality of                           |
!|            .not. ( a .or. b )     and      (.not.a  and .not. b )    |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none

  integer :: i, j             !  loop variables
  logical :: a, b, c          !  see above

!------------------------------------------------------------------------

!----------------------------------------------------
!---> check all possible combinations of a and b <---
!----------------------------------------------------

!--->
!---> outer loop over all possible values of a 
!--->

   do i = 0,1
     a = i==1         ! values of a: F,T

!--->
!---> inner loop over all possible values of b 
!--->

     do j = 0,1
       b = j==1       ! values of b: F,T

!--->
!---> c is .true. if ".not.(a.or.b)" is equal to  ".not.a .and. .not.b"
!--->
       c = (.not. (a .or. b)) .eqv. (.not.a .and. .not.b)
!--->
!---> output for the current combination of a and b
!--->
       write(unit=*, fmt=*) a, b, c
     end do

   end do

end program aenderung_der_logik

