program verstanden

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 2.7.1: Verstanden ?                           |
!|                                                                      |
!|  Problem: which values the variables have after the execution of     |
!|           the read-statements                                        |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none

  integer :: i,j,k,l,m,n
  real    :: g

!------------------------------------------------------------------------

!---> execute all read statments    

  read(unit=*, fmt=*) i,j
  read(unit=*, fmt=*) k
  read(unit=*, fmt=*) l,m,n
  read(unit=*, fmt=*) 
  read(unit=*, fmt=*) g

!---> write out all read variables

  write(*,*) i,j
  write(*,*) k
  write(*,*) l,m,n
  write(*,*) g

!------------------------------------------------------------------------

! Input data
!1 2 3 4      2 values are read, additionals are read
! 5 6 7       ditto
!8 9          too few values, next record is also taken
!10 11
!12           skipped, because no variable list
!             not enough values, continuing with next line
!13.8

!------------------------------------------------------------------------

! Output 
!1 2
!5
!8 9 10
!13.8

end program verstanden
