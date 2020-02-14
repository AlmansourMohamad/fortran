program fakult

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 3.3.9: Non_plus_ultra                         |
!|                                                                      |
!|  Problem: write out side by side n and factorial n!                  |
!|           using an adjustable precision as far as available.         |
!|           The loop should reach to 30.                               |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none

  integer,parameter :: ip = selected_int_kind(9)  ! selectable precision
  integer,parameter :: loopmax=30                  ! maximum number of loops
  integer(kind=ip)  :: faku                        ! contains current factorial i!
  integer           :: i                           ! loopindex

!------------------------------------------------------------------------

  faku = 1_ip

  do i=1,loopmax

!---> if maximum available number of integertype of faku <---
!---> divided by i is larger than faku itself then faku  <---
!---> can be multiplied once more by i without overflow  <---
!---> i.e.  i*faku < huge(faku) ==> faku < huge(faku)/i  <---

    if ( huge(faku)/i > faku ) then    
       faku = faku*i
       print "(i3,i20)", i, faku
    else

!---> else leave loop

       exit
    end if

  end do

end program fakult
