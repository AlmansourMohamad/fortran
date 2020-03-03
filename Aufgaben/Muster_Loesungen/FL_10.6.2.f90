
!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 10.6.2: Endspurt                              |
!|                                                                      |
!|  Problem: solve a triangulized linear system of equations            |
!|                                                                      |
!+----------------------------------------------------------------------+


module solve
 implicit none
 public :: loesung
contains

!pure function loesung(a,b) result(loes)
 function loesung(a,b) result(loes)

!--------------------
!--- declarations ---
!--------------------

    real,dimension(:,:),intent(in) :: a
    real,dimension(:),intent(in)   :: b
    real,dimension(size(b,1))      :: loes
    real,dimension(size(b,1))      :: r
    integer                        :: i

!-----------------------------------------------------------------------

!---> store the right hand input vector onto an auxiliary vector

    r = b

!---------------------------------
!---> loop over all equations <---
!---------------------------------

    do i =size(b,1),1,-1

      loes(i) = r(i)/a(i,i)                  ! solution of the last current equation 
      r(:i-1) = r(:i-1) - a(:i-1,i)*loes(i)  ! eliminate the last colummn of the matrix a
                                             ! using the last current solution
    end do

  end function loesung

end module solve

!-<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>-

program endspurt

!+----------------------------------------------------------------------+
!|                                                                      |
!| create a testmatrix and call function loesung                        |
!|                                                                      |
!+----------------------------------------------------------------------+

!--------------------
!--- declarations ---
!--------------------


  use solve, only: loesung
  implicit none
  real,dimension(4,4),parameter :: a = &    ! initialize triangulized testmatrix a (4,4)
    reshape((/9.0,0.0,0.0,0.0,2.0,8.0,0.0,0.0, &
              0.5,1.5,5.0,0.0,3.0,-1.0,3.0,7.0/) , (/4,4/) )
  real,dimension(4),parameter ::   b = (/58,22,25,35/)  ! initialize right hand side

!-----------------------------------------------------------------------

  print *, loesung(a,b)

end program endspurt
