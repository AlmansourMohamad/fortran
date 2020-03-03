! Aufgabe: Klausur96
! F-Loesung


!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 10.1.1: Klausur 96                            |
!|                                                                      |
!|  Problem: calculate a function f(x,y) and show it via printplot -    |
!|                                                                      |
!+----------------------------------------------------------------------+

module plane

!----------------------------------
!---> contains all subroutines <---
!----------------------------------

  implicit none
  public :: fun_to_mat, print_plot

contains

  subroutine fun_to_mat(fun,gx,gy,mat)

!+----------------------------------------------------------------------+
!|                                                                      |
!| creates x- and y-values for the external function fun to be called   |
!|                                                                      |
!+----------------------------------------------------------------------+

!---------------------
!--- declarations <---
!---------------------

    real,intent(in),dimension(:)    :: gx,gy
    real,intent(out),dimension(:,:) :: mat

    interface
      function fun(x,y) result(fu)
        real, intent(in) :: x,y
        real             :: fu
      end function fun
    end interface

    real             :: ax,ex,ay,ey,hx,hy,x,y,dx,dy
    integer          :: i,j,nx,ny

!-----------------------------------------------------------------------

!---> find out the number of x- and y- values

    nx = size(mat,2)
    ny = size(mat,1)

!---> number of intervals between start and end 

    hx = nx 
    hy = ny

!---> deltas between 2 neighboured points 

    dx = (gx(2) - gx(1)) / hx 
    dy = (gy(2) - gy(1)) / hy 

!---> find the upper left point (start)

    ax = gx(1) + dx/2.
    ay = gy(2) - dy/2.


!---> create x- and y-values, fill matrix mat with function-values of fun 

    do j=1,nx
      x = ax + (j-1)*dx

      do i=1,ny
        y = ay - (i-1)*dy

        mat(i,j) = fun(x,y)      ! call function
      end do

    end do

  end subroutine fun_to_mat

!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

  subroutine print_plot(mat)

! Das Programm rechnet die Werte von mat
! in kleine Buchstaben um
! (a-niedrigste Werte, z-hoechste Werte).
! Die Buchstabenmatrix wird ausgedruckt.

    real,intent(in),dimension(:,:)  :: mat
    character(len=1),dimension(size(mat,1),size(mat,2)) :: plo
    real             :: mi,ma,near26
    integer          :: i

    mi = minval(mat)
    ma = maxval(mat)
    near26 = 26.0-3*spacing(26.0)
    plo = char( int( 97+((mat-mi)/(ma-mi))*near26 ) )

    do i = 1, size(mat,1)
      write(unit=*,fmt=*) plo(i,:)
    end do

  end subroutine print_plot

end module plane

!-<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>-

module ratform

!----------------------------------------
!---> contains the external function <---
!----------------------------------------

  implicit none
  public                                    :: ratfun
  real, dimension(:,:), allocatable, public :: a

 contains
   
   function ratfun(x,y) result(ratf)

!---------------------
!--- declarations <---
!---------------------

     real, intent(in) :: x,y
     real             :: xw,yw
     real             :: ratf
     integer          :: l1,u1,l2,u2,i,j

!-----------------------------------------------------------------------

!---> find out the bounds of a

     l1 = lbound(a,1)
     u1 = ubound(a,1)
     l2 = lbound(a,2)
     u2 = ubound(a,2)

     ratf = 0 

    xw = x
    yw = y

     do j=l2,u2

       do i=l1,u1
         ratf = ratf + a(i,j) * xw**i * yw**j
       end do

     end do

   end function ratfun

end module ratform

!-<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>--<>-

program klausur_96

!+----------------------------------------------------------------------+
!|                                                                      |
!| allocate matrix mat and coefficient matrix a and define range of     |
!|                          x- and y-values                             |
!|                                                                      |
!+----------------------------------------------------------------------+

  use ratform, only: ratfun, a
  use plane, only: fun_to_mat, print_plot

!--------------------
!--- declarations ---
!--------------------

  implicit none

  real,dimension(30,78) :: mat
  real,dimension(2),parameter :: gx = (/-1,1/), gy = (/-1,1/)

!-----------------------------------------------------------------------

  allocate (a(0:2,0:2))
  a = reshape( (/0,1,-1,1,1,0,-1,0,0 /), (/3,3/) )   ! initialize matrix a

  call fun_to_mat(ratfun,gx,gy,mat)

  call print_plot(mat)

end program klausur_96
