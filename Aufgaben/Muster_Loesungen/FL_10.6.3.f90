
!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 10.6.3: Klausur 97                            |
!|                                                                      |
!|  Problem: calculation of a polynom-value                             |
!|                                                                      |
!+----------------------------------------------------------------------+


module polynom

  implicit none
  public   :: poly
  private  :: lpoly, ipoly, dpoly, spoly

!---> parameters for the different precisions <---

  integer,parameter,private :: dp = selected_real_kind(2*precision(1.0)), &
                               sp = kind(0.0), &
                               ip = selected_int_kind(9), &
                               lp = selected_int_kind(18)

  integer,public,parameter  :: nlim_poly = 8     ! limit for the Horner scheme

!----------------------------------------------------------------
!---> generic function poly and its appropriate subfunctions <---
!----------------------------------------------------------------

  interface poly
    module procedure lpoly, ipoly, dpoly, spoly
  end interface 

 contains

   function lpoly(a,x) result(p)              ! pure

!+----------------------------------------------------------------------+
!|                                                                      |
!|    function to calculate all in 8-byte-integer                       |
!|                                                                      |
!+----------------------------------------------------------------------+

!--------------------
!--- declarations ---
!--------------------

     integer(kind=lp),intent(in),dimension(0:) :: a
     integer(kind=lp),intent(in)               :: x
     integer(kind=lp)                          :: p
     integer(kind=lp),allocatable,dimension(:) :: b
     integer(kind=lp)                          :: xp
     integer                                   :: i,n,j

!+----------------------------------------------------------------------+

     n = ubound(a,1)

!---> up to nlim grade use Horner scheme <---

     if( n < nlim_poly ) then
       p = a(n)

       do i=n-1,0,-1
         p = a(i) + x*p

       end do

!---> beyond this limit use a parallel vector reduction algorithm <---

     else 
       allocate( b(0:n+1) )
       b(0:n) = a
       b(n+1) = 0
       j = n
       xp = x
 
       do 
         j = j/2
         i = 2*j
         b(:j) = b(0:i:2) +xp*b(1:i+1:2)

         if( j == 0 ) then
           exit
         end if

         b(j+1) = 0
         xp = xp**2
       end do

       p = b(0)
       deallocate( b )
     end if

   end function lpoly

!-+-+-+--+-+--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

   function ipoly(a,x) result(p) ! pure
     integer(kind=ip),intent(in),dimension(0:) :: a
     integer(kind=ip),intent(in)               :: x
     integer(kind=ip)                          :: p
     integer(kind=ip),allocatable,dimension(:) :: b
     integer(kind=ip)                          :: xp
     integer                                   :: i,n,j

     n = ubound(a,1)
     if( n < nlim_poly ) then ! Horner
       p = a(n)
       do i=n-1,0,-1
         p = a(i) + x*p
       end do
     else                ! parallel
       allocate( b(0:n+1) )
       b(0:n) = a
       b(n+1) = 0
       j = n
       xp = x
       do 
         j = j/2
         i = 2*j
         b(:j) = b(0:i:2) +xp*b(1:i+1:2)
         if( j == 0 ) then
           exit
         end if
        b(j+1) = 0
        xp = xp**2
      end do
      p = b(0)
      deallocate( b )
    end if

  end function ipoly

  function dpoly(a,x) result(p) ! pure
    real(kind=dp),intent(in),dimension(0:) :: a
    real(kind=dp),intent(in)               :: x
    real(kind=dp)                          :: p
    real(kind=dp),allocatable,dimension(:) :: b
    real(kind=dp)                          :: xp
    integer                                :: i,n,j

    n = ubound(a,1)
    if( n < nlim_poly ) then ! Horner
      p = a(n)
      do i=n-1,0,-1
        p = a(i) + x*p
      end do
    else                ! parallel
      allocate( b(0:n+1) )
      b(0:n) = a
      b(n+1) = 0
      j = n
      xp = x
      do 
        j = j/2
        i = 2*j
        b(:j) = b(0:i:2) +xp*b(1:i+1:2)
        if( j == 0 ) then
          exit
        end if
        b(j+1) = 0
        xp = xp**2
      end do
      p = b(0)
      deallocate( b )
    end if

  end function dpoly

  function spoly(a,x) result(p)  ! pure
    real(kind=sp),intent(in),dimension(0:) :: a
    real(kind=sp),intent(in)               :: x
    real(kind=sp)                          :: p
    real(kind=sp),allocatable,dimension(:) :: b
    real(kind=sp)                          :: xp
    integer                                :: i,n,j

    n = ubound(a,1)
    if( n < nlim_poly ) then ! Horner
      p = a(n)
      do i=n-1,0,-1
        p = a(i) + x*p
      end do
    else                ! parallel
      allocate( b(0:n+1) )
      b(0:n) = a
      b(n+1) = 0
      j = n
      xp = x
      do 
        j = j/2
        i = 2*j
        b(:j) = b(0:i:2) +xp*b(1:i+1:2)
        if( j == 0 ) then
          exit
        end if
        b(j+1) = 0
        xp = xp**2
      end do
      p = b(0)
      deallocate( b )
    end if

  end function spoly

end module polynom
program demo
  use polynom,only : poly, nlim_poly
  implicit none
  integer,parameter :: lp = selected_int_kind(9)
  integer,parameter :: dp = selected_real_kind(13)
  integer(kind=lp),parameter,dimension(0:4) :: nen = (/ 15,194,712,1024,512 /)
  integer(kind=lp),parameter,dimension(0:2) :: zae = (/ 47, 151, 120 /)
  integer(kind=lp)                          :: i
  integer,parameter                         :: n = 11
  real(kind=dp),dimension(0:n)              :: coef
  real(kind=dp)                             :: pot16

  ! nlim_poly = 16 ! Man kann, wenn man will, nlim_poly aendern.
  pot16 = 1
  do i=0,n
    coef(i) = poly(zae,i)/(poly(nen,i)*pot16)
    pot16 = pot16*16
  end do
  print *, poly(coef,1.0_dp)

end program demo
