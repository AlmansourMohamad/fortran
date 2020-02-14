program  euklid               

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 3.3.11: 3000 vor Christus                     |
!|                                                                      |
!|  Problem: program the Euclid's algorithm for finding the greatest    |
!|           common divisor of two integernumbers m and n.              |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none

  integer,parameter     :: l = selected_int_kind(9) ! eventuell 18
  integer(kind=l)       :: zae, nen, ggt, m, r
  integer               :: ios

!------------------------------------------------------------------------

!---> infinite loop until end of data (Ctrl d)

  do

    write(unit=*,fmt="(A)",advance="no") "Bitte Zaehler und Nenner eingeben! "
    read(unit=*,fmt=*,iostat=ios) zae, nen

    if (ios /= 0) exit

    m   = abs(zae)
    ggt = abs(nen)

!--> infinite loop 

    do
      r = modulo(m,ggt)
      if( r==0 ) exit       ! gcd found if r == 0  ==> leave loop

      m   = ggt
      ggt = r
    end do

    print *, zae,nen,ggt
  end do

end program euklid
