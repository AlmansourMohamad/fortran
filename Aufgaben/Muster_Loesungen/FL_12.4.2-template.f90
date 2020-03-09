! Aufgabe: Bruchrechnung
! F-Loesung

module brueche




end module brueche
!=========================================================================
program bruchrechnung           

  ! Programm zur Bruchrechnung
  use brueche, b=>bruch
  implicit none

  type(b)           :: erg
  integer,parameter :: dp = selected_real_kind(13)
  real(kind=dp)     :: r

  erg = 3+1/(7+b(1,16))
  write(unit=*,fmt=*) erg, acos(-1.0_dp)
!  r = erg
!  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
!  erg = 3+1/(7+1/(15+1/(1+b(1,293))))
!  r = erg
!  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
!  erg = 4/(1+1/(3+1/(1+1/(1+1/(1+1/(15+b(1,2)))))))
!  r = erg
!  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
!  erg = 2+b(1,3)*(2+b(2,5)*(2+b(3,7)*(2+b(4,9)*(2+b(5,11)* &
!        (2+b(6,13)*(2+b(7,15)*(2+b(8,17)*(2+b(9,19)*(2+b(10,21)* &
!        (2+b(11,23)*2))))))))))
!  r = erg
!  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)

end program bruchrechnung
