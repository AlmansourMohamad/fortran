module genau
   implicit none
   type, public :: arim
     real       :: arimittel
     real       :: fehler
   end type arim
end module genau

module berechnungen
    implicit none
    public:: berechnung
 contains
  function berechnung(w) result(erg)
    use genau, only :arim
    real,intent(in),dimension(:) :: w
    type(arim)                   :: erg
    integer                      :: n,i
    n=size(w)
    erg%arimittel = sum(w)/n
    erg%fehler    = sqrt( sum( (w-erg%arimittel)**2 ) / ((n-1)*n) )
  end function berechnung
end module berechnungen

program ari
  use genau,        only : arim
  use berechnungen, only : berechnung
  implicit none
  integer, parameter :: n=8
  real, dimension(n),parameter :: w=(/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0/)
  type(arim)         :: erg
  erg = berechnung(w)  
  write (*,*) erg
end program ari
