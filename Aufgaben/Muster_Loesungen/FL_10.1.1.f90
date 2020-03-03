
!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 10.1.1: Darstellung als Kettenbruch           |
!|                                                                      |
!|  Problem: Verwandeln Sie n-stellige Zahlen (z.B pi, e) in Ketten-    |
!|           brueche! Ansatz:                                           |
!|                                                                      |
!|        r = j0 + 1/(j1 + 1/(j2 + 1/(j3 + .......)))                   |
!|                                                                      |
!|                                                                      |
!+----------------------------------------------------------------------+

module precis

!-------------------------------------------------------------------------------
!---> enthaelt die Definition der benoetigten Genauigkeiten und Zahlentypen <---
!-------------------------------------------------------------------------------

  integer,parameter,public :: rp = selected_real_kind(13)       ! doppelte Genauigkeit
end module precis

!=<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>=

module kette

!--------------------------------------------
!---> enthaelt saemtliche Unterprogramme <---
!--------------------------------------------

  use precis, only: rp

  implicit none
  private
  public :: to_fraction, from_fraction               ! Freigabe der Namen der Unterroutinen

 contains

  subroutine to_fraction(x,v)

!+----------------------------------------------------------------------+
!|                                                                      |
!|  teilt die Zahl x in einen Kettenbruch auf und gibt die gefundenen   |
!|         Koeffizienten auf dem Vector v zurueck.                      |
!|                                                                      |
!+----------------------------------------------------------------------+

!-----------------------
!---> Deklarationen <---
!-----------------------

    real(kind=rp),intent(in)          :: x           
    integer,dimension(0:),intent(out) :: v

    real(kind=rp)                     :: y
    real(kind=rp)                     :: delta
    integer                           :: i,k

!------------------------------------------------------------------------  

    v = 0
    y = x

!--->
!---> Schleife ueber die laenge des Vektors
!--->

    do i=0,ubound(v,1)            
      k = y                              ! Integeranteil des augenblicklichen Restes 
      v(i) = k                           ! Speicherung im Vektor
      delta = y - real(k, rp)            ! Rest ohne Integeranteil
      if ( delta < 64*epsilon(y) ) then  ! Vermeiden von Division durch 0
          v(i+1:) = 0
          exit
      else
         y = 1.0_rp / delta    ! Berechnung des naechsten Restes 
      end if
    end do

  end subroutine to_fraction

!==========================================================================

  function from_fraction(v) result(erg)

!+----------------------------------------------------------------------+
!|                                                                      |
!|  berechnet aus dem Vektor mit den Koeffizienten die entspechende     |
!|                          Gleitkommazahl                              |
!|                                                                      |
!+----------------------------------------------------------------------+

!-----------------------
!---> DeKlarationen <---
!-----------------------

    integer,dimension(0:),intent(in) :: v          ! Inputvektor mit den Koeffizienten
    real(kind=rp)                    :: erg        ! Ergebnis (berechnete Gleitkommazahl
    integer                          :: n          ! Laenge des Vektors
    integer                          :: i          ! Loopindex

!---------------------------------------------------------------------------

    n = ubound(v,1)               ! Obere Grenze des Vektors

!--->
!---> Schleife ueber alle Komponenten des Vektors 
!--->

    do i = n, 0, -1
       if ( v(i) /= 0.0 ) exit
    end do

    erg = v(i)

    do i=i-1,0,-1
      erg = v(i) + 1/erg
    end do

 

  end function from_fraction

end module kette

!=<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>==<>=

program kettpi

!+----------------------------------------------------------------------+
!|                                                                      |
!|        Hauptprogramm      (Beschreibung siehe oben)                  |
!|                                                                      |
!+----------------------------------------------------------------------+


!-----------------------
!---> Deklarationen <---
!-----------------------

  use precis, only: rp
  use kette, only: to_fraction, from_fraction
  implicit none
  integer                 :: n 
  integer,dimension(:),allocatable :: v
  real(kind=rp)           :: pi

  character(len=80)       :: ads_message
  integer                 :: ads

!---------------------------------------------------------------------------
  write (unit=*, fmt="(a)", advance="no") "n = "
  read (unit=*, fmt=*) n

  allocate (v(0:n), stat = ads, errmsg = ads_message)
  if ( ads /= 0 ) then
     write (unit = 0, fmt = *) "Can't allocate array ""v"" with&
          & dimension ""(0:n)"":", trim(ads_message)
     stop
  endif

  pi = acos(-1.0_rp)
!  pi = 4.0_rp*atan(1.0_rp)

  write(unit=*,fmt=*) pi

  call to_fraction(pi,v)
  write(unit=*,fmt=*) v

  write(unit=*,fmt=*) "x = ", from_fraction(v)

end program kettpi
