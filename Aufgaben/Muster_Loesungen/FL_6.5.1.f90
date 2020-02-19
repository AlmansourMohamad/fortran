! F-Loesung

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 6.5.1: Blick aufs Meer                        |
!|                                                                      |
!|  Problem: How far you can see from a tower, mountain or balloon?     |
!|           Find out the distance on the curved surface of the earth.  |
!|           The earth should be considered as a sphere having  a       |
!|           radius of 6367467 meter.                                   |
!|                                                                      |
!|                                                                      |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------


program Blick_aufs_Meer
  integer,parameter       :: rp = selected_real_kind(13)
  real(kind=rp),parameter :: radius = 6367467
  real(kind=rp)           :: phi,sichtweite,faktor,h
  integer                 :: i,j

!------------------------------------------------------------------------

!---> loop over all heights wanted (0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50...... 10000, 20000, 5000)

  write(unit=*,fmt="(t4,a,t18,a)") "Hoehe/m", "Sichtweite/km"
  write(unit=*,fmt="(t4,a,t18,a)") "=======", "============="

  do i = -1,4
    faktor = 10.0_rp**i    ! 0.1,1,10,100,1000,10000

    do j = 0,2
      h = (1 + j*j)*faktor   ! 1,2,5

    !--> angle between radius and radius+height
      phi = acos(radius/(radius+h))

     !--> range of sight corresponds related to the rule of proportion
      sichtweite = radius*phi/1000._rp  ! in Km

      write(unit=*,fmt="(f10.1,es20.6)") h,sichtweite

    end do
  end do

!--> Special case: 1 Quadrant overlook

  h = radius*(sqrt(2.0_rp)-1)
  phi = acos(radius/(radius+h))
  sichtweite = radius*phi/1000  ! in Km
  write(unit=*,fmt="(f10.1,es20.6)") h,sichtweite

end program Blick_aufs_Meer



