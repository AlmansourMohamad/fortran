program kalkul_risiko

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 2.4.1: Kalkuliertes Risiko                    |
!|                                                                      |
!|  Problem: calculate carefully the amount of possibilties to fill     |
!|           a form of sweepstake (Lotto) (6 out of 49) according       |
!|           to the formula                                             |
!|                                                                      |
!|                  (49*47*46*45*44)/(1*2*3*4*5*6)                      |
!|                                                                      |
!|           use integer arithmetics; change - if possible - the        |
!|           sequence of the calculations                               |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------
 implicit none

 integer,parameter :: ip = selected_int_kind(18) 
! using this higher precision all calculations, but first, are o.k.

!  integer,parameter :: ip = selected_int_kind(9)

! bei nagf90 :  zweimal Integer  (KIND=3) overflow zur Compile-Zeit
! pgf90      :  zweimal falsche Ergebnisse 
! xlf90      :  zweimal falsche Ergebnisse 

  integer(kind=ip)  :: erg

!------------------------------------------------------------------------


!-----------------------------------------------------------
!---> zero step: calculation using the original sequence <---
!--->           all variables have the precision ip     <---
!-----------------------------------------------------------
  erg = (49*48*47*46*45*44)/(2*3*4*5*6)
  print *, erg     ! --> 2053351 , wrong

!-----------------------------------------------------------
!---> 1st step: calculation using the original sequence <---
!--->           all variables have the precision ip     <---
!-----------------------------------------------------------
  erg = (49_ip*48_ip*47_ip*46_ip*45_ip*44_ip)/(2_ip*3_ip*4_ip*5_ip*6_ip)
  print *, erg     ! --> 2053351 , wrong

!-----------------------------------------------------------
!---> 2nd step: calculation using the original sequence <---
!--->        only the 1st variable has precision ip     <---
!-----------------------------------------------------------
  erg = (49_ip*48*47*46*45*44)/(2_ip*3*4*5*6)
  print *, erg     ! --> 2053351 , wrong

!----------------------------------------------------------
!---> 3rd step: calculation using a different sequence <---
!--->           all variables have the precision ip     <---
!----------------------------------------------------------
  erg = 49_ip*48_ip/2_ip*47_ip/3_ip*46_ip/4_ip*45_ip/5_ip*44_ip/6_ip
  print *, erg     ! --> 13983816 , ok

!----------------------------------------------------------
!---> 4th step: calculation using a different sequence <---
!--->        only the 1st variable has precision ip     <---
!----------------------------------------------------------
  erg = 49_ip*48/2*47/3*46/4*45/5*44/6
  print *, erg     ! --> 13983816 , ok

end program kalkul_risiko
