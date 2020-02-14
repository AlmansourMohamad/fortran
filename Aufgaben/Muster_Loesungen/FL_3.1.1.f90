program gewichtsabweichung

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 3.1.1: Gewichtsabweichung (BMI)               |
!|                                                                      |
!|  Problem: find out the weight difference from a standard weight      |
!|           supposition: weight  [g] = volume [cm**3]                  |
!|                                                                      |
!|           weight = tallness * width * depth                          |
!|           depth  = weight / (tallness * width)                       |
!|           width  = 2/9 * tallness                                    |
!|    ==>    depth  = weight / ( tallness**2 * 2/9 )                    |
!|                  = 9 * weight / ( 2 * tallness**2 )                  |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none

  integer :: Dicke, Gewicht, Laenge

!------------------------------------------------------------------------

!---> infinite loop
 
  do 
     write(*,*)'==> enter weight and tallness'

!---> read weight and tallness      

     read (unit=*, fmt=*) Gewicht,  Laenge
     write (unit=*, fmt=*) Gewicht,  Laenge

!---> calculate thickness 
     Dicke = (9 * Gewicht) / (2 * Laenge * Laenge)
     write (unit=*, fmt=*) Dicke

!-----------------------------
!---> implement the "BMI" <---
!-----------------------------
 
!---> solution with if-statements  - few cases 

     write(*,*)'========= Loesung mit if-Statements ========='

     if (Dicke == 10) then
       print *, " normalgewichtig"
     elseif (Dicke == 9 .or. Dicke == 11) then
       print *, " ertraegliche Abweichung"
     else 
       print *, " nicht ertraegliche Abweichung"
     endif
 
!---> solution with case-statements - many cases (faster)

     write(*,*)'======== Loesung mit case-Statements ========'

     select case (Dicke)
       case (:8) 
           print *, " ein bisschen (???) zu hager"
       case ( 9) 
           print *, " fast normal (etwas hager, aber ertraeglich)"
       case (10)  
           print *, " totaaaall noorrmmaaaaaallll!"
       case (11) 
           print *, " fast normal (etwas kurz fuer das Gewicht, aber ertraeglich)"
       case (12:)
           print *, " etwas (???) weniger waere mehr"
       end select

   end do

end program gewichtsabweichung
