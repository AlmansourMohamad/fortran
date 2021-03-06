program Zwei_bemerkenswerte_Zahlen

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 3.3.1 :  Zwei_bemerkenswerte_Zahlen           |
!|                                                                      |
!|  Problem: find out the numbers where following statement is valid    |
!|                                                                      |
!|    abcdefgh = a**a + b**b + c**c +d**d + e**e + f**f + g**g + h**h   |
!|                                                                      |
!|         on the left side a,b,c,d,e,f,g,h are the digits of which     |
!|         the number consists                                          |
!|                                                                      |
!|                                                                      |
!|  In this version no n**n is calculated but taken from a vector       |
!|  zahlen, where all available n**n's are stored (0**0,.....,9**9)     |
!|  Without Leave loop early                                            |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter     :: ip = selected_int_kind(9)  ! specify precision 

   integer(kind=ip)       :: summe1 = 0
   integer(kind=ip)       :: summe2 = 0


   integer(kind=ip)       ::  i0,  i1,  i2,  i3,  i4,  i5,  i6,  i7
   integer(kind=ip)       :: ie0, ie1, ie2, ie3, ie4, ie5, ie6
   integer(kind=ip)       :: id0, id1, id2, id3, id4, id5, id6

   integer(kind=ip),dimension(0:9), parameter :: zahlen=(/ 0,1,4,27,256, 5**5, 6**6, 7**7, 8**8, 9**9 /)

   real   :: time_start, time_end, time_used

!--------------------------------------------------------------------------

!--------------------
!---> time start <---
!--------------------

   call cpu_time(time_start)

!---------------------------------------------
!---> 8 nested loops ( 1 for each digit ) <---
!---------------------------------------------

   do, i0=0,9

      ie0 = zahlen(i0)
      id0 = i0*10000000


!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

      do, i1=0,9

         ie1 = ie0 + zahlen (i1)
         id1 = id0 + i1*1000000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

         do, i2=0,9

            ie2 = ie1 + zahlen (i2)
            id2 = id1 + i2*100000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

            do, i3=0,9

               ie3 = ie2 + zahlen (i3)
               id3 = id2 + i3*10000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

               do, i4=0,9

                  ie4 = ie3 + zahlen (i4)
                  id4 = id3 + i4*1000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                  do, i5=0,9

                     ie5 = ie4 + zahlen (i5)
                     id5 = id4 + i5*100

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                     do, i6=0,9

                        ie6 = ie5 + zahlen (i6)
                        id6 = id5 + i6*10

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                        do, i7=0,9
       

!---> create the 2 sums to be compared 

                           Summe1 = ie6 + zahlen(i7)
                           Summe2 = id6 + i7

!---> equality ?

                           if ( Summe1 == Summe2 ) then 
                              write(*,*) 'Summe1 = Summe2 =',summe2
                           end if
       
!--<>------<>------<>------<>------<>------<>------<>------<>------<>------<>----

                        end do

                     end do

                  end do

               end do

            end do

         end do

      end do

   end do

!-------------------
!---> time used <---
!-------------------

   call cpu_time(time_end)

   time_used = time_end - time_start

   write(0,*)'===> Zeitverbrauch:',time_used,'Sekunden CPU'

end program Zwei_bemerkenswerte_Zahlen
