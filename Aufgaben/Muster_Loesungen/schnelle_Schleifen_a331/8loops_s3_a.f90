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
!|  In this version all if's are executed as soon as possible.          |
!|  In addition subtotals are created as soon as possible.              |
!|  In addition no if's in the innermost loops for 0**0=0               |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter     :: ip = selected_int_kind(7)  ! specify precision 

   integer(kind=ip)       :: summe1 = 0
   integer(kind=ip)       :: summe2 = 0


   integer                ::  i0,  i1,  i2,  i3,  i4,  i5,  i6,  i7
   integer                :: is0, is1, is2, is3, is4, is5, is6, is7
   integer                :: ie0, ie1, ie2, ie3, ie4, ie5, ie6
   integer                :: id0, id1, id2, id3, id4, id5, id6

   integer                :: i

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

      if ( i0 == 0 ) then
         is0 = 0
      else
         is0 = i0**i0 
      end if

      ie0 = is0
      id0 = i0*10000000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

      do, i1=0,9

         if ( i1 == 0 ) then
            is1 = 0
         else
            is1 = i1**i1 
         end if

         ie1 = ie0 + is1
         id1 = id0 + i1*1000000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

         do, i2=0,9

            if ( i2 == 0 ) then
               is2 = 0
            else
               is2 = i2**i2 
            end if

            ie2 = ie1 + is2
            id2 = id1 + i2*100000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

            do, i3=0,9

               if ( i3 == 0 ) then
                  is3 = 0
               else
                  is3 = i3**i3 
               end if

               ie3 = ie2 + is3
               id3 = id2 + i3*10000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

               do, i4=0,9

                  if ( i4 == 0 ) then
                     is4 = 0
                  else
                     is4 = i4**i4
                  end if

                  ie4 = ie3 + is4
                  id4 = id3 + i4*1000

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                  do, i5=0,9

                     if ( i5 == 0 ) then
                        is5 = 0
                     else
                        is5 = i5**i5
                     end if

                     ie5 = ie4 + is5
                     id5 = id4 + i5*100

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                     do, i6=0,9

                        is6 = i6
           
                        do, i=2,i6
                          is6 = is6 * i6
                        end do

                        ie6 = ie5 + is6
                        id6 = id5 + i6*10

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                        do, i7=0,9
       
                           is7 = i7

                           do, i=2,i7
                              is7 = is7 * i7
                           end do

!---> create the 2 sums to be compared 

                           Summe1 = ie6 + is7
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
