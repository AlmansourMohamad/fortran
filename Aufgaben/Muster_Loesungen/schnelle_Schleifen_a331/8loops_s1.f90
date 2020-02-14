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
!|  In this version all if's for n**n are executed as soon as possible. |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter     :: ip = selected_int_kind(8)  ! specify precision 

   integer(kind=ip)    :: summe1 = 0
   integer(kind=ip)    :: summe2 = 0


   integer             ::  i0,  i1,  i2,  i3,  i4,  i5,  i6,  i7
   integer             :: is0, is1, is2, is3, is4, is5, is6, is7

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

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

      do, i1=0,9

         if ( i1 == 0 ) then
            is1 = 0
         else
            is1 = i1**i1 
         end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

         do, i2=0,9

            if ( i2 == 0 ) then
               is2 = 0
            else
               is2 = i2**i2 
            end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

            do, i3=0,9

               if ( i3 == 0 ) then
                  is3 = 0
               else
                  is3 = i3**i3 
               end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

               do, i4=0,9

                  if ( i4 == 0 ) then
                     is4 = 0
                  else
                     is4 = i4**i4
                  end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                  do, i5=0,9

                     if ( i5 == 0 ) then
                        is5 = 0
                     else
                        is5 = i5**i5
                     end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                     do, i6=0,9

                        if ( i6 == 0 ) then
                           is6 = 0
                        else
                           is6 = i6**i6
                        end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                        do, i7=0,9
       
                           if ( i7 == 0 ) then
                              is7 = 0
                           else
                              is7 = i7**i7
                           end if

!---> create the 2 sums to be compared 

                        Summe1 = is0 + is1 + is2 + is3 + is4 + is5 + is6 + is7
                        Summe2 = i0*10000000 + i1*1000000 + i2*100000 + i3*10000 &
                               + i4*1000     + i5*100     + i6*10     + i7

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
