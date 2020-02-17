program Merry_Christmas_1994

!+-------------------------------------------------------------------------+
!|                                                                         | 
!|               T A S K :         Merry_Christmas_1994                    |
!|               =======                                                   |
!|                                                                         |
!|  Problem:        There are 2 formulas, where to find valid digits       |
!|                  for each letter                                        |
!|                                                                         |
!|-> 1st formula:   merry x mas + a = happy x new - year + 1994            |
!|                                                                         |
!|-> 2nd formula:   merry x mas + a + happy = new + year * 1994            |
!|                                                                         |
!|                  Each letter means a different digit                    |
!|                  x   means multiply                                     |
!|                                                                         |
!|                  write a program finding all possible combinations      |
!|                  of digits. If one found, don't stop, try to find out,  |
!|                  if there are more combinations possible. Both formulas |
!|                  should be handled in one common set of nested loops.   |
!|                                                                         |
!| Optimization: all if's are done as early as possible. The if regarding  |
!|               to i0 and i1 is stored in loop 1, etc.                    |
!|                                                                         |
!+-------------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter      :: ip  = selected_int_kind(8)

   integer(kind=ip)        :: i0, i1, i2, i3, i4, i5, i6, i7, i8, i9  ! loop-indices
   integer(kind=ip)        :: merry
   integer(kind=ip)        :: mas
   integer(kind=ip)        :: a
   integer(kind=ip)        :: happy
   integer(kind=ip)        :: new
   integer(kind=ip)        :: year
   integer(kind=ip)        :: left_side, right_side

   integer(kind=ip)        :: loopcount = 0

   real             :: time_start, time_end, time_used

   integer          :: mmm

!------------------------------------------------------------------------

!--------------------
!---> time start <---
!--------------------

  call cpu_time(time_start)


do,mmm=1,10


  write(*,'(a)' )'------------------------------------------------------'
  write(*,'(a)' )'formula   m e r y a s h p n w -   result  -  loopcount'
  loopcount = 0 

  loop0: do, i0=0,9    ! loop for "m"
! write(*, '(a,i2,a,i10)' )'--info--> loop0 =',i0,', loopcount =',loopcount

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

    loop1: do, i1=0,9    ! loop for "e"

           if ( i1 == i0 ) cycle loop1

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

      loop2: do, i2=0,9    ! loop for "r"

             if ( i2 == i0 ) cycle loop2
             if ( i2 == i1 ) cycle loop2

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

        loop3: do, i3=0,9    ! loop for "y"

               if ( i3 == i0 ) cycle loop3
               if ( i3 == i1 ) cycle loop3
               if ( i3 == i2 ) cycle loop3

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

          loop4: do, i4=0,9    ! loop for "a"

                 if ( i4 == i0 ) cycle loop4
                 if ( i4 == i1 ) cycle loop4
                 if ( i4 == i2 ) cycle loop4
                 if ( i4 == i3 ) cycle loop4

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

            loop5: do, i5=0,9    ! loop for "s"

                   if ( i5 == i0 ) cycle loop5
                   if ( i5 == i1 ) cycle loop5
                   if ( i5 == i2 ) cycle loop5
                   if ( i5 == i3 ) cycle loop5
                   if ( i5 == i4 ) cycle loop5

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

              loop6: do, i6=0,9    ! loop for "h"

                     if ( i6 == i0 ) cycle loop6
                     if ( i6 == i1 ) cycle loop6
                     if ( i6 == i2 ) cycle loop6
                     if ( i6 == i3 ) cycle loop6
                     if ( i6 == i4 ) cycle loop6
                     if ( i6 == i5 ) cycle loop6

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                loop7: do, i7=0,9    ! loop for "p"

                       if ( i7 == i0 ) cycle loop7
                       if ( i7 == i1 ) cycle loop7
                       if ( i7 == i2 ) cycle loop7
                       if ( i7 == i3 ) cycle loop7
                       if ( i7 == i4 ) cycle loop7
                       if ( i7 == i5 ) cycle loop7
                       if ( i7 == i6 ) cycle loop7

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                  loop8: do, i8=0,9    ! loop for "n"
  
                         if ( i8 == i0 ) cycle loop8
                         if ( i8 == i1 ) cycle loop8
                         if ( i8 == i2 ) cycle loop8
                         if ( i8 == i3 ) cycle loop8
                         if ( i8 == i4 ) cycle loop8
                         if ( i8 == i5 ) cycle loop8
                         if ( i8 == i6 ) cycle loop8
                         if ( i8 == i7 ) cycle loop8

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                    loop9: do, i9=0,9    ! loop for "w"

                           if ( i9 == i0 ) cycle loop9
                           if ( i9 == i1 ) cycle loop9
                           if ( i9 == i2 ) cycle loop9
                           if ( i9 == i3 ) cycle loop9
                           if ( i9 == i4 ) cycle loop9
                           if ( i9 == i5 ) cycle loop9
                           if ( i9 == i6 ) cycle loop9
                           if ( i9 == i7 ) cycle loop9
                           if ( i9 == i8 ) cycle loop9

!---

                           loopcount = loopcount + 1

                           merry = i0*10000 + i1*1000 + i2*110 + i3
                           mas   = i0*100   + i4*10   + i5  
                           a     = i4
                           happy = i6*10000 + i4*1000 + i7*110 + i3
                           new   = i8*100   + i1*10   + i9
                           year  = i3*1000  + i1*100  + i4 *10 + i2

!--->
!---> 1st formula: merry x mas + a = happy * new - year + 1994
!--->
                           left_side  = merry * mas + a 
                           right_side = happy * new - year + 1994

                           if ( left_side == right_side ) then
                              write(*, '(a,10i2,i12,i12)' ) &
                               '    1:   ',i0,i1,i2,i3,i4,i5,i6,i7,i8,i9, &
                                         left_side, loopcount

!--->
!---> 2nd formula: merry x mas + a + happy = new + year * 1994
!--->
                           else
                              left_side  = merry * mas + a + happy
                              right_side = new + year * 1994

                              if ( left_side == right_side ) then
                                 write(*, '(a,10i2,i12,i12)' ) &
                                  '    2:   ',i0,i1,i2,i3,i4,i5,i6,i7,i8,i9, &
                                                        left_side, loopcount
                              end if

                           end if

                    end do loop9

                  end do loop8 

                end do loop7

              end do loop6

            end do loop5

          end do loop4

        end do loop3

      end do loop2

    end do loop1

  end do loop0

  write(*, '(a,i21)' )'          after all loopcount   ',loopcount

end do

!-------------------
!---> time used <---
!-------------------

  call cpu_time(time_end)

  time_used = time_end - time_start

  write(0,*)'===> Zeitverbrauch:',time_used,'Sekunden CPU'


end program Merry_Christmas_1994
