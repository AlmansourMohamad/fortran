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
!|                  whether there are more combinations possible. Both     |
!|                  formulas should be handled in one common set of        |
!|                  nested loops.                                          |
!|                                                                         |
!| Optimization: 1. logcical-vector "digits"                               |
!|               2. intermediate results are calculated as soon as poss.   |
!|               3. most inner loop is replaced by a subtraction           |
!|               4. the last 3 digits are found in loop7a and stored on    |
!|                  a vector. With these 3 numbers all permutations are    |
!|                  done via vector numbers and stored to i7, i8, and i9.  |
!|                                                                         |
!+-------------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter      :: ip  = selected_int_kind(8)

   integer(kind=ip)        :: lll
   integer(kind=ip)        :: i0, i1, i2, i3, i4, i5, i6, i7, i8, i9  ! loop-indices
   integer(kind=ip)        :: merry
   integer(kind=ip)        :: mas
   integer(kind=ip)        :: a
   integer(kind=ip)        :: happ, happ1, happy
   integer(kind=ip)        :: new
   integer(kind=ip)        :: year
   integer(kind=ip)        :: mma
   integer(kind=ip)        :: left_side, right_side

   integer(kind=ip)         :: s1, s2, s3, s4, s5, s6, s7

   integer(kind=ip)        :: loopcount = 0

   logical,dimension(0:9)  :: digits=.false.

   integer, dimension(0:9),parameter :: hunderter_zehner=   &
        (/ 0,110,220,330,440,550,660,770,880,990 /)
   integer, dimension(0:9),parameter :: zehner=             &
        (/ 0,10,20,30,40,50,60,70,80,90 /)
   integer, dimension(0:9),parameter :: hunderter=          &
        (/ 0,100,200,300,400,500,600,700,800,900 /)
   integer, dimension(0:9),parameter :: tausender=          &
        (/ 0,1000,2000,3000,4000,5000,6000,7000,8000,9000 /)
   integer, dimension(0:9),parameter :: zehntausender=      &
        (/ 0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000 /)

   integer(kind=ip)                 :: k7, n7
   integer(kind=ip)                 :: anz
   integer(kind=ip), dimension(3)   :: numbers
   integer(kind=ip), dimension(3,6) :: order = reshape ((/ 1,2,3,  1,3,2,  2,1,3, &

                                               2,3,1,  3,1,2,  3,2,1 /) , (/ 3,6 /) )

   real             :: time_start, time_end, time_used

   integer          :: mmm

!------------------------------------------------------------------------

!--------------------
!---> time start <---
!--------------------

  call cpu_time(time_start)


do, mmm=1,10

  write(*,'(a)' )'------------------------------------------------------'
  write(*,'(a)' )'formula   m e r y a s h p n w -   result  -  loopcount'
  loopcount = 0 

!outerloop: do, lll=1,10

  loop0: do, i0=0,9    ! loop for "m"
         digits (i0) = .true.
! write(*, '(a,i2,a,i10)' )'--info--> loop0 =',i0,', loopcount =',loopcount

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

    loop1: do, i1=0,9    ! loop for "e"

           if ( digits(i1) ) cycle loop1
           digits (i1) = .true.

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

      loop2: do, i2=0,9    ! loop for "r"

             if ( digits(i2) ) cycle loop2
             digits (i2) = .true.

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

        loop3: do, i3=0,9    ! loop for "y"

               if ( digits(i3) ) cycle loop3
               digits (i3) = .true.
               merry = zehntausender(i0) + tausender(i1) + hunderter_zehner(i2) + i3
 
!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

          loop4: do, i4=0,9    ! loop for "a"

                 if ( digits(i4) ) cycle loop4
                 digits (i4) = .true.
                 year  = tausender(i3) + hunderter(i1) + zehner(i4) + i2
                 a     = i4

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

            loop5: do, i5=0,9    ! loop for "s"

                   if ( digits(i5) ) cycle loop5
                   digits (i5) = .true.
                   mas   = hunderter(i0) + zehner(i4) + i5 
                   mma   = merry * mas   + a 

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

              loop6: do, i6=0,9    ! loop for "h"

                     if ( digits(i6) ) cycle loop6
                     digits (i6) = .true.
                     happ =  zehntausender(i6) + tausender(i4) + i3

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                     anz = 0

!---> find out remaining 3 digits  and store it to vector "numbers"

                     loop7a: do, k7=0,9    
               
                       if ( .not. digits(k7) ) then
                          anz           = anz + 1
                          numbers (anz) = k7
                          if ( anz >= 3 ) exit loop7a
                       end if

                     end do loop7a

!---> loop over all possible variations of "numbers"
 

                     loop7b: do, n7=1,6
                       i7 = numbers(order(1,n7))
                       i8 = numbers(order(2,n7))
                       i9 = numbers(order(3,n7))
 
!---

                       loopcount = loopcount + 1

                       happy = happ  + hunderter_zehner(i7)
                       new   = hunderter(i8)+ zehner(i1)  + i9

!--->
!---> 1st formula: merry x mas + a = happy * new - year + 1994
!--->
                       left_side  = mma
                       right_side = happy * new - year + 1994

                       if ( left_side == right_side ) then
                          write(*, '(a,10i2,i12,i12)' ) &
                           '    1:   ',i0,i1,i2,i3,i4,i5,i6,i7,i8,i9, &
                                     left_side, loopcount

!--->
!---> 2nd formula: merry x mas + a + happy = new + year * 1994
!--->
                       else
                          left_side  = mma + happy
                          right_side = new + year * 1994

                          if ( left_side == right_side ) then
                             write(*, '(a,10i2,i12,i12)' ) &
                              '    2:   ',i0,i1,i2,i3,i4,i5,i6,i7,i8,i9, &
                                        left_side, loopcount
                          end if

                       end if

                end do loop7b

              digits (i6) = .false.
              end do loop6

            digits (i5) = .false.
            end do loop5

          digits (i4) = .false.
          end do loop4

        digits (i3) = .false.
        end do loop3

      digits (i2) = .false.
      end do loop2

    digits (i1) = .false.
    end do loop1

  digits (i0) = .false.
  end do loop0

!end do outerloop

  write(*, '(a,i21)' )'          after all loopcount   ',loopcount

end do

!-------------------
!---> time used <---
!-------------------

  call cpu_time(time_end)

  time_used = time_end - time_start

  write(0,*)'===> Zeitverbrauch:',time_used,'Sekunden CPU'


end program Merry_Christmas_1994






