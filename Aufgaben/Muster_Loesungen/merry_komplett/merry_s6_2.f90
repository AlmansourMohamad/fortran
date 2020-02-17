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
!| Optimization: 1. logical-vector "digits"                                |
!|               2. intermediate results are calculated as soon as poss.   |
!|               3. all loops are programed as permutation                 |
!|               3. loop 10 and loop 9 are eliminated like 5.2             |
!|                                                                         |
!+-------------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

      implicit none

      integer  ::  s,  w,  r,  y,  a,  m,  e,  n,  p,  h
      integer  :: k0, k1, k2, k3, k4, k5, k6, k7
      integer  :: merry
      integer  :: mas
      integer  :: happy
      integer  :: new
      integer  :: year
      integer  :: mma
      integer  :: left_side, right_side
      integer  :: loopcount

      integer, dimension(0:9)  :: numbers = (/ 0,1,2,3,4,5,6,7,8,9 /)

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

      real             :: time_start, time_end, time_used

      integer          :: mmm, faktor = 10

!------------------------------------------------------------------------


!--------------------
!---> time start <---
!--------------------

      call cpu_time(time_start)


do, mmm=1,10*faktor

      if ( mmm < 11 ) then
         write(*,'(a)' ) &
          '------------------------------------------------------'

         write(*,'(a)' ) &
          'formula   m e r y a s h p n w -   result  -  loopcount'
      end if

      loopcount = 0 

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "m" <---

      loop_0: do, k0=0,9    
! write(*, '(a,i2,a,i10)' )'--info--> loop0 =',i0,', loopcount =',loopcount

        m = numbers (k0) 
        numbers (k0) = numbers (9)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "e" <---

        loop_1: do, k1=0,8

          e = numbers (k1)
          numbers (k1) = numbers (8)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "r" <---

          loop_2: do, k2=0,7

            r = numbers (k2)
            numbers (k2) = numbers (7)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "y" <---

            loop_3: do, k3=0,6

              y = numbers (k3)
              numbers (k3) = numbers (6)

              merry = zehntausender(m) + tausender(e) + hunderter_zehner(r) + y

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "a" <---

              loop_4: do, k4=0,5

                a = numbers (k4)
                numbers (k4) = numbers (5)

                year  = tausender(y) + hunderter(e) + zehner(a) + r

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "s" <---

                loop_5: do, k5=0,4

                  s = numbers (k5)
                  numbers (k5) = numbers (4)

!                 mas   = m*100   + a*10   + s   
                  mas = hunderter(m) + zehner(a) + s

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "h" <---

                  loop_6: do, k6=0,3

                    h = numbers (k6)
                    numbers (k6) = numbers (3) 

                    mma   = merry * mas + a

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "p" <---

                    loop_7: do, k7=0,2

                      p = numbers (k7)
                      numbers (k7) = numbers ( 2)

                      happy = h*10000 + a*1000 + p*110 + y

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "n" <---

                        n = numbers (1)

!---> loop for "w" <---
                        w = numbers (0)

                        loopcount = loopcount + 1

!                       new = n*100   + e*10   + w
                        new = hunderter(n) + zehner(e) + w

!--->
!---> 1st formula: merry x mas + a = happy * new - year + 1994
!--->
                        left_side  = mma
                        right_side = happy * new - year + 1994

                        if ( left_side .eq. right_side ) then
                           if ( mmm < 11 ) write(*, '(a,10i2,i12,i12)' )              &
                                '    1:   ',m,e,r,y,a,s,h,p,n,w, & 
                                left_side, loopcount

!--->
!---> 2nd formula: merry x mas + a + happy = new + year * 1994
!--->
                        else
                           left_side  = mma + happy
                           right_side = new + year * 1994
                         
                           if ( left_side .eq. right_side ) then
                              if ( mmm < 11 ) write(*, '(a,10i2,i12,i12)' )         &
                                   '    2:   ',m,e,r,y,a,s,h,p, &
                                   n,w,left_side, loopcount 
                           end if

                        end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "n" <---

                        n = numbers (0)

!---> loop for "w" <---
                        w = numbers (1)

                        loopcount = loopcount + 1

!                       new = n*100   + e*10   + w
                        new = hunderter(n) + zehner(e) + w

!--->
!---> 1st formula: merry x mas + a = happy * new - year + 1994
!--->
                        left_side  = mma
                        right_side = happy * new - year + 1994

                        if ( left_side .eq. right_side ) then
                           if ( mmm < 11 ) write(*, '(a,10i2,i12,i12)' )              &
                                '    1:   ',m,e,r,y,a,s,h,p,n,w, & 
                                left_side, loopcount

!--->
!---> 2nd formula: merry x mas + a + happy = new + year * 1994
!--->
                        else
                           left_side  = mma + happy
                           right_side = new + year * 1994
                         
                           if ( left_side .eq. right_side ) then
                              if ( mmm < 11 ) write(*, '(a,10i2,i12,i12)' )         &
                                   '    2:   ',m,e,r,y,a,s,h,p, &
                                   n,w,left_side, loopcount 
                           end if

                        end if

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                      numbers( 2) = numbers(k7)
                      numbers(k7) = p

                    end do loop_7

                    numbers( 3) = numbers(k6)
                    numbers(k6) = h

                  end do loop_6

                  numbers( 4) = numbers(k5)
                  numbers(k5) = s

                end do loop_5

                numbers( 5) = numbers(k4)
                numbers(k4) = a

              end do loop_4

              numbers( 6) = numbers(k3)
              numbers(k3) = y

            end do loop_3

            numbers( 7) = numbers(k2)
            numbers(k2) = r

          end do loop_2

        numbers( 8) = numbers(k1)
        numbers(k1) = e

        end do loop_1

        numbers( 9) = numbers(k0)
        numbers(k0) = m

      end do loop_0


      if ( mmm < 11 ) write(*, '(a,i21)' )'          after all loopcount   ',loopcount

end do

!-------------------
!---> time used <---
!-------------------

      call cpu_time(time_end)

      time_used = (time_end - time_start)/real(faktor)

      write(0,*)'===> Zeitverbrauch:',time_used,'Sekunden CPU'


      end program Merry_Christmas_1994
