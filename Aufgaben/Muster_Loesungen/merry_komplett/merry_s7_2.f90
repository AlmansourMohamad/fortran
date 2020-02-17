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
!| Optimization:    leave early: last digits left side /= right side       |
!|                               y,s,a,w,r needed, loop order changed      | 
!|                  removal of most inner loop like 5.2                    |       
!|                                                                         |
!+-------------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

      implicit none

      integer, parameter :: ip = selected_int_kind(8)
      integer (kind=ip)  :: k0, k1, k2, k3, k4, k5, k6, k7
      integer (kind=ip)  ::  s,  w,  r,  y,  a,  m,  e,  n,  p,  h
      integer (kind=ip)  :: ls1, ls2, rs1, rs2
      integer (kind=ip)  :: merry
      integer (kind=ip)  :: mas
      integer (kind=ip)  :: happy,happy1
      integer (kind=ip)  :: new
      integer (kind=ip)  :: year, year1, year2
      integer (kind=ip)  :: mma

      integer (kind=ip)  :: left_side, right_side
      integer (kind=ip)  :: loopcount
      integer (kind=ip), dimension(0:9)  :: numbers

      integer (kind=ip), dimension(0:9),parameter :: hunderter_zehner=   &
           (/ 0,110,220,330,440,550,660,770,880,990 /)
      integer (kind=ip), dimension(0:9),parameter :: zehner=             &
           (/ 0,10,20,30,40,50,60,70,80,90 /)
      integer (kind=ip), dimension(0:9),parameter :: hunderter=          &
           (/ 0,100,200,300,400,500,600,700,800,900 /)
      integer (kind=ip), dimension(0:9),parameter :: tausender=          &
           (/ 0,1000,2000,3000,4000,5000,6000,7000,8000,9000 /)
      integer (kind=ip), dimension(0:9),parameter :: zehntausender=      &
           (/ 0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000 /)

      real             :: time_start, time_end, time_used

      integer (kind=ip)          :: mmm, faktor=50

!------------------------------------------------------------------------

!--------------------
!---> time start <---
!--------------------

      call cpu_time(time_start)


do, mmm=1,10*faktor

      numbers = (/ 0,1,2,3,4,5,6,7,8,9 /)

      if ( mmm < 11 ) then
         write(*,'(a)' ) &
            '------------------------------------------------------'

         write(*,'(a)' ) &
            'formula   m e r y a s h p n w -   result  -  loopcount'
      end if

      loopcount = 0 

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----
!---> loop for "a" <---

      loop_0: do, k0=0,9    
! write(*, '(a,i2,a,i10)' )'--info--> loop0 =',s,', loopcount =',loopcount

        a = numbers (k0) 
        numbers (k0) = numbers (9)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "y" <---

        loop_1: do, k1=0,8

          y = numbers (k1)
          numbers (k1) = numbers (8)

          happy1 = a*1000 + y
!         happy1 = tausender(a) + y

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "r" <---

          loop_2: do, k2=0,7

            r = numbers (k2)
            numbers (k2) = numbers (7)

            year1  = 1000*y + 10*a + r
!           year1  = tausender (y) + zehner (a) + r

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "w" <---

            loop_3: do, k3=0,6

              w = numbers (k3)
              numbers (k3) = numbers (6)

              rs1 = mod (y*w-r+4,10)
              rs2 = mod (w+r*4,10)


!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "s" <---

              loop_4: do, k4=0,5

                s = numbers (k4)
                numbers (k4) = numbers (5)

                ls1 = mod (y*s+a,10)
                ls2 = mod (ls1+y,10)

                if ( ls1 /= rs1  .and.  ls2 /= rs2 ) then
                   numbers( 5) = numbers(k4)
                   numbers(k4) = s
                   cycle loop_4
                end if
     

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "m" <---

                loop_5: do, k5=0,4

                  m = numbers (k5)
                  numbers (k5) = numbers (4)

                  mas   = m*100   + a*10   + s   
!                 mas = hunderter(m) + zehner(a) + s

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "e" <---

                  loop_6: do, k6=0,3

                    e = numbers (k6)
                    numbers (k6) = numbers (3) 

                    merry = m*10000 + e*1000 + r*110 + y
!                   merry = zehntausender(m) + tausender(e) + hunderter_zehner(r) + y

                    year  = year1 + e*100
!                   year  = year1 + hunderter(e)

                    mma   = merry * mas + a

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "n" <---

                    loop_7: do, k7=0,2

                      n = numbers (k7)
                      numbers (k7) = numbers ( 2)

                      new = n*100   + e*10   + w
!                     new = hunderter(n) + zehner(e) + w

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "p" <---

                        p = numbers (1)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "h" <---
                        h = numbers (0)

                       loopcount = loopcount + 1

                        happy = happy1 + h*10000 + p*110
!                       happy = happy1 + zehntausender(h) + hunderter_zehner(p)

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
!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "p" <---

                        p = numbers ( 0)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

!---> loop for "h" <---
                        h = numbers ( 1)

!                       loopcount = loopcount + 1


                        happy = happy1 + h*10000 + p*110
!                       happy = happy1 + zehntausender(h) + hunderter_zehner(p)

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
                      numbers(k7) = n

                    end do loop_7

                    numbers( 3) = numbers(k6)
                    numbers(k6) = e

                  end do loop_6

                  numbers( 4) = numbers(k5)
                  numbers(k5) = m

                end do loop_5

                numbers( 5) = numbers(k4)
                numbers(k4) = s

              end do loop_4

              numbers( 6) = numbers(k3)
              numbers(k3) = w

            end do loop_3

            numbers( 7) = numbers(k2)
            numbers(k2) = r

          end do loop_2

        numbers( 8) = numbers(k1)
        numbers(k1) = y

        end do loop_1

        numbers( 9) = numbers(k0)
        numbers(k0) = a

      end do loop_0

      if ( mmm < 11 ) write(*, '(a,i21)' )'          after all loopcount   ',loopcount


end do


!-------------------
!---> time used <---
!-------------------

      call cpu_time(time_end)

      time_used = (time_end - time_start)/real(faktor)

      write(0,*)'===> Zeitverbrauch:',time_used,'Sekunden CPU'

      write(*, '(10i2)' ) numbers
      write(*, '(a,10i2)' ) 'kind =',ip


      end program Merry_Christmas_1994
