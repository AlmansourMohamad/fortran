!
!  Aufgabe: Sprueche auf der Standardausgabe ausgeben
!
!  Autoren: T. Dickhaus
!           R. Kuelheim
!
!-----------------------------------------------------------------------
   program alfout

   use satzlaenge,   only : rl

   implicit none

   integer, parameter                 :: eof = -1
   character (len=*), parameter       :: fname1 = 'szd', fname2 = 'dzd'

   logical                            :: gibts
   character (len=rl)                 :: ausgabe
   integer                            :: i, n, zahl
   integer, dimension(:), allocatable :: zufall
   real                               :: zufallszahl

!-----------------------------------------------------------------------

!----> test existence of file containing the idioms <----

   inquire(file=fname2, exist=gibts)

   if ( gibts ) then

!---> open file containing state of randomgenerator

      open(unit=8, file=fname1, access='sequential', status='old',  &
           action='readwrite', form='unformatted')

!---> open file containing idioms

      open(unit=9, file=fname2, access='direct', status='old',  &
           recl=rl, action='read', form='formatted')

!---> initialize randomgenerator

      call random_seed(size=n)

      write(*,*)'Vektorgroesse =',n
      allocate(zufall(n))

      read(unit=8) zufall

      call random_seed(put=zufall)
      call random_number(zufallszahl)

!---> read number of idioms

      read(unit=9, fmt='(i3)', rec=1) i

!---> specify one idiom, read it and write it to stdout
!---> 1 + a + (b-a)*zufallszahl

      zahl = int( (i-1) * zufallszahl) + 2
      read(unit=9, fmt='(a)', rec=zahl) ausgabe

      write(*, fmt='(a)' ) ausgabe

!---> save state of randomgenerator for recall

      call random_seed(get=zufall)
      rewind(unit=8)
      write(unit=8)zufall
      deallocate(zufall)

      close(unit=8)
      close(unit=9)

   end if


   end program   alfout
