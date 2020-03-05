!
!  Aufgabe: Sprueche einlesen und in Datei ablegen
!
!  Autor: T. Dickhaus
!

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 11.5.1: Alfs Sprueche                         |
!|                                                                      |
!|  Problem: create a number of slogans and write it onto a file        |
!|                                                                      |
!+----------------------------------------------------------------------+


   program alfin

!--------------------
!--- declarations ---
!--------------------

   use satzlaenge,   only : rl

   implicit none

   integer, parameter                 :: eof = -1
   character (len=*), parameter       :: fname = 'dzd'

   logical                            :: gibts
   character (len=rl)                 :: zeile
   integer                            :: i, ios, n
   integer, dimension(:), allocatable :: zufall

!-----------------------------------------------------------------------

!---> test existence of file <---

   inquire(file=fname, exist=gibts)

!---> file exists ==> read number of stored slogans 

   if ( gibts ) then
      open(unit=8, file=fname, access='direct', status='old',  &
           recl=rl, action='readwrite', form='formatted')
      read(unit=8, fmt='(i3)', rec=1) i
      i = i + 1

!---> file does not exist ==> created new, number of stored slogans set to 0 

   else
      open(unit=8, file=fname, access='direct', status='new',  &
           recl=rl, action='readwrite', form='formatted')
      write(unit=8, fmt='(i3)', rec=1) 1
      i = 2

!---> create a defined start-state of random number generator and 
!---> write it tio file "szd"

!      call random_seed()
      call random_seed(size=n)
      allocate(zufall(n))
      call random_seed(get=zufall)
      open(unit=9, file='szd', access='sequential', status='new',  &
           action='write', form='unformatted')
      write(unit=9) zufall
      deallocate(zufall)
   end if

!---> read new slogans and write them to file "dzd"

   do
      read(unit=*,fmt='(a)', iostat=ios) zeile

      if ( ios /= 0 ) then
         exit
      end if

      write(unit=8, fmt='(a)', rec=i) zeile
      i = i + 1
   end do

!---> write total number of slogans stoerd onto file "dzd"

   write(unit=8, fmt='(i3)', rec=1) i - 1
   close(unit=8)
   close(unit=9)

   end program   alfin
