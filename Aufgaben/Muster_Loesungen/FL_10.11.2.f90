program Geld_regiert_die_Welt

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 10.11.2: Geld regiert die Welt                |
!|                                                                      |
!|  Problem: Write a code for an ATM (Automatic-tele-machine or         |
!|           "Any Time Money"                                           |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  implicit none
  integer,parameter               :: rp = selected_real_kind(13)   ! precision

  real(kind=rp)                   :: Euro                 ! amount of money wanted
  character(len=1)                :: c                    ! read answer (j or n)
  integer, parameter              :: MaxFehler = 3        ! max. no. of errors allowed
  integer, parameter              :: MaxCent = 1000000    ! max. Cents
  integer,dimension(15),parameter :: einh = &             ! available notes and coins
    (/50000,20000,10000,5000,2000,1000,500,200,100,50,20,10,5,2,1/)
  integer,dimension(15)           :: sonder, wieviele     ! special requests, result
  integer                         :: i                    ! loop-variable
  integer                         :: ios                  ! i/o-error-indicator  
  integer                         :: Cent                 ! money wanted in Cent 
  integer                         :: Cent_sonder          ! special money requested in Cent 

!------------------------------------------------------------------------

!-------------------------------------------
!---> get the amount of money requested <---
!-------------------------------------------

  do i=1,MaxFehler
    print "(a)", "Wieviel Euro wollen Sie? "
    print "(a)", "Zwei Stellen hinter dem Dezimalpunkt fuer Cent!"
    write(unit=*,fmt="(a)",advance="no") "Geben Sie hier ein: "
    read(unit=*,fmt=*,iostat=ios) Euro

    if ( ios /= 0 ) then         ! Returncode from read
      stop
    end if

!---> amount bigger than maximum possible integernumber

    if ( nint(Euro*100) > MaxCent ) then
      print *, "Zahl zu gross!"
      cycle
    end if

!---> Input echo data check

    print "(a,f16.2)", "Sie gaben ein: ",Euro
    print *, "Stimmt das? Bitte bestaetigen !  j/n"
    read *, c

    if (scan(c,'jyJY') > 0 ) then 
      exit
    end if

  end do

!---> maximum allowed errors exceeded ?

  if ( i > MaxFehler ) then
    print *, "Zu viele Fehler!"
    stop
  end if

!--------------------------------
!---> enter special requests <---
!--------------------------------

  Cent = nint(Euro*100)               ! amount of requested money in Cent

  do i = 1 , MaxFehler
    print *, "Bis zu drei Sonderwuensche eintragen, Zahlen kleiner 10!"
    print "(9i5,6f6.2)", einh(:9)/100,einh(10:)/100.0
    read(unit=*,fmt="(9i5,6i6)",iostat=ios) sonder

    if ( ios /= 0 ) then        ! Returncode from read
      stop
    end if

!---> too many special requests  ( > 3 )

    if ( count( sonder /= 0 ) > 3 ) then
      print *, "----- Zu viele Sonderwuensche! -----"
      cycle
    end if

     if ( any ( sonder > 9 .or. sonder < 0 ) ) then
       print  *, "----- Falsche Anzahl bei einzelnen Sonderwuenschen! -----"
       cycle
     end if

    Cent_sonder = dot_product(sonder,einh)    ! amount of special request in Cent

!--->  amount of special request bigger than total amount requested

    if ( Cent_sonder > Cent ) then    
      print *, "----- Zu grosse Sonderwuensche! (Gesamtsumme) -----"
      cycle
    end if

    exit
  end do

!---> maximum allowed errors exceeded ?

  if( i > MaxFehler ) then 
    print *, "Zu viele Fehler!"
    stop
  end if

!--->                  
!---> amount of money wanted after deduction of the amount of special request
!--->

  Cent = Cent - Cent_sonder 

!------------------------------------------------------------------
!---> get the number of notes and coins excl. special requests <---
!------------------------------------------------------------------

  do i=1,15
    wieviele(i)  = Cent/einh(i)          ! Wieviele ohne Sonderwuensche
    Cent = Cent - wieviele(i)*einh(i)
  end do

!----------------------------------
!---> add the special requests <---
!----------------------------------

  wieviele = wieviele + sonder

!----------------
!---> output <---
!----------------

  print *, "Sie bekommen:"
  print "(9i5,6f6.2)", einh(:9)/100,einh(10:)/100.0
  print "(9i5,6i6)", wieviele
  print *, "Davon wegen Sonderwuenschen:"
  print "(9i5,6i6)", sonder

end program Geld_regiert_die_Welt
