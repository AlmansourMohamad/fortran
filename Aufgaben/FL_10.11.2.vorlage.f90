program a10_11_2
  implicit none
  integer,dimension(15),parameter :: einh = &             ! available notes and coins
       (/50000,20000,10000,5000,2000,1000,500,200,100,50,20,10,5,2,1/)
  integer,dimension(15)           :: sonder, wieviele     ! special requests, result
  integer :: i,ios
  real :: eingabe
  character(len=1):: bestaetigen
  integer,dimension(size(sonder)):: restult
  print *, "Geben Sie hier ein: "
  read(unit=*,fmt="(f5.2)") eingabe
!  print *, "Stimmt das? Bitte bestaetigen ! j/n"
!  read(*,"(a)") bestaetigen
!  print *, "Bis zu drei Sonderwuensche eintragen, Zahlen kleiner 10!"
  print "(9i5,6f6.2)", einh(:9)/100,einh(10:)/100.0
!  read(unit=*,fmt="(9i5,6i6)",iostat=ios) sonder

!  do i = 1, size(einh)
!     if(einh(i) < eingabe) then
!        result(i) = 0
!     else
!        result(

end program a10_11_2
