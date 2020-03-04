program a10_11_2
  implicit none
  integer,dimension(15),parameter :: einh = &             ! available notes and coins
       (/50000,20000,10000,5000,2000,1000,500,200,100,50,20,10,5,2,1/)
  integer,dimension(15)           :: sonder, wieviele     ! special requests, result
  integer :: j,ios
  real :: eingabe, comp
  character(len=1):: bestaetigen
  integer,dimension(size(sonder)):: res
  print *, "Geben Sie hier ein: "
  read(unit=*,fmt="(f5.2)") eingabe
  !  print *, "Stimmt das? Bitte bestaetigen ! j/n"
  !  read(*,"(a)") bestaetigen
  !  print *, "Bis zu drei Sonderwuensche eintragen, Zahlen kleiner 10!"
  print "(9i5,6f6.2)", einh(:9)/100,einh(10:)/100.0
  !  read(unit=*,fmt="(9i5,6i6)",iostat=ios) sonder
  comp = 0
  j = 0
  h = 100
  x = int(eingabe/h)

  do
     x = int(x/h)
     call set_elems(res,j,h,x)
     h = h/10
  end do

contains
  subroutine set_elems(res,j,h,x)
    integer :: x
    do
       if(x>5) then
          res(j) = res(j) + 1
          x = x-5*h
          if(x<5)  j = j + 1
       else if(x<5 .and. x>2) then
          res(j) = res(j) + 1
          x = x - 2*h
          if(x<2)  j = j + 1
       else ! if(x<2) then
          res(j) = res(j) + 1
          x = x - 1*h
          if(x<1) j = j + 1
       end if
       if(x == 0) exit
    end do


  end program a10_11_2
