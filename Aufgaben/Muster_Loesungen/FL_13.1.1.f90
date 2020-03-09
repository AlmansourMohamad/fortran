! Aufgabe: Wie das Leben so spielt
! F-Loesung fuer die professionelle Version von F, xlf90 Compiler darunter
! gamlif.data :
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                               
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                    
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                    
!   XX XX XX XX XX XX       XX XX XX XX XX XX       XX XX XX XX XX XX
!   XX XX XX XX XX XX       XX XX XX XX XX XX       XX XX XX XX XX XX
!           X                       X                       X
!   XX XX XX XX XX XX       XX XX XX XX XX XX       XX XX XX XX XX XX
!   XX XX XX XX XX XX       XX XX XX XX XX XX       XX XX XX XX XX XX
                                                                                    
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                    
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                               
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                    
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                    
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
!XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
                                                                                   
module field
  implicit none
  public :: inner,set_pointer,frame1,frame2,frame3,frame4,frame5
  interface
    subroutine system(c)
      character(len=*),intent(in) :: c
    end subroutine system
  end interface
  integer,parameter,public   :: k = selected_int_kind(2)
  integer,parameter,public   :: ny=30, nx=78
  character(len=1),parameter,public :: mark="X"
  integer(kind=k),target,public,dimension(0:ny+1,0:nx+1) :: x
  integer(kind=k),pointer,private :: co_le_up,co_le_lo,co_ri_up,co_ri_lo
  integer(kind=k),pointer,dimension(:,:),public  :: x00
  integer(kind=k),pointer,dimension(:,:),private :: xn,xnw,xw,xsw,xs,xso,xo,xno
  integer(kind=k),pointer,dimension(:),private   :: left,right,upper_m,lower_m, &
      left_m,right_m

contains

  subroutine set_pointer()

    !   Die Pointer setzen

    x00 => x(1:ny,1:nx)
    xs  => x(2:ny+1,1:nx)
    xso => x(2:ny+1,2:nx+1)
    xo  => x(1:ny,2:nx+1)
    xno => x(0:ny-1,2:nx+1)
    xn  => x(0:ny-1,1:nx)
    xnw => x(0:ny-1,0:nx-1)
    xw  => x(1:ny,0:nx-1)
    xsw => x(2:ny+1,0:nx-1)
    left => x(:,0)
    right => x(:,nx+1)
    left_m => x(1:ny,0)
    right_m => x(1:ny,nx+1)
    lower_m => x(ny+1,1:nx)
    upper_m => x(0,1:nx)
    co_le_up => x(0,0)
    co_le_lo => x(ny+1,0)
    co_ri_up => x(0,nx+1)
    co_ri_lo => x(ny+1,nx+1) 

  end subroutine set_pointer

  subroutine frame1() ! Zuchtschale
    logical,save :: first=.true.

    if (first) then
      left = 0_k
      right = 0_k
      lower_m = 0_k
      upper_m = 0_k
      first = .false.
    end if

  end subroutine frame1

  subroutine frame2() ! liegender Zylinder
    logical,save :: first=.true.

    if (first) then
      left = 0_k
      right = 0_k
      first = .false.
    end if
    lower_m = x00(1,1:nx)
    upper_m = x00(ny,1:nx)

  end subroutine frame2

  subroutine frame3() ! Moebius-Band, unten-oben verbunden
    logical,save :: first=.true.

    if (first) then
      left = 0_k
      right = 0_k
      first = .false.
    end if
    lower_m = x00(nx:1:-1,ny)
    upper_m = x00(nx:1:-1,1)

  end subroutine frame3

  subroutine frame4() ! Torus

    left_m = x00(nx,1:ny)
    right_m = x00(1,1:ny)
    lower_m = x00(:,ny)
    upper_m = x00(:,1)
    co_le_up = x00(nx,ny)
    co_ri_up = x00(1,ny)
    co_le_lo = x00(nx,1)
    co_ri_lo = x00(1,1)

  end subroutine frame4

  subroutine frame5() ! Kleinsche Flasche

    left_m = x00(nx,ny:1:-1)
    right_m = x00(1,ny:1:-1)
    lower_m = x00(nx:1:-1,ny)
    upper_m = x00(nx:1:-1,1)
    co_le_up = x00(1,ny)
    co_ri_up = x00(nx,ny)
    co_le_lo = x00(1,1)
    co_ri_lo = x00(nx,1)

  end subroutine frame5

  subroutine inner()
    integer(kind=k),dimension(ny,nx) :: su

    !   Das Innere berechnen

    su = x00+xs+xso+xo+xno+xn+xnw+xw+xsw
    where( su == 3 .or. su == 4)   ! nur hier kann Leben entstehen oder bleiben
      x00 = merge(1_k,x00,su==3)   ! Fall 3: Zelle lebt mit 2 Nachbarn oder ist tot mit drei->Leben, Fall 4: so wie vorher
      elsewhere
      x00 = 0_k
    end where

  end subroutine inner
end module field

program gamlif
! use f90_unix_proc
  use field,only: k,nx,ny,mark,x00,inner,set_pointer,frame1,frame2,frame3,&
      frame4,frame5
  implicit none
  character(len=1),dimension(nx) :: c
  character(len=7)               :: fo
  character(len=*),parameter     :: datei = "gamlif.data"
! Sun: merge mit character fuehrt zu Fehler
  !character(len=1)               :: outchar
!
  integer                        :: i,knd,ios,iter

  fo = "(???A1)"

  ! Die verwendeten Pointer setzen

  call set_pointer()

  ! Art einlesen

  print *, "Welche Art ? (1 Schale 2 Zylinder 3 Moebius 4 Torus 5 Klein)"
  read(unit=*,fmt=*,iostat=ios) knd
  if( ios /= 0 ) then
    print *, "fini"
    stop 
  end if

  ! Anfangswerte setzen ( Einlesen ) und Ausdrucken

  open(unit=1,file=datei,action="READ",status="OLD")
  write(unit=fo(2:4),fmt="(I3.3)") nx
  x00 = 0_k
  do i=1,ny
    read(unit=1,fmt=fo,iostat=ios) c
    if( ios /= 0 ) then
      exit
    end if
    write(unit=* ,fmt="(1X," // fo(2:) ) c
    x00(i,:) = merge(1_k,0_k,c==mark)
 !   print*, x00(i,:)
  end do

  ! Iterieren

  !do iter=1,10
  do
    call system("sleep 0.1")
    call system("clear")
    select case(knd)
    case(1)
      call frame1()
    case(2)
      call frame2()
    case(3)
      call frame3()
    case(4)
      call frame4()
    case(5)
      call frame5()
    case default
      print *,"Falsche Eingabe."
      stop 
    end select
    call inner()
    do i=1,ny
!     if (x00(:,i)==1_k) then
!       outchar="X"
!     else 
!       outchar=" "
!     endif
!     write(unit=*,fmt="(1X,"//fo(2:)) outchar
!SUN-err:  write(unit=*,fmt="(1X,"//fo(2:)) merge("X"," ",x00(:,i)==1_k)
      write(unit=*,fmt="(1X,"//fo(2:)) merge("X"," ",x00(i,:)==1_k)
    end do
  end do
end program gamlif
