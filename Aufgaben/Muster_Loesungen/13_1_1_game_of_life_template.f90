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
  public :: inner,set_pointer,frame1


  integer,parameter,public   :: k = selected_int_kind(2)

  integer,parameter,public   :: ny=30, nx=78

  integer(kind=k),target,public,dimension(0:ny+1,0:nx+1) :: x                    ! Gitter mit Rand
  
  integer(kind=k),pointer,dimension(:,:),public  :: x00                          ! Gitter
  integer(kind=k),pointer,dimension(:,:),private :: xn,xnw,xw,xsw,xs,xso,xo,xno  ! Nachbarn
  integer(kind=k),pointer,dimension(:),private   :: left,right,upper_m,lower_m   ! Ränder

contains

  subroutine set_pointer()

    !   Die Pointer setzen
    !   HIER PROGRAMMIEREN

    x00 => x(1:ny,1:nx)
    xs  => 
    xso => 
    xo  => 
    xno => 
    xn  => 
    xnw => 
    xw  => 
    xsw => 
    left => 
    right =>
    lower_m =>
    upper_m =>

  end subroutine set_pointer

  subroutine frame1() ! Zuchtschale
    logical,save :: first=.true.

    if (first) then
      left    = 0_k
      right   = 0_k
      lower_m = 0_k
      upper_m = 0_k
      first   = .false.
    end if

  end subroutine frame1

  subroutine inner()
    integer(kind=k),dimension(ny,nx) :: su   !Summenfeld

    !   Das Innere berechnen
    !   HIER PROGRAMMIEREN, x00 belegen


  end subroutine inner
end module field

program gamlif

  use field,only: k,nx,ny,x00,inner,set_pointer,frame1
  implicit none
  character(len=1),dimension(nx) :: c
  character(len=*),parameter     :: datei = "game_of_life.data"
 
  integer                        :: i,ios,iter

!------------------------------------------------------------------------

  ! Die verwendeten Pointer setzen

  call set_pointer()


  ! Anfangswerte setzen ( Einlesen ) und Ausdrucken

  open(unit=1,file=datei,action="READ",status="OLD")

  x00 = 0_k

  do i=1,ny
    read(unit=1,fmt='(78a1)',iostat=ios) c
    if( ios /= 0 ) then
      exit
    end if
    write(unit=* ,fmt="(1x,78a1)" ) c
    x00(i,:) = merge(1_k,0_k,c=='X')
 !   print*, x00(i,:)
  end do

  do
    call system("sleep 0.1")
    call system("clear")

    call frame1()

    call inner()

    do i=1,ny
      write(unit=*,fmt="(1x,78a1)") merge("X"," ",x00(i,:)==1_k)
    end do

  end do


end program gamlif
