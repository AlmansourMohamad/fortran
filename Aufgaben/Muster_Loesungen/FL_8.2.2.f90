module hilfen

  implicit none
  public :: ggt, kuerze
  integer,parameter,private :: l = selected_int_kind(9)

 contains

  function ggt(zae,nen) result(gcd)

    ! Funktion zur Berechnung des groessten gemeinsamen Teilers von
    ! zae und nen ( Euklids Algorithmus)
    ! Mit einem Modul und USE koennte man die Zahlenlaenge
    ! fuer alle beteiligten Programme an nur einer Stelle
    ! vereinbaren.

    integer(kind=l),intent(in) :: zae,nen
    integer(kind=l)            :: gcd
    integer(kind=l)            :: m,r

    m = abs(zae)
    gcd = abs(nen)
    do
      r = modulo(m,gcd)
      if( r==0 ) then
        exit
      end if
      m = gcd
      gcd = r
    end do

  end function ggt

  subroutine kuerze(zae,nen) 

    ! Unterprogramm zum Kuerzen eines Bruches
    ! Benoetigte Funktion : ggt

    ! Mit einem Modul und USE koennte man die Zahlenlaenge
    ! fuer alle beteiligten Programme an nur einer Stelle
    ! vereinbaren.

    integer(kind=l),intent(in out) :: zae,nen
    integer(kind=l)                :: teiler

    teiler = ggt(zae,nen)
    zae = zae/teiler
    nen = nen/teiler

  end subroutine kuerze

end module hilfen

program bruchrechnung      

  ! Programm zur Bruchrechnung
  ! Benoetigte Unterprogramme : ggt und kuerze

  use hilfen, only: kuerze, ggt
  implicit none

  ! Mit einem Modul und USE koennte man die Zahlenlaenge
  ! fuer alle beteiligten Programme an nur einer Stelle
  ! vereinbaren.

  integer,parameter :: l = selected_int_kind(9)
  integer(kind=l)   :: z1,n1,z2,n2,z3,n3,t,n2dt
  integer           :: ios
  character(len=1)  :: op

  do
    write(unit=*,fmt=*) "Eingabe : z1 n1 op z2 n2"
    read(unit=*,fmt=*,iostat=ios)  z1, n1, op, z2, n2
    if ( ios/=0 ) then 
      exit
    end if
    if ( n1==0 .or. n2==0) then
      write(unit=*,fmt=*) "Einer der Nenner war 0!"
      cycle
    end if
    call kuerze(z1,n1)
    call kuerze(z2,n2)

    select case( op )
     case( "*" )
       call kuerze(z1,n2) ! ueberkreuz
       call kuerze(z2,n1) ! ueberkreuz
       z3 = z1*z2
       n3 = n1*n2
     case( ":" )
       if( z2==0 ) then
         write(unit=*,fmt=*) "Der Zaehler des Divisors war 0!"
         cycle
       end if
       call kuerze(z1,z2)
       call kuerze(n1,n2)
       z3 = z1*n2
       n3 = z2*n1
     case( "+" , "-" )
       if ( op=="-" ) then
         z2 = -z2
       end if
       t = ggt(n1,n2)
       n2dt = n2/t
       z3 = z1*n2dt + z2*(n1/t)
       n3 = n1*n2dt
       call kuerze(z3,n3)
     case default
       write(unit=*,fmt=*) "Erlaubt sind nur die Operatoren +,-,*,: !"
       cycle
    end select
    write(unit=*,fmt=*) z3,n3

  end do

end program bruchrechnung

