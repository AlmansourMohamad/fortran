program bruchrechnung      

  ! Programm zur Bruchrechnung
  ! Benoetigte Unterprogramme : ggt und kuerze

  implicit none


  integer,parameter :: l = selected_int_kind(9)
  integer(kind=l)   :: z1,n1,z2,n2,zae,nen
  integer           :: ios
  character(len=1)  :: operator

  do
    write(unit=*,fmt=*) "Eingabe : z1 n1 operator z2 n2"
    read(unit=*,fmt=*,iostat=ios)  z1, n1, operator, z2, n2
    if ( ios/=0 ) then 
      exit
    end if
    if ( n1==0 .or. n2==0) then
      write(unit=*,fmt=*) "Einer der Nenner war 0!"
      cycle
    end if
    call kuerze(z1,n1,z1,n1)
    call kuerze(z2,n2,z2,n2)

    select case( operator )
     case( "*" )
        call bruch_mal_bruch(z1,n1,z2,n2,zae,nen)
     case( ":" )
        if ( z2 == 0 ) then
            write(unit=*,fmt=*) "Der Zaehler des Divisors war 0!"
            cycle
        end if
        call bruch_durch_bruch(z1,n1,z2,n2,zae,nen)
     case( "+" )
        call bruch_plus_bruch(z1,n1,z2,n2,zae,nen)
     case( "-" )
        call bruch_minus_bruch(z1,n1,z2,n2,zae,nen)
     case default
       write(unit=*,fmt=*) "Erlaubt sind nur die Operatoren +,-,*,: !"
       cycle
    end select
    write(unit=*,fmt=*) zae,nen

  end do

  contains

  function ggt(zae,nen) result(grgte)

    ! Funktion zur Berechnung des groessten gemeinsamen Teilers von
    ! zae und nen ( Euklids Algorithmus)

    integer,parameter          :: l = selected_int_kind(9)
    integer(kind=l),intent(in) :: zae,nen
    integer(kind=l)            :: grgte
    integer(kind=l)            :: m,r

    m = abs(zae)
    grgte = abs(nen)
    do
      r = modulo(m,grgte)
      if( r==0 ) then
        exit
      end if
      m = grgte
      grgte = r
    end do

  end function ggt


  subroutine kuerze(zae,nen,zae_k,nen_k) 

    ! Unterprogramm zum Kuerzen eines Bruches
    ! Benoetigte Funktion : ggt


    integer,parameter           :: l = selected_int_kind(9)
    integer(kind=l),intent(in ) :: zae,nen
    integer(kind=l),intent(out) :: zae_k,nen_k
    integer(kind=l)             :: teiler

    if ( nen ==0 ) then
       zae_k = huge(teiler)
       nen_k = 0
    else
       teiler = ggt(zae,nen)
       zae_k  = zae/teiler
       nen_k  = nen/teiler
    endif

  end subroutine kuerze






  subroutine bruch_mal_bruch(z1,n1,z2,n2,zae,nen)
     integer,parameter            :: l = selected_int_kind(9)
     integer(kind=l),intent(in )  :: z1,n1,z2,n2
     integer(kind=l),intent(out)  :: zae,nen
     integer(kind=l)              :: gz1,gn1,gz2,gn2

     call kuerze(z1,n2,gz1,gn2)   ! Kuerzen ueber Kreuz bei bereits gekuerzten Bruechen (HP),
                                  ! kein weiteres Kuerzen mehr erforderlich
     call kuerze(z2,n1,gz2,gn1)
     zae = gz1*gz2
     nen = gn1*gn2
  end subroutine bruch_mal_bruch

  subroutine bruch_durch_bruch(z1,n1,z2,n2,zae,nen)
     integer,parameter            :: l = selected_int_kind(9)
     integer(kind=l),intent(in )  :: z1,n1,z2,n2
     integer(kind=l),intent(out)  :: zae,nen
     integer(kind=l)              :: gz1,gn1,gz2,gn2

     call kuerze(z1,z2,gz1,gz2)
     call kuerze(n2,n1,gn2,gn1)
     zae = gz1*gn2
     nen = gn1*gz2
  end subroutine bruch_durch_bruch

  subroutine bruch_plus_bruch(z1,n1,z2,n2,zae,nen)
     integer,parameter            :: l = selected_int_kind(9)
     integer(kind=l),intent(in )  :: z1,n1,z2,n2
     integer(kind=l),intent(out)  :: zae,nen
     integer(kind=l)              :: t,n2dt
     integer(kind=l)              :: kl_gem_nen,zaehler

     t    = ggt(n1,n2)
     n2dt = n2/t
     kl_gem_nen = n1*n2dt   !n1*n2/ggt(n1,n2)
     zaehler    = z1*n2dt + z2*(n1/t)
     call kuerze(zaehler,kl_gem_nen,zae,nen)
  end subroutine bruch_plus_bruch

  subroutine bruch_minus_bruch(z1,n1,z2,n2,zae,nen)
     integer,parameter            :: l = selected_int_kind(9)
     integer(kind=l),intent(in )  :: z1,n1,z2,n2
     integer(kind=l),intent(out)  :: zae,nen 
     integer(kind=l)              :: t,n2dt
     integer(kind=l)              :: kl_gem_nen,zaehler

     t    = ggt(n1,n2)
     n2dt = n2/t
     kl_gem_nen = n1*n2dt
     zaehler    = z1*n2dt - z2*(n1/t)
     call kuerze(zaehler,kl_gem_nen,zae,nen)
  end subroutine bruch_minus_bruch


end program bruchrechnung

