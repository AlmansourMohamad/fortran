! Aufgabe: Bruchrechnung
! F-Loesung

module brueche
  implicit none
  public  :: operator(+), operator(-), operator(*), operator(/), &
      assignment(=), real
  private :: bruch_durch_integer, bruch_durch_bruch, integer_durch_bruch, &
      bruch_mal_integer, bruch_mal_bruch, integer_mal_bruch, &
      bruch_minus_integer, bruch_minus_bruch, integer_minus_bruch, &
      bruch_plus_integer, bruch_plus_bruch, integer_plus_bruch, &
      bruch_aus_integer, real_aus_bruch, real_bruch1, kuerzt, ggt
  integer,parameter,private :: ip = kind(1),  & 
      dp = selected_real_kind(2*precision(0.0)), &
      rp = dp
  type,public :: bruch
    integer(kind=ip) :: zae,nen
  end type bruch
  interface operator(/)
    module procedure bruch_durch_integer, bruch_durch_bruch, &
        integer_durch_bruch
  end interface
  interface operator(*)
    module procedure bruch_mal_integer, bruch_mal_bruch, &
        integer_mal_bruch
  end interface
  interface operator(-)
    module procedure bruch_minus_integer, bruch_minus_bruch, &
        integer_minus_bruch
  end interface
  interface operator(+)
    module procedure bruch_plus_integer, bruch_plus_bruch, &
        integer_plus_bruch
  end interface
  interface assignment(=)
    module procedure bruch_aus_integer, real_aus_bruch
  end interface
  interface real
    module procedure real_bruch1
  end interface
contains

  function bruch_minus_integer(br,i) result(bru_minus_int)
    integer(kind=ip),intent(in) :: i
    type(bruch),intent(in)      :: br
    type(bruch)                 :: bru_minus_int
    type(bruch)                 :: h1,h2,g
    integer(kind=ip)            :: t, n2dt

    h2 = kuerzt(br)
    h1 = kuerzt(bruch( i, h2 % nen ))
    h1 % zae = h1 % zae *  h2 % nen
    g = bruch( h1 % nen, h2 % nen )
    t = ggt(g)
    n2dt = h2 % nen / t
    bru_minus_int % zae =-h1 % zae * n2dt + h2 % zae * (h1 % nen / t)
    bru_minus_int % nen = h1 % nen * n2dt
    bru_minus_int = kuerzt(bru_minus_int)

  end function bruch_minus_integer

  function integer_minus_bruch(i,br) result(int_minus_bru)
    integer(kind=ip),intent(in) :: i
    type(bruch),intent(in)      :: br
    type(bruch)                 :: int_minus_bru
    type(bruch)                 :: h1,h2,g
    integer(kind=ip)            :: t, n2dt

    h2 = kuerzt(br)
    h1 = kuerzt( bruch( i, h2 % nen ) )
    h1 % zae = h1 % zae *  h2 % nen
    g = bruch( h1 % nen, h2 % nen )
    t = ggt(g)
    n2dt = h2 % nen / t
    int_minus_bru % zae = h1 % zae * n2dt - h2 % zae * (h1 % nen / t)
    int_minus_bru % nen = h1 % nen * n2dt
    int_minus_bru = kuerzt(int_minus_bru)

  end function integer_minus_bruch

  function bruch_minus_bruch(br1,br2) result(bru_minus_bru)
    type(bruch),intent(in) :: br1,br2
    type(bruch)            :: bru_minus_bru
    type(bruch)            :: h1,h2,g 
    integer(kind=ip)       :: t, n2dt

    h1 = kuerzt(br1)
    h2 = kuerzt(br2)
    g = bruch( h1 % nen, h2 % nen )
    t = ggt(g)
    n2dt = h2%nen/t
    bru_minus_bru % zae = h1 % zae * n2dt - h2 % zae * (h1 % nen / t)
    bru_minus_bru % nen = h1 % nen * n2dt
    bru_minus_bru = kuerzt(bru_minus_bru)

  end function bruch_minus_bruch

  function bruch_plus_integer(br,i) result(bru_plus_int)
    integer(kind=ip),intent(in) :: i
    type(bruch),intent(in)      :: br
    type(bruch)                 :: bru_plus_int
    type(bruch)                 :: h1,h2,g
    integer(kind=ip)            :: t, n2dt

    h2 = kuerzt(br)
    h1 = kuerzt(bruch( i, h2 % nen ))
    h1 % zae = h1 % zae *  h2 % nen
    g = bruch( h1 % nen, h2 % nen )
    t = ggt(g)
    n2dt = h2 % nen / t
    bru_plus_int % zae = h1 % zae * n2dt - h2 % zae * (h1 % nen / t)
    bru_plus_int % nen = h1 % nen * n2dt
    bru_plus_int = kuerzt(bru_plus_int)

  end function bruch_plus_integer

  function integer_plus_bruch(i,br) result(int_plus_bru)
    integer(kind=ip),intent(in) :: i
    type(bruch),intent(in)      :: br
    type(bruch)                 :: int_plus_bru
    type(bruch)                 :: h1,h2,g
    integer(kind=ip)            :: t, n2dt

    h2 = kuerzt(br)
    h1 = kuerzt(bruch( i, h2 % nen ))
    h1 % zae = h1 % zae *  h2 % nen
    g = bruch( h1 % nen, h2 % nen )
    t = ggt(g)
    n2dt = h2 % nen / t
    int_plus_bru % zae = h1 % zae * n2dt + h2 % zae * (h1 % nen / t)
    int_plus_bru % nen = h1 % nen * n2dt
    int_plus_bru = kuerzt(int_plus_bru)

  end function integer_plus_bruch

  function bruch_plus_bruch(br1,br2) result(bru_plus_bru)
    type(bruch),intent(in) :: br1,br2
    type(bruch)            :: bru_plus_bru
    type(bruch)            :: h1,h2,g 
    integer(kind=ip)       :: t, n2dt

    h1 = kuerzt(br1)
    h2 = kuerzt(br2)
    g = bruch( h1%nen, h2%nen )
    t = ggt(g)
    n2dt = h2%nen/t
    bru_plus_bru % zae = h1%zae*n2dt + h2%zae*(h1%nen/t)
    bru_plus_bru % nen = h1%nen * n2dt
    bru_plus_bru = kuerzt(bru_plus_bru)

  end function bruch_plus_bruch

  function integer_mal_bruch(i,br) result(int_mal_bru)
    type(bruch),intent(in)      :: br
    integer(kind=ip),intent(in) :: i
    type(bruch)                 :: int_mal_bru
    type(bruch)                 :: t

    t % zae = i
    t % nen = 1
    int_mal_bru = t * br

  end function integer_mal_bruch

  function integer_durch_bruch(i,br) result(int_durch_bru)
    type(bruch),intent(in)      :: br
    integer(kind=ip),intent(in) :: i
    type(bruch)                 :: int_durch_bru
    type(bruch)                 :: t

    t % zae = i
    t % nen = 1
    int_durch_bru = t / br

  end function integer_durch_bruch

  function bruch_mal_bruch(br1,br2) result(bru_mal_bru)
    type(bruch),intent(in) :: br1,br2
    type(bruch)            :: bru_mal_bru
    type(bruch)            :: h1,h2,g1,g2

    h1 = kuerzt(br1)
    h2 = kuerzt(br2)
    g1 = kuerzt(bruch( h1%zae , h2%nen ))
    g2 = kuerzt(bruch( h2%zae , h1%nen ))

    bru_mal_bru = bruch( g1%zae * g2%zae , g1%nen * g2% nen)

  end function bruch_mal_bruch

  function bruch_durch_bruch(brz,brn) result(bru_durch_bru)
    type(bruch),intent(in) :: brz,brn
    type(bruch)            :: bru_durch_bru
    type(bruch)            :: hz,hn,gz,gn

    hz = kuerzt(brz)
    hn = kuerzt(brn)
    gz = kuerzt( bruch( hz%zae , hn%zae ) )
    gn = kuerzt(bruch( hn%nen , hz%nen ))

    bru_durch_bru = bruch( gz%zae * gn%zae , gz%nen * gn% nen)

  end function bruch_durch_bruch

  function real_bruch1(br) result(rbru1)
    type(bruch),intent(in)      :: br
    real(kind=rp)               :: rbru1

    rbru1 = real(br%zae,dp)/real(br%nen,dp)

  end function real_bruch1

  subroutine real_aus_bruch(r,br)
    type(bruch),intent(in)    :: br
    real(kind=rp),intent(out) :: r

    r = real(br%zae,dp)/real(br%nen,dp)

  end subroutine real_aus_bruch

  subroutine bruch_aus_integer(br,i)
    type(bruch),intent(out) :: br
    integer(kind=ip),intent(in)  :: i

    br % zae = i
    br % nen = 1

  end subroutine bruch_aus_integer

  function bruch_mal_integer(br,i) result(bru_mal_int)
    type(bruch),intent(in)      :: br
    integer(kind=ip),intent(in) :: i
    type(bruch)                 :: bru_mal_int
    type(bruch)                 :: t

    t % zae = i
    t % nen = 1 
    bru_mal_int = br * t     ! Mult. von Bruechen

  end function bruch_mal_integer

  function bruch_durch_integer(br,i) result(bru_durch_int)
    type(bruch),intent(in)      :: br
    integer(kind=ip),intent(in) :: i
    type(bruch)                 :: bru_durch_int
    type(bruch)                 :: t

    t % zae = i
    t % nen = 1 
    bru_durch_int = br / t

  end function bruch_durch_integer

  function kuerzt(br) result( kuerz )

    ! Funktion zum Kuerzen eines Bruches
    ! Benoetigte Funktion : ggt

    type(bruch),intent(in) :: br
    type(bruch)            :: kuerz
    integer(kind=ip)       :: i

    if ( br % nen == 0 ) then
      kuerz % zae = huge(i)
      kuerz % nen = 0
    else
      i = ggt(br)
      kuerz % zae = br % zae / i
      kuerz % nen = br % nen / i
    end if

  end function kuerzt

  function ggt(br) result(groetei)

    ! Funktion zur Berechnung des groessten gemeinsamen Teilers von
    ! br % zae und br % nen ( Euklids Algorithmus)

    type(bruch),intent(in) :: br
    integer(kind=ip)       :: groetei
    integer(kind=ip)       :: m,r

    m = abs( br % zae )
    groetei = abs( br % nen)
    do
      r = modulo(m,groetei)
      if( r==0 ) then
        exit
      end if
      m = groetei
      groetei = r
    end do

  end function ggt

end module brueche
!=========================================================================
program bruchrechnung           

  ! Programm zur Bruchrechnung
  use brueche, b=>bruch
  implicit none

  type(b)           :: erg
  integer,parameter :: dp = selected_real_kind(13)
  real(kind=dp)     :: r

  erg = 3+1/(7+b(1,16))
  r = erg
  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
  erg = 3+1/(7+1/(15+1/(1+b(1,293))))
  r = erg
  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
  erg = 4/(1+1/(3+1/(1+1/(1+1/(1+1/(15+b(1,2)))))))
  r = erg
  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)
  erg = 2+b(1,3)*(2+b(2,5)*(2+b(3,7)*(2+b(4,9)*(2+b(5,11)* &
        (2+b(6,13)*(2+b(7,15)*(2+b(8,17)*(2+b(9,19)*(2+b(10,21)* &
        (2+b(11,23)*2))))))))))
  r = erg
  write(unit=*,fmt=*) erg, r, real(erg), acos(-1.0_dp)

end program bruchrechnung
