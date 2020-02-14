program a262
  implicit none
  integer :: j, a, b, c, m, s, m_g, n_g, d
  integer :: d_g
  integer :: e

  read(*,*) j
  m = ((3+(8*(j/100)))/25)-2
  s = (j/100)-(j/400)-2
  m_g = mod((15+s-m),30)
  n_g = mod((m_g + 19*a),30)
  select case (d)
  case (29)
     d_g = 28
  case (28)
     select case (a)
     case (11:)
        d_g = 27
     case default
        write(*,*) "default case .. Fehlermeldung von select_2"
     end select
  case (:27)
     
     d_g = d
  case default
     write(*,*) "default case .. Fehlermeldung von select_1"
  end select
  e = mod((b+b+c+c+c+c+(6*d_g)+n_g),7)
  write(*,'('a = ',i0,/,'b = ',i0,/,'c = ',i0,/,'d = ',i0,/,'e = ',i0,/,'m_g = ',i0,/,'n_g = ',i0,/,'d_g = ',i0)') &
       a, b, c, d, e, m_g, n_g, d_g

