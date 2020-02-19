program a822
  implicit none

  integer::z1,n1,z2,n2,z,n,ios,z_res,n_res
  character(len=1)::op
  !-------------------------
  !  write(*,'(a)') "Zaehler des ersten Bruchs eingeben: "
  !  read(*,*,iostat=ios) z1
  !  write(*,'(a)') "Nenner des ersten Bruchs eingeben: "
  !  read(*,*,iostat=ios) n1
  z1 =3;n1=4;z2=5;n2=2
  do
     write(*,'(a)') "Operation eingeben: "
     read(*,*,iostat=ios) op
     if((ios == 0) .and. (ichar(op) .lt. 48) .and. (ichar(op) .gt. 42)) then
        z_res = z_rechnen(z1,n1,z2,n2,op)
        n_res = n_rechnen(z1,n1,z2,n2,op)
        exit
     else
        write(*,'(a)') "Eingabe ist ungueltig. Porgramm wird an dieser Stelle beendet."
        stop
     end if
  end do
  !  write(*,'(a)') "Nenner des zweiten Bruchs eingeben: "
  !  read(*,*,iostat=ios) n2  
  !  write(*,'(a)') "Zaehler des zweiten Bruchs eingeben: "
  !  read(*,*,iostat=ios) z2

  !  if(ios /= 0) then
  !     write(*,'(a)') "Eingabe ist ungueltig. Porgramm wird an dieser Stelle beendet."
  !     stop
  !  else
  !     z_res = z_rechnen(z1,n1,z2,n2,op)
  !     n_res = n_rechnen(z1,n1,z2,n2,op)
  !  end if
  write(*,'(i0,a,i0)') z_res,"/",n_res

contains
  function z_rechnen(z1,n1,z2,n2,op) result (z_res)
    integer::z1,n1,z2,n2,z_res
    character(len=1)::op
    select case(op)
    case('+')
       z_res = (z1*n2)+(z2*n1)
    case('-')
       z_res = (z1*n2)-(z2*n1)
    case('*')
       z_res = z1*z2
    case('/')
       z_res = z1*n2
    case default
       write(*,'(a)') "Fehlermeldung => liefert falsches Ergebnis"
    end select
  end function z_rechnen

  function n_rechnen(z1,n1,z2,n2,op) result (n_res)
    integer::z1,n1,z2,n2,n_res
    character(len=1)::op
    select case(op)
    case('+','-','*')
       n_res = n1*n2
    case('/')
       n_res = z2*n1
    case default
       write(*,'(a)') "Fehlermeldung => liefert falsches Ergebnis"
    end select
  end function n_rechnen

end program a822
