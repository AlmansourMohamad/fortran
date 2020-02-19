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
  write(*,'(a)') "Operation eingeben: "
  read(*,*,iostat=ios) op
  !  write(*,'(a)') "Nenner des zweiten Bruchs eingeben: "
  !  read(*,*,iostat=ios) n2  
  !  write(*,'(a)') "Zaehler des zweiten Bruchs eingeben: "
  !  read(*,*,iostat=ios) z2

  z_res = 0
  n_res = 0
  if(ios /= 0) then
     write(*,'(a)') "Eingabe ist ungueltig. Porgramm wird an dieser Stelle beendet."
     stop
  else
     call erg_rechnen(z1,n1,z2,n2,op,z_res,n_res)
  end if
  !  z = z_rechnen(z1,n1,z2,n2,op)
  !  n = n_rechnen(n1,n2)
  write(*,'(i0,a,i0)') z_res,"/",n_res
  !end program a822
contains
  subroutine erg_rechnen(z1,n1,z2,n2,op,z_res,n_res) 
    integer::z1,n1,z2,n2,res,z_res,n_res
    character(len=1)::op
    select case(op)
    case('+')
       z_res = (z1*n2)+(z2*n1)
       n_res = n1*n2
       write(*,'(i0,a,i0)') z_res,"/",n_res
    case('-')
       z_res = (z1*n2)-(z2*n1)
       n_res = n1*n2
       write(*,'(i0,a,i0)') z_res,"/",n_res
    case('*')
       z_res = z1*z2
       n_res = n1*n2
       write(*,'(i0,a,i0)') z_res,"/",n_res
    case('/')
       z_res = z1*n2
       n_res = z2*n1
       write(*,'(i0,a,i0)') z_res,"/",n_res
    case default
       write(*,'(a)') "Fehlermeldung => liefert falsches Ergebnis"
    end select
  end subroutine erg_rechnen

end program a822
