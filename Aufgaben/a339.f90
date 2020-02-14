program a339
  implicit none
  integer :: i, n_f
  n_f = 1
  i = 1
  do
     n_f = (n_f * i)
     write(*,"(i0,' ',i0)") i,(n_f)
     i = (i+1)
     if(i > 30) exit
  end do

end program a339
