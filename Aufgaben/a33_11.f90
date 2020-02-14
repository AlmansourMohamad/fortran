program a33_11
  implicit none

  integer,parameter::AnzStellen = selected_int_kind(10)
  integer(AnzStellen)::m,n,r

  !print*, "m = "
  !read(*,*) m
  !print*, "n = "
  !read(*,*) n
  
  m = 24
  n = 36

  do
     r = modulo(m,n)
     if(r .ne. 0) then
        m = n
        n = r
     endif
     if(r == 0) exit
  enddo
  write(*,"(i0)") n

end program a33_11
