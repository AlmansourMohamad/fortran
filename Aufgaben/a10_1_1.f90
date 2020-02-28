program a10_1_1
  implicit none

  integer, dimension(10) :: vec
  integer :: i,k
  real :: r  = 3.14
  do i=1,10
!     k = modulo(r,10)
     k = int(r)
     vec(i:i) = k
     r = r - k
     r = 1.0/r    
  end do
  write(*,*) vec

end program a10_1_1
