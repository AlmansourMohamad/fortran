program a8_17_2
  implicit none

  real :: x,n

  x = 1.2
  n = 7
  write(*,'(f10.6)') expo(x,n)

contains
  function expo(x,n)
    real,intent(in)    :: x
    integer,intent(in) :: n
    real               :: expo = 1
    integer :: i = 0
    if(n < 0) then
       n = abs(n)
       i = 1
    else if(modulo(n,2) == 0) then
       n = 0.5*n
    end if
    do
       if(n == 0) exit
       expo = expo(x,(n-1))
       
!       else if(modulo(n,2) == 0) then
!          expo = expo(x,(0.5*n))
!       else if(modulo(n,2) /= 0) then
!          expo = x*expo(x,(0.5*n))
       else
          expo = x * expo
       end if
    end do
    
    if(i == 1) then
       expo = 1.0/expo
    end if
  end function expo
end program a8_17_2
