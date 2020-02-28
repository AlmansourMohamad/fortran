program a913
  implicit none

  character(len=:),allocatable:: c
  integer :: p,n
  read(*,'(a)') c
  n = len(c)
  read(*,*) p
  call anagram(c,p)

contains
!  function fakultaet(x) result(y)
!    integer, intent(in):: x
!    integer, intent(out) :: y
!    integer :: k
!    do k = 1,(x-1)
!       y = x*k
!    end do
!  end function fakultaet
  
  recursive subroutine anagram(str,i) 
    character(len = n) :: str
    integer :: i
    !    integer,intent(in),optional:: k
    !    do j = i, fakultaet(n-i)
    do
       if(i >= n) then
          write(*,'(a)') str
          exit
       else
          call anagram(str,i+1)
       end if
    end do

  end subroutine anagram

end program a913
