program a10_2_3
  implicit none
  integer :: x = 1
  integer :: y = 2
  call my_func(fun,x,y)
  write(*,*) y
  
contains
  
  integer function fun(c)
    integer, intent(in) :: c
    fun = c+1
  end function fun

  subroutine my_func(f,a,b)
    integer, intent(in):: a
    integer, intent(out)::b
    interface
       integer function f(c) result(d)
         integer, intent(in)::c
       end function f
    end interface
    b = f(a)
  end subroutine my_func

end program a10_2_3



