module a_08_17_02_mod

  implicit none

  public expo, expo_end, expo_loop, expo_opt

contains


  recursive function expo(x, n) result(f)
    ! calculate the n-th power of x by an recursive algorithm
    real, intent(in)    :: x
    integer, intent(in) :: n    ! must be positive (> 0)
    real                :: f

    integer, save :: count = 0

    count = count + 1
    write (unit=*, fmt=*) "number of recursions:", count

    if ( n == 0 ) then
       f = 1
    else
       ! simple implementation:
       ! the exponent is reduced by one or by half depending
       ! of its oddness
       if ( btest(n, 0) ) then  ! instead of modulo(n, 2) /= 0
          f = x*expo(x, n - 1)
       else
          f = expo(x*x, n/2)
       end if
    end if

  end function expo

  recursive function expo_opt(x, n) result(f)
    ! calculate the n-th power of x by an optimized recursive algorithm
    real, intent(in)    :: x
    integer, intent(in) :: n
    real                :: f

    integer :: m
    real :: tmp

    integer, save :: count = 0

    count = count + 1
    write (unit=*, fmt=*) "number of recursions:", count

    m = abs(n)
    if ( m == 0 ) then
       f = 1
    else
       ! optimization:
       ! the exponent is always reduced by half
       tmp = expo_opt(x, m/2)
       if ( btest(m, 0) ) then
          f = x*tmp*tmp         ! correcting the reduced
                                ! exponent in the case of
                                ! m being odd
       else
          f = tmp*tmp
       end if
    end if

    if (n < 0) then
       f = 1.0/f
    end if

  end function expo_opt

  function expo_end(x, n) result(f)
    ! calculate the n-th power of x by an end recursive algorithm
    real, intent(in)    :: x
    integer, intent(in) :: n    ! must be positive (> 0)
    real                :: f

    integer, save :: count

    count = 0
    f = expo_end_internal(x, 1.0, n)

    return

  contains
    ! the end recursive solution: intermediate solutions
    ! are stored as a third argument
    recursive function expo_end_internal(x, tmp, n) result(f)
      real, intent(in)    :: x
      real, intent(in)    :: tmp
      integer, intent(in) :: n
      real             :: f

      count = count + 1
      write (unit=*, fmt=*) "number of recursions:", count

      if ( n == 0 ) then
         f = tmp
      else
         ! simple implementation:
         ! the exponent is reduced by one or by half depending
         ! of its oddness
         if ( btest(n, 0) ) then
            f = expo_end_internal(x, x*tmp, n - 1)
         else
            f = expo_end_internal(x*x, tmp, n/2)
         end if
      end if

    end function expo_end_internal
  end function expo_end

  function expo_end_gen(x, n) result(f)
    ! calculate the n-th power of x by an optimized recursive algorithm
    real, intent(in)    :: x
    integer, intent(in) :: n
    real                :: f

    integer, save :: count

    f = expo_end_gen_internal(x, 1.0, n)

    return

  contains
    recursive function expo_end_gen_internal(x, tmp, n) result(f)
      real, intent(in)    :: x
      real, intent(in)    :: tmp
      integer, intent(in) :: n
      real             :: f

      count = count + 1
      write (unit=*, fmt=*) "number of recursions:", count

      if ( n == 0 ) then
         f = tmp
      else if ( n == -1 ) then
         f = 1/expo_end_gen_internal(x*x, tmp*x, n/2) 
      else
         ! the exponent is always reduced by half
         ! correct the reduction by half by multiplying the base with itself
         if ( modulo(n, 2) /= 0 ) then
               f = expo_end_gen_internal(x*x, tmp*x, n/2)    
         else
               f = expo_end_gen_internal(x*x, tmp, n/2)
         end if
      end if

    end function expo_end_gen_internal
  end function expo_end_gen

  recursive function expo_loop(x, n) result(f)
    ! calculate the n-th power of x by an optimized iterative algorithm
    real, intent(in)    :: x
    integer, intent(in) :: n
    real                :: f

    integer :: i_loc
    real :: x_loc

    integer, save :: count = 0

    f = 1
    i_loc = abs(n)
    x_loc = x

    do while ( i_loc /= 0 )

       count = count + 1
       write (unit=*, fmt=*) "number of iterations:", count

       if ( btest(i_loc, 0) ) then  ! statt modulo(n, 2) /= 0
          f = f*x_loc
          i_loc = i_loc - 1
       end if
       x_loc = x_loc*x_loc
       i_loc = i_loc/2
    end do

    if ( n < 0 ) then
       f = 1.0/f
    end if

  end function expo_loop

end module a_08_17_02_mod


program a_08_17_02

  use a_08_17_02_mod, only: expo, expo_end, expo_end_gen, expo_loop, expo_opt

  implicit none

  integer :: n
  real :: x, y

  x = 2.0
  n = -3

!  write (unit=*, fmt=*) "Ausgangsloesung"
!  y = expo(x, n)
!  write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y

  write (unit=*, fmt=*) "optimierte Ausgangsloesung"
  y = expo_opt(x, n)
  write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y

!  write (unit=*, fmt=*) "Endrekursive Loesung"
!  y = expo_end(x, n)
!  write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y

  write (unit=*, fmt=*) "Endrekursive Loesung, verallgemeinert"
  y = expo_end_gen(x, n)
  write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y

  write (unit=*, fmt=*) "Loesung mit optimierter Schleile"
  y = expo_loop(x, n)
  write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y

  ! write (unit=*, fmt=*) "Endrekursive Loesung, verallgemeinert: 0-10"
  ! do n = 0, 10
  !    y = expo_end(x, n)
  !    write (unit=*, fmt="(f6.2, '**', i2, ' = ', f10.2)") x, n, y
  ! end do

end program a_08_17_02
