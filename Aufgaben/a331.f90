program a331
  implicit none
  integer,parameter::ST=selected_int_kind(4)
  integer::sum
  integer(ST)::n
  sum = ((4**4)+(3**3)+(8**8)+(5**5)+(7**7)+(9**9)+(0)+(8**8)+(8**8))
  print*, sum
  n = sum
  print*, n
end program a331
