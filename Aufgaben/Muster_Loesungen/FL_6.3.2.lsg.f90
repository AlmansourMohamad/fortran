PROGRAM ue632

implicit none 

integer :: i  
real(kind=4) :: k1
real(kind=8) :: k2
real(kind=10) :: k3
real(kind=16) :: k4  ! real(kind=selected_real_kind(33)) :: k4

do i=5,33
   write(*,*) i, selected_real_kind(i)
end do

write(*,*) "kind digits(),huge(),epsilon(),maxexponent(),minexponent(),precision(),radix(),range(),tiny()"
write(*,*) "kind=4",digits(k1),huge(k1),epsilon(k1),maxexponent(k1),minexponent(k1),precision(k1),radix(k1),range(k1),tiny(k1)
write(*,*) "kind=8",digits(k2),huge(k2),epsilon(k2),maxexponent(k2),minexponent(k2),precision(k2),radix(k2),range(k2),tiny(k2)
write(*,*) "kind=10",digits(k3),huge(k3),epsilon(k3),maxexponent(k3),minexponent(k3),precision(k3),radix(k3),range(k3),tiny(k3)
write(*,*) "kind=16",digits(k4),huge(k4),epsilon(k4),maxexponent(k4),minexponent(k4),precision(k4),radix(k4),range(k4),tiny(k4)

   
END PROGRAM ue632
