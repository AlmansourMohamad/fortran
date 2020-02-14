program wie_weit

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 2.4.3: Wie weit kann man gehen                |
!|                                                                      |
!|  Problem: declare variables using selected_int_kind(1,2,...,19).     |
!|           Write out following values of these variables:             |
!|                                                                      |
!|                 kind, radix, digits, huge, range                     |
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

 implicit none

!---> find out all available kind-parameters for 1 to 19 digits <---

 integer, parameter :: iki1 =selected_int_kind(1)
 integer, parameter :: iki2 =selected_int_kind(2)
 integer, parameter :: iki3 =selected_int_kind(3)
 integer, parameter :: iki4 =selected_int_kind(4)
 integer, parameter :: iki5 =selected_int_kind(5)
 integer, parameter :: iki6 =selected_int_kind(6)
 integer, parameter :: iki7 =selected_int_kind(7)
 integer, parameter :: iki8 =selected_int_kind(8)
 integer, parameter :: iki9 =selected_int_kind(9)
 integer, parameter :: iki10=selected_int_kind(10)
 integer, parameter :: iki11=selected_int_kind(11)
 integer, parameter :: iki12=selected_int_kind(12)
 integer, parameter :: iki13=selected_int_kind(13)
 integer, parameter :: iki14=selected_int_kind(14)
 integer, parameter :: iki15=selected_int_kind(15)
 integer, parameter :: iki16=selected_int_kind(16)
 integer, parameter :: iki17=selected_int_kind(17)
 integer, parameter :: iki18=selected_int_kind(18)
 integer, parameter :: iki19=selected_int_kind(19)

!---> declare corresponding variables <---

 integer (kind=iki1)  :: i1
 integer (kind=iki2)  :: i2
 integer (kind=iki3)  :: i3
 integer (kind=iki4)  :: i4
 integer (kind=iki5)  :: i5
 integer (kind=iki6)  :: i6
 integer (kind=iki7)  :: i7
 integer (kind=iki8)  :: i8
 integer (kind=iki9)  :: i9
 integer (kind=iki10) ::i10
 integer (kind=iki11) ::i11
 integer (kind=iki12) ::i12
 integer (kind=iki13) ::i13
 integer (kind=iki14) ::i14
 integer (kind=iki15) ::i15
 integer (kind=iki16) ::i16
 integer (kind=iki17) ::i17
 integer (kind=iki18) ::i18

 ! integer (kind=iki19) ::i19    ! commented, because:
   !nag:  KIND value (-1) does not specify a valid representation method
   !PGF90-S-0081-Illegal selector - KIND value must be non-negative 

 integer ::i                     ! without kind-parameter

!------------------------------------------------------------------------

!---> write out kind- radix, digits, huge and range-values for each 
!--->                declared variable

 write(*,*) "kind  "  , "radix  ", "digits  ","huge  ","range  "
 write(*,*) kind(i1), radix(i1), digits(i1), huge(i1), range(i1)
 write(*,*) kind(i2), radix(i2), digits(i2), huge(i2), range(i2)
 write(*,*) kind(i3), radix(i3), digits(i3), huge(i3), range(i3)
 write(*,*) kind(i4), radix(i4), digits(i4), huge(i4), range(i4)
 write(*,*) kind(i5), radix(i5), digits(i5), huge(i5), range(i5)
 write(*,*) kind(i6), radix(i6), digits(i6), huge(i6), range(i6)
 write(*,*) kind(i7), radix(i7), digits(i7), huge(i7), range(i7)
 write(*,*) kind(i8), radix(i8), digits(i8), huge(i8), range(i8)
 write(*,*) kind(i9), radix(i9), digits(i9), huge(i9), range(i9)
 write(*,*) kind(i10),radix(i10),digits(i10),huge(i10),range(i10)
 write(*,*) kind(i11),radix(i11),digits(i11),huge(i11),range(i11)
 write(*,*) kind(i12),radix(i12),digits(i12),huge(i12),range(i12)
 write(*,*) kind(i13),radix(i13),digits(i13),huge(i13),range(i13)
 write(*,*) kind(i14),radix(i14),digits(i14),huge(i14),range(i14)
 write(*,*) kind(i15),radix(i15),digits(i15),huge(i15),range(i15)
 write(*,*) kind(i16),radix(i16),digits(i16),huge(i16),range(i16)
 write(*,*) kind(i17),radix(i17),digits(i17),huge(i17),range(i17)
 write(*,*) kind(i18),radix(i18),digits(i18),huge(i18),range(i18)
! write(*,*) kind(i19)
 write(*,*) "ohne Typparameter:"
 write(*,*) kind(i),radix(i),digits(i),huge(i),range(i)

!Results :
!pgi  nag      
!kind kind    radix       digits                   huge        range  
! 1   1         2            7                      127            2
! 1   1         2            7                      127            2
! 2   2         2           15                    32767            4
! 2   2         2           15                    32767            4
! 4   3         2           31               2147483647            9
! 4   3         2           31               2147483647            9
! 4   3         2           31               2147483647            9
! 4   3         2           31               2147483647            9
! 4   3         2           31               2147483647            9
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18
! 8   4         2           63      9223372036854775807           18

! 8   4         2           63      9223372036854775807           18
! ohne Typparameter:
! 4   3         2           31               2147483647            9


end program wie_weit
