! Aufgabe 9.1.3:     Kult mit Namen

   module umschreibe

     implicit none

     public :: anagram

   contains


     recursive subroutine anagram(c,p)
!*******************************************************************************
!*                                                                             *
!*      Recursive subroutine, were all permutations of the string "c"          *
!*                          are composed.                                      *
!*                                                                             *
!*******************************************************************************

!--- Deklarationen

     character(len=*),intent(inout) :: c    ! inputstring    
     integer,intent(in)             :: p    ! starting position
     integer                        :: i    ! loopindex
     integer                        :: l    ! length of c
     character(len=len(c))          :: h    ! auxiliar string, contains the original state of c

!------------------------------------------------------------------

     l = len(c)                             ! length of inputstring       

!-----------------------------
!---> criterion for abort <---
!-----------------------------
write(*,*) p
     if( p>=l ) then
        print *, c

!---------------------
!---> permutation <---
!---------------------

     else
        h = c                        ! store the original state              

!---> all permutations starting with the i-th character
        do i = p,l
           c = h                      ! restore the the original state
           c(p:p) = h(i:i)            ! exchange the startting with the i-th character
           c(i:i) = h(p:p)      
           call anagram(c,p+1)        ! all permutations having this constellation 
         end do

     endif

     end subroutine anagram

   end module umschreibe


!----------------------
!---> Main program <---
!----------------------

   program anagra

     use umschreibe,only:anagram

     !---> declarations 

     implicit none

     character(len=*),parameter :: cp = "beil"
     character(len=len(cp))     :: cv

     !-----------------------------------------------------------------

     cv = cp   ! to avoid writing on a constant (intent inout)

     call anagram(cv,1)

   end program anagra
