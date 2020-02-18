Program Wortklauberei

!+----------------------------------------------------------------------+
!|                                                                      |
!|                Aufgabe 5.1.1: Wortklauberei                          |
!|                                                                      |
!|  Problem: a line consist of several words. Words contain neither     |
!|           blanks nor commas. Print out the complete string and       |
!|           the separate words. Furthermore find out wether a word     |
!|           contains nothing but an integernumber of any desired length|
!|                                                                      |
!+----------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

  Character(len=80)          :: c, chilf
  Character(len=*),Parameter :: trenn = " ,"
  Character(len=16)          :: komment

  Integer                    :: von,bis,i,d

  Logical                    :: ob

!------------------------------------------------------------------------

!---> read and echo string

  Read (unit=*,fmt="(a)") c
  Write(unit=*,fmt="(3a)") '>',c,'<'

!---> store the original string onto an auxiliary string 
!     so it can be changed

  chilf = c

!----> exchange all comma against blanks
  do
     i = index(chilf, ',')

     if ( i == 0 ) then
        exit
     else
        chilf (i:i) = ' ' 
     end if

  end do

!-------------------------------------------
!---> loop over all words of the string <---
!-------------------------------------------

  Do

!---> adjust the string left so the 1st word starts on position 1
    chilf = adjustl (chilf)

!---> find end of 1st word (only blanks in last iteration-->return 1)
    bis = index(chilf, ' ') - 1

!---> all words found
    if ( bis == 0 ) exit

!---> word found an integernumber?   

     d = Merge(2,1, (chilf(1:1) == "-" ))        ! negative integer?

     ob = Verify(chilf(d:bis),"0123456789")==0   ! true if words contains only digits

     komment = Merge(" ist  eine Zahl."," ist keine Zahl.", ob )    
              ! Merge type and type parameters need to be the same --> extra blank


     Write(unit=*,fmt=*) """", chilf(1:bis),"""", komment

     chilf(1:bis) = ' '
  End Do

End Program Wortklauberei
