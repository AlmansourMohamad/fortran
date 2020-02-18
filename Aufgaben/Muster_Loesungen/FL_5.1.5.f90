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

  Character(len=80)          :: c
  Character(len=*),Parameter :: trenn = " ,"
  Character(len=16)          :: komment

  Integer                    :: von,bis,i,d

  Logical                    :: ob

!------------------------------------------------------------------------

!---> read and echo string

  Read (unit=*,fmt="(a)") c
  Write(unit=*,fmt="(3a)") '>',c,'<'

!-------------------------------------------
!---> loop over all words of the string <---
!-------------------------------------------

  bis = 0

  Do

!---> find the beginning of the next word

    i = Verify(c(bis+1:),trenn)

!---> end of string ?  

    If( i == 0 ) Then
      Exit
    End If

    von = bis + i

!---> find end of word

    i = Scan(c(von:),trenn)

!    if ( i == 0 ) i = len(c) + 2 - von
!    bis = von + i - 2           ! position of end of word on string
    if (i==0) then
       bis = len(c)
    else
       bis = von + i - 2           ! position of end of word on string
    endif

!---> word found an integernumber?   

    d = Merge(1,0,c(von:von)=="-" )   ! negative integer?

    ob = Verify(c(von+d:bis),"0123456789")==0   ! true if words contains only digits

    komment = Merge(" ist  eine Zahl."," ist keine Zahl.", ob )
              ! Merge type and type parameters need to be the same --> extra blank

    Write(unit=*,fmt=*) """", c(von:bis),   &
                        """", komment
  End Do

End Program Wortklauberei
