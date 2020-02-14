program a311
  implicit none
  integer :: g,l,d
  write(*,*) "Gewicht in kg eingeben: "
  read(*,*) g
  g = (g*1000)
  write(*,*) "Groesse in cm eingeben: "
  read(*,*) l
  d = (9*g)/(2*(l**2))
  if(d .eq. 10) then
     write(*,*) "normalgewicht :)"
     !  else if((d .gt. 10) .or. (d .lt. 10))
  else
     write(*,*) "ertraegliche Abweichung"
  end if

end program a311
