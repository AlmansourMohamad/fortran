   program Reihenfolge

     implicit none

     integer, parameter  :: ip  = selected_int_kind(8)

     integer             :: dim = 10000
     integer             :: rep = 1
     integer             :: i,k,l


     integer(kind=ip), dimension(:,:), allocatable  :: f1, f2, res

     real                :: tanf, tend, tdauer

!----------------------------------------------------------------

   call cpu_time (tanf)

   allocate (f1(dim,dim) )
   allocate (f2(dim,dim) )
   allocate (res(dim,dim) )
 
   do,i=1,dim
      do, k=1,dim
         f1 (k,i) = 100000*i + k
         f2 (k,i) = 100000*i + k
      end do
   end do

   call cpu_time (tend)

   write(0,*) 'Zeit fuer allocate und initialisieren', tend - tanf

 
!---
!--- falsche Reihenfolge
!---

   call cpu_time (tanf)

   do, l=1,rep
      
      do,i=1,dim

         do, k=1,dim
            res(i,k) = f1 (i,k) - f2 (i,k) 
         end do

      end do

   end do

   call cpu_time (tend)

   write(0,*) 'Zeit fuer die falsche  Reihenfolge ', tend - tanf

   if ( count( res /= 0 ) > 0 ) then
      write(*,*) '=== Achtung ===> Ergebnisfeld an',count( res /= 0 ),' ungleich 0'
   end if

!---
!--- Richtige Reihenfolge
!---

   call cpu_time (tanf)

   do, l=1,rep
      
      do,i=1,dim

         do, k=1,dim
            res(k,i) = f1 (k,i) - f2 (k,i) 
         end do



      end do

   end do

   call cpu_time (tend)

   write(0,*) 'Zeit fuer die richtige Reihenfolge ', tend - tanf

   if ( count( res /= 0 ) > 0 ) then
      write(*,*) '=== Achtung ===> Ergebnisfeld an',count( res /= 0 ),' ungleich 0'
   end if

   end program Reihenfolge
