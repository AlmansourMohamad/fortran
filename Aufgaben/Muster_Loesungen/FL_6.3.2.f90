      program kind_types

!**********************************************************************
!*                                                                    *
!*  finds all kind-types of integer numbers and real-numbers for the  *
!*            fortran-declaration-statements                          *
!*                                                                    *
!**********************************************************************

!--------------------
!--- declarations ---
!--------------------

      integer    :: i            ! loop-variable
      integer    :: last_type    ! former type
      integer    :: type         ! actual type of kind within print-loop
      integer    :: ianf,iend    ! start and end of actual kind-type
      
      character(len=32)   :: string='      integer, parameter  :: int'      
      character(len= 4)   :: kind_type

!-----------------------------------------------------------------------

      open(unit=11, file='precisions_available.f90', status='replace', &
              access='sequential', action='write')

      write(11, '(6x,a,/)' )'module precisions'

      write(11, '(a)' )'!--------------------------------------------------------------'
      write(11, '(a)' )'!--- contains all available kind types for integer and real ---'
      write(11, '(a)' )'!--------------------------------------------------------------'

!------------------------------------------------------------
!---> Print a short table of all available integer-kinds <---
!------------------------------------------------------------

      write(11, '(/,a)' )'!--->'
      write(11, '(  a)' )'!---> integer'
      write(11, '(  a)' )'!--->'

      write(*, '(/,a)' )'Kind-type-description for integer-values'
      write(*, '(a)' )'number of digits'
      write(*, '(a)' )'from  to  kind'

      last_type = selected_int_kind (1)
      ianf      = 1
      i         = 1

      do
         i    = i + 1
         type = selected_int_kind (i)
            
         if ( type /= last_type ) then
            iend = i - 1
            write(*, '(i3,a,i2,i4)' ) ianf,' - ',iend,last_type

            write(kind_type, '(i4)' ) last_type
            kind_type = adjustl(kind_type)
            write(11, '(4a)' ) string,trim(kind_type),' = ',trim(kind_type)

            ianf      = i
            last_type = type
         end if

         if ( type < 0 ) exit

      end do

!---------------------------------------------------
!---> Print a short table of all available real <---
!---------------------------------------------------

      write(11, '(/,a)' )'!--->'
      write(11, '(  a)' )'!---> real'
      write(11, '(  a)' )'!--->'

      string (30:32) ='rl '
      write(*, '(/,a)' )'Kind-type-description for real-values'
      write(*, '(a)' )'number of digits'
      write(*, '(a)' )'from  to  kind'
      last_type = selected_real_kind(1)
      ianf = 1
      i    = 1

      do
         i = i + 1
         type = selected_real_kind(i)

         if ( type /= last_type ) then
            iend = i - 1
            write(*, '(i3,a,i2,i4)' ) ianf,' - ',iend,last_type

            write(kind_type, '(i4)' ) last_type
            kind_type = adjustl(kind_type)
            write(11, '(4a)' ) trim(string),trim(kind_type),' = ',trim(kind_type)

            ianf = i
            last_type = type
         end if

         if ( type < 0 ) exit

      end do

!------------------
!--- close file ---
!------------------

      write(11, '(/,6x,a)' )'end module precisions'

      close(unit=11, status='keep')

      write(*, '(/,a)')   &
      'all available kind parameter written onto Fortran file "precisions_alailable.f90"'

      stop

      end program kind_types
