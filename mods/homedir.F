      module HOMEDIR
      character*80 confdir
      interface
      subroutine ENVHOME(home,lhome) bind(C, name="ENVHOME")
      use iso_c_binding, only: c_int, c_char
      character(kind=c_char), intent(out) :: home(*)
      integer(c_int), intent(out) :: lhome
      end subroutine
      end interface

      end module HOMEDIR
