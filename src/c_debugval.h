          subroutine DEBUGVAL(mode) bind(C,name="DEBUGVAL")
          use iso_c_binding, only: c_int
          integer(c_int), intent(out) :: mode
          end subroutine DEBUGVAL
          subroutine DEBUGLEVEL(mode) bind(C,name="DEBUGLEVEL")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in) :: mode
          end subroutine DEBUGLEVEL
