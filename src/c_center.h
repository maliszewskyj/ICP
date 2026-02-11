      subroutine CENTER_RESET() bind(C,name="CENTER_RESET")
      use iso_c_binding
      end subroutine CENTER_RESET
      subroutine CENTER_CHECK(centered) bind(C,name="CENTER_CHECK")
      use iso_c_binding
      integer(c_int) , intent(out)::centered
      end subroutine CENTER_CHECK
      subroutine CENTER(orientation) bind(C,name="CENTER")
      use iso_c_binding
      integer(c_int), intent(in)::orientation
      end subroutine CENTER
