C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
          subroutine RS232_INIT() bind(C,name="RS232_INIT")
          use iso_c_binding
          end subroutine RS232_INIT
          subroutine RS232END() bind(C,name="RS232END")
          use iso_c_binding
          end subroutine RS232END
          subroutine RS232(wrstring,lwr,rdstring,lrd) bind(C,name="RS232")
          use iso_c_binding
          character(kind=c_char), intent(in)::wrstring
          integer(c_int)        , intent(in)::lwr
          character(kind=c_char), intent(out)::rdstring
          integer(c_int)        , intent(out)::lrd
          end subroutine RS232
          subroutine RS232F_INIT() bind(C,name="RS232F_INIT")
          use iso_c_binding
          end subroutine RS232F_INIT
          subroutine RS232F(wrstring,lwr,rdstring,lrd) bind(C,name="RS232F")
          use iso_c_binding
          character(kind=c_char), intent(in)::wrstring
          integer(c_int)        , intent(in)::lwr
          character(kind=c_char), intent(out)::rdstring
          integer(c_int)        , intent(out)::lrd
          end subroutine RS232F
