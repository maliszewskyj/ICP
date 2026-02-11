C     Interface functions for C Parker Zeta6104 extension
C     Include between "interface" and "end interface"
          subroutine RS232Z_INIT() bind(C,name="RS232Z_INIT")
          use iso_c_binding
          end subroutine RS232Z_INIT
          integer(c_int) function ZETASETPOS(motorno, position) bind(C,name="ZETASETPOS")
          use iso_c_binding, only: c_int, c_float
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in)::  position
          end function ZETASETPOS
          integer(c_int) function ZETAGETPOS(motorno, position) bind(C,name="ZETAGETPOS")
          use iso_c_binding, only: c_int,c_float
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(out)::  position
          end function ZETAGETPOS
          integer(c_int) function ZETAGOPOS(motorno, position) bind(C,name="ZETAGOPOS")
          use iso_c_binding, only: c_int,c_float
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in):: position
          end function ZETAGOPOS
          integer(c_int) function ZETASTATUS(motorno, moving,limstat) bind(C,name="ZETASTATUS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          integer(c_int), intent(out):: moving
          integer(c_int), intent(out):: limstat
          end function ZETASTATUS
          integer(c_int) function ZETAHALT(motorno) bind(C,name="ZETAHALT")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          end function ZETAHALT

