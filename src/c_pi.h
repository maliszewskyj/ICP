C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
          subroutine RS232P_INIT() bind(C,name="RS232P_INIT")
          use iso_c_binding
          end subroutine RS232P_INIT
          integer(c_int) function PISETPOS(motorno, position) bind(C,name="PISETPOS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in)::  position
          end function PISETPOS
          integer(c_int) function PIGETPOS(motorno, position) bind(C,name="PIGETPOS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(out)::  position
          end function PIGETPOS
          integer(c_int) function PIGOPOS(motorno, position) bind(C,name="PIGOPOS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in):: position
          end function PIGOPOS
          integer(c_int) function PISTATUS(motorno, moving, limstat) bind(C,name="PISTATUS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          integer(c_int), intent(out):: moving
          integer(c_int), intent(out):: limstat
          end function PISTATUS
          integer(c_int) function PIHALT() bind(C,name="PIHALT")
          use iso_c_binding, only: c_int
          end function PIHALT
