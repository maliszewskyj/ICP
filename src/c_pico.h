C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
          subroutine RS232I_INIT() bind(C,name="RS232I_INIT")
          use iso_c_binding
          end subroutine RS232I_INIT
          integer(c_int) function PICOSETPOS(motorno, position) bind(C,name="PICOSETPOS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in)::  position
          end function PICOSETPOS
          integer(c_int) function PICOGETPOS(motorno, position) bind(C,name="PICOGETPOS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(out)::  position
          end function PICOGETPOS
          integer(c_int) function PICOGOREL(motorno, position) bind(C,name="PICOGOREL")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in):: motorno
          integer(c_int), intent(in):: position
          end function PICOGOREL
          integer(c_int) function PICOSTATUS(motorno, moving) bind(C,name="PICOSTATUS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          integer(c_int), intent(out):: moving
          end function PICOSTATUS
          integer(c_int) function PICOHALT() bind(C,name="PICOHALT")
          use iso_c_binding, only: c_int
          end function PICOHALT
