C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
          subroutine RS232EZ_INIT() bind(C,name="RS232EZ_INIT")
          use iso_c_binding
          end subroutine RS232EZ_INIT
          integer(c_int) function  EZSETPOS(motorno, position) bind(C,name="EZSETPOS")
          use iso_c_binding, only: c_int, c_float
          integer(c_int), intent(in):: motorno
          real(c_float), intent(in)::  position
          end function EZSETPOS
          integer(c_int) function EZGETPOS(motorno, position) bind(C,name="EZGETPOS")
          use iso_c_binding, only: c_int, c_float
          integer(c_int), intent(in):: motorno
          real(c_float), intent(out)::  position
          end function EZGETPOS
          integer(c_int) function EZGOPOS(motorno, position) bind(C,name="EZGOPOS")
          use iso_c_binding, only: c_int, c_float
          integer(c_int), intent(in):: motorno
          real(c_float), intent(in)::  position
          end function EZGOPOS
          integer(c_int) function EZSTATUS(motorno, moving, plus, minus, home) bind(C,name="EZSTATUS")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          integer(c_int), intent(out):: moving
          integer(c_int), intent(out):: plus  
          integer(c_int), intent(out):: minus
          integer(c_int), intent(out):: home
          end function EZSTATUS
          subroutine EZHOME(motorno, direction) bind(C,name="EZHOME")
          use iso_c_binding
          integer(c_int), intent(in)::motorno
          integer(c_int), intent(in)::direction
          end subroutine EZHOME
          integer(c_int) function EZHALT(motorno) bind(C,name="EZHALT")
          use iso_c_binding, only: c_int
          integer(c_int), intent(in)::  motorno  
          end function EZHALT
