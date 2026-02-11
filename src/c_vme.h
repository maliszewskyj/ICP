C     Interface functions for C Parker Zeta6104 extension
C     Include between "interface" and "end interface"
      subroutine RS232V_INIT() bind(C,name="RS232V_INIT")
      use iso_c_binding
      end subroutine RS232V_INIT
      subroutine VSCALERSTART(preset,mode) bind(C,name="VSCALERSTART")
      use iso_c_binding
      integer(c_int), intent(in)::preset
      integer(c_int), intent(in)::mode
      end subroutine VSCALERSTART
      subroutine VSCALERSTATUS(iscounting) bind(C,name="VSCALERSTATUS")
      use iso_c_binding
      integer(c_int), intent(out)::iscounting
      end subroutine VSCALERSTATUS
      subroutine VSCALERSTOP() bind(C,name="VSCALERSTOP")
      use iso_c_binding
      end subroutine VSCALERSTOP
      subroutine VSCALERSELECT(which) bind(C,name="VSCALERSELECT")
      use iso_c_binding
      integer(c_int), intent(in)::which
      end subroutine VSCALERSELECT
      subroutine VSCALERADD() bind(C,name="VSCALERADD")
      use iso_c_binding
      end subroutine VSCALERADD
      subroutine VSCALERRESET() bind(C,name="VSCALERRESET")
      use iso_c_binding
      end subroutine VSCALERRESET
      subroutine VSCALERREAD(seconds,counts) bind(C,name="VSCALERREAD")
      use iso_c_binding
      real(c_float), intent(out)::seconds
      integer(c_int)::counts(37)
      end subroutine VSCALERREAD
      integer(c_int) function VMESETPOS(motorno, position)
     $     bind(C,name="VMESETPOS")
      use iso_c_binding, only: c_int, c_float
      integer(c_int), intent(in):: motorno
      real(c_float), intent(in)::  position
      end function VMESETPOS
      integer(c_int) function VMEGETPOS(motorno, position)
     $     bind(C,name="VMEGETPOS")
      use iso_c_binding, only: c_int,c_float
      integer(c_int), intent(in):: motorno
      real(c_float), intent(out)::  position
      end function VMEGETPOS
      integer(c_int) function VMEGOPOS(motorno, position)
     $     bind(C,name="VMEGOPOS")
      use iso_c_binding, only: c_int,c_float
      integer(c_int), intent(in):: motorno
      real(c_float), intent(in):: position
      end function VMEGOPOS
      integer(c_int) function VMESTATUS(motorno, moving,limstat)
     $     bind(C,name="VMESTATUS")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in)::  motorno  
      integer(c_int), intent(out):: moving
      integer(c_int), intent(out):: limstat
      end function VMESTATUS
      integer(c_int) function VMEHALT(motorno) bind(C,name="VMEHALT")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in)::  motorno  
      end function VMEHALT
      subroutine VMEENABLE(motorno) bind(C,name="VMEENABLE")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in)::  motorno  
      end subroutine VMEENABLE
      subroutine VMEDISABLE(motorno) bind(C,name="VMEDISABLE")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in)::  motorno  
      end subroutine VMEDISABLE
      subroutine VMEHOME(motorno, direction) bind(C,name="VMEHOME")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in):: motorno
      integer(c_int), intent(in):: direction
      end subroutine VMEHOME
      subroutine VMEGOLIM(motorno, direction) bind(C,name="VMEGOLIM")
      use iso_c_binding, only: c_int,c_float
      integer(c_int), intent(in):: motorno
      integer(c_int), intent(in):: direction
      end subroutine VMEGOLIM
      subroutine VPARPORT(inval) bind(C,name="VPARPORT")
      use iso_c_binding, only: c_int
      integer(c_int), intent(out):: inval
      end subroutine VPARPORT

