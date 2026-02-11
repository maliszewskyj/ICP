C     Interface functions for AM9513 counter timer chip
C     Include between "interface" and "end interface"
      subroutine SCALERINIT() bind(C,name="SCALERINIT")
      use iso_c_binding
      end subroutine SCALERINIT
      subroutine SCALERCLOSE() bind(C,name="SCALERCLOSE")
      use iso_c_binding
      end subroutine SCALERCLOSE
      subroutine SCALERSTART(preset,mode)
     $     bind(C,name="SCALERSTART")
      use iso_c_binding, only: c_int, c_float
      integer(c_int), intent(in):: preset
      integer(c_int), intent(in):: mode
      end subroutine SCALERSTART
      integer(c_int) function SCALERPROGRESS()
     $     bind(C,name="SCALERPROGRESS")
      use iso_c_binding, only: c_int,c_float
      end function SCALERPROGRESS
      subroutine SCALERSTATUS(iscounting) bind(C,name="SCALERSTATUS")
      use iso_c_binding, only: c_int
      integer(c_int), intent(out)::iscounting
      end subroutine SCALERSTATUS
      subroutine SCALERREAD(seconds,moncounts,scalercounts)
     $     bind(C,name="SCALERREAD")
      use iso_c_binding, only: c_int,c_float
      real(c_float),  intent(out)::seconds
      integer(c_int), intent(out)::moncounts  
      integer(c_int), intent(out)::scalercounts
      end subroutine SCALERREAD
      subroutine SCALERRESET() bind(C,name="SCALERRESET")
      use iso_c_binding, only: c_int
      end subroutine SCALERRESET
      subroutine AMDIO_RELAY(bit, state) bind(C,name="AMDIO_RELAY")
      use iso_c_binding, only: c_int
      integer(c_int), intent(in)::bit
      integer(c_int), intent(in)::state
      end subroutine AMDIO_RELAY

