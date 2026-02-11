C     Interface functions for AIM style counters
C     Include between "interface" and "end interface"
      subroutine AIM_INIT() bind(C,name="AIM_INIT")
      use iso_c_binding
      end subroutine AIM_INIT
      subroutine AIM_ARM() bind(C,name="AIM_ARM")
      use iso_c_binding
      end subroutine AIM_ARM
      subroutine AIM_DISARM() bind(C,name="AIM_DISARM")
      use iso_c_binding
      end subroutine AIM_DISARM
      subroutine AIM_CLEAR() bind(C,name="AIM_CLEAR")
      use iso_c_binding
      end subroutine AIM_CLEAR
      integer(c_int) function AIM_DIMS(xdim,ydim) bind(C,name="AIM_DIMS")
      use iso_c_binding
      integer(c_int), intent(out) :: xdim
      integer(c_int), intent(out) :: ydim
      end function AIM_DIMS
      integer(c_int) function AIM_XFER(hist,histsize)
     $     bind(C,name="AIM_XFER")
      use iso_c_binding
      integer(c_int), intent(out) :: hist(*)
      integer(c_int), intent(in)  :: histsize
      end function AIM_XFER
      subroutine AIM_IMAGE(histo,nx,ny) bind(C,name="AIM_IMAGE")
      use iso_c_binding
      integer(c_int), intent(in)  :: histo(*)
      integer(c_int), intent(in)  :: nx
      integer(c_int), intent(in)  :: ny
      end subroutine AIM_IMAGE
      subroutine AIM_SAVE(histo,nx,ny) bind(C,name="AIM_SAVE")
      use iso_c_binding
      integer(c_int), intent(in)  :: histo(*)
      integer(c_int), intent(in)  :: nx
      integer(c_int), intent(in)  :: ny
      end subroutine AIM_SAVE
      
      
