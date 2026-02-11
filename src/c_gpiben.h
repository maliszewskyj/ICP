C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
      subroutine ENET_INIT() bind(C,name="ENET_INIT")
      use iso_c_binding
      end subroutine
      subroutine GPIBEN_LISTEN(addr,err) bind(C,name="GPIBEN_LISTEN")
      use iso_c_binding
      integer(c_int), intent(in)::addr
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBEN_UNL(err) bind(C,name="GPIBEN_UNL")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBEN_TALK(addr,err) bind(C,name="GPIBEN_TALK")
      use iso_c_binding
      integer(c_int), intent(in)::addr
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBEN_UNT(err) bind(C,name="GPIBEN_UNT")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBEN_EXECUTE(err) bind(C,name="GPIBEN_EXECUTE")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine 
      subroutine GPIBEN_WR(string,length,err) bind(C,name="GPIBEN_WR")
      use iso_c_binding
      character(kind=c_char), intent(in)::string
      integer(c_int)        , intent(in)::length
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBEN_RD(string,expected,length,err)
     $     bind(C,name="GPIBEN_RD")
      use iso_c_binding
      character(kind=c_char), intent(out)::string
      integer(c_int)        , intent(in)::expected
      integer(c_int)        , intent(out)::length
      integer(c_int)        , intent(out)::err
      end subroutine
      subroutine GPIBEN_RDBLK(string,expected,length,err)
     $     bind(C,name="GPIBEN_RDBLK")
      use iso_c_binding
      character(kind=c_char), intent(out)::string
      integer(c_int)        , intent(in)::expected
      integer(c_int)        , intent(out)::length
      integer(c_int)        , intent(out)::err
      end subroutine
