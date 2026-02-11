C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
      subroutine RS232G_INIT() bind(C,name="RS232G_INIT")
      use iso_c_binding
      end subroutine RS232G_INIT
      subroutine RS232G_END() bind(C,name="RS232G_END")
      use iso_c_binding
      end subroutine RS232G_END
      subroutine GPIBCT_LISTEN(addr,err) bind(C,name="GPIBCT_LISTEN")
      use iso_c_binding
      integer(c_int), intent(in)::addr
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBCT_UNL(err) bind(C,name="GPIBCT_UNL")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBCT_TALK(addr,err) bind(C,name="GPIBCT_TALK")
      use iso_c_binding
      integer(c_int), intent(in)::addr
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBCT_UNT(err) bind(C,name="GPIBCT_UNT")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBCT_EXECUTE(err) bind(C,name="GPIBCT_EXECUTE")
      use iso_c_binding
      integer(c_int), intent(out)::err
      end subroutine 
      subroutine GPIBCT_WR(string,length,err) bind(C,name="GPIBCT_WR")
      use iso_c_binding
      character(kind=c_char), intent(in)::string
      integer(c_int)        , intent(in)::length
      integer(c_int), intent(out)::err
      end subroutine
      subroutine GPIBCT_RD(string,expected,length,err)
     $     bind(C,name="GPIBCT_RD")
      use iso_c_binding
      character(kind=c_char), intent(out)::string
      integer(c_int)        , intent(in)::expected
      integer(c_int)        , intent(out)::length
      integer(c_int)        , intent(out)::err
      end subroutine
      subroutine GPIBCT_RDBLK(string,expected,length,err)
     $     bind(C,name="GPIBCT_RDBLK")
      use iso_c_binding
      character(kind=c_char), intent(out)::string
      integer(c_int)        , intent(in)::expected
      integer(c_int)        , intent(out)::length
      integer(c_int)        , intent(out)::err
      end subroutine
