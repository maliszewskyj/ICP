      subroutine RS232HC_INIT(baud,bits,parity,stop)
     $     bind(C,name="RS232HC_INIT")
      use iso_c_binding
      integer(c_int)       , intent(in)::baud
      integer(c_int)       , intent(in)::bits
      integer(c_int)       , intent(in)::parity
      integer(c_int)       , intent(in)::stop
      end subroutine RS232HC_INIT
      subroutine RS232HC_SC(wrstring,lwr,rdstring,lrd,wait,rdreply)
     $     bind(C,name="RS232HC_SC")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::wait
      integer(c_int)        , intent(in) ::rdreply
      end subroutine
      subroutine RS232_OX(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232_OX")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      end subroutine
      subroutine RS232_OXMAG(wrstring,lwr,rdstring,lrd,getreply)
     $     bind(C,name="RS232_OXMAG")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::getreply
      end subroutine
      integer(c_int) function RS232_ADAM(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232_ADAM")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      end function
      subroutine ADAM_SETVOLT(volt,err) bind(C,name="ADAM_SETVOLT")
      use iso_c_binding
      real(c_float)         , intent(in)::volt
      integer(c_int)        , intent(out)::err
      end subroutine
      subroutine ADAM_GETVOLT(volt,err) bind(C,name="ADAM_GETVOLT")
      use iso_c_binding
      real(c_float)         , intent(out)::volt
      integer(c_int)        , intent(out)::err
      end subroutine
      subroutine PSSW_INIT() bind(C,name="PSSW_INIT")
      use iso_c_binding
      end subroutine
      subroutine PSSW_TOGGLE(err) bind(C,name="PSSW_TOGGLE")
      use iso_c_binding
      integer(c_int)        , intent(out)::err
      end subroutine PSSW_TOGGLE
      subroutine PSSW_STATE(sign,busy,err) bind(C,name="PSSW_STATE")
      use iso_c_binding
      integer(c_int)        , intent(out)::sign
      integer(c_int)        , intent(out)::busy
      integer(c_int)        , intent(out)::err
      end subroutine PSSW_STATE
      integer(c_int) function ADAM_TALK(fd,wrstring,lout,rdstring,lin)
     $     bind(C,name="ADAM_TALK")
      use iso_c_binding
      integer(c_int)        , intent(in)::fd
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lout
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lin
      end function
      
