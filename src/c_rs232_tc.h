      subroutine RS232TC_INIT(baud,bits,parity)
     $     bind(C,name="RS232TC_INIT")
      use iso_c_binding
      integer(c_int)       , intent(in)::baud
      integer(c_int)       , intent(in)::bits
      integer(c_int)       , intent(in)::parity
      end subroutine RS232TC_INIT
      subroutine RS232TC_RD(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232TC_RD")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      end subroutine RS232TC_RD
      subroutine RS232TC_LK(wrstring,lwr,rdstring,lrd,rdreply)
     $     bind(C,name="RS232TC_LK")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring(*)
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring(*)
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::rdreply
      end subroutine RS232TC_LK
      subroutine OM232O(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="OM232O")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine OM232O
      subroutine RS232TC_OM(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232TC_OM")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine RS232TC_OM
      subroutine RS232TC_NL(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232TC_NL")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine RS232TC_NL
      subroutine RS232TC_LV(wrstring,lwr,rdstring,lrd,getreply)
     $     bind(C,name="RS232TC_LV")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in)::getreply
      end subroutine RS232TC_LV
      subroutine LV_READY() bind(C,name="LV_READY")
      use iso_c_binding
      end subroutine
      subroutine RS232TC_LR700(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232TC_LR700")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine RS232TC_LR700
      subroutine RS232TC_LTC(wrstring,lwr,rdstring,lrd,rdreply)
     $     bind(C,name="RS232TC_LTC")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::rdreply
      end subroutine RS232TC_LTC
      subroutine RS232TC_FRN(wrstring,lwr,rdstring,lrd, tchar, cksum)
     $     bind(C,name="RS232TC_FRN")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::tchar
      integer(c_int)        , intent(in) ::cksum
      end subroutine RS232TC_FRN
      subroutine RS232TC_CR(wrstring,lwr,rdstring,lrd,rdreply)
     $     bind(C,name="RS232TC_CR")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      integer(c_int)        , intent(in) ::rdreply
      end subroutine RS232TC_CR
      subroutine RS232TC_OX(wrstring,lwr,rdstring,lrd)
     $     bind(C,name="RS232TC_OX")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine RS232TC_OX
      subroutine OMCN_READTEMPS(sample,control,setpoint,err)
     $     bind(C,name="OMCN_READTEMPS")
      use iso_c_binding
      real(c_float), intent(out)::sample
      real(c_float), intent(out)::control
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine OMCN_READTEMPS
      subroutine OMCNI_READTEMPS(sample,control,setpoint,err)
     $     bind(C,name="OMCNI_READTEMPS")
      use iso_c_binding
      real(c_float), intent(out)::sample
      real(c_float), intent(out)::control
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine OMCNI_READTEMPS
      subroutine THERMO_READTEMPS(sample,control,setpoint,err)
     $     bind(C,name="THERMO_READTEMPS")
      use iso_c_binding
      real(c_float), intent(out)::sample
      real(c_float), intent(out)::control
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine THERMO_READTEMPS
      subroutine OMCN_WRITETEMP(setpoint,err)
     $     bind(C,name="OMCN_WRITETEMP")
      use iso_c_binding
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine OMCN_WRITETEMP
      subroutine OMCNI_WRITETEMP(setpoint,err)
     $     bind(C,name="OMCNI_WRITETEMP")
      use iso_c_binding
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine OMCNI_WRITETEMP
      subroutine THERMO_WRITETEMP(setpoint,err)
     $     bind(C,name="THERMO_WRITETEMP")
      use iso_c_binding
      real(c_float), intent(out)::setpoint
      integer(c_int),intent(out)::err
      end subroutine THERMO_WRITETEMP
      subroutine OMCN_PIDR(P,I,D,R,err)
     $     bind(C,name="OMCN_PIDR")
      use iso_c_binding
      real(c_float), intent(in)::P
      real(c_float), intent(in)::I
      real(c_float), intent(in)::D
      real(c_float), intent(in)::R
      integer(c_int),intent(out)::err
      end subroutine OMCN_PIDR
      subroutine PF_QUERY(newset,setpoint,sample,control,err)
     $     bind(C,name="PF_QUERY")
      use iso_c_binding
      real(c_float), intent(in)::newset
      real(c_float), intent(out)::setpoint
      real(c_float), intent(out)::sample
      real(c_float), intent(out)::control
      integer(c_int),intent(out)::err
      end subroutine PF_QUERY
