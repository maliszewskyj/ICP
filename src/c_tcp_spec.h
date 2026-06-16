      subroutine SPEC_CONNECT() bind(C,name="Spec_Connect")
      use iso_c_binding
      end subroutine SPEC_CONNECT
      subroutine SPEC_DISCONNECT() bind(C,name="Spec_Disconnect")
      use iso_c_binding
      end subroutine SPEC_DISCONNECT
      subroutine SPEC_CMD(wrstring,lwr,error,rdstring,lrd)
     $     bind(C,name="Spec_Cmd")
      use iso_c_binding
      character(kind=c_char), intent(in)::wrstring
      integer(c_int)        , intent(in)::lwr
      integer(c_int)        , intent(in) ::error
      character(kind=c_char), intent(out)::rdstring
      integer(c_int)        , intent(out)::lrd
      end subroutine SPEC_CMD
