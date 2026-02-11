C     Interface functions for C allmotion ez extension
C     Include between "interface" and "end interface"
          subroutine LIBWAIT(fracsec, status) bind(C,name="LIBWAIT")
          use iso_c_binding, only: c_int,c_float
          real(c_float), intent(in)  :: fracsec
          integer(c_int), intent(out):: status
          end subroutine LIBWAIT
