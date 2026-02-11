      MODULE ICPSIG
      use iso_c_binding
      integer(c_int), bind(C,name="sigint")  :: sigint
      integer(c_int), bind(C,name="sigquit") :: sigquit
      integer(c_int), bind(C,name="sigtstp") :: sigtstp
      integer(c_int), bind(C,name="sigterm") :: sigterm
      END MODULE ICPSIG
