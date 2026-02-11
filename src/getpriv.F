#include "icp.h"
	Subroutine GETPRIV
C	*******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	                                                                      C
C  GETPRIV sets common flag priv to  true if this process is using the 
C   privileged hardware   connection.
C	Gayle Greene: July, 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use ISO_C_BINDING
	IMPLICIT     none
	interface
	   integer(c_int) function CHKPRIV() bind(C,name="CHKPRIV")
	   use ISO_C_BINDING
	   end function CHKPRIV
	end interface

	INTEGER*4    iterm

	iterm = 0
	priv = .false.
	iterm = CHKPRIV()
	if (iterm .gt. 0) priv = .true.

	RETURN
	END
        
