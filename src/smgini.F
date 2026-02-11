#include "icp.h"
	SUBROUTINE Smgini
C	*****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       ROUTINE TO INITIALIZE SCREEN MANAGEMENT                                 C
C       Called from: ICP_MAIN                                                   C
C       C
C       Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use icpsig
	use iso_c_binding
	IMPLICIT   none
	interface
          integer(c_int) function CATCH() bind(C,name="CATCH")
          use iso_c_binding
          end function CATCH
	end interface

	integer retn

	sigint = 0
	sigquit = 0
	sigtstp = 0
	sigterm = 0

	IF (priv) then
	   retn = CATCH()
	   sigterm = 0
	endif

	END      
