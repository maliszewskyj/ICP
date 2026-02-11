#include "icp.h"
	SUBROUTINE CLREF(iflag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$CLREF 		      C
C                                                                             C
C	Gayle Greene, April 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	use icpsig
	use eventflag
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	iflag
	evflags(iflag) = .false.

	if (iflag .eq. contr_a) then
	  sigint = 0
	elseif (iflag .eq. pause) then
	  sigtstp = 0
	endif

	RETURN
	END
