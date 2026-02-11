#include "icp.h"
	SUBROUTINE SETEF(iflag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$SETEF 		      C
C                                                                             C
C	Gayle Greene, April 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use eventflag
	IMPLICIT NONE
	INTEGER*4	iflag

	evflags(iflag) = .true.

	RETURN
	END

