#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE NUMTIME_NOW(timearr,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$NUMTIM 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	status
	INTEGER*2	timearr(7)

C  System Call
	INTEGER*4	SYS$NUMTIM


	status = SYS$NUMTIM(timearr,)


	RETURN
	END
