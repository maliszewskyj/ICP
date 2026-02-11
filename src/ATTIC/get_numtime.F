#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE GET_NUMTIME(timearr,intime,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$NUMTIM 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	intime(2), status
	INTEGER*2	timearr(7)

C  System Call
	INTEGER*4	SYS$NUMTIM


	status = SYS$NUMTIM(timearr,intime)


	RETURN
	END
