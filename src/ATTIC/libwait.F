#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE LIBWAIT(rsecs,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$WAIT     		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	REAL*4		rsecs
	INTEGER*4	status

C  System Call 
	INTEGER*4	LIB$WAIT

	status =  LIB$WAIT(%ref(rsecs))

	RETURN
	END
