#include "icp.h"
	SUBROUTINE EXT_TO_INT_TIME(intime,extime,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$CVT_FROM_INTERNAL_TIME    C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	intime(2), !internal format at output
     1			status,
     1			extime !external time in secs


	intime(1) = extime	

	RETURN
	END
