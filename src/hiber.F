#include "icp.h"
	SUBROUTINE HIBER()
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$HIBER   		      C
C                                                                             C
C	Gayle Greene, July     , 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE


	call sleep(36000) !10 hours

	RETURN
	END
