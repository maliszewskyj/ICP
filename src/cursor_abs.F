#include "icp.h"
	SUBROUTINE CURSOR_ABS(DISPID,IROW,ICOL)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$SET_CURSOR_ABS	      C
C                                                                             C
C	Gayle Greene, MARCH, 1995                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID, !DISPLAY ID
     1			IROW,ICOL

	RETURN
	END
