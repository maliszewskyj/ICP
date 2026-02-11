#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE PUTLINE_WRAPCHAR(DISPID,OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID !DISPLAY ID
	CHARACTER*(*)	OUTSTR

C  System Call
	INTEGER*4	SMG$PUT_LINE

	INCLUDE         '($SMGDEF)'


	CALL SMG$PUT_LINE(DISPID,OUTSTR,,,,SMG$M_WRAP_CHAR)


	RETURN
	END
