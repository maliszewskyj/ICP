#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE PUTCHARS_REND(DISPID,OUTSTR,IROW,ICOL,IRSET)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID, !DISPLAY ID
     1			IROW,ICOL,IRSET
	CHARACTER*(*)	OUTSTR

C  System Call
	INTEGER*4	SMG$PUT_CHARS
C  Local Variables for call
	INTEGER*4	iflags, ircompl, ichar

	INCLUDE		'($SMGDEF)' 

C  Set optional parameters to default values
	iflags = 0
	ircompl = 0
	ichar = SMG$C_ASCII


	CALL SMG$PUT_CHARS(DISPID,OUTSTR,IROW,ICOL,IFLAGS,IRSET,IRCOMPL,
     1			    ICHAR)

	RETURN
	END
