#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE Ring_Bell(DISPID,numb)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$RING_BELL                 C
C                                                                             C
C	Gayle Greene, March , 1995                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID, !DISPLAY ID
     1			NUMB    !Number of times

C  System Call
	INTEGER*4	SMG$RING_BELL




	CALL SMG$RING_BELL (DISPID,numb)


	RETURN
	END
