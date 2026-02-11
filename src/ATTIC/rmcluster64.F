#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE rmCluster64
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$DACEFC for cluster #64    C
C                                                                             C
C	Gayle Greene, April 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

#if defined(VMS)
C  System Call
	INTEGER*4	SYS$DACEFC

	Call SYS$DACEFC (%VAL(64))
#endif						 	    ! communication
	RETURN
	END
