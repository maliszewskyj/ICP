#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE LibSignal(status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$SIGNAL   		      C
C                                                                             C
C	Gayle Greene, April   18, 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	status

#if defined(VMS)
C  System Call 
	INTEGER*4	LIB$SIGNAL

	include '($ssdef)' !do we need this?

	CALL  LIB$SIGNAL(%val(status))
#else
	type *,' Lib Signal Err # ',status
#endif
	RETURN
	END
