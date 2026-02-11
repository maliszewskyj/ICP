#include "icp.h"
	SUBROUTINE RLYON(NORELAY)
C
	use instr
	IMPLICIT NONE
	INTEGER*4 NORELAY
C
C
C	norelay = norelay - 1
C Reverse because the expectation is that we're really driving a relay
C	IF (AMSCALER) CALL AMDIO_RELAY(NORELAY, 0) 

C
	RETURN 
	END
