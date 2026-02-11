#include "icp.h"
	SUBROUTINE READ_RELAY(inval)

	use instr
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_vme.h"
	end interface
	INTEGER*4 INVAL

	inval = 0 ! default if error
	CALL VPARPORT(INVAL)

	RETURN 
	END
