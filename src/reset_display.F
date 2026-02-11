#include "icp.h"
	SUBROUTINE Reset_Display

	use instr
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_vme.h"
#include "c_amscaler.h"
	end interface

	IF (nsta.ne.2.or.nsta.ne.9) THEN !small gate counters from storeroom
	   CALL RlyOn (3)		         ! Reset little display counter
	   CALL RlyOff (3)
	ELSE !NIM scaler module
	   CALL RlyOff (3)		         ! Reset NIM display counter
	   CALL RlyOn (3)
	ENDIF
	IF (vmedata) call VSCALERRESET
	IF (amscaler) call SCALERRESET
C
	RETURN 
	END
