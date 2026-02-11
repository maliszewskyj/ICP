#include "icp.h"
	SUBROUTINE HoldS (tim_sec)
C	*************************

C-----------------------------------------------------------------------------C
C                                                                             C
C    Routine will pause icp for the given time in seconds.                    C	
C    Called from: New_Temp, Parse_Contr                                       C
C                                                                             C
C    Gayle Greene, March, 1994                                                C
C-----------------------------------------------------------------------------C

	use instr
	use smg_prep
	use iso_c_binding
	implicit none
	interface
#include "c_libwait.h"
	end interface

	integer status
	REAL*4    tim_sec
	LOGICAL	  Readef !function

	call setef(stop)
	call libwait(tim_sec,status)
        if (readef(contr_a)) call clref(stop)
	call clref (Contr_A)
	call clref (hdw_event) !remote abort

	RETURN
	END

