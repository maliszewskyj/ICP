#include "icp.h"
	SUBROUTINE Hold (tim_min)
C	*************************

C-----------------------------------------------------------------------------C
C                                                                             C
C    Routine will pause icp for the given time in minutes.                    C	
C    Called from: New_Temp, Parse_Contr                                       C
C                                                                             C
C    Beat Schmid, February 1990                                               C
C-----------------------------------------------------------------------------C
	use instr
	use smg_prep
	use iso_c_binding
	IMPLICIT  none
	interface
#include "c_libwait.h"
	end interface
	real      fsecs
        integer*4 status
	REAL*4    tim_min
	LOGICAL	  Readef	

C	print *,'Hold = ',tim_min
	fsecs = tim_min*60.
	call setef(stop)
	call libwait(fsecs,status)
        if (readef(contr_a)) call clref(stop)
	call clref (Contr_A)
	call clref (hdw_event) !remote abort
C        print *,'Hold finished' 
	RETURN
	END

