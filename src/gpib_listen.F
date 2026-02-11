#include "icp.h"
	SUBROUTINE Gpib_listen (psno,errflg)
C     	************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send set CAMAC GBIP module in talk mode and power supply     C
C     PSNO in listen mode                                                     C
C									      C
C     Gayle Greene, January 1991					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface
	INTEGER*4   psno  !power supply number (1 to 6)
        INTEGER*2   i2data
	INTEGER*4   ierr
	LOGICAL     errflg

	ierr=0
	errflg=.false. !signal no errors for now
	if (gpibenet) then
	   CALL GPIBEN_UNL(ierr)
	   CALL GPIBEN_LISTEN(psno,ierr)
	else 
	   CALL GPIBCT_UNL(ierr)
	   CALL GPIBCT_LISTEN(psno,ierr)
	endif
	if (ierr.ne.0) errflg=.true.
	RETURN
	END
      
      
