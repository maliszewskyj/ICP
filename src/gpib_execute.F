#include "icp.h"
	SUBROUTINE Gpib_Execute (errflg)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send "group execute trigger"    via CAMAC GBIP module        C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface

	INTEGER*4   ierr
	LOGICAL     errflg
      
	ierr=0
	errflg=.false. !no errors for now
	IF (gpibenet) THEN
	   CALL GPIBEN_EXECUTE(ierr)
	ELSE
	   CALL GPIBCT_EXECUTE(ierr)
	ENDIF
	if (ierr.ne.0) errflg=.true.
	RETURN
	END
      
      
