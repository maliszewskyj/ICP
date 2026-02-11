#include "icp.h"
	SUBROUTINE Gpib_unlisten (errflg)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to unlisten all power supply units via CAMAC GBIP module        C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface

	INTEGER*2   i2data
	INTEGER*4   ierr
	LOGICAL     errflg

	ierr=0
	errflg=.false. !no errors for now
	IF (gpibenet) THEN
	   CALL GPIBEN_UNL(ierr)
	ELSE
	   CALL GPIBCT_UNL(ierr)
	ENDIF
	if (ierr.ne.0) errflg=.true.
	RETURN
	END
      
      
