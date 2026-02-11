#include "icp.h"
	SUBROUTINE Gpib_Talk (gpadr,errflg)
C     	************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send set CAMAC GBIP module in listen mode and IEEE module    C
C     GPADR in talk  mode                                                     C
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

	INTEGER*4   gpadr  !gpadr
        INTEGER*4   ierr
	LOGICAL     errflg

	ierr=0
	errflg=.false. !signal no errors for now
	if (gpibenet) then
	   CALL GPIBEN_TALK(gpadr,ierr)
	else
	   CALL GPIBCT_TALK(gpadr,ierr)
	endif
	if (ierr.ne.0) errflg=.true.
	RETURN
	END
      
      
