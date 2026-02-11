#include "icp.h"
	SUBROUTINE WrPol (string, length, error,pstype)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send character string over GPIB bus via CAMAC module         C
C                                                                             C
C     Gayle Greene, February 1991
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT      none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface
	INTEGER*2     i2data,pstype !type of power supply. ie:poltype(psno)
	CHARACTER*20  string
	LOGICAL       error
	INTEGER*4     length, i, status,ierr
      
	ierr=0
	error=.false.
	IF (gpibenet) THEN
	   CALL GPIBEN_WR(string,length,ierr)
	ELSE
	   CALL GPIBCT_WR(string,length,ierr)
	ENDIF
	if (ierr.ne.0) error=.true.
	END      
      
