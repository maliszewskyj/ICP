#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Wake_Hold
C     	********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine Executed When Timer In Hold Expires.                         C
C     'Abuses' the stop flag to signal normal completion.                     C
C                                                                             C
C     Beat Schmid, February 1990                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	INCLUDE   INSTR_CMN

	CALL SETEF (stop)
	CALL SETEF (Contr_A)
	CALL SYS$WAKE (,)
	END
