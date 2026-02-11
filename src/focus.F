#include "icp.h"
	REAL*4 FUNCTION foc7 (x)
C	************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates the focussing angle (no. 7) as a function of angle 2         C
C     Called from: Run_Q_Buf, Parse_Contr                                     C
C                                                                             C
C     Beat Schmid, August 1990                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT      none
	REAL*4        x

	IF (nsta .EQ. -5)THEN
	  foc7 = 8.17086 - 0.13342 * x + .0005871 * x**2
	ELSE
	  foc7 = 24.024 - 0.333 * x + 0.00218 * x**2
	ENDIF
	END

	REAL*4 FUNCTION foc8 (x)
C	************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates the focussing angle (no. 7) as a function of angle 2         C
C     Called from: Run_Q_Buf, Parse_Contr                                     C
C                                                                             C
C     Beat Schmid, August 1990                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT      none
	REAL*4        x

	foc8 = 22.644 - 0.210 * x + 0.00117 * x**2
	END
