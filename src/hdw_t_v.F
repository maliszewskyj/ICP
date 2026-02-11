#include "icp.h"


	REAL*4 Function HDW_T_V(ctemp)
c       ********************************

C This routine converts a value in temperature which is passed as the 
c  calling parameter to a value in volts which is returned
c  as the value of the function. It uses the curve stored in hardware
c  arrays HDWvolts and HDWtemp to do this. There are HDWlong points
c  in HDWvolts and HDWtemp.
	use tc
	IMPLICIT NONE

C  Calling parameters
	REAL*4 ctemp !contains value at call. Is not changed.

C  Local variables
	INTEGER*4 I
	REAL*4 SLOPE

C  Loop through array to find larger temp value
	I=1
	DO WHILE (I .LE. MAXPNTS .AND. HDWtemp(i) .GT. ctemp)
	  i=i+1
	ENDDO
c	---------------------CHECK RANGE-------------------------
	IF (HDWtemp(i) .EQ. 0) THEN 		!temp too low
	  HDW_T_V = -1
	ELSE IF (CTEMP .GT. HDWtemp(1)) THEN 	!temp too high 
	  HDW_T_V = -2
	ELSE IF (CTEMP .EQ. HDWtemp(1)) THEN	!special case 
	  HDW_T_V=HDWvolts(1)
	ELSE
C	------------------LINEAR EXTRAPOLATION------------------  
	  SLOPE = (HDWvolts(i)-HDWvolts(i-1))/(HDWtemp(i)-HDWtemp(i-1))
	  HDW_T_V = HDWvolts(i)+SLOPE*(CTEMP-HDWtemp(i))
	ENDIF

	RETURN
	END
