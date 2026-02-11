#include "icp.h"
	REAL*4 Function SNS_T_V(ctemp)
c       ********************************

C This routine converts a value in temperature which is passed as the 
c  calling parameter to a value in volts which is returned
c  as the value of the function. It uses the curve stored in sensor
c  arrays snsvolts and snstemp to do this. There are snslong points
c  in snsvolts and snstemp.
	use tc
	IMPLICIT NONE

C  Calling parameters
	REAL*4 ctemp !contains value at call. Is not changed.

C  Local variables
	INTEGER*4 I
	REAL*4 SLOPE

C  Loop through array to find larger temp value
	I=1
	DO WHILE (I .LE. MAXPNTS .AND. snstemp(i) .GT. ctemp)
	  i=i+1
	ENDDO
c	---------------------CHECK RANGE-------------------------
	IF (snstemp(i) .EQ. 0) THEN 		!temp too low
	  SNS_T_V = -1
	ELSE IF (CTEMP .GT. snstemp(1)) THEN 	!temp too high 
	  SNS_T_V = -2
	ELSE IF (CTEMP .EQ. snstemp(1)) THEN	!special case 
	  SNS_T_V=snsvolts(1)
	ELSE
C	------------------LINEAR EXTRAPOLATION------------------  
	  SLOPE = (snsvolts(i)-snsvolts(i-1))/(snstemp(i)-snstemp(i-1))
	  SNS_T_V = snsvolts(i)+SLOPE*(CTEMP-snstemp(i))
	ENDIF

	RETURN
	END
