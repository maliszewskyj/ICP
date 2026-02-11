#include "icp.h"
	REAL*4 Function SNS_V_T(volts)
c       ********************************

C This routine converts a value in volts which is passed as the 
c  calling parameter to a value in temperature which is returned
c  as the value of the function. It uses the curve stored in sensor
c  arrays snsvolts and snstemp to do this. There are snslong points
c  in snsvolts and snstemp.
	use tc
	IMPLICIT NONE

C  Calling parameters
	REAL*4 volts !contains value at call. Is not changed.

C  Local variables
	INTEGER*4 I
	REAL*4 SLOPE

C  Loop through array to find larger volt value
	I=1
	DO WHILE (I .LE. maxpnts .AND. snsvolts(i) .LT. volts)
	  i=i+1
	ENDDO
C	--------------------------CHECK RANGE------------------------
	IF (snsvolts(1) .GT. VOLTS) THEN !volts too high-->set T=-1
		SNS_V_T = -1
	ELSE IF (i .GT. maxpnts) THEN !volts too low
	  	SNS_V_T = -2 
	ELSE IF (VOLTS .EQ. snsvolts(1)) THEN !special case
		SNS_V_T=snstemp(1)
	ELSE

C	 --------------------LINEAR EXTRAPOLATION--------------------
C
	 SLOPE = (snstemp(i)-snstemp(i-1))/(snsvolts(i)-snsvolts(i-1))
	 SNS_V_T = snstemp(i-1) + (SLOPE*(VOLTS-snsvolts(i-1)))
	ENDIF
C	--------------------------------------------------------------

	RETURN
	END


