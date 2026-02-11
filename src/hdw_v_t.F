#include "icp.h"
	REAL*4 Function HDW_V_T(volts)
c       ********************************

C This routine converts a value in volts which is passed as the 
c  calling parameter to a value in temperature which is returned
c  as the value of the function. It uses the curve stored in sensor
c  arrays HDWvolts and HDWtemp to do this. There are HDWlong points
c  in HDWvolts and HDWtemp.
	use tc
	IMPLICIT NONE

C  Calling parameters
	REAL*4 volts !contains value at call. Is not changed.

C  Local variables
	INTEGER*4 I
	REAL*4 SLOPE

C  Loop through array to find larger volt value
	I=1
	DO WHILE (I .LE. maxpnts .AND. HDWvolts(i) .LT. volts)
	  i=i+1
	ENDDO
C	--------------------------CHECK RANGE------------------------
	IF (HDWvolts(1) .GT. VOLTS) THEN !volts too high-->set T=-1
		HDW_V_T = -1
	ELSE IF (i .GT. maxpnts) THEN !volts too low
	  	HDW_V_T = -2 
	ELSE IF (VOLTS .EQ. HDWvolts(1)) THEN !special case
		HDW_V_T=HDWtemp(1)
	ELSE

C	 --------------------LINEAR EXTRAPOLATION--------------------
C
	 SLOPE = (HDWtemp(i)-HDWtemp(i-1))/(HDWvolts(i)-HDWvolts(i-1))
	 HDW_V_T = HDWtemp(i-1) + (SLOPE*(VOLTS-HDWvolts(i-1)))
	ENDIF
C	--------------------------------------------------------------

	RETURN
	END


