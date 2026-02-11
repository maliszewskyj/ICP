#include "icp.h"
	REAL*4 Function Ohms_To_K(ohms)
c       ********************************

C This routine converts a value in resistance which is passed as the 
c  calling parameter to a value in temp (K) which is returned
c  as the value of the function. It uses the coefficients stored in
c  array hdwtemp to do this.
	use tc
	IMPLICIT NONE

C  Calling parameters
	REAL*4 ohms !contains resistance value at call. Is not changed.

C  Local variables
	INTEGER*4 irange,i
	REAL*4	log_ohms, ctemp
	LOGICAL	found

C  External function
	REAL*4   Chebev

C  Determine applicable temperature range by ???
	irange = 1
	IF (hdwlong .GT. 1 )THEN !must decide which temp range to use
	  found = .false.
	  i = 1
	  DO WHILE (i .LE. hdwlong .AND. .NOT. found)
	    IF (ohms .le. incfs(i)%lotemp .AND. 
     1		 ohms .ge. incfs(i)%hitemp)THEN
	      found = .true.
	      irange = i
	     ENDIF
	     i = i + 1
	  ENDDO !while
	  IF (.not. found)THEN
	    CALL Putcontr(' Resistance out of conversion range')
	    Ohms_To_K = -999. !error flag
	    RETURN
	  ENDIF
	ENDIF

C  Use Chebyshev evaluation to convert log(Resistance) to Temp
	log_ohms =  alog10(ohms)
	ctemp = chebev(incfs(irange)%loz,incfs(irange)%hiz,
     1		hdwtemp(incfs(irange)%start),incfs(irange)%numb,log_ohms)

	IF (ctemp .NE. -999.)THEN
	  Ohms_To_K = ctemp
	ELSE
	  Ohms_To_K = -999. !error flag
	ENDIF

	RETURN
	END
