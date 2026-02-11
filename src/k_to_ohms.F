#include "icp.h"
	REAL*4 Function K_To_Ohms(ctemp)
c       ********************************

C       This routine converts a value in temperature which is passed as the 
c       calling parameter to a value in ohms which is returned
c       as the value of the function. It uses the coefficients stored in
c       array snstemp to do this.
	use tc
	IMPLICIT NONE

C       Calling parameters
	REAL*4 ctemp		!contains temp value at call in Kelvin. Is not changed.

C       Local variables
	INTEGER*4 irange, i
	REAL*4    ohms
	LOGICAL	  found

C       External function
	REAL*4    Chebev

C       Determine applicable temperature range by looking at outcfs.lotemp & outcfs.hitemp
	irange = 1
	IF (snslong .GT. 1 )THEN !must decide which temp range to use
	   found = .false.
	   i = 1
	   DO WHILE (i .LE. snslong .AND. .NOT. found)
	      IF (ctemp .ge. outcfs(i)%lotemp .AND. 
	1	   ctemp .le. outcfs(i)%hitemp)THEN
		 found = .true.
		 irange = i
	      ENDIF
	      i = i + 1
	   ENDDO		!while
	   IF (.not. found)THEN
	      CALL Putcontr(' Temp out of conversion range')
	      K_To_Ohms = -999. !error flag
	      RETURN
	   ENDIF
	ENDIF

C       Use Chebyshev evaluation to convert temp to log(Resistance)
	ohms = chebev(outcfs(irange)%loz,outcfs(irange)%hiz,
	1  snstemp(outcfs(irange)%start),outcfs(irange)%numb,ctemp)

	IF (ohms .NE. -999.)THEN
	   K_To_Ohms = 10**ohms
	ELSE
	   K_To_Ohms = -999.	!error flag
	ENDIF

	RETURN
	END
