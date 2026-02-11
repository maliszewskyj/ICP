#include "icp.h"
	CHARACTER*7 FUNCTION PutStr (arg)
C	*********************************

	REAL*4      arg
	
	IF (ABS(arg) .LT. 10.) THEN
	   WRITE (UNIT=PutStr, FMT= '(f7.4)') arg
        ELSE IF (ABS(arg) .LT. 100.) THEN
	   WRITE (UNIT=PutStr, FMT= '(f7.3)') arg
        ELSE
	   WRITE (UNIT=PutStr, FMT= '(f7.2)') arg
	END IF
	END
