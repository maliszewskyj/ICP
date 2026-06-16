#include "icp.h"
	SUBROUTINE Downcase(dest,source)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC STR$UPCASE  		      C
C                                                                             C
C	Gayle Greene, July     , 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

	character*(*) dest,source

	integer*4 i,j,minlen,asc

	i = LEN(source)
	j = LEN(dest)
	minlen = i
	if (j.lt.i)minlen=j
	DO i=1,minlen
	   asc = ichar(source(i:i))
	   IF (asc .GE. 65 .AND. asc .LE. 90) THEN !convert to uppercase
	      asc = asc + 32
	      dest(i:i) = char(asc)
	   ELSE
	      dest(i:i) = source(i:i)
	   ENDIF
	ENDDO

	RETURN
	END
