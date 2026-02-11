#include "icp.h"
	SUBROUTINE Getval (buffer,value,*)

C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C                                                                             C
C	Gayle Greene,  October 1997					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use globals
	IMPLICIT     none
	CHARACTER*(*)  buffer
	REAL*4   tmpval,value
	INTEGER*4	i, iv
	LOGICAL		isdigit

	IF (buffer(1:2) .EQ. 'VL')THEN  !vlat
	  value = vlat
	ELSEIF (buffer(1:2) .EQ. 'VZ')THEN  !vz
	  value = vz
	ELSEIF (buffer(1:1) .EQ. 'V')THEN
	  IF (.NOT. isdigit(buffer(2:2)))RETURN 1 !error
	  i = 2
	  do while (isdigit(buffer(i:i)))
	    i = i + 1
	  enddo
	  read(unit=buffer(2:i-1),fmt=*,err=900)iv
	  IF (iv .lt. 0 .OR. iv .gt. 9) RETURN 1 !error
	  value = vg(iv)
	ELSE
	  IF (.NOT. isdigit(buffer(1:1)))RETURN 1 !error
	  read(unit=buffer(1:),fmt=*,err=900)tmpval
	  value = tmpval
	ENDIF
	RETURN

 900	RETURN 1 !error return
	END
