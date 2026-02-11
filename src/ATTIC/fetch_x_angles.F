#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Fetch_X_Angles (xbuf, a_line, *)
C	*****************************************

	IMPLICIT     none
	INTEGER*4    j, restore, a_line(6), k
	REAL*4       RdAngl, af, afn
	CHARACTER*7  str7, PutStr

	INCLUDE  INSTR_CMN
	INCLUDE  DARTZ_CMN

	RECORD /x_buf/ xbuf

C--------------------- FETCH CURRENT ANGLES INTO BUFFER ----------------------C
 
	IF (.NOT. priv) THEN
	   CALL SMG$RING_BELL (prpx, 2)
	ELSE
	   DO j = 1, 6
	     k = xbuf.mn(j)
	     IF (k .NE. 0)THEN
	      IF (motfl(k)) THEN
		 xbuf.a(j) = RdAngl(k) - z(k)
	   	 IF (xbuf.lpts(j) .NE. 0.) THEN
	            af = xbuf.a(j) + (xbuf.lpts(j) -1)* xbuf.i(j)
	   	 ELSE
	      	    af = xbuf.a(j) 
	   	 END IF
		 str7 = PutStr (xbuf.a(j))
	   	 CALL PUTCHARS_REV (prpx, str7, a_line(j), 27)
		 str7 = PutStr (af)
	   	 CALL PUTCHARS_REV (prpx, str7, a_line(j), 60)
	      END IF
	     ENDIF !k .ne. 0
	   END DO
	END IF
	RETURN 1
	END
