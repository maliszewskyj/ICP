#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Fetch_D_Angles (dbuf, a_line, *)
C	*****************************************
	IMPLICIT     none
	INTEGER*4    j, restore, a_line(6), k
	REAL*4       RdAngl, af, afn
	CHARACTER*7  str7, PutStr
	INCLUDE INSTR_CMN      ! Instrument and hardware parameters
	INCLUDE DARTZ_CMN
	RECORD /d_buf/ dbuf
C--------------------- FETCH CURRENT ANGLES INTO BUFFER ----------------------C
	IF (.NOT. priv) THEN
	   CALL SMG$RING_BELL (prpd, 2)
	ELSE
	   DO j = 1, 6
	     k = dbuf.mn(j)
	     IF (k .NE. 0)THEN
	      IF (motfl(k)) THEN
		 dbuf.a(j) = RdAngl(k) - z(k)
	   	 IF (dbuf.lpts(j) .NE. 0.) THEN
	            af = dbuf.a(j) + (dbuf.lpts(j) -1)* dbuf.i(j)
	   	 ELSE
	      	    af = dbuf.a(j) 
	   	 END IF
		 str7 = PutStr (dbuf.a(j))
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 15)
		 str7 = PutStr (af)
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 47)
	      END IF
	     ENDIF !k .ne. 0
	   END DO
	END IF
	RETURN 1
	END
