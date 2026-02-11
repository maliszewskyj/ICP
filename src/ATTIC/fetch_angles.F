#include "icp.h"
	SUBROUTINE Fetch_Angles (ibuf, a_line, *)
C	*****************************************

	IMPLICIT     none
	INTEGER*4    j, restore, a_line(6), lowest, highest
	REAL*4       RdAngl, af, afn
	CHARACTER*7  str7, PutStr

	INCLUDE  INSTR_CMN
	INCLUDE  SMGPREP_CMN
	INCLUDE  XRAY_CMN !for poly

	RECORD /i_buf/ ibuf

C-------------------------- DETERMINE WHETHER MOTOR ONE IS MOVED--------------C
	if (nsta.eq.4 .or. nsta.eq.-5 .or. nsta.eq.2 .or.
     1	    nsta.eq.9 .or. nsta.eq.-1 .OR.
     1	    (nsta .EQ. 0 .AND. poly) ) THEN
	  lowest=1 !1 is moved on these instruments
	  highest = 6
	elseif (nsta .eq. 1) THEN
	  lowest = 3
	  highest = 4
	else
	  lowest = 2 !1 is not moved on most instruments
	  highest = 6
	endif

C--------------------- FETCH CURRENT ANGLES INTO BUFFER ----------------------C
 
	IF (.NOT. priv) THEN
	   CALL SMG$RING_BELL (prpi, 2)
	ELSE
	   DO j = lowest, highest
	      IF (motfl(j)) THEN
		 ibuf.a(j) = RdAngl(j) - z(j)
	   	 IF (ibuf.pts .NE. 0.) THEN
	            af = ibuf.a(j) + (ibuf.pts -1)* ibuf.i(j)
	   	 ELSE
	      	    af = ibuf.a(j) 
	   	 END IF
		 str7 = PutStr (ibuf.a(j))
	   	 CALL PUTCHARS_REV (prpi, str7, a_line(j), 27)
		 str7 = PutStr (af)
	   	 CALL PUTCHARS_REV (prpi, str7, a_line(j), 60)
	      END IF
	   END DO
	END IF
	RETURN 1
	END
