#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Update_Xpts (xbuf, a_line, curmot,tot_line)
C	*****************************************

	IMPLICIT     none
	INTEGER*4    j, a_line(6), curmot,k, tot_line, ios, npts(6),i,tpts
	REAL*4       af, afn
	CHARACTER*7  str7, PutStr
	CHARACTER*4  str4
	CHARACTER*5  str5

	INCLUDE  INSTR_CMN
	INCLUDE  DARTZ_CMN

	RECORD /x_buf/ xbuf

	   DO j = 1, 6
	     IF (xbuf.lpn(j) .EQ. xbuf.lpn(curmot))THEN !same loop
	       IF (j .NE. curmot)THEN
	         xbuf.lpts(j) = xbuf.lpts(curmot)
		 WRITE (UNIT= str4, FMT= '(i4)',iostat=ios) xbuf.lpts(j)
		 CALL PUTCHARS_REV (prpx, str4, a_line(j), 75)
	       ENDIF
	       IF (xbuf.mn(j) .NE. 0)THEN !motor is defined
	   	 IF (xbuf.lpts(j) .NE. 0.) THEN
	            af = xbuf.a(j) + (xbuf.lpts(j) -1)* xbuf.i(j)
	   	 ELSE
	      	    af = xbuf.a(j) 
	   	 END IF
		 str7 = PutStr (xbuf.a(j))
	   	 CALL PUTCHARS_REV (prpx, str7, a_line(j), 27)
		 str7 = PutStr (af)
	   	 CALL PUTCHARS_REV (prpx, str7, a_line(j), 60)
	       ENDIF !motor is defined
	     ENDIF !same loop
	   ENDDO

C  Calc total number of points
	  DO i = 1,6 !set number of points for loop i to 0
	    npts(i) = 0
	  ENDDO

	  DO i = 1,6 !set number of points for loop i to actual value
	    IF (xbuf.mn(i) .NE. 0)THEN !motor is defined
	      npts(xbuf.lpn(i)) = xbuf.lpts(i)
	    ENDIF
	  ENDDO

	  tpts = 0
	  DO i = 6,1,-1 !go through loops backwards
	    IF (npts(i) .NE. 0)then
	      IF (tpts .EQ. 0)THEN
	        tpts = npts(i)
	      ELSE
	        tpts = tpts * npts(i)
	      ENDIF
	    ENDIF
	  ENDDO
	  xbuf.pts = FLOAT(tpts)

	   write(unit=str5,fmt='(I5)',iostat=ios)NINT(xbuf.pts)
	   CALL PUTCHARS (prpx,str5,tot_line,75)

	RETURN 
	END
