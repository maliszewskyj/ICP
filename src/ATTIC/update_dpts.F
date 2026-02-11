#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Update_Dpts (dbuf, a_line, curmot,tot_line)
C	*****************************************
	IMPLICIT     none
	INTEGER*4    j, a_line(6), curmot,k, tot_line, ios, npts(6),i,tpts
	REAL*4       af, afn, afxyz
	CHARACTER*7  str7, PutStr
	CHARACTER*4  str4
	CHARACTER*5  str5
	INCLUDE INSTR_CMN      ! Instrument and hardware parameters
	INCLUDE DARTZ_CMN
	RECORD /d_buf/ dbuf
	   DO j = 1, 6
	     IF (dbuf.lpn(j) .EQ. dbuf.lpn(curmot))THEN !same loop
	       IF (j .NE. curmot)THEN
	         dbuf.lpts(j) = dbuf.lpts(curmot)
		 WRITE (UNIT= str4, FMT= '(i4)',iostat=ios) dbuf.lpts(j)
		 CALL PUTCHARS_REV (prpd, str4, a_line(j), 63)
	       ENDIF
	       IF (dbuf.mn(j) .NE. 0)THEN !motor is defined
	   	 IF (dbuf.lpts(j) .NE. 0.) THEN
	            af = dbuf.a(j) + (dbuf.lpts(j) -1)* dbuf.i(j)
	            afxyz = dbuf.a_xyz(j) + (dbuf.lpts(j) -1)* dbuf.i_xyz(j)
	   	 ELSE
	      	    af = dbuf.a(j) 
		    afxyz = dbuf.a_xyz(j) 
	   	 END IF
		 str7 = PutStr (dbuf.a(j))
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 15)
		 str7 = PutStr (af)
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 31)
		 str7 = PutStr (dbuf.a_xyz(j))
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 39)
		 str7 = PutStr (afxyz)
	   	 CALL PUTCHARS_REV (prpd, str7, a_line(j), 55)
	       ENDIF !motor is defined
	     ENDIF !same loop
	   ENDDO
C  Calc total number of points
	  DO i = 1,6 !set number of points for loop i to 0
	    npts(i) = 0
	  ENDDO
	  DO i = 1,6 !set number of points for loop i to actual value
	    IF (dbuf.mn(i) .NE. 0)THEN !motor is defined
	      npts(dbuf.lpn(i)) = dbuf.lpts(i)
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
	  dbuf.pts = FLOAT(tpts)
	   write(unit=str5,fmt='(I5)',iostat=ios)NINT(dbuf.pts)
	   CALL PUTCHARS (prpd,str5,tot_line,75)
	RETURN 
	END
