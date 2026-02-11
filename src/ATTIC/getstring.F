#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE GetString (k_id, d_id, row, col, fmt, lg, trm, leng, data)
C	*********************************************************************  

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C     Routine To Read New Input From A Virtual Display And Transform It To    C 
C     To The Specified Format                                                 C
C     Called from: PrepI, PrepQ                                               C
C									      C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    k_id, d_id, row, col, fmt, lg, trm, leng
	REAL         data
	CHARACTER*7  PutStr
	CHARACTER*8  str8

	INCLUDE SMGPREP_CMN
	
 100  	CALL CURSOR_ABS (d_id, row, col)
	CALL SMG$READ_STRING (k_id, str8(1:lg),, lg, m$_nenr,,, leng, trm)
	IF (leng .GE. 1) THEN                    
	   READ (UNIT= str8(1:lg), FMT= *, ERR= 99) data

	   IF (fmt .EQ. 1) THEN        ! (* NOTE: length always 7 *)
	      IF (data .LT. 10000.) THEN 
	         str8(1:7) = PutStr (data)
	      ELSE
	         WRITE (UNIT= str8(1:7), FMT= '(e7.2e1)') data
	      END IF
	   ELSE IF (fmt .EQ. 2) THEN
	      WRITE (UNIT= str8(1:lg), FMT= '(i<lg>)') NINT(data)
	   ELSE IF (fmt .EQ. 3) THEN
	      IF (ABS(data) .LT. 10) THEN
	         WRITE (UNIT= str8(1:7), FMT= '(f7.4)') data
	      ELSE IF (ABS(data) .GE. 100.) THEN
	         WRITE (UNIT= str8(1:7), FMT= '(f7.2)') data
	      ELSE
	         WRITE (UNIT= str8(1:7), FMT= '(f7.3)') data
	      END IF
	   ELSE IF (fmt .EQ. 4) THEN           !  (* NOTE: length <= 8 *)
	      CALL Putmax(data,str8(1:lg),lg,lg-1)
	   ELSE IF (fmt .EQ. 5) THEN
	      data = ABS(data)
	      IF (data .LT. 1) THEN
	         WRITE (UNIT= str8(1:3), FMT= '(f3.1)') data
	      ELSE
	         WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(data)
	      END IF
	   ELSE IF (fmt .EQ. 6) THEN !tmp err
	      data = ABS(data)
	      IF (data .LT. 1) THEN
	         WRITE (UNIT= str8(1:3), FMT= '(f3.2)') data
	      ELSEIF (data .LT. 10) THEN
	         WRITE (UNIT= str8(1:3), FMT= '(f3.1)') data
	      ELSE
	         WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(data)
	      END IF
	   END IF         
	   CALL SMG$ERASE_CHARS (d_id, lg, row, col)
	   CALL PUTCHARS_REV (d_id, str8(1:lg), row, col)
	   RETURN
	END IF
	RETURN


C---------------------------- ERROR DURING INPUT -----------------------------C

  99  	str8(1:lg) = '------'
	CALL PUTCHARS_BLINK (d_id, str8(1:lg), row, col)
	CALL SMG$RING_BELL (d_id, 2)
	GOTO 100
	END


