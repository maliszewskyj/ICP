#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE GetStringf (k_id, d_id, row, col, fmt, lg, trm, leng, data)
C	*********************************************************************  

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C     Routine To Read New Input From A Virtual Display And Transform It To    C 
C     To The Specified Format                                                 C
C     Called from: PrepR                                                      C
C									      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    k_id, d_id, row, col, fmt, lg, trm, leng
	REAL         data
	CHARACTER*7  PutStr
	CHARACTER*10  str10

	INCLUDE SMGPREP_CMN
	
 100  	CALL CURSOR_ABS (d_id, row, col)
	CALL SMG$READ_STRING (k_id, str10(1:lg),, lg, m$_nenr,,, leng, trm)
	IF (leng .GE. 1) THEN                    
	   READ (UNIT= str10(1:lg), FMT= *, ERR= 99) data
	   IF (lg .eq. 9) THEN
	      WRITE (UNIT= str10(1:9), FMT= '(f9.5)') data
	   ELSE !lg .eq. 10
	      WRITE (UNIT= str10(1:10), FMT= '(f10.6)') data
	   ENDIF
	   CALL SMG$ERASE_CHARS (d_id, lg, row, col)
	   CALL PUTCHARS_REV (d_id, str10(1:lg), row, col)
	   RETURN
	END IF
	RETURN


C---------------------------- ERROR DURING INPUT -----------------------------C

  99  	str10(1:) = '---------'
	CALL PUTCHARS_BLINK (d_id, str10(1:lg), row, col)
	CALL RING_BELL (d_id, 2)
	GOTO 100
	END


