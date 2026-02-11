#include "icp.h"

	SUBROUTINE PUTLINE(DISPID,OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C       C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       
	use instr
	use smg_prep
	IMPLICIT NONE

C       Calling parameters
	INTEGER*4	DISPID	!DISPLAY ID
	CHARACTER*(*)	OUTSTR

C       System Call

C       Variable for truncating string
	INTEGER*4	len1
C       Variables for adding time to log file output
	CHARACTER*23	timstr
	INTEGER*4	status

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	   len1 = len1 - 1
	enddo

 	print *,outstr(1:len1)

	IF (makelog .AND. dispid .EQ. contr)THEN !add to log file
	   timstr(1:)=' '
	   CALL Get_AscTime(timstr,status)
	   OPEN(UNIT=4,file=logname//'.LOG',access='APPEND',status='UNKNOWN',
	1	err=100)	!just skip if err opening
	   WRITE(UNIT=4,fmt='(a)',err=99)timstr(13:20)//': '//outstr(1:len1)
 99	   CLOSE(UNIT=4,err=100)	 
	ENDIF

 100	RETURN
	END
