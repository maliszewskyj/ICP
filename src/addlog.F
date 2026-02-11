#include "icp.h"
	SUBROUTINE ADDLOG(OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Routine to write string to log file              		      C
C                                                                             C
C	Gayle Greene, March , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	IMPLICIT NONE

C  Calling parameters
	CHARACTER*(*)	OUTSTR

C  Variable for truncating string
	INTEGER*4	len1
C  Variables for adding time to log file output
	CHARACTER*23	timstr
	INTEGER*4	status

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo


	timstr(1:)=' '
	CALL Get_AscTime(timstr,status)
	  OPEN(UNIT=4,file=logname//'.LOG',access='APPEND',status='UNKNOWN',
     1		err=100) !just skip if err opening
	WRITE(UNIT=4,fmt='(a)',err=99)timstr(13:20)//': '//outstr(1:len1)
 99	CLOSE(UNIT=4,err=100)	 

 100	RETURN
	END
