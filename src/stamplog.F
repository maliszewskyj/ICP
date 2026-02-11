#include "icp.h"
	SUBROUTINE StampLog(enabled)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C  StampLog: write time and whether log file is being enabled or disabled    C
C                                                                             C
C	Gayle Greene, Janauary , 1997                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	IMPLICIT NONE

C  Calling parameters
	LOGICAL		enabled

C  Variables for adding time to output to file
	CHARACTER*23	timstr
	INTEGER*4	status

	timstr(1:)=' '
	CALL Get_AscTime(timstr,status)
	OPEN(UNIT=4,file=logname//'.LOG',access='APPEND',
	1    status='UNKNOWN',RECL=1024,err=100) !just skip if err opening
	IF (enabled)THEN
	   WRITE(UNIT=4,fmt='(a)',err=99)timstr(1:20)//': '//' Log Enabled'
	ELSE
	   WRITE(UNIT=4,fmt='(a)',err=99)timstr(1:20)//': '//' Log Disabled'
	ENDIF
 99	CLOSE(UNIT=4,err=100)	 

 100	RETURN
	END
