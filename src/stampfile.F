#include "icp.h"
	SUBROUTINE StampFile(fname)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       StampFile: write time to file fname                                   C
C                                                                             C
C	Gayle Greene, September, 1997                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       
	IMPLICIT NONE

C       Calling parameters
	CHARACTER*12	fname


C       Variables for adding time to file
	CHARACTER*23	timstr
	INTEGER*4	status, ios

	timstr(1:)=' '
	CALL Get_AscTime(timstr,status)
	OPEN (UNIT=2, FILE=fname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1    STATUS= 'OLD', IOSTAT=ios)
	IF (ios .EQ. 0) WRITE(unit=2,fmt='(a)',err=99)'$ '//timstr(1:20)
 99	CLOSE(UNIT=2,err=100)	 

 100	RETURN
	END
