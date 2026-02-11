#include "icp.h"
	SUBROUTINE ad201_File(fname,errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE AD201 Data File                      C
C     Called from: Check_Channel                                             C
C                                                                             C
C     GAYLE GREENE, February, 1998                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use ad201
	IMPLICIT NONE
	CHARACTER*12  fname
	CHARACTER*28  date
	INTEGER*4  ios, lres, i
	LOGICAL    errflag,tofile/.true./
	CHARACTER*10 sp10/'          '/
	character*80 response
	character*4 str4

	errflag=.FALSE.

	ad2file = fname(1:9)//'ad2'

	CALL DATE_TIME (date)
c       write date to file
	OPEN (UNIT=2, FILE= ad2file, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1	 STATUS= 'NEW', err=300)
	WRITE(Unit=2,fmt='(1x,a)',iostat=ios)date(1:17)
	response(1:) = ' Point Chan  Counts'
	lres = 19
	WRITE(Unit=2,fmt='(1x,a)',iostat=ios)response(1:lres)
	CLOSE(unit=2)

	CALL Putcontr_NT('Ad201 data in file: '//ad2file)
	CALL Putcontr_NF(' ')
	RETURN

 300	errflag = .TRUE.
	RETURN 
	END

