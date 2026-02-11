#include "icp.h"
	SUBROUTINE Tdb_File(fname,errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE Transducer Board Data File                      C
C     Called from: Check_Channel                                             C
C                                                                             C
C     GAYLE GREENE, September, 1997                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tdb
	IMPLICIT NONE
	CHARACTER*12  fname
	CHARACTER*28  date
	INTEGER*4  ios,i
	LOGICAL    errflag
	CHARACTER*10 sp10/'          '/


	errflag=.FALSE.

	tdbpoint = 0
	do i = 1,256
	  tdbdata(i) = -999. !err flag
	enddo

	tdbfile = fname(1:9)//'tdb'

	CALL DATE_TIME (date)
c       write date to file
	OPEN (UNIT=2, FILE= tdbfile, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1	 STATUS= 'NEW', err=300)
	WRITE(Unit=2,fmt='(1x,a)',iostat=ios)date(1:17)
	CLOSE(unit=2)

	CALL Putcontr('Transducer Board data in file: '//tdbfile)
	RETURN

 300	errflag = .TRUE.
	RETURN 
	END

