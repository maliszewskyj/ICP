#include "icp.h"
	SUBROUTINE Lia_File(fname,errflag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE SR830 LockIn AMP Data File                        C
C     Called from: Check_Channel                                              C
C                                                                             C
C     GAYLE GREENE, November, 1998                                            C
C     Adapted to F95 NCM October 2011                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use lia
	IMPLICIT NONE
	CHARACTER*12  fname
	CHARACTER*28  date
	INTEGER*4  ios
	LOGICAL    errflag,tofile/.true./
	CHARACTER*10 sp10/'          '/

	errflag=.FALSE.

	liafile = fname(1:9)//'lia'

	CALL DATE_TIME (date)
c       write date to file
	OPEN (UNIT=2, FILE= liafile, FORM= 'FORMATTED',
	1    ACCESS= 'SEQUENTIAL',STATUS= 'NEW',err=300)
	WRITE(Unit=2,fmt='(1x,a)',iostat=ios)date(1:17)
	CLOSE(unit=2)
	CALL Lia_status(tofile,errflag)
	OPEN (UNIT=2, FILE= liafile, FORM= 'FORMATTED',
	1    ACCESS= 'APPEND',STATUS= 'OLD', iostat=ios)
	IF (ios .eq. 0)THEN
	  WRITE(Unit=2,fmt='(1x,a)',iostat=ios)' Ptnum'//'        X(V)'//
     1     sp10//' Y(V)'//sp10//'  FREQ'//sp10//'SLVL'//
     1	   '           AUXV1         AUXV2 '
	  CLOSE(unit=2)
	ENDIF

	CALL Putcontr_NT('SR830 data in file: '//liafile)
	CALL Putcontr_NF(' ')
	RETURN

 300	errflag = .TRUE.
	RETURN 
	END

