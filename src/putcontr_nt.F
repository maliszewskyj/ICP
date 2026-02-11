#include "icp.h"

	SUBROUTINE PUTCONTR_NT(OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C              Unlike Putcontr, Putcontr_NT does not write time to log file   C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	use smg_prep
	IMPLICIT NONE

C  Calling parameters
	CHARACTER*(*)	OUTSTR

C  System Call

C  Variable for truncating string
	INTEGER*4	len1

	CHARACTER*1	empstr/' '/

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo

 	print *,outstr(1:len1)
C	write(*,'(a,$)') outstr(1:len1)

	IF (makelog)THEN !add to log file
	    OPEN(UNIT=4,file=logname//'.LOG',access='APPEND',status='UNKNOWN',
     1        err=100) !just skip if err opening
	  WRITE(UNIT=4,fmt='(a)',err=99)empstr//outstr(1:len1)
 99	  CLOSE(UNIT=4,err=100)	 
	ENDIF

 100	RETURN
	END
