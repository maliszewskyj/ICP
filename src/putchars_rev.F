#include "icp.h"
	SUBROUTINE PUTCHARS_REV(DISPID,OUTSTR,IROW,ICOL)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID, !DISPLAY ID
     1			IROW,ICOL
	CHARACTER*(*)	OUTSTR

	write(*,fmt='(a,a,$)') char(27),'[7m'
	write(*,'(a,$)') outstr
	write(*,fmt='(a,a)') char(27),'[0m'

	RETURN
	END
