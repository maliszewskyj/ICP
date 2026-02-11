#include "icp.h"
	SUBROUTINE PUTLINE_REVERSE(DISPID,OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID !DISPLAY ID
	CHARACTER*(*)	OUTSTR

C  Local Variables
	INTEGER*4	line_adv, !number of line to advance after output
     1			irset

	line_adv = 1   !This is the default
	write(*,fmt='(a,a,$)') char(27),'[7m'
	write(*,fmt='(a,$)') outstr
	write(*,fmt='(a,a)') char(27),'[0m'

	RETURN
	END
