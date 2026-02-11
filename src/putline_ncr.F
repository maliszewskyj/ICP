#include "icp.h"
	SUBROUTINE PUTLINE_NCR(DISPID,OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C        Unlike putline, putline_ncr will not write line to log file  	      C
C         putline_ncr will not throw a cr at end
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	use smg_prep
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	DISPID !DISPLAY ID
	CHARACTER*(*)	OUTSTR


C  Variable for truncating string
	INTEGER*4	len1

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo

	write(*,270)outstr(1:len1)
 270	format(a,$)

 	RETURN
	END
