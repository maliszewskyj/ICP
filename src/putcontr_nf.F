#include "icp.h"
	SUBROUTINE PUTCONTR_NF(OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C       Unlike putcontr, putcontr_nf will not output to log file	      C
C                                                                             C
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

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo

 	print *,outstr(1:len1)
C	write(*,'(a,$)') outstr(1:len1)

	RETURN
	END
