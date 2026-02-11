#include "icp.h"
	SUBROUTINE PUTLINE_NF(DISPID,OUTSTR)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$PUT_LINE 		      C
C        Unlike putline, putline_nf will not write line to log file  	      C
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

C  System Call

C  Variable for truncating string
	INTEGER*4	len1

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo


 	print *,outstr(1:len1)
C	write(*,fmt='(a,$)') outstr(1:len1)

 	RETURN
	END
