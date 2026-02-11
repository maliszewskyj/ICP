#include "icp.h"
	SUBROUTINE NameScan(callid,type,mot,psfile,errno)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C    At call:
C	callid = 1 : called by interactive find_peak
C	callid = 2 : called by command file find_peak
C	callid = 3 : called by slign_peak
C       mot has number of motor to be scanned
C	type = 1 searches one angle 
C       type = 2, does a 2:1 scan                C
C	type = 3 is for monitor peak                                          C
C	type = 4 is for special xray scan. Twotheta only used for fpx         C
C	type = 5 is for RM scan
C	type = 6 is for RA scan
C	type = 7 searches one angle with sigmoidal curve 
C       type = -x is for scan at detector x (FPD and FPL)		      C
C    At return, psfile has filename 
C		errno = 0 ok
C		errno = -1 abort
C	 	errno = 1 no file created
C                                                                             C
C	Gayle Greene, August 1997                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none
	CHARACTER*12 psfile
	INTEGER*4    mot, type, callid
	INTEGER*4    errno
	INTEGER*4    context, status, i, fver, fversion
	CHARACTER*80 res_fname

	if (type.eq.1 .or. type .lt. 0)then !FP or FPD
	   Psfile(1:5) = 'fpx00'
	else if (type.eq.2)then !FPT
	   Psfile(1:5) = 'fpt00'
	else if (type.eq.3)then !FM type 
	   Psfile(1:5) = 'fmx00'
	else if (type .eq. 4)then !special xray
	   Psfile(1:5) = 'frx00'
	else if (type .eq. 5)then !FP/RM
	   Psfile(1:5) = 'frm00'
	else if (type .eq. 6)then !FP/RA
	   Psfile(1:5) = 'fra00'
	else if (type.eq.7)then !FPS
	   Psfile(1:5) = 'fps00'
	else
	   errno = -1		!abort
	   CALL Putcontr(' Find Peak error - type not recognized')
	   RETURN
	endif

	Write(psfile(4:5),fmt='(i2.2)',err=100)mot
	IF (nsta .gt. 0) THEN
	   Psfile(6:11) = '001.bt'         
	ELSEIF (nsta .eq. 0) THEN
	   Psfile(6:11) = '001.xr'         
	ELSEIF (nsta .eq. -11) THEN
	   Psfile(6:11) = '001.cg'         
	ELSE
	   Psfile(6:11) = '001.ng'
	ENDIF         
	IF (nsta .eq. -11) THEN
	   psfile(12:12)='1'
	ELSE
	   WRITE (UNIT= Psfile(12:12), FMT= '(i1)',err=100)iabs(nsta)
	ENDIF
	context = 0

	fver = 1	 
	CALL FIND_FILE (Psfile, res_fname, context,status)
	DO WHILE (status.ne.0 .and. fver .lt. 999)
	   fver = fver + 1
	   write (UNIT= psfile(6:8), FMT= '(i3.3)',err=100) fver
	   CALL FIND_FILE (Psfile, res_fname, context,status)
	END DO
	CALL FIND_FILE_END (context)
	CALL Putcontr_NT ('File created: '//psfile(1:12))

	errno = 0		!ok
	RETURN

 100	errno = 1		!no file
	RETURN

	END
