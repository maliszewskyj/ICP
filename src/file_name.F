#include "icp.h"
	CHARACTER*12 FUNCTION File_Name (pattern)
C	*****************************************
C-----------------------------------------------------------------------------C
C                                                                             C
C	Routine to determine the datafile name based on the first five        C
C	characters of the comment. If the comment is still 'empty', i.e.      C
C	filled with '-' or invalid for a filename the filename will be        C
C       DEFLTxxx.BTy                                                          C
C	Called from: Run_Q_Buf, Run_I_Buf, Run_T_Buf			      C
C                                                                             C
C	Beat Schmid, November 1989                                            C
C	Modified by G. Greene, May 1991, to search polarized datafiles        C
C	Modified by G. Greene, Sept. 1992 to allow NG instruments             C
C	Modified by G. Greene, Sept. 1993 to allow XR instruments             C
C	Modified by N. Maliszewskyj Oct 2003 to allow CG1 instrument          C
C-----------------------------------------------------------------------------C

	use instr
	IMPLICIT      none
	CHARACTER*5   pattern
	INTEGER*4     context, status
	INTEGER*4     i, fver, fversion, ios
	CHARACTER*80  res_fname
	character*24  scratch
	CHARACTER*1   pol(4)

	DATA pol/'a','b','c','d'/

 	File_Name(1:5)=pattern
	DO i = 1, 5
	   status = ICHAR(File_Name(i:i))
	   IF (status .LT. 48 .OR. status .GT. 122 .OR.
     1	       (status .GT. 57 .AND. status .LT. 65) .OR.
     1	       (status .GT. 90 .AND. status .LT. 97) ) THEN !not letter or num	
		 File_Name(1:5) = 'deflt'
		 GOTO 100
	   END IF
	END DO	   
 100	IF (nsta .gt. 0) THEN
	  File_Name(6:11) = '???.b?'
	ELSEIF (nsta .eq. 0) THEN         
	  File_Name(6:11) = '???.x?'
        ELSEIF (nsta .eq. -11) THEN
	  File_Name(6:11) = '???.c?'
	ELSE
	  File_Name(6:11) = '???.n?'         
	ENDIF
	i = nsta
	if (nsta .eq. -11) then
	   i = 1
	else
	   i = abs(nsta)
	endif
	WRITE (UNIT= File_Name(12:12), FMT= '(i1)') i
	context = 0

	fversion = def_ver
	scratch(1:) = file_name(1:5)//'[0-9][0-9][0-9]'//file_name(9:12)
	CALL FIND_FILE (scratch, res_fname, context, status)

	DO WHILE (status.ne.0)
	   i = 0
	   READ (UNIT= res_fname(i+6:i+8), FMT= *) fver
	   IF (fver .GE. fversion) fversion = fver + 1
	   WRITE (UNIT= File_Name(6:8), FMT= '(i3.3)',iostat=ios) fversion
	   CALL FIND_FILE (File_Name, res_fname, context,status)
	END DO
	CALL FIND_FILE_END (context)
	IF (fversion .GT. 999) fversion = 1
	WRITE (UNIT= File_Name(6:8), FMT= '(i3.3)',iostat=ios) fversion
	IF (nsta .gt. 0) THEN
	  File_Name(11:11) = 't'
	ELSEIF (nsta .eq. 0) THEN
	  File_Name(11:11) = 'r'
	ELSE
	  File_Name(11:11) = 'g'
	ENDIF
	END
