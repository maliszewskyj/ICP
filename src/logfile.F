#include "icp.h"
	SUBROUTINE Logfile

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Determine name of log file based on date and what earlier files       C
C       with same name basis exist.                                           C
C	Create files so all future writes are appends                         C
C                                                                             C
C	Gayle Greene, March, 1994                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT NONE

	INTEGER*4	status
	INTEGER*4	context,fver,fversion,ios
	CHARACTER*24	time_str
	CHARACTER*10	day_str
	CHARACTER*80	foundname, filename
	CHARACTER*24    scratch

c  make basic filename from date
	CALL Get_asctime(time_str,status)
	IF (status.eq.0)time_str='aaaaaaaaaaa'
	IF (time_str(1:1) .EQ. ' ')time_str(1:1)='0'
	day_str(1:) = time_str(4:6)//time_str(1:2)//time_str(8:11)//'_'
	foundname(1:)= ' '

	filename(1:)=day_str(1:10)//'01.LOG'
	scratch(1:)=day_str(1:10)//'[0-9][0-9]'//'.LOG'
C  See what earlier files with same date exist
	context = 0
	fversion = 1
	CALL FIND_FILE (scratch,foundname,context,status)
	DO WHILE (status.ne.0)
          READ (UNIT= foundname(11:12), FMT= *) fver	
	  IF (fver .GE. fversion) fversion = fver + 1
	  write(unit=filename(11:12),fmt='(I2.2)',iostat=ios)fversion
	  CALL FIND_FILE (filename,foundname,context,status)
	ENDDO
	CALL FIND_FILE_END(context)
	IF (fversion .GT. 99) fversion = 1
	WRITE(UNIT=filename(11:12),FMT='(I2.2)',IOSTAT=ios)fversion
	logname(1:)=filename(1:12)

	RETURN
	END
		
