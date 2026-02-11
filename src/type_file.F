#include "icp.h"
	SUBROUTINE Type_File (fcommand)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       C
C       Gayle Greene, September 1997                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

C       NOTE FILES ARE NOT OPENED WITH SHARED STATUS

	IMPLICIT      none
	INTEGER*4     lbuf
	INTEGER*4     io_stat
	CHARACTER*50  fcommand
	CHARACTER*120 buffer

C       Open file.
 	OPEN (UNIT = 1, FILE= fcommand, STATUS='OLD',
	1    iostat=io_stat)
	IF (io_stat .ne. 0) THEN
	   CALL Putcontr_NT(' File could not be opened for type command')
	   RETURN
	ENDIF

 182	READ (1,fmt='(a)',end=500,err=990)buffer
	CALL Putcontr_NT(buffer)
	GOTO 182

C       Close File
 500	CLOSE(1)
	RETURN

C-------------------Handle file error-----------------------------------------C
 990	CALL Putcontr_NT('Error reading file: '//fcommand)
	RETURN
	END      
