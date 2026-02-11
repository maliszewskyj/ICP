#include "icp.h"
	SUBROUTINE Status_File (info)
C	**********************************************
C	Note: The contents of info may be changed by this routine

	use instr
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface
 	CHARACTER*80  info(4)
	INTEGER*4     maxtry,ios,status,notries/5/,openfai/30/ !from $fordef
	BYTE	      bcr/13/,blf/10/
	CHARACTER*1   cr,lf
	EQUIVALENCE   (bcr,cr)
	EQUIVALENCE   (blf,lf)
	INTEGER*4     i,j
	CHARACTER*80  GETHOME

C       Replace any unprintable characters with blank
	do i = 1,4		!check each string
	   do j = 1,80		!check each character in string
	      if (info(i)(j:j) .LT. ' ' .OR. info(i)(j:j) .GT. '~')
	1	   info(i)(j:j) = ' ' !substitue blank
	   enddo
	enddo

	maxtry = 0
	OPEN (UNIT= 3, FILE= STATUS_ICP, STATUS= 'UNKNOWN', 
	1    ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= STAT_RECL,IOSTAT=ios)
	DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	   CALL libwait(.2,status)
	   OPEN (UNIT= 3, FILE= STATUS_ICP, STATUS= 'UNKNOWN', 
	1	ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= STAT_RECL,IOSTAT=ios)
	   maxtry = maxtry + 1
	ENDDO
	IF (ios .NE. 0)RETURN	!forget update if can not open file
	IF (info(1)(1:8) .NE. 'new_temp') THEN 
	   WRITE (3,REC=1,iostat=ios) info(1)
	   WRITE (3,REC=2,iostat=ios) info(2)
	END IF
	WRITE (3,REC=3,iostat=ios) info(3)
	WRITE (3,REC=4,iostat=ios) info(4)
	CLOSE(3)

	RETURN
	END
	
