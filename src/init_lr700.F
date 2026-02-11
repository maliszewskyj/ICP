#include "icp.h"
	SUBROUTINE Init_LR700(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE LR700 TEMPERATURE CONTROLLER                      C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, June, 1995                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	CHARACTER*80  command,reply
	INTEGER*4     lreply,lcom
	INTEGER*4     errflag
	INTEGER*4     irange
	CHARACTER*30  str30

c       variables for lr700
	real*4   v7ranges(10)/.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./

	command(1:)='T 0,B 4,F 0,E 4'
	lcom=15
	call lr700_232(command,lcom,reply,lreply)
	IF (reply(1:lreply).EQ. 'OK')THEN
	   errflag = 0
	   command(1:)='G 6'
	   lcom = 3
	   call lr700_232(command,lcom,reply,lreply)
	   IF (lreply .NE. 0)THEN
	      irange = -99
	      read(unit=reply(1:1),fmt='(I1)',iostat=ios)irange
	      IF (irange .LT. 0 .OR. irange .GT. 9)THEN
		 CALL Putcontr(' Error reading back range. Use SETP0')
		 rangelr = 200.
	      ELSE
		 rangelr = v7ranges(irange+1)
		 write(unit=str30,fmt=*,iostat=ios)rangelr
		 CALL Putcontr(' Range set to '//str30)
	      ENDIF
	   ELSE
	      CALL Putcontr(' No reply reading back range. Use SETP0')
	      rangelr = 200.
	   ENDIF
	ELSE
	   IF (lreply .GT. 10)lreply = 10 !to reduce chance of crash
	   call putcontr(' NO REPLY OR invalid reply received from lr700: '//reply(1:lreply))
	   errflag=1
	ENDIF

	RETURN 
	END




