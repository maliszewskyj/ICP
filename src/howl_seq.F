#include "icp.h"
	SUBROUTINE Howl_Seq
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     (1,nbuf+1) passes it to the time_com routine and continues until        C
C     it reads the end_of_command character '#'.                              C
C     Called from: Parse_contr                                                C
C                                                                             C
C    Gayle Greene  February 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	IMPLICIT      none
	INTEGER*4     i, j,  ios
	CHARACTER*50  command
	CHARACTER*80  response
	INTEGER*4  lcom
	CHARACTER*320 seq 
	INTEGER*4     asc
	REAL*4		time,tottime,timemax,totmax
	CHARACTER*20   flagstr
	LOGICAL		readef

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD',
	1    ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL=IBUF_RECL)
	READ (1,REC=NBUF+1) seq
	CLOSE (1)
	j = INDEX (seq, '#')

	tottime = 0.
	totmax = 0.

C---------------------------- START LOOPING HERE -----------------------------C

	DO WHILE (j .GT. 1)		                  ! string is not empty


!          C--------------- UPDATE SEQUENCE STRING ---------------------------C

	   i = INDEX (seq, ';')
	   IF (i .GT. 1) THEN                               ! multiple commands
	      command(1:) = seq(1:i-1)
	      lcom = i-1
	      seq = seq(i+1:)                          
	   ELSE  				   	       ! single command
	      command(1:) = seq(1:j-1)
	      lcom = j-1
	      seq(1:) = '# '
	   END IF
c	   WRITE (1,REC=nbuf+1) seq

C  Change command to uppercase and check it.
	      DO i=1,lcom
	        asc = ichar(command(i:i))
	        IF (asc .GE. 97 .AND. asc .LE. 122) THEN !convert to uppercase
		  asc = asc - 32
		  command(i:i) = char(asc)
	        ENDIF
	      ENDDO
	      CALL Time_com (command,lcom,time,timemax,flagstr)
	      IF (time.NE. 0. .OR. timemax .NE. 0)THEN
	        tottime=tottime+time
		totmax = totmax + timemax
		response(1:) = ' '
		response(1:30) = command(1:lcom)
		IF (time .EQ. timemax)THEN
		  write(response(32:),FMT='(F8.1,11x,a)',iostat=ios)
     1		  time,flagstr
		ELSE
	          WRITE(UNIT=response(32:),fmt='(F8.1,a3,F8.1,a)',
     1			iostat=ios)time,'   ',timemax,flagstr
		ENDIF
 	        CALL PUTCONTR ( response )
	      ENDIF

 	   IF (READEF (contr_A))THEN !abort
	        CALL clref (contr_A)
	        CALL clref (hdw_event) !remote abort
	        response=' Howlong terminated by control A'
		CALL PUTCONTR ( response)
		RETURN 
	   END IF

!          C----------------- READ NEXT COMMAND ------------------------------C

c	   READ (1,REC=nbuf+1) seq
	   j = INDEX (seq, '#')
	END DO


C-------------------------------- FINISHED ----------------------------------C

 	response(1:)='Total Execution Time in Hours: '
	IF (tottime .EQ. totmax)THEN
	    WRITE(UNIT=response(32:),fmt='(1x,F7.1)',iostat=ios)tottime/60.
 	    CALL PUTCONTR ( response)
	ELSE
	    WRITE(UNIT=response(32:),fmt='(1x,F7.1,1x,a3,F7.1)',iostat=ios)
     1		tottime/60.,' - ',totmax/60.
 	    CALL PUTCONTR ( response)
	ENDIF
	RETURN 
	END      
