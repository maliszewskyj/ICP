#include "icp.h"
	SUBROUTINE Do_Sequence (*)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     (1,NBUF+1) passes it to the parser and executes the the command until   C
C     it reads the end_of_command character '#'.                              C
C     Called from: Control                                                    C
C                                                                             C
C     Beat Schmid, August 1989                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	IMPLICIT      none
	INTEGER*4     i, j,  idum, leng
	CHARACTER*50  command, response
	CHARACTER*320 seq 
	LOGICAL		readef

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	READ (1,REC=NBUF+1) seq
	CLOSE (1)
	j = INDEX (seq, '#')


C---------------------------- START LOOPING HERE -----------------------------C

	DO WHILE (j .GT. 1)		                  ! string is not empty


!          C--------------- UPDATE SEQUENCE STRING ---------------------------C

	   i = INDEX (seq, ';')
	   IF (i .GT. 1) THEN                               ! multiple commands
	      command(1:) = seq(1:i-1)
	      seq = seq(i+1:)                          
	   ELSE  				   	       ! single command
	      command(1:) = seq(1:j-1)
	      seq(1:) = '# '
	   END IF
           OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', 
	1        ACCESS= 'DIRECT', FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	   WRITE (1,REC=NBUF+1) seq
	   CLOSE (1)


!          C------------ EXECUTE EXACTLY ONE COMMAND -------------------------C

	   CALL Putcontr('RS: '//command)
	   CALL Parse_Contr (command, *110)
 110	   IF (READEF (contr_A))THEN
	    IF (.NOT. READEF (hdw_event))THEN !not remote
	       CALL clref (contr_a)
	       IF (i .NE. 0) THEN ! there could be more
		  CALL READ_STRING (keyb, command, 
	1	       '* Abort entire command sequence [YES]? ', 50,
	2	       idum,idum,idum, leng, idum, contr) !idum for absoft
		  IF (command(1:1) .NE. 'N') THEN
	      	    seq(1:) = '# '
	      	    OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD',
	1		 ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1		 RECL=IBUF_RECL)
	      	    WRITE (1,REC=NBUF+1) seq
	      	    CLOSE (1)
		    GOTO 140                            ! Terminate immediately
		 END IF
	      END IF
	    ELSE  !remote abort
	      CALL clref (contr_a)
	      CALL clref (hdw_event) !remote abort
	    ENDIF !remote abort
	   END IF

!          C----------------- READ NEXT COMMAND ------------------------------C

	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1        FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	   READ (1,REC=NBUF+1) seq
	   CLOSE (1)
	   j = INDEX (seq, '#')
	END DO


C-------------------------------- WELL DONE ----------------------------------C

 140	response = 'Command sequence terminated !'
	CALL putcontr (response)
	RETURN 1
	END      
