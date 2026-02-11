#include "icp.h"
	SUBROUTINE Check_Sequence (*)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     (1,nbuf+1) passes it to the check parser and executes the the command   C
C     until it reads the end_of_command character '#'.                        C
C     Called from: Control                                                    C
C                                                                             C
C     Gayle Greene July 1992                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT      none
	INTEGER*4     i, j, cpos, spos
	CHARACTER*50  command, response
	CHARACTER*320 seq 
	LOGICAL       isdigit, Readef

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', RECL=IBUF_RECL)
	READ (1,REC=NBUF+1) seq
	CLOSE (1)
	j = INDEX (seq, '#')

C Check to verify all commas followed by number.   
	  if (j.eq.0)goto 182 !skip check
	  cpos = INDEX(seq(1:j), ',') !check for commas
	  DO WHILE (cpos .ne. 0)
	    if (cpos.ge.j)goto 182 !, at end of line ok
	    cpos=cpos+1 !find next nonblank character:
	    DO WHILE (seq(cpos:cpos).eq.' '.and.cpos .le.j) 
	      cpos=cpos+1
	    ENDDO
	    IF (cpos.ge.j)goto 182 !, at end of line ok
	    IF (.not. isdigit(seq(cpos:cpos)))THEN
	        CALL PUTCONTR(
     1	         ' Verify commas used for parameters only.')
	        goto 182
	    ENDIF
	    spos=cpos !remember offset in seq
	    cpos = INDEX(seq(spos:j), ',')
	    if (cpos .ne. 0) cpos = cpos  + spos -1
 	  ENDDO

 182	  continue !finished checking commas

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

!          C------------ EXECUTE EXACTLY ONE COMMAND -------------------------C

	   CALL Parse_Check (command, *110)
 110	   IF (READEF (contr_A))THEN
	      CALL clref (contr_A)
	      CALL clref (hdw_event)
		GOTO 140                            ! Terminate immediately
	   END IF

!          C----------------- READ NEXT COMMAND ------------------------------C

	   j = INDEX (seq, '#')
	END DO
	goto 150

C-------------------------------- WELL DONE ----------------------------------C

 140	response = 'Command sequence check terminated by control A!'
	CALL PUTCONTR (response)
	GOTO 160
 150	response = 'Entire command sequence checked'
	CALL PUTCONTR (response)
 160	RETURN 1
	END      
