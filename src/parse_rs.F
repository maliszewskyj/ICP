#include "icp.h"
	SUBROUTINE Parse_RS (command, *)
C	********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   Routine To Unfold 'RS=' Commands; Necessary Because Fortran Does Not      C
C   Allow Recursive Calls.                                                    C
C   Called from: Control, Def_Sequence                                        C
C                                                                             C
C   Beat Schmid,  December 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT       none
	
	CHARACTER*50   command, sav_com, local
	CHARACTER*320  seq, tmp_seq
	CHARACTER*1    str1
	CHARACTER*2    str2
	CHARACTER*80   help_line(3)
	INTEGER*4      start, len, avail, end, i, j, l, m, inc, loop, iend
	LOGICAL        multiple, isdigit


	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT',
	1     FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	READ (1,REC=NBUF+1) seq
	start = INDEX (seq, '#')	          ! Determine starting position
 	avail = 320 - start			  ! for next command


C---------------------- REMOVE ALL BLANKS FROM STRING ------------------------C

	command(1:) = command(4:)                 ! Remove 'RS='
	j = 1
	DO i = 1, 50
	   IF (command(i:i) .NE. ' ') THEN
	      command(j:j) = command(i:i)
	      j = j + 1
	   END IF
	END DO
	command(j:50) = ' '                   	          ! Blank is terminator
	command = ';' // command           ! comma is for correct loop handling


C------------------ FIND NEXT COMMA OR FIND CR AND RETURN --------------------C

  50	i = 1
 	DO WHILE (command(i:i) .NE. ';' .AND. command(i:i) .NE. ' ')
	   i = i + 1
	END DO
	IF (command(i:i) .EQ. ' ' .OR. command(i+1:i+1) .EQ. ' ') THEN
	   WRITE (1,REC=NBUF+1) seq
	   CLOSE(1)
	   RETURN 1
	ELSE
	   command(1:) = command(i+1:)
	END IF

C-------------------------- CHECK FOR REPEAT LOOP ----------------------------C

 100	loop = 1
	multiple = .FALSE.
	IF (isdigit(command(1:1))) THEN
	   j = 2
	   DO WHILE (isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO	     
	   j = j - 1
	   READ (UNIT= command(1:j), FMT= *, ERR=160) loop
	   IF (command(j+1:j+1) .NE. '(') GOTO 165                ! '(' missing
	   command(1:) = command(j+2:)
	   i = INDEX (command, ')')
	   IF (i .EQ. 0) GOTO 170                                 ! ')' missing
	   sav_com(1:) = command(i+1:)
	   command(1:) = command(1:i-1) // ' '
	   multiple = .TRUE.
	END IF


C----------------------------- PROCESS COMMAND -------------------------------C

	len = 0
 110	IF (command(1:3) .EQ. 'DEL') THEN        		      ! RS= DEL
	   start = 1
	   command(1:) = command(5:)
	ELSE IF (command(1:2) .EQ. 'RI' .OR. command(1:2) .EQ. 'RQ' .OR.
	1        command(1:2) .EQ. 'RT' .OR. command(1:2) .EQ. 'RB' .OR.
     1	         command(1:2) .EQ. 'RR') THEN
	   IF (command(2:2) .EQ. 'I') THEN
	      str1 = 'I'
	   ELSE IF (command(2:2) .EQ. 'Q') THEN
	      str1 = 'Q'
	   ELSE IF (command(2:2) .EQ. 'B') THEN
	      str1 = 'B'
	   ELSE IF (command(2:2) .EQ. 'R') THEN
	      str1 = 'R'
	   ELSE
	      str1 = 'T'
	   END IF
	   j = INDEX (command, ';')
	   IF ( j .EQ. 0 ) THEN		   ! (* only one command on this line *)
	      local(1:) = command (1:)
	      iend=50
	   ELSE                            ! (* more than one command on line *)
	      local(1:) = command(1:j-1)
	      iend=j-1
	   END IF
	   j = INDEX (local, '-')
	   IF ( j .EQ. 0 ) THEN                  ! (* execute one buffer only *)
	      READ (UNIT=local(3:iend), FMT= *, ERR= 155) l
	      m = l
	   ELSE                                 ! (* execute range of buffers *)
	      READ (UNIT=local(3:j-1), FMT= *, ERR= 155) l
	      READ (UNIT=local(j+1:iend),  FMT= *, ERR= 155) m
	   END IF
 122	   inc = 1
	   IF (l .GT. m) inc = -1
	   DO j = l, m, inc
	      IF (j .LT. 10) THEN 				   ! Write str1
		 WRITE (UNIT= str2(1:1), FMT= '(i1)') j
		 tmp_seq(len+1:) = ';R'// str1 // str2(1:1)                  
		 len = len + 4
	      ELSE 						   ! Write str2
		 WRITE (UNIT= str2(1:2), FMT= '(i2)') j
		 tmp_seq(len+1:) = ';R'// str1 // str2(1:2)
		 len = len + 5
	      END IF
	   END DO
	ELSE						    ! Any other command
	   IF (avail .LT. 20) GOTO 150				  ! Buffer full
	   i = INDEX (command, ';')
	   IF (i .LE. 1) i = INDEX (command, ' ')	       ! single command
	   tmp_seq(len+1:) = ';'// command (1:i-1)
	   len = len + i
	   command(1:) = command(i:)
	END IF
	end = start + len - 1
  	IF (multiple) THEN
	   i = 1
	   DO WHILE (command(i:i) .NE. ';' .AND. command(i:i) .NE. ' ')
	      i = i + 1
	   END DO
	   IF (command(i:i) .EQ. ';') THEN		! More than one command
	      command(1:) = command(i+1:)
	      GOTO 110
	   ELSE						      ! n(...) finished
	      command(1:) = sav_com(1:)
	   END IF
	END IF
     	IF (len .GT. 0) THEN
	   DO j = 1, loop
	      IF (avail-len .LT. 20) GOTO 150 			 ! Buffer full
	      seq(start:end) = tmp_seq(1:len)
	      start = end + 1
	      end   = end + len
	      avail = avail - len
	   END DO
	END IF
	seq(start:start) = '#'
	seq(start+1:) = ' '
	IF (seq(1:1) .EQ. ';') THEN  			     ! Remove first ';'
	   seq(1:) = seq(2:)	     
	   start = start - 1
	END IF
	GOTO 50	
	

C------------------------------ ERROR MESSAGES -------------------------------C

 150	seq(start:start) = '#'
	seq(start+1:) = ' '
	IF (seq(1:1) .EQ. ';') THEN  			     ! Remove first ';'
	   seq(1:) = seq(2:)	     
	   start = start - 1
	END IF
	WRITE (1,REC=NBUF+1) seq
	CLOSE (1)
	help_line(1) = 'Sequence buffer is full!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL help_ring_bell ( 1)
	CALL Put_Help (help_line)
	RETURN 1
 155    help_line(1) = 'Error reading ''n'' in RXn-m!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL help_ring_bell ( 1)
	CALL Put_Help (help_line)
	RETURN 1
 160    help_line(1) = 'Error reading ''n'' in n(...) !'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL help_ring_bell ( 1)
	CALL Put_Help (help_line)
	RETURN 1
 165    help_line(1) = 'Left bracket missing in n(...) !'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL help_ring_bell ( 1)
	CALL Put_Help (help_line)
	RETURN 1
 170    help_line(1) = 'Right bracket missing in n(...) !'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL help_ring_bell ( 1)
	CALL Put_Help (help_line)
	RETURN 1
	END
