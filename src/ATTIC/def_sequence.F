#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Def_Sequence
C	***********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine defines a sequence of commands to be executed                   C
C     Called from: PrepI, PrepQ, PrepT				              C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT      none
	INCLUDE      INSTR_CMN               ! Common variable block
	INCLUDE      SMGPREP_CMN ! Program specific SMG-definitions
	
	INTEGER*4     status, i, k, leng, trm
	INTEGER*4     SMG$PASTE_VIRTUAL_DISPLAY, SMG$CREATE_MENU
	CHARACTER*50  command
	CHARACTER*80  line(nbuf), help_line(3)
	CHARACTER*320 linestr, seq

	status = SMG$PASTE_VIRTUAL_DISPLAY (sequ, paste, 14, 1)


C------------------------- DISPLAY DEFINED SEQUENCE --------------------------C

	help_line(1) = ' Give any valid ICP commands'//
     1	  ' separated by semicolons, or a DEL command! '
	help_line(2) = ' BE CAREFUL: Input is not checked'//
     1    ' for errors or invalid commands.'
	help_line(3) = ' Use DRS in CONTROL mode to check run sequence syntax.'
	CALL Put_Help (help_line)
	CALL PUTCHARS_REV(sequ, ' Return: F20 or RS=F20 ', 7, 33)

  60	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=IBUF_RECL)
	READ (1,REC=nbuf+1) seq
	CLOSE 	(1)

	i = 1
	DO  WHILE (seq(i:i) .NE. '#')
	   i = i + 1
	END DO
	i = i - 1
	IF (i .GT. 1) THEN
	   linestr = ' Defined Sequence: ' // seq(1:i)
	ELSE
	   linestr = ' Defined Sequence: NONE'
	END IF
	CALL CURSOR_ABS (sequ, 2, 1)
	CALL PUTLINE_WRAPCHAR (sequ, linestr)


C------------------------------ GET NEW INPUT --------------------------------C

  80	CALL SMG$ERASE_LINE (sequ, 6, 1)
	CALL CURSOR_ABS (sequ, 6, 1)
	CALL SMG$READ_STRING (keyb, command, ' RS= ', 50,,,,leng, trm, sequ)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. SMG$K_TRM_CR .AND. leng .GE. 1)  THEN    ! Process command
	   command = 'RS=' // command
	   CALL STR$UPCASE (command, command)
	   CALL Parse_RS (command, *60)
	ELSE IF (trm .EQ. SMG$K_TRM_F20) THEN
	   CALL SMG$UNPASTE_VIRTUAL_DISPLAY (sequ, paste)
	   CALL SMG$ERASE_DISPLAY (sequ, 1, 1) 
	   RETURN
	END IF         
	CALL SMG$RING_BELL (sequ, 1)
	GOTO 80
	END

