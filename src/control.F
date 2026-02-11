#include "icp.h"
	SUBROUTINE Control
C	******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	                                                                      C
C     Routine to take control of all the hardware, i.e. CAMAC, MOTORS, TEMP,  C
C     etc., to start data acquisition and to process the RS command because   C
C     Fortran does not allow recursive calls.                                 C
C     Called from: ICP_MAIN                                                   C
C                                                                             C
C     Beat Schmid, July 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	IMPLICIT     none
	INTEGER*4    leng, trm, i, ios
	CHARACTER*4  str4
	CHARACTER*50 command
	CHARACTER*80 help_line(3)
	LOGICAL	Readef, isdigit


	INTEGER*4    i1,lbuf/50/
	CHARACTER*3   prompt


C----------------------- PASTE SCREEN AND SETUP HELP -------------------------C


  	help_line(1) = ' Type HELP for commands'
	IF (nsta.eq.1) THEN
	  write(unit=str4(1:4),fmt='(I4)')collim(1)
	  help_line(2)=' Collimation: '//str4//'  MONO='//monobt1
	  help_line(3)=' Use COL= command to change'
	  CALL Put_Help (help_line)
	ELSE
	  help_line(2) = ' '
	  help_line(3) = ' '
	ENDIF


	CALL Status_Line

 101	IF (clrstat) THEN
	  CALL Putcontr_NF(' ')
	  CALL Status_Line
	ENDIF

	prompt='% '//char(0)
	CALL READ_STRING (keyb, command, prompt,lbuf,i1,i1,i1,leng, trm, contr)

	   IF (makelog)CALL addlog(command) !add command to log file
	   IF (command(1:1) .EQ. '!')THEN
	     GOTO 101 !No action on comment
	   ELSE IF (command(1:3) .EQ. 'RS=') THEN
	      CALL Parse_RS (command, *101)
	   ELSE IF (command(1:4) .EQ. 'DRSF') THEN
	      CALL Check_File_Sequence (command,*101)
	   ELSE IF (command(1:3) .EQ. 'DRS') THEN
	      CALL Check_Sequence (*101)
	   ELSE IF (command(1:3) .EQ. 'RSF') THEN
	      IF (.not. priv) THEN
		call putcontr(' RSF can only be executed from privileged terminal')
	      ELSE
	        CALL Do_File_Sequence (command,*101)
 	      ENDIF
	   ELSE IF (command(1:2) .EQ. 'RS') THEN
	      IF (.not. priv) THEN
		call putcontr(' RS can only be executed from privileged terminal')
	      ELSE
	        CALL Do_Sequence (*101)
	      ENDIF
	   ELSE
	     IF (nsta .NE. 0) THEN !not xray
	       CALL Parse_Contr (command, *101)
	     ELSE !xray machine
	       CALL Parse_Contr (command, *201)
 201	       IF (READEF (contr_a))
     1		   CALL CLREF (contr_A)
	       GOTO 101
	     ENDIF !xray machine
	   END IF


	GOTO 101

	RETURN
	END  
