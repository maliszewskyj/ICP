#include "icp.h"
	SUBROUTINE HOME(motorno,dirc)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Home: Send motor indicated to HOME in direction indicated by dir      C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, January 1994                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_vme.h"
#include "c_rs232_acs.h"
#include "c_ez.h"
	end interface
	INTEGER*4    motorno
	CHARACTER*1  dirc
	CHARACTER*80 command, reply
	INTEGER*4    lcom, lreply
	CHARACTER*80 GETHOME

	IF (mottype(motorno) .EQ. ACSF)THEN
	  command = motorstring(motorno) //'H'//dirc
	  lcom = 4
	  CALL RS232F(command, lcom, reply, lreply)
	  moving(motorno) = .true.
	  IF (lreply .EQ. 0 .OR. reply(1:2) .NE. motorstring(motorno)) GOTO 800 !err message and return
c         Write new position of motor to motors.buf
	    OPEN(UNIT=1,FILE=MOTORS_BUF,STATUS='OLD',RECL= MOTBUF_RECL,
     1	      ACCESS='DIRECT', FORM='UNFORMATTED',err=780)
	    READ (1,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
     1        back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
     1        vmot2(motorno), vmot3(motorno), vmot4(motorno)
	    IF (dirc.eq.'-')THEN
	      vmot4(motorno) = vmot1(motorno)*pulse(motorno)
	    ELSE
	      vmot4(motorno) = vmot2(motorno)*pulse(motorno)
	    ENDIF
	    WRITE (1,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
     1        back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
     1        vmot2(motorno), vmot3(motorno), vmot4(motorno)
	    CLOSE(1)
        ELSEIF (mottype(motorno) .EQ. VME)THEN
	  IF (dirc.eq.'-')THEN
	     CALL VMEHOME(motorno,0)
	  ELSE
	     CALL VMEHOME(motorno,1)
          ENDIF
	ELSEIF (mottype(motorno) .EQ. EZ) THEN
	   moving(motorno)=.true.
	   CALL EZHOME(motorno,0)
	ELSE !standard ACS
	  command = motorstring(motorno) //'H'//dirc
	  lcom = 4
	  CALL RS232(command, lcom, reply, lreply)
	  IF (lreply .EQ. 0) GOTO 800 !err message and return
	ENDIF
	RETURN

C  Treat errors
 780	CALL Putcontr(' Error updating position for motor '
     1		//motorstring(motorno))
c	CALL setef(camerr)                       ! set error flag
	RETURN	
 
 800	CALL PUTCONTR(' ERROR SENDING MOTOR TO HOME')
c	CALL setef(camerr)                       ! set error flag

	RETURN
	END
C
