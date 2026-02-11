#include "icp.h"
	SUBROUTINE Init_Auxiliary (mot_no, aux)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	                                                                      C
C	Routine to initialize either a spare or a scrificed motor controller  C
C	for use with the magnet (aux = 1) or the furnace (aux = 2)            C
C	Called from: Parse_Contr                                              C
C	                                                                      C
C	Beat Schmid, August 1990                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
 	INTEGER*2    lreply
	INTEGER*4    mot_no, aux, base, top, pulse
	CHARACTER*80 command, reply

#if defined(VMS)

	base  = 80
	top   = 200
	pulse = 342


C----------------- GIVE FEEDBACK OF WHAT WE ARE DOING HERE -------------------C
	
        CALL putcontr ( 'Initializing auxiliary motor controller:')
	CALL putcontr ( '                             Base  = 80')
	CALL putcontr ( '                             Top   = 200')
	CALL putcontr ( '                             Pulse = 342')
	CALL putcontr ( ' ')
	

C----------------- WRITE BASERATE TO BASE AND JOG REGISTER -------------------C

	WRITE (UNIT=command(1:), FMT= '(i2.2,a2,i3)') mot_no, 'B=', base
	CALL RS232 (command, 7, reply, lreply)
	IF (lreply .EQ. 0) RETURN
	WRITE (UNIT=command(1:), FMT= '(i2.2,a2,i3)') mot_no, 'J=', base
	CALL RS232 (command, 7, reply, lreply)
	IF (lreply .EQ. 0) RETURN


C--------------- WRITE TOPSPEED TO AUTO AND MANUAL REGISTERS -----------------C

	WRITE (UNIT=command(1:), FMT= '(i2.2,a2,i3)') mot_no, 'V01=', top
	CALL RS232 (command, 9, reply, lreply)
	IF (lreply .EQ. 0) RETURN
	WRITE (UNIT=command(1:), FMT= '(i2.2,a2,i3)') mot_no, 'V00=', top 
	CALL RS232 (command, 9, reply, lreply)
	IF (lreply .EQ. 0) RETURN


C----------------- WRITE RAMP TO AUTO AND MANUAL REGISTERS -------------------C

	WRITE (UNIT=command(1:), FMT= '(i2.2,a5)') mot_no, 'R01=1'
	CALL RS232 (command, 7, reply, lreply)
	IF (lreply .EQ. 0) RETURN
	WRITE (UNIT=command(1:), FMT= '(i2.2,a5)') mot_no, 'R00=1'
	CALL RS232 (command, 7, reply, lreply)
	IF (lreply .EQ. 0) RETURN


C--------------- WRITE CONVERSION AND RATE DIVISION CONSTANTS ----------------C

	WRITE (UNIT=command(1:), FMT= '(i2.2,a2,i3)') mot_no, 'C=', pulse
	CALL RS232 (command, 7, reply, lreply)
	IF (lreply .EQ. 0) RETURN
	WRITE (UNIT=command(1:), FMT= '(i2.2,a4)') mot_no, 'K=1'
	CALL RS232 (command, 6, reply, lreply)
	IF (lreply .EQ. 0) RETURN
#endif
C NOT USED!!
	END
