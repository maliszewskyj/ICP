#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Yield_Auxiliary
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   Routine will wait until the magnetic field or furnace motor has reached   C
C   its destination.                                                         C
C   Called from: Parse_Contr, New_Temp                                        C
C                                                                             C
C   Beat Schmid, June 1990                                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


#if defined(VMS)
	IMPLICIT     NONE
	LOGICAL*1    finished, stoperr
	INTEGER*4    io_stat, state, ipos, status, notries
	INTEGER*4    SYS$SETIMR, SYS$HIBER, SYS$WAKE, SYS$READEF
	CHARACTER*80 command, reply
	INTEGER*2    lreply
	REAL*4       currentpos, aux_destination

	COMMON  /Yield_A/ aux_destination

	EXTERNAL Motor_Timer_Ast

	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
	INCLUDE '($SSDEF)'


        status = SYS$READEF (%VAL(contr_A), state)
        IF (status .EQ. SS$_WASSET) GOTO 675

 100	finished = .TRUE.

C----------------------------- GET MOTOR STATUS ------------------------------C

	WRITE (UNIT=command(1:3), FMT= '(i2.2, a1)') ABS(tdev.mot), 'E'
	CALL RS232_MOVING (command, 3, reply, lreply)
	IF (lreply .LT. 0) GOTO 675
	IF (lreply .EQ. 0) THEN
	   CALL RS232_MOVING (command, 3, reply, lreply)       ! repeat once
	   IF (lreply .LT. 0) GOTO 675
	   IF (lreply .EQ. 0) GOTO 660
	ENDIF  ! (* status error *)


C------------------------ MOTOR HAS STOPPED ALREADY --------------------------C

	IF (reply(9:9) .EQ. '0') THEN               ! motor is no longer moving
	   WRITE (UNIT=command(1:3), FMT= '(i2.2, a1)') ABS(tdev.mot), 'P'
	   CALL RS232(command, 3, reply, lreply)
	   IF (lreply .LT. 0) GOTO 675
	   IF (lreply .EQ. 0 .OR. reply(3:3) .NE. 'P') THEN
	      CALL RS232(command, 3, reply, lreply)            ! repeat once
	      IF (lreply .LT. 0) GOTO 675
	      IF (lreply .EQ. 0) GOTO 660
	   ENDIF ! (* status error *)
           READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) Ipos
 	   IF (io_stat .NE. 0) THEN
	     call putcontr(' err decoding auxiliary motor current position: '//
     1		reply(5:lreply))
c	     CALL SYS$SETEF(%VAL(camerr))                      ! set error flag
	     GOTO 675                                          ! give up
	   ENDIF ! (* iostat error *)

	   currentpos = float(ipos) / 342.
	   IF (ABS(currentpos - aux_destination) .GT. 0.01) THEN
              status = SYS$READEF (%VAL(contr_A), state)
              IF (status .EQ. SS$_WASSET) GOTO 675
	      CALL Drive_Auxiliary (ABS(tdev.mot), aux_destination)
	      finished = .FALSE.
	   ENDIF ! (* too far off target *)


C---------------------------- MOTOR STILL MOVING -----------------------------C

        ELSE
	   finished = .FALSE.
	END IF 


	IF (finished) RETURN

C----------------------- ABORT IF CONTR_A OR PROBLEM -------------------------C

 660    status = SYS$READEF (%VAL(contr_a), state)
        IF (status .EQ. SS$_WASSET) THEN
 675	   notries = 0
 700	   stoperr = .FALSE.
	   WRITE (UNIT=command(1:3), FMT= '(i2.2, a1)') ABS(tdev.mot), 'Q'
	   CALL RS232(command, 3, reply, lreply)
	   IF (lreply .EQ. 0) stoperr = .TRUE.
	   IF (stoperr) THEN
	      notries = notries + 1
	      IF (notries .LT. 50) THEN
	         GOTO 700
	      ELSE
	         GOTO 800
	      ENDIF
	   ELSE
	     GOTO 800                                                 ! return
	   END IF
        END IF

C--------------------------- WAIT BEFORE RECHECK -----------------------------C

	status = SYS$SETIMR (, motor_wait_time, Motor_Timer_Ast,)
	IF (.NOT. status) CALL LIB$SIGNAL(%VAL(status))
	status = SYS$HIBER()
	IF (.NOT. status) CALL LIB$SIGNAL(%VAL(status))
	GOTO 100

 750	call putcontr(' Error in motor parameters for auxiliary motor ')
 800    continue
c 800	CALL SYS$SETEF(%VAL(camerr))                           ! set error flag
#endif
 900	RETURN
	END
