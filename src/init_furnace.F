#include "icp.h"
	SUBROUTINE Init_Furnace(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE EUROTHERM FURNACE TEMPERATURE CONTROLLER          C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, Feb, 1997                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_libwait.h"
	end interface


	CHARACTER*80  command,reply
	INTEGER*2     lreply,lcom, termchar
	LOGICAL	      errflag
	REAL*4	      temp, hldtemp, rrate
	CHARACTER*20  str20
	INTEGER*4     ios, ig, i, j, status
	BYTE		repbyte
	CHARACTER*1     repchar
	EQUIVALENCE   (repbyte,repchar)

c  furnace id string
	character*4	idstr/'0011'/

c  read temp from controller
	command(1:) = CHAR(04)//idstr//'PV'//CHAR(05)
	lcom = 8
	CALL frn232(command,lcom,reply,lreply,termchar,.FALSE.) !no cksum
	IF (lreply .EQ. 0 .AND. termchar .EQ. 0) GOTO 96 !err branch.
	read(reply(4:lreply-1),fmt=*,err=96)temp
	IF (temp .LT. -100 .OR. temp .GT. 2500)GOTO 98
	CALL libwait(.5,status)
	hldtemp = temp
c  reread temp from controller to verify
	command(1:) = CHAR(04)//idstr//'PV'//CHAR(05)
	lcom = 8
	CALL frn232(command,lcom,reply,lreply,termchar,.FALSE.) !no cksum
	IF (lreply .EQ. 0 .AND. termchar .EQ. 0) GOTO 96 !err branch.
	read(reply(4:lreply-1),fmt=*,err=96)temp
	IF (temp .LT. -100 .OR. temp .GT. 2500)GOTO 98
	IF (abs(hldtemp - temp) .GT. 100.0)GOTO 98
	CALL libwait(.5,status)
c  write temp back to controller
	write(unit=str20,fmt='(i20)',err=100)NINT(temp)
	i = 1
	do while(str20(i:i).eq.' ' .AND. i .LT. 20)
	    i = i + 1
	enddo
	command(1:) = CHAR(04)//idstr//CHAR(02)//'SL'//
     1		str20(i:20)//CHAR(03)
	lcom = 30 - i
	CALL FRN232(command,lcom,reply,lreply,termchar,.TRUE.) !add cksum
	IF (termchar .NE. 6)GOTO 100
	CALL libwait(.5,status)
C  ask user for ramp rate
 25 	CALL READ_STRING(keyb, str20,
     1		'Enter ramp rate (degrees/hour) (default: 600): ',20,
     1		j,j,j, ig,j, contr)  !j is for unix
	IF (ig .EQ. 0)THEN
	       rrate = 600.
	ELSE
	        READ(unit=str20(1:ig),fmt=*,err=25)rrate
	ENDIF
C  write reset to controller
	command(1:) = CHAR(04)//idstr//CHAR(02)//'OS>0000'//
     1		CHAR(03)
	lcom = 14
	CALL FRN232(command,lcom,reply,lreply,termchar,.TRUE.) !add cksum
	IF (termchar .NE. 6)GOTO 105
	CALL libwait(.5,status)
c  write ramp rate to controller
	write(unit=str20,fmt='(i20)',err=150)NINT(rrate)
	i = 1
	do while(str20(i:i).eq.' ' .AND. i .LT. 20)
	    i = i + 1
	enddo
	command(1:) = CHAR(04)//idstr//CHAR(02)//'RR'//
     1		str20(i:20)//CHAR(03)
	lcom = 30 - i
	CALL FRN232(command,lcom,reply,lreply,termchar,.TRUE.) !add cksum
	IF (termchar .NE. 6)GOTO 150
	CALL libwait(.5,status)
C  put controller in ramp to setpoint mode
	command(1:) = CHAR(04)//idstr//CHAR(02)//'OS>0002'//
     1		CHAR(03)
	lcom = 14
	CALL FRN232(command,lcom,reply,lreply,termchar,.TRUE.) !add cksum
	IF (termchar .NE. 6)GOTO 160

	errflag = .FALSE.
	RETURN 

C  error branch
 96	CALL Putcontr(' Err reading back temp')
	errflag = .TRUE.
	RETURN
 98	CALL Putcontr(' Temp read back is out of range')
	errflag = .TRUE.
	RETURN
 100	CALL Putcontr(' Err writing set point to controller')
	errflag = .TRUE.
	RETURN
 105	CALL Putcontr(' Err writing reset to controller')
	errflag = .TRUE.
	RETURN
 150	CALL Putcontr(' Err writing ramp rate to controller')
	errflag = .TRUE.
	RETURN
 160	CALL Putcontr(' Err writing ramp-to-setpoint mode')
	errflag = .TRUE.
	RETURN
	END




