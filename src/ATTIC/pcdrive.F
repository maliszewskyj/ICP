#include "icp.h"
      include COMPILE_OPTS
      SUBROUTINE PCDrive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drive: Move PC Motor Specified To Physical Angle Indicated              C
C                                                                             C
C     Gayle Greene, November 1996					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      IMPLICIT     none
      CHARACTER*80 command, reply, gocommand
      INTEGER*2    lcom, lreply, errno, notries, gocom
      INTEGER*4    motorno, bigtries, status,state
      REAL*4       targetangle,oldangle,angchg, distance
#if defined(VMS)
      INTEGER*4    io_stat, SYS$SETEF, i, SYS$READEF, SS$_WASSET
#else
      INTEGER*4    io_stat, i
      LOGICAL      readef
#endif
      LOGICAL      retry
	CHARACTER*4  errstr

      INCLUDE INSTR_CMN
      INCLUDE MOTORS_CMN
c      INCLUDE XRAY_CMN

	retry=.true.  !retry once decoding starting position
	IF (.not. noback .AND. back(motorno).NE. 0)THEN !worry about direction
	  notries = 20 !retry to get position up to 10 times
 20	  command(1:3)=motorstring(motorno)//'P'
	  lcom=3	
 	  call rs232PC_retry(command,lcom,reply,lreply,errno)
	  IF (errno .NE. 0) THEN
	      IF (notries .GT. 0)THEN
	        notries = notries - 1
		CALL Putcontr('Drive: will retry to get starting position: '//
     1			command(1:lcom))
#if defined(VMS)
                status = SYS$READEF (%VAL(contr_a), state)  !check for control a
                IF (status .EQ. SS$_WASSET) THEN
#else
                IF (readef(contr_a)) THEN
#endif
		  CALL Putcontr('Control_a detected')
	          RETURN
                ENDIF

	        call libwait(.5,status)
		GOTO 20
	      ELSE
		errno = 1
	        GOTO 750
	      ENDIF !notries gt 0
	  ENDIF !errno ne 0 getting starting position

	  READ(reply(5:lreply), FMT= *, iostat=io_stat) oldangle
          IF (io_stat .NE. 0) then
	      if (retry)then !retry whole position business once
	        retry=.false.
	        call putcontr(' Drive: decode needs to retry command: '//command(1:lcom))
		CALL Putcontr('   Last reply: '//reply(1:lreply))
#if defined(VMS)
                status = SYS$READEF (%VAL(contr_a), state)  !check for control a
                IF (status .EQ. SS$_WASSET) THEN
#else
                IF (readef(contr_a)) THEN
#endif
		  CALL Putcontr('Control_a detected')
	          RETURN
		ENDIF

	        call libwait(.5,status)
	        goto 20
	      else
	        errno=2
		CALL Putcontr(' Drive: giving up decoding: command: '//
     1		  command(1:lcom)//'  reply: '//reply(1:lreply) )
	        goto 750
      	      endif
	  ENDIF !errno ne 0 reading starting position from reply
	  angchg = targetangle-oldangle                 ! get change in angle


	  IF (abs(angchg) .LT. 
     1		abs(float(merr(motorno))/float(pulse(motorno))) )RETURN !there
	  IF (angchg .LT.0.)THEN
	      overshot(motorno)=.TRUE.
	      angchg = targetangle-(float(back(motorno))/float(pulse(motorno)))
	  ELSE
	      overshot(motorno) = .FALSE.
	      angchg = targetangle
	  ENDIF
	ELSE !noback flag is set
	      overshot(motorno) = .FALSE.
	      angchg = targetangle
	ENDIF !noback flag set

	gocommand(1:)=' '
	write(unit=gocommand(1:20),fmt=*,iostat=io_stat)abs(angchg)      
	i = 1
	DO WHILE (gocommand(i:i) .EQ. ' ' .AND. i .LT. 20) !get rid of blanks
	    i = i + 1
	ENDDO
	IF (i .EQ. 20)THEN
	    errno = 3
	    CALL Putcontr(' Drive: err in angchg generated: '//gocommand(1:20))
	    GOTO 750
	ENDIF
	gocommand(1:)=gocommand(i:20)

C  We now know which way we are headed and by how much
	  if(trackmoves)call memcurpos(motorno,1,targetangle)
	  IF (nsta .NE. 8) THEN !no command handshaking with PC
	    moving(motorno) = .TRUE.
	    destination(motorno) = targetangle
	    IF (angchg .GE. 0.)THEN
	      command = motorstring(motorno) //'G+'//gocommand(1:20)
	      lcom = 24
	    ELSE
	      command = motorstring(motorno) //'G-'//gocommand(1:20)
	      lcom = 24
	    END IF

 	    CALL rs232PC_retry(command,lcom,reply,lreply,errno)
	    IF (errno .NE. 0) THEN
	      CALL Putcontr
     1	       ('Drive:  Error sending drive command to motor '//motorstring(motorno))
	    ENDIF
	    RETURN
	  ENDIF !nsta .NE. 8

C  PC based motors on BT8 only: Do handshaking with PC for extra security on BT8	  
	  retry = .TRUE.
	  notries = 20
 40	  IF (angchg .GE. 0.)THEN
	    command = motorstring(motorno) //'D=+'//gocommand(1:20)
	    lcom = 25
	  ELSE
	    command = motorstring(motorno) //'D=-'//gocommand(1:20)
	    lcom = 25
	  END IF

	  CALL RS232PC_retry(command, lcom, reply, lreply,errno) !send out distance command
	  IF (errno .ne. 0)THEN
	    IF (notries .GT. 0)THEN
	      notries = notries - 1
	     CALL Putcontr('Drive: will retry to send distance: '//
     1			command(1:lcom))
#if defined(VMS)
             status = SYS$READEF (%VAL(contr_a), state)  !check for control a
             IF (status .EQ. SS$_WASSET) THEN
#else
             IF (readef(contr_a)) THEN
#endif
	          errno = -1
		  CALL Putcontr('Control_a detected')
	          RETURN
	      ENDIF
	      call libwait(.5,status)
	      GOTO 40
	    ELSE
	      CALL Putcontr
     1	       (' Drive: Error sending distance command to motor '//motorstring(motorno))
#if defined(VMS)
	      IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
              IF (nsta .EQ. 8)CALL setef(contr_a)
#endif
	      RETURN
	    ENDIF
	  ENDIF

	  notries = 20
 50	  command = motorstring(motorno) //'D'
	  lcom = 3
	  CALL RS232PC_retry(command, lcom, reply, lreply,errno) !send out distance command
	  IF (errno .ne. 0)THEN
	    IF (notries .GT. 0)THEN
	      notries = notries - 1
	      CALL Putcontr('Drive: will retry to ask distance: '//
     1			command(1:lcom))
#if defined(VMS)
              status = SYS$READEF (%VAL(contr_a), state)  !check for control a
              IF (status .EQ. SS$_WASSET) THEN
#else
              IF (readef(contr_a)) THEN
#endif
	          errno = -1
		  CALL Putcontr('Control_a detected')
	          RETURN
	      ENDIF
	      call libwait(.5,status)
	      GOTO 50
	    ELSE
	      CALL Putcontr
     1	       (' Drive: Error reading distance back from motor '//motorstring(motorno))
#if defined(VMS)
	      IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
	      IF (nsta .EQ. 8)CALL setef(contr_a)
#endif
	      RETURN
	    ENDIF
	  ENDIF
	  READ(reply(5:lreply), FMT= *, iostat=io_stat) distance
          IF (io_stat .NE. 0) then	    
	    IF (retry)THEN
	      retry = .FALSE.
	      notries = 20
	      CALL Putcontr('Drive: will retry to ask distance: '//
     1			'last reply: '//reply(1:lreply))
#if defined(VMS)
              status = SYS$READEF (%VAL(contr_a), state)  !check for control a
              IF (status .EQ. SS$_WASSET) THEN
#else
              IF (readef(contr_a)) THEN
#endif
	          errno = -1
		  CALL Putcontr('Control_a detected')
	          RETURN
	      ENDIF
	      call libwait(.5,status)
	      goto 40
	    ELSE !give up
	      CALL Putcontr
     1	       (' Drive: Error decoding distance back from motor '//motorstring(motorno))
	      CALL Putcontr('   distance reply received: '//reply(1:lreply) )
#if defined(VMS)
  	      IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
              IF (nsta .EQ. 8)CALL setef(contr_a)
#endif

	      RETURN
	    ENDIF
	  ENDIF
	  IF (abs(angchg-distance) .GT. 
     1		abs(float(merr(motorno))/float(pulse(motorno))) )THEN !not equal
	    CALL Putcontr
     1	      (' Drive: distance returned: '//reply(5:lreply)//' not that sent: '//
     1		 gocommand(1:20) )
#if defined(VMS)
	    IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
            IF (nsta .EQ. 8)CALL setef(contr_a)
#endif

	    RETURN
	  ENDIF
	  command = motorstring(motorno) //'G'
	  lcom = 3
	  CALL RS232PC_retry(command, lcom, reply, lreply,errno) !send out distance command
	  IF (errno .ne. 0)THEN
	    CALL Putcontr
     1	     (' Drive: Error sending GO command to motor '//motorstring(motorno))
#if defined(VMS)
	    IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
            IF (nsta .EQ. 8)CALL setef(contr_a)
#endif
	    RETURN
	  ENDIF

	  moving(motorno) = .TRUE.
	  destination(motorno) = targetangle
	  RETURN
 750  Write(Unit=errstr,fmt=*,iostat=io_stat)errno
	CALL Putcontr(' Error no. '//errstr//' moving motor: '//
     1	  motorstring(motorno))
	if (errno.eq.1.or.errno.eq.2) 
     1	   CALL Putcontr(' ***** MOTOR '//motorstring(motorno)//' NOT MOVED *****')
#if defined(VMS)
	IF (nsta .EQ. 8)CALL SYS$SETEF(%VAL(contr_a))
#else
        IF (nsta .EQ. 8)CALL setef(contr_a)
#endif
	RETURN
	END
