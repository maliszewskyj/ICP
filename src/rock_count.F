#include "icp.h"
	SUBROUTINE Rock_Count (errno)
C     	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to rock motor rkmot between rpos1 & rpos22 while waiting        C
C       for scaler to finish counting                                         C
C     Called from: Run_I_Buf                                                  C
C                                                                             C
C     Gayle Greene, February, 1996                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
#include "c_rs232_acs.h"
#include "c_vme.h"
#include "c_amscaler.h"
	end interface

	INTEGER*4	imot, errno
	REAL*4		target(2)

	INTEGER*4    state, alarm(2), status
	CHARACTER*80 command, reply, gocommand
	CHARACTER*50 whcom(2)
	INTEGER*4    lcom, lreply, notries, gocom,lwhcom(2)
	INTEGER*4    nsteps, newpos(2), iwhich, mtries
	INTEGER*4    io_stat, imove, i, ilim,secwait,retn
	REAL*4	     targetangle
	CHARACTER*4  integer_fmt, errstr
	CHARACTER*1  size, DIRC
	CHARACTER*10 stepstring
	LOGICAL      goretry, finished, stoperr, bad_reply, Readef

	imot = rkmot !from instr.cmn
	target(1) = rkpos1
	target(2) = rkpos2

	errno = 0

	IF (.not. rockfl .OR. .not. motfl(imot) .OR. fixed(imot))THEN
	  moving(imot) = .FALSE.
	  RETURN
	ENDIF

C  Set up character strings to send motor to each position
	if (mottype(imot).EQ.ACS) THEN
	   newpos(1) = NINT(target(1)*pulse(imot)) !take nearest no. of steps
	   newpos(2) = NINT(target(2)*pulse(imot)) !take nearest no. of steps
	   DO iwhich = 1,2
	      IF (newpos(iwhich).LT.0)THEN
		 dirc='-'	!For ACS, this is sign of new position, not direction!
	      ELSE
		 dirc='+'
	      ENDIF
	      imove = iabs(newpos(iwhich)) !Write abs pos to character string
	      nsteps = 1 
	      DO WHILE (10**nsteps .LE. imove) ! imove.lt.10**9
		 nsteps = nsteps + 1
	      END DO
	      WRITE(size, FMT= '(i1)', iostat=io_stat) nsteps
	      IF (io_stat .NE. 0) GOTO 750
	      integer_fmt='(i'//size//')'
	      WRITE(stepstring, FMT= integer_fmt, iostat=io_stat) imove 
	      IF (io_stat .NE. 0) GOTO 750 !quit if error
	      whcom(iwhich) = motorstring(imot) //'G'//dirc// stepstring(1:nsteps)
	      lwhcom(iwhich) = 4 + nsteps
	   ENDDO
	ENDIF

C  Drive motor to next position. Wait for it to get there. Repeat while counting.
	DO WHILE (counting) !any other way I want to bale out?
C	  Make sure motor has terminated last move in case this is a repeat call
	  IF (.NOT. moving(imot))THEN !start it moving
C	    Set up for alternate target
	      IF (iwhich .EQ. 1)THEN !alternate between targets
	        iwhich = 2
	      ELSE
	        iwhich = 1
	      ENDIF
	      IF (mottype(imot).eq.ACS) THEN
		 gocommand(1:) = whcom(iwhich)(1:lwhcom(iwhich))
		 gocom = lwhcom(iwhich)
	      ENDIF
C	    Set flags for yield (in case it is called later)
	      moving(imot) = .TRUE.
	      destination(imot) = target(iwhich)
	      overshot(imot) = .FALSE.
C	    Send out drive command (with a few retries if necessary)

	      IF (mottype(imot).eq.ACS) THEN
	      notries = 2
	      goretry=.true.
	      DO WHILE (notries .GT. 0 .AND. goretry)
		goretry = .FALSE. !only try once by default
		IF (notries .NE. 2)call libwait(.1,status) !wait between calls
 	        CALL RS232(gocommand, gocom, reply, lreply) !send out go command
	        IF (lreply.ne.2)then !not a valid reply. Try a status command
	          mtries=10 !retry a status command up to 10 times
		  bad_reply = .TRUE.
		  DO WHILE (mtries.GT.0 .AND. bad_reply)
 	            command(1:3)=motorstring(imot)//'E'
	            lcom=3	
		    IF (.not. counting) return
	            call libwait(.1,status)
#if defined(VMS)
 	            call rs232_MOVING(command,lcom,reply,lreply) !get status
#else
                    call rs232(command,lcom,reply,lreply) ! get status
#endif
	            if (lreply.lt.0)goto 675 !control a detected
	    	    bad_reply = lreply.eq.0 .or. reply(8:8).ne.'A'
	            mtries=mtries-1
	    	  ENDDO
		  IF (.NOT. bad_reply .AND. reply(9:9).eq.'0')
     1		    goretry = .TRUE. !motor not moving, but communicating now
	        ENDIF !lreply ne 2
	        notries = notries - 1
	      ENDDO		!while notries gt 0
	      ELSE
		 CALL DRIVE(imot,target(iwhich))
	      ENDIF

	  ENDIF !.not. moving(imot) (Motor was not moving. Should be now.)
C	  Wait for motor to get there
	  finished = .FALSE.
	  DO WHILE (.NOT. finished .AND. counting)
C           Check for control_a
 100	      IF (READEF(contr_a)) goto 675 
	      io_stat = 1
	      if (vmedata) then
		 CALL VSCALERSTATUS(io_stat) ! Use VME scalers
	      else if (amscaler) then
		 CALL SCALERSTATUS(io_stat) ! Use AM9513 type scalers
	      else 
                                        ! Some other scaler type
	      endif
	      if (io_stat .ne. 1) THEN
		 counting = .FALSE.
		 CALL ABS_TIME (stopsecs)
	      endif	      

	      IF (.not. counting) return
C    	    Get motor status (Repeat until valid reply received)
	      IF (mottype(imot).eq.VME) THEN
		 retn = VMESTATUS(imot,imove,ilim)
		 if (imove .eq. 0) then
		    moving(imot)=.false.
		    finished=.true.
		 else
		    IF (READEF(contr_a)) goto 675 
		    IF (.NOT. counting)RETURN !return if counting has finished
		    call libwait(1.0,status)
                 endif
	      ELSE IF (mottype(imot).eq.ACS) THEN
		 mtries=10	!retry a given command up to 10 times
		 bad_reply = .TRUE. !to execute loop first time
		 DO WHILE (mtries .GT. 0 .AND. bad_reply)
		    lreply=0	!no answer yet
		    reply(1:)=' ' !no valid reply yet
		    command(1:3)=motorstring(imot)//'E'
		    lcom=3	
		    IF (mtries .NE. 10)call libwait(.5,status) !wait between tries
		    bad_reply = lreply.EQ.0 .OR. reply(8:8).NE.'A'
		    IF (lreply.lt.0)goto 675 !control a detected
		    IF (.NOT. counting)RETURN
		    mtries=mtries-1
		 ENDDO
		 IF (bad_reply)then !no good reply after 10 tries
		    moving(imot)=.false. !give up on this motor
		    call putcontr(' could not confirm position of rocking motor ')
		 ELSE		!reply is of proper format
c       Have a valid reply so see if motor has stopped moving
		 IF (reply(9:9).eq.'0')then !motor is no longer moving
		    moving(imot)=.false. !set flag
		    finished = .TRUE.
		 ELSE		!motor still moving
		    IF (READEF(contr_a)) goto 675 
		    IF (.NOT. counting)RETURN !return if counting has finished
		    call libwait(1.0,status)
		 ENDIF		!motor no longer moving
	      ENDIF		!bad_reply
	   ENDIF		! mottype
	ENDDO			!(yield) while not finished and counting
	ENDDO !(drive-yield)while counting

C  Counting flag must be cleared
	RETURN 

c  Control_a detected. Stop motor if still moving.
 675	IF (.NOT. moving(imot))RETURN !motor must have already finished
	notries=0
	stoperr = .TRUE. !to execute loop 1st time
	DO WHILE (stoperr .AND. notries .LT. 10)
	  stoperr=.false.
	  command=motorstring(imot)//'Q'
	  lcom = 3
	  call rs232(command,lcom,reply,lreply)
	  IF (lreply.eq.0)THEN
	    stoperr=.true.
	    notries=notries+1
	  ELSE !motor acknowledged abort command
	    moving(imot)=.false.
	    RETURN
	  ENDIF
	ENDDO

c 	call setef(camerr) !set error flag since could not abort it
	moving(imot)=.false.
 	RETURN


C  Treat errors
 750  Write(Unit=errstr,fmt=*,iostat=io_stat)errno
	CALL Putcontr(' Error setting up commands to rock motor')
	RETURN
	END

