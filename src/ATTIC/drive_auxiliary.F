#include "icp.h"
	include COMPILE_OPTS
      	SUBROUTINE Drive_Auxiliary (mot_no, target) 
C     	*******************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Move Motor Specified To Physical Angle Indicated (Magnet, Furnace).     C
C     The controller must have been initialized previously by Init_Auxiliary. C
C     Called from: Parse_Contr, New_Temp                                      C
C                                                                             C
C     Beat Schmid, Gayle Greene, August 1990                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      	IMPLICIT     none
      	CHARACTER*80 command, reply
      	INTEGER*2    lcom, lreply, errno
      	INTEGER*4    mot_no, nsteps, oldpos, newpos, change, notries, io_stat,
     1			status
      	REAL*4       target, aux_destination
	LOGICAL      retry
	character*2	errstr

#if defined(VMS)
	COMMON /Yield_A/ aux_destination

      	INCLUDE INSTR_CMN


C---------------------- GET NEWPOS IN TERMS OF PULSES ------------------------C

 20   	newpos = NINT ( target * 342 )                 ! truncate to undershoot


C------------------- GET CURRENT MOTOR POSITION USING P: ---------------------C
c     Get current position using P:
	notries=20 !try up to 20 times 
	retry=.true.
	WRITE (UNIT=command(1:), FMT= '(i2.2,a1)') mot_no, 'P'
	lcom=3	
 30	call rs232(command,lcom,reply,lreply)
	if (reply(3:3).ne.'P'.or.lreply.eq.0)then !error in reply
	  if (retry)then
	    if (notries.gt.0)then
	        notries=notries-1
	    else
		retry=.false.
	    endif
c	    CALL Putcontr(' drive retrying command: '//command(1:lcom))
	    call libwait(.5,status)
	    goto 30
	  else
	    errno=1
	    goto 750
	  endif
	endif
	retry=.true.
      READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) oldpos
      IF (io_stat .NE. 0) then
	if (retry)then
	  retry=.false.
c	  CAll Putcontr
c     1		(' drive decode needs to retry command: '//command(1:lcom))
	  call libwait(.5,status)
	  goto 30
	else
	  errno=2
	  goto 750
      	endif
      ENDIF





      	change = newpos - oldpos                             ! change in pulses
      	IF (ABS(change) .LE. 2) RETURN                       ! motor there
        aux_destination = target
      
      
      
C------------- SEND OUT WHERE TO MOVE (IN ABSOLUTE PULSE VALUE) --------------C

	nsteps = 1
      	DO WHILE (10**nsteps .LE. IABS(newpos))               ! newpos.lt.10**9
           nsteps = nsteps + 1
      	END DO
      	IF (newpos .GE. 0) THEN
	   WRITE (UNIT=command(1:), FMT = '(i2.2,a2,i<nsteps>)' )
	1        mot_no, 'G+', newpos
       	ELSE
	   WRITE (UNIT=command(1:), FMT = '(i2.2,a2,i<nsteps>)' )
	1        mot_no, 'G-', ABS(newpos)
      	END IF
	lcom = 4 + nsteps
      	CALL RS232 (command, lcom, reply, lreply)
	GOTO 900

 750  write(unit=errstr,fmt=*,iostat=io_stat)errno
	Call Putcontr
     1	 (' Error no. '//errstr//' moving auxiliary motor')
	if (errno.eq.1.or.errno.eq.2) 
     1	   Call putcontr(' ***** Auxiliary MOTOR NOT MOVED *****')
c 800  CALL setef(camerr)                             ! set error flag
 800	continue
#endif
 900  RETURN
      	END
