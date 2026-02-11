#include "icp.h"
	SUBROUTINE Careful_Approach(temp,ter,timeup,
     1		info,temp_ok, abortflag, stopf)
C-----------------------------------------------------------------------------C
C UNIX notes: will not work as is. Need to do something about secnds call.
c                                                                            C
c move the set point towards the target temperature
c when temp first passes thru setpoint measure the approach speed
c when later msr of approach speed is less than crossing speed by tolerance
c factor, continue to next accel step
c until within error or timeout
c
C init:		beta = 1/(1 + caution)
C
C accel step:  	Tset = Tnow + beta*(Tfinal - Tnow)
C
C go to next accel when:
C		speednow <= speedcross*ABS(deviation)/maxdif/(caution+2)
C		beta  = (Tset - Tnow)/(T0 - Tnow)/(caution+1)
C		T0    = alpha*(Tnow - Tset) + Tset
C		alpha = speednow/(speedcross - speednow)
C
C
C accel step loop:
C beta = 1 -> tset = final setpoint	enough deceleration
C beta = 0 -> tset = tempnow		need more deceleration
C                                                                             C
C   Ross Erwin: DEC, 1996                                                 C
C-----------------------------------------------------------------------------C

	IMPLICIT      none
C  Calling parameters
C    Passed from new_temp_field. Not to be changed here:
	REAL*4	      temp, !final target temp. Passed to this routine.
     1		      ter !err allowed in final temp. Passed to this routine.
	INTEGER*4     stopf, !number of event flag for stop. passed to this routine
     1		      timeup(2) !contains time in internal format.
C    Given value or modified by careful_approach
	CHARACTER*80	info(4) !message for status file passed in.
	LOGICAL	      abortflag, !set by this routine if control_a used
     1		      temp_ok !flag set by this routine if temp reaches target
#if defined(VMS)
C  Local variables
	REAL*4	      setpt !value supplied by this routine
	CHARACTER*8	str8
	CHARACTER*80	response
	LOGICAL		out_of_time, Readef
	REAL*4	      tsamp, tempnow, tsetp, deviation
	INTEGER*4     s, status
	REAL*4        tset, tempprev, setdeviation, deltatemp
	REAL*4	      ratenow, ratecross, ratetarget, absetdev, tempadv
	REAL*4	      beta, alpha, speednow, speedcross, speedtarget, targetrat
	REAL*4	      absdev, maxdif, devratio
	REAL*4	      timearrive, timeset, timenow
	REAL*4        deltatime, deltanow, holdtime, timechar
	REAL*4	      deltatimep, rateprev 
	REAL*4	      cautone, cauttwo
	REAL*4	      tmovefact/0.33/
	REAL*4	      saveoffset
	REAL*4	      params(6)
	LOGICAL       crossed, waiting, approach, nowapp

C  External calls
	LOGICAL	      timeyet

	INCLUDE TC_CAREFUL_CMN !common variables for Ross careful approach

c init in common does not work so do it here
	caution = 2.
	maxoffset = 0.

	abortflag = .FALSE. !default
	out_of_time = .FALSE. !initial value

	     holdtime = 3. ! in seconds
	     CALL RdTemp(Tsamp,tempnow,Tsetp)
	     timenow = secnds(0.) ! initial timer at first temp read
	     deltanow = 0.
	     deltatemp = 0.
	     deltatime = 0.
	     tempprev = tempnow
	     timechar = 0.
	     deviation = temp - tempnow
	     IF (caution .LT. 0.) caution = 0.
	     cautone = 1. + caution
	     cauttwo = 2. + caution
	     beta = 1./cautone
c main control loop
	     DO WHILE (.NOT. temp_OK .AND. .NOT. out_of_time) !repeat until max wait time exceeded or temp at target
		tset = tempnow + deviation*beta ! do accel step
		tempadv = tset - tempnow
		setpt = tset + offset
		CALL WrTemp(setpt)
c		WRITE (UNIT= str8, FMT= '(f8.4)') tset
c		response(1:) = 'accel: crossing temp is now ='// str8
c		CALL PUTCONTR(response)
c		WRITE (UNIT= str8, FMT= '(f8.4)') offset
c		response(1:) = 'with setpoint offset ='// str8
c		CALL PUTCONTR(response)
		setdeviation = temp - tset
		absetdev = ABS(setdeviation)
		maxdif = absetdev
		saveoffset = offset
		crossed = .FALSE.  ! marks crossing of current setpoint, tset
		waiting = .TRUE.   ! marks waiting for decel after cross
		approach = .FALSE. ! marks approach towards final destination
C deceleration wait loop:
		DO WHILE(.NOT. temp_OK .AND. .NOT. out_of_time .AND. waiting)
		  tempprev = tempnow   ! make previous temp equal to last read
		  call libwait(2.,status)
C		  CALL check_heater
	          info(1)(1:) = 'new_temp'
	          info(2)(1:) = 'new_temp'
	          info(3)(1:) = ' '
	          info(4)(1:) = ' '
	          WRITE (UNIT=info(3), FMT= '(a,f6.2,a,f6.2)')	      
     1	             'Waiting for temperature to reach setpoint! SET= ', 
     2                temp, ' NOW= ', tempnow
	          CALL Status_File (info)
		  IF (.NOT. timeyet(timeup))THEN
	             CALL Holds (holdtime) !at return: stop is clear if ctrl_a detected
	             IF (.NOT. READEF (stopf))GOTO 999 !RETURN immediately
	             CALL clref (stopf)   ! Normal completion
		  ELSE
		     out_of_time = .TRUE.
		  ENDIF
		  CALL RdTemp(Tsamp,tempnow,Tsetp) ! read temp
		  deltanow = secnds(timenow)
		  timenow = timenow + deltanow
		  deviation = temp - tempnow
		  absdev = ABS(deviation)
		  IF (absdev .GT. maxdif) maxdif = absdev ! track biggest dev
		  deltatime = deltatime + deltanow
C calculate current rate estimate and speed
		  IF (tempnow .NE. tempprev) THEN
		     deltatemp = tempnow - tempprev
		     rateprev = ratenow
		     deltatimep = deltatime
		     ratenow = deltatemp/deltatime
		     IF (ratenow*rateprev .LT. 0.) ratenow=(ratenow+rateprev)/2.
		     deltatime = 0.
		  ELSE
		     ratenow = deltatemp/(deltatimep + deltatime)
		  ENDIF
		  speednow = ABS(ratenow)
		  WRITE (UNIT= str8, FMT= '(f8.5)') ratenow
		  response(1:) = 'decel loop rate is now = '// str8
		  CALL PUTCONTR(response)
		  IF (absetdev .LT. 1.E-4) THEN
			devratio = 0.99
			targetrat = 1.
		  ELSE
			devratio = deviation/setdeviation
			targetrat = absdev/maxdif
		  ENDIF
C look for setpoint crossing, devratio < 1 while approaching
		  IF (.NOT. crossed .AND. approach .AND. devratio .LT. 1.) THEN
			crossed = .TRUE.
			ratecross = ratenow
			speedcross = speednow
c			WRITE (UNIT= str8, FMT= '(f8.5)') ratecross
c			response(1:) = 'rate crossing set-point = '// str8
c			CALL PUTCONTR(response)
		  ENDIF
C must cross setpoint
C look for slowing to ratetarget which is some fraction of crossing rate
		  IF (absdev .LE. ter) temp_OK=.TRUE.
		  IF (crossed) THEN
			ratetarget = ratecross*targetrat/cauttwo
			speedtarget = ABS(ratetarget)
c			WRITE (UNIT= str8, FMT= '(f8.5)') ratetarget
c			response(1:) = 'target rate now = '// str8
c			CALL PUTCONTR(response)
			IF (ratetarget .LT. 0.) THEN
				IF (ratenow .GT. ratetarget) waiting = .FALSE.
			ELSE
				IF (ratenow .LT. ratetarget) waiting = .FALSE.
			ENDIF
			IF (.NOT. waiting) THEN
			  IF (absdev .LE. ter) THEN
			   temp_OK=.TRUE.
c			   WRITE (UNIT= str8, FMT= '(f8.5)') absdev
c			   response(1:) = 'rate and terr OK = '// str8
c			   CALL PUTCONTR(response)
			  ENDIF
			  alpha = speedcross/(speedcross - speedtarget)
			  beta = tempadv/(alpha*(tempnow-tset)+tempadv)/cautone
c			  WRITE (UNIT= str8, FMT= '(f8.5)') beta
c			  response(1:) = 'finished wait with beta = '// str8
c			  CALL PUTCONTR(response)
			ENDIF
		  ENDIF

		  nowapp = .FALSE.
		  IF (ratenow .GT. 0. .AND. tempnow .LE. temp) nowapp = .TRUE.
		  IF (ratenow .LT. 0. .AND. tempnow .GE. temp) nowapp = .TRUE.
		  IF (approach .AND. .NOT. nowapp .AND. .NOT. crossed) THEN
			offset = setpt - tempnow
			IF (ABS(offset) .GT. maxoffset) offset = 0.
			setpt = tset + offset
			CALL WrTemp(setpt)
c			WRITE (UNIT= str8, FMT= '(f8.4)') offset
c			response(1:) = 'new offset = '// str8
c			CALL PUTCONTR(response)
c			WRITE (UNIT= str8, FMT= '(f8.4)') setpt
c			response(1:) = 'setpoint changed to = '// str8
c			CALL PUTCONTR(response)
		  ENDIF
		  approach = nowapp

		ENDDO ! end waiting to slow down

	     END DO !end main control loop when temp OK or out_of_time

	     IF (.NOT. temp_OK) offset = saveoffset
	     setpt = temp + offset
	     CALL WrTemp(setpt)

	RETURN

C  Return after control_a

 999	abortflag = .TRUE.
#endif
	RETURN
	END
