#include "icp.h"
	SUBROUTINE New_Temp_Field (temp,t0, it, tw, ter, th0, th, 
     1	  hf, loop, ihf, hfhow,hfwait)

C-----------------------------------------------------------------------------C
C                                                                             C
C   Routine Calculates The New Temperature based on the given parameters,     C
C    sets controllers, and returns it in temp.                                C
C   Routine calculates and sets magnetic field based on hf (initial),         C
C     and ihf(increment). Value to which field is set is returned in field.   C
C   Routine delays return until temperature is within error or timeout.       C
C   Before returning, routine executes any requested holds.                   C
C   Called from: Run_Q_Buf, Run_I_Buf, Run_T_Buf, Run_P_Buf, Run_R_Buf        C
C                                                                             C
C   Gayle Greene: March, 1994                                                 C
C     Based on Beat Schmid earlier New_Temp routine                         C
C-----------------------------------------------------------------------------C
	use instr
	use tc
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface
	INTEGER*4     loop
	REAL*4        temp, t0, it, tw, ter, th, hf, hfhow,hfwait
	REAL*4        st, ihf, dummy1, dummy2, th0
	CHARACTER*8   str8
	CHARACTER*80  response, info(4)
 	LOGICAL       temp_chg, dowait, Readef
C       Variables used to pass info to Ross careful_approach
 	REAL*4	        temp_local,
	1    ter_local		!local vaule of ter to pass
	INTEGER*4	stop_local !used to pass stop evflag number

C       Misc variables not used for xray
	INTEGER*4     status
	INTEGER*4     temp_int(2),timeup(2),itw
	REAL*4        field, afield, deviation
 	LOGICAL       field_chg, temp_OK, aborted, timeyet,out_of_time
C       Variables used to get info from Ross careful_approach
	LOGICAL		abortflag !used if control_a detected there
C       End of variables not used for xray

	stop_local = stop	!will serve as local var with stop evflag #
	temp_local = temp
	ter_local = ter
	info(3)(1:) = ' '
	info(4)(1:) = ' '

C       !--------------------- SET TEMPERATURE -------------------------------C

	IF ((tempctr .AND. t0 .NE. 0.) .AND. !temp = 0 is special flag for no change
     1	     (loop .eq. 0 .or. it .ne. 0.)) THEN !only write if new or changed
   	   temp = t0  + loop*it	!calculate new temp
	   IF (.NOT. tcareful .OR. tw .EQ.0) CALL WrTemp (temp)
	   call libwait(0.5,status)
	   temp_chg = .TRUE.
	ELSE
	   temp_chg = .FALSE.
	ENDIF

C       !--------- SET UP TIME TO STOP WAITING ON TEMP -----------C
	IF (temp_chg)THEN
	   itw = NINT(tw*60.)	!convert to seconds for conversion to int_time
	   IF (itw .EQ. 0)THEN
	      temp_int(1) = 0
	      temp_int(2) = 0
	   ELSE
	      CALL ext_to_int_time(temp_int,itw,status)
	   ENDIF
	   CALL Sched_Action(timeup,temp_int)
	ENDIF			!temp_chg

C       !--------- SET MAGNETIC FIELD AND WAIT UNTIL DONE --------C

	IF (magnctr.and.minit_done.and.
	1    (loop .eq. 0 .or. ihf .ne. 0.)) THEN !only write if new or changed
	   field = hf + loop*ihf !calculate new field
	   IF (magtype.eq.1 .and. tdev%mot .LT. 0) THEN
	      afield = -field	! (* reverse direction *)
	   ELSE
	      afield = field
	   ENDIF
	   afield = afield / hfconv !convert field to amps
	   CALL setmag(afield,hfwait,aborted) !return when query indicates set
	   IF (aborted)GOTO 999
	   field_chg = .TRUE.
	   IF (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5)
     1	        CALL upd_helium !tell magnet controller to be checking helium level
	ELSE
	   field_chg = .FALSE.
	END IF


C       !------------- WAIT UNTIL WITHIN ERROR OR TIMEOUT FOR TEMP----------C

	IF (temp_chg .AND. tw .NE. 0.) THEN !Wait for temp to be within error
	   temp_OK = .FALSE.
	   out_of_time = .FALSE.
	   IF (.NOT. tcareful) THEN !normal procedure
	      DO WHILE (.NOT. temp_OK .AND. .NOT. out_of_time) !repeat until max wait time exceeded or temp at target
		 IF (which_temp.eq.2) THEN
		    CALL RdTemp (dummy1, st, dummy2)
		 ELSEIF (which_temp.eq.3) THEN !setpoint
		    CALL RdTemp (dummy1, dummy2, st)
		 ELSE		!sample
		    CALL RdTemp (st, dummy1, dummy2)
		 ENDIF
		 deviation = ABS (temp - st)	
		 IF (deviation .LT. ter) THEN !temp at target
		    temp_OK = .TRUE.
		 ELSE
		    call libwait(.5,status)
		    CALL check_heater
		    info(1)(1:) = 'new_temp'
		    info(2)(1:) = 'new_temp'
		    WRITE (UNIT=info(3), FMT= '(a,f6.2,a,f6.2)')	      
	1		 'Waiting for temperature to reach setpoint! SET= ', 
	2		 temp, ' NOW= ', st
		    CALL Status_File (info)
		    IF (.NOT. timeyet(timeup))THEN
		       CALL Hold (1.) !at return: stop is clear if ctrl_a detected
		       IF (.not. READEF (stop)) GOTO 999 !RETURN immediately
		       CALL clref (stop) ! Normal completion
		    ELSE
		       out_of_time = .TRUE.
		    ENDIF
		 ENDIF		!temp at target
	      END DO	   	   
	   ELSE			!tcareful is set. Use Ross procedure	   	   
	   CALL Careful_Approach(temp_local,ter_local,timeup,info,
     1		temp_ok,abortflag,stop_local)
	   IF (abortflag) GOTO 999
	ENDIF			!tcareful
	
	IF (.NOT. temp_OK)THEN	!try once more
	   call libwait(.5,status) !do not communicate too soon after last

	   IF (which_temp.eq.2)THEN !control
	      CALL RdTemp (dummy1, st, dummy2)

	   ELSEIF (which_temp.eq.3) THEN !setpoint
	      CALL RdTemp (dummy1, dummy2, st)
	   ELSE			!sample
	      CALL RdTemp (st, dummy1, dummy2)
	   ENDIF
	   deviation = ABS (temp - st)	
	   IF (deviation .LT. ter)temp_OK = .TRUE.
	ENDIF
	info(1)(1:) = 'new_temp'
	info(2)(1:) = 'new_temp'
	IF (.not. temp_OK)THEN
	   WRITE (UNIT= str8, FMT= '(f8.3)') st
	   response(1:) = 'Max wait time reached ===> continue: Temp = '// str8
c       CALL PUTCONTR (' ')
	   CALL PUTCONTR(response)
	   info(3) = response
	   CALL Status_File (info)
	ELSE
	   WRITE (UNIT=info(3), FMT= '(a,f6.2,a,f6.2)')	      
	1	'Temperature at setpoint! SET= ', 
	2	temp, ' NOW= ', st
	   CALL Status_File (info)
	ENDIF
	ENDIF			!t0 .ne. 0 !special flag for no temperature action


C       ---------------------- WAIT IF EITHER WAIT FLAG SET --------------------C

	dowait = .FALSE.	!set to no waits requested by default
	IF (loop .EQ. 0 .AND. th0 .NE. 0) THEN !hold executed first point only
	   CALL Hold (th0)	!at return, stop is clear if ctrl_a detected
	   IF (.not. READEF (stop)) GOTO 999 !RETURN immediately
	   CALL clref (stop)
	   dowait = .TRUE.
	ENDIF
	IF (th .NE. 0) THEN	!hold executed at all points
	   CALL Hold (th)	!at return, stop is clear if ctrl_a detected
	   IF (.not. READEF (stop)) GOTO 999 !RETURN immediately
	   CALL clref (stop)
	   dowait = .TRUE.
	ENDIF

	IF (temp_chg .AND. dowait)THEN
	   IF (which_temp.eq.2)THEN !control
	      CALL RdTemp (dummy1, st, dummy2)
	   ELSEIF (which_temp.eq.3) THEN !setpoint
	      CALL RdTemp (dummy1, dummy2, st)
	   ELSE			!sample
	      CALL RdTemp (st, dummy1, dummy2)
	   ENDIF
	   WRITE (UNIT= str8, FMT= '(f8.3)') st
	   response = 'After requested wait: control temp = '// str8
c       CALL PUTCONTR ( ' ')
	   CALL PUTCONTR (response)
	ENDIF

C       Jump here to Return after a control_a	  	
 999	RETURN
	END

