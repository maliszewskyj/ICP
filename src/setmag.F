#include "icp.h"
C note: if i want contr_a treated like an abort command, need to set aborted
c   and leave contr_a set. To just stop holds, do not leave set.
c  1 notes: this will not work under 1. See (among other things)
c    the calls to ext_to_int_time
c
c     1-5-01 DCD Changed persistent switch warmup time 10->15 seconds, and 
c     switch cooldown time 30->60seconds. Oxford 9T VFSCmag had quenched 
c     with shorter cooldown time at high (7T) fields.

      	SUBROUTINE SetMag (field,waitmax,aborted) 
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Set magnet to field specified   (in amps)                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use tc
	use magnmodel
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_hc.h"
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	REAL*4	     field
	LOGICAL	     aborted
	REAL*4	     waitmax
	REAL*4       tolerance
	CHARACTER*9   newstr, str9(3)
	CHARACTER*12   str12
	CHARACTER*20   STR20
	CHARACTER*80  command,reply,vcommand
	INTEGER*4     lcom,lreply,lvcom
     	INTEGER*2     hpdev/0/ !used in comm with HP mag controller
	CHARACTER*20  outstr
	INTEGER*4     lstr, ios,  st, iwait, dbg

	LOGICAL       READEF

	LOGICAL       errflg, firstloop, breakloop, timeyet, def_wait, gotthere
	INTEGER*4     pstatus, status, istepstatus, timeup(2), wait_int(2)
	INTEGER*4     ctimeup(2), cwait_int(2)
	REAL*4	      pshis, checktime, wtime, ratenew, neverup,c_out,voltout,rval
	REAL*4        psoffwait, psonwait
	REAL*4	      tsample /120./ !120 seconds
	REAL*4        maxtime !default max wait calculated by this routine
	REAL*4        mintime !min time to change field on charging calculated below
	integer*4     GETREPLY/1/,NOREPLY/0/
	LOGICAL       MAGSHUTDOWN
	REAL*4	      rate, Vcharge
	REAL*4        maxfieldlast
	EXTERNAL      timeyet, rate, Vcharge
	REAL*4  	Iouterr
	PARAMETER	(Iouterr = 0.1)

	aborted=.false.
	tolerance = 0.001
	IF (magmodel .eq. 2) THEN
	   call setmag_oxford(field,waitmax,aborted)
	   return
	ELSE IF (magmodel .eq. 3) THEN
	   call putcontr('You cannot set the field with a gaussmeter!')
	   RETURN
	ENDIF
	IF (magtype .eq. 1) THEN
	   call putcontr('Aux magnet not supported')
	   return
	ELSEIF (magtype .eq. 2) THEN !HP/PS
	   CALL Gpib_Listen(magport, errflg) ! set up controller to listen
	   write(unit=newstr(1:9),fmt='(f9.4)',err=98)field
	   outstr(1:)=';ISET '//newstr(1:9)//';'
	   lstr = 16
	   CALL WrPol(outstr, lstr, errflg,hpdev)
	   CALL Gpib_Unlisten(errflg)
	ELSEIF (magtype .eq. 6) THEN ! 12T magnet
	   CALL RdField (ratenew)
	   if (abs(field - ratenew) .lt. tolerance) return ! no need to set
	   command(1:19) = 'set field setpoint '
	   write(unit=command(20:28),fmt='(f9.4)',err=98) field
	   lcom=28
	   call rs232tc_lv(command,lcom,reply,lreply,0)

	   call lv_ready_f	! See if we're finished
	ELSEIF (magtype .eq. 7) THEN ! Electromagnet
! Just go ahead and set the new value
	   voltout = field
	   CALL ADAM_Setvolt(voltout,status)
	ELSEIF (magtype .eq. 3 .OR. magtype .eq. 4 .OR. magtype .eq. 5) THEN !Scmag without or with PSH
C       RWE Jan 99  if want to do nothing if field == fieldlast use following line
	   IF (field .EQ. fieldlast)THEN
	      IF (magtype .eq. 4 .AND. hfmode .eq. 1)THEN
		 command(1:)='PSH?' !get persistent switch heater status
		 lcom = 4
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF( magshutdown )THEN
		    CALL Putcontr(' Error reading in psh.') !Debug
		    RETURN
		 ENDIF
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)pstatus
		 IF (pstatus .EQ. 0) RETURN
		 command(1:)='PSH0'
		 lcom = 4
		 vcommand(1:)='PSH?'
		 lvcom = 4
		 CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error turning psh off.') !Debug
		    RETURN
		 ENDIF
		 wtime = 60.
C       dan dender opts for longer cooldown time. 1-5-01
		 CALL Holds(wtime)
		 IF (READEF (stop))THEN !Holds aborted 
		    CALL CLREF(contr_A) 
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)
		 ENDIF
		 str9(3)(1:)=' '
		 WRITE(UNIT=str9(3),FMT='(F9.4)',iostat=ios)field
		 IF (istepstatus .NE. 0)THEN ! use ramp to control current step rate
		    command(1:)='RAMP1,'//str9(3)//',0,8.'
		    lcom = 20
		    vcommand='RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting ramp.') !Debug
		       RETURN
		    ENDIF
		    wtime =3.
		    CALL Holds(wtime)
		    command(1:)='RMP11' 
		    lcom = 5
		    vcommand(1:) = 'RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,1.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error initiating ramp.') !Debug
		       RETURN
		    ENDIF
		 ELSE		! just set current
		    IF (.not. (tinit_done .and. temptype .eq. 13)
	1		 .and. .not. (magtype .eq. 5)   ) THEN !unless lia
C       June 99 change voltage to discharge leads to fixed 1 V
C       voltout = Vcharge(c_out)
		       voltout = 1.5
		       str9(1)(1:)=' '
		       WRITE(UNIT=str9(1),FMT='(F9.4)',iostat=ios)voltout
		       command(1:)='VSET '//str9(1)
		       lcom = 14
		       vcommand(1:) = 'VSET?'
		       lvcom = 5
		       CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,voltout,magshutdown)
		       IF (magshutdown )THEN
			  CALL Putcontr(' Error setting VSET.') !Debug
			  RETURN
		       ENDIF
		       CALL Holds(1.)
		    ENDIF
		    command(1:)='ISET 0' !Start ramp 
		    lcom = 14
		    vcommand(1:) = 'ISET?'
		    lvcom = 5
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting ISET.') !Debug
		       RETURN
		    ENDIF
		 ENDIF

		 CALL holds(1.)
		 wtime = ABS(fieldlast)/8. + 5.
c       IF (ios .NE. 0)WHAT ROSS?
		 CALL Holds(wtime)
		 command(1:)='IOUT?' !Read back current 
		 lcom = 5
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error reading IOUT.') !Debug
		    RETURN
		 ENDIF
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)rval
		 IF (abs(rval) .GT. 0.3)THEN ! not zero yet SO magnet discharges on lowering current
		    CALL Shut_mag
		    CALL Putcontr(' Magnet discharged on lowering current in PS mode forced SHUTDOWN !')
		    CALL Putcontr(' Verify true status of magnet.')
		    CALL Putcontr(' Magnet device will have to be reset using hdev.')
		    magnctr = .FALSE.     
		    RETURN		
		 ENDIF
		 IF (READEF (stop) )THEN !Holds aborted 
		    CALL CLREF(contr_A)
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF(stop)
		 ENDIF
		 RETURN
	      ENDIF
	   ENDIF

C       RWE Jan 99  cck if utep liit

C       Turn off ISTPS because of bugs in Lakeshore 622 implementation
	   IF( magmodel .EQ. 1 )THEN ! Lakeshore 622
	      command(1:)='ISTPS0' !status of output current step limit
	      lcom = 6
	      vcommand(1:) = 'ISTPS?'
	      lvcom = 6
	      CALL sc232(command,lcom,vcommand,lvcom,NOREPLY)
	   ENDIF

C       command(1:)='ISTPS?' !get istep status
C       lcom = 6
C       CALL vsc232(command,lcom,reply,lreply,GETREPLY,0,magshutdown)
C       IF (magshutdown )THEN
C       CALL Putcontr(' Error reading ISTPS.') !Debug
C       RETURN
C       ENDIF
C       READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)istepstatus
C       
C!!!!!!!!Fake out reply. Just because Dan's watching.
	   istepstatus=0
	   IF (abs(field).GT. fieldmax)THEN
	      IF (field.LT.0.)THEN
		 field = -fieldmax
	      ELSE
		 field = fieldmax
	      ENDIF
	      str9(2)(1:)=' '
	      WRITE(UNIT=str9(2),FMT='(F9.4)',iostat=ios)fieldmax
	      CALL Putcontr(' Field exceeded range so set to limit '//str9(2)(1:9)//'amps')
	   ENDIF
	   IF (magtype .eq. 4)THEN !SCmag with persistent only = PERSISTENT MODE code
	      command(1:)='PSH?' !get persistent switch heater status
	      lcom = 4
	      CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
	      IF (magshutdown )THEN
		 CALL Putcontr(' Error reading PSH.') !Debug
		 RETURN
	      ENDIF
	      READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)pstatus
	      IF (pstatus .EQ. 0)THEN
		 command(1:)='PSHIS?' !get Current setting when PSH was turned off
		 lcom = 6
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error reading PSHIS.') !Debug
		    RETURN
		 ENDIF
C       pshis = fieldlast !default if error reading or decoding
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)pshis

C       maxfieldlast = max(abs(pshis),abs(fieldlast))
C       IF ( maxfieldlast .GT. 0.5 )THEN
C       IF ( abs(pshis - fieldlast)/(max(abs(pshis),abs(fieldlast))
C       1			     +0.02) .GT. .05)THEN
C       CALL Putcontr(' Using software value of last known magnet current!')
C       pshis = fieldlast
C       ENDIF
C       ENDIF

		 str9(1)(1:)=' '
		 WRITE(UNIT=str9(1),FMT='(F9.4)',IOSTAT=ios)pshis
		 IF (ios .NE. 0)THEN !Fatal error. Do what, ROSS??
		    CALL Putcontr('Cannot read last known current in magnet PS mode ! SHUTTING DOWN!')
		    CALL Shut_mag
		    magnctr = .FALSE.     
		    RETURN
		 ENDIF
		 command(1:)='IOUT?' !Read back current 
		 lcom = 5
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error reading IOUT.') !Debug
		    RETURN
		 ENDIF
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)rval
		 str9(2)(1:)=' '
		 WRITE(UNIT=str9(2),FMT='(F9.4)',IOSTAT=ios)rval

C       RWE Jan 99  if istepstatus then ramp else set current
		 IF (istepstatus .NE. 0)THEN ! use ramp to control current step rate
		    command(1:)='RAMP1,'//str9(2)//','//str9(1)//',8.'
		    lcom = 28
		    vcommand(1:) = 'RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting RAMP1.') !Debug
		       RETURN
		    ENDIF
		    command(1:)='RMP11'
		    lcom = 5
		    vcommand(1:) = 'RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,1.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error starting ramp.') !Debug
		       RETURN
		    ENDIF

		 ELSE		! just set current
		    IF (.not. (tinit_done .and. temptype .eq. 13) .and. .not. (magtype .eq. 5)) THEN !unless lia
C       voltout = Vcharge(c_out)
C       June 99 change lead charging with no steplimit to 1 V
C       Nov 2000 go to 1.5V
		       voltout = 1.5
		       str9(2)(1:)=' '
		       WRITE(UNIT=str9(2),FMT='(F9.4)',iostat=ios)voltout
		       command(1:)='VSET '//str9(2)
		       lcom = 14
		       vcommand(1:) = 'VSET?'
		       lvcom = 5
		       CALL sc232(command,lcom,vcommand,lvcom,NOREPLY)
		       CALL holds(1.)
		    ENDIF
		    command(1:)='ISET '//str9(1) !Start ramp 
		    lcom = 14
		    vcommand(1:) = 'ISET?'
		    lvcom = 5
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,pshis,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting ISET.') !Debug
		       RETURN
		    ENDIF

		 ENDIF
		 CALL holds(1.)
		 wtime = ABS(pshis-rval)/4. + 10
		 CALL Holds(wtime)
		 command(1:)='IOUT?' !Read back current 
		 lcom = 5
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error reading IOUT.') !Debug
		    RETURN
		 ENDIF
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)rval
		 IF (READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF
		 command(1:)='PSH11' !turn on switch heater
		 pstatus = 1
		 lcom = 5
		 vcommand = 'PSH?'
		 lvcom = 4
		 CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,1.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error turning on PSH.') !Debug
		    RETURN
		 ENDIF
		 
		 wtime = 15.	!wait for switch to go normal
c       dan dender opts for longer warmup time. 1-5-01
		 CALL Holds(wtime)
		 IF (READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF
	      ENDIF		!PSH = 0
	   ENDIF		!SCmag with persistent swith only
	   command(1:)='ERR?'	!check 
	   lcom = 4
	   CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
	   IF (magshutdown )THEN
	      CALL Putcontr(' Error reading back status of ERR.') !Debug
	      RETURN
	   ENDIF
	   IF (lreply .EQ. 3)THEN !got reply of valid form
	      IF (reply(1:lreply) .NE. '000')THEN
		 CALL Shut_mag	! set IOUT to zero
		 CALL Putcontr(' Magnet Error forced SHUTDOWN !: '//reply(1:lreply))
		 CALL Putcontr(' Verify true status of magnet.')
		 CALL Putcontr(' Magnet device will have to be reset using hdev.')
		 magnctr = .FALSE.     
		 RETURN
	      ENDIF
	   ENDIF
	   IF (waitmax .GT. 0 .AND. hfmode .EQ. 0)THEN !decide how long to allow loop to continue
	      iwait = NINT(waitmax)
	      CALL ext_to_int_time(wait_int,iwait,status) !user specified time
	      def_wait = .FALSE.
	   ELSEIF (waitmax .LT. 0 .AND. hfmode .EQ. 0)THEN !do not execute loop at all
	      wait_int(1) = 0
	      wait_int(2) = 0
	      def_wait = .FALSE.
	   ELSEIF (waitmax .EQ. 0)THEN !just let loop determine exit
	      def_wait = .TRUE.
	   ELSE
	      def_wait = .TRUE.
	   ENDIF
	   firstloop = .TRUE.
	   breakloop = .FALSE.
	   gotthere = .FALSE.
	   checktime = 3.
	   c_out = fieldlast
C       New magnet charging Loop Dec 1996 RWErwin using ISET and VSET :
	   DO WHILE (.NOT. breakloop)
	      fieldlast = c_out
	      command(1:)='IOUT?' !Read back current 
	      lcom = 5
	      CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
	      READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)c_out
	      IF (magshutdown )THEN
		 c_out = fieldlast
		 CALL Putcontr(' Error reading back IOUT.') !Debug
		 RETURN
	      ENDIF

	      IF (.not. (tinit_done .and. temptype .eq. 13)
	1	   .and. .not. (magtype .eq. 5)   ) THEN !unless lia
		 voltout = Vcharge(c_out)
		 str9(1)(1:)=' '
		 WRITE(UNIT=str9(1),FMT='(F9.4)',iostat=ios)voltout
		 command(1:)='VSET '//str9(1)
		 lcom = 14
		 vcommand(1:) = 'VSET?'
		 lvcom = 5
		 CALL sc232(command,lcom,vcommand,lvcom,NOREPLY)
		 WRITE(UNIT=str9(2),FMT='(F9.4)',iostat=ios)field
		 command(1:)='ISET '//str9(2) !Start ISET voltage controlled ramp
		 lcom = 14
		 vcommand(1:) = 'ISET?'
		 lvcom = 5
		 CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,field,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error setting ISET.') !Debug
		    RETURN
		 ENDIF
		 CALL holds(1.)
	      ENDIF
	      IF (firstloop) THEN ! set target current and max wait
		 IF (def_wait)THEN
		    maxtime = 100. * abs(field-c_out)
		    iwait = NINT(maxtime)
		    CALL ext_to_int_time(wait_int,iwait,status) !use def max time
		 ENDIF
		 CALL Sched_Action(timeup,wait_int)
		 mintime = abs(field-c_out)/2.
		 iwait = NINT(mintime)
		 CALL ext_to_int_time(cwait_int,iwait,status)
		 CALL Sched_Action(ctimeup,cwait_int)
	      ENDIF
	      IF (timeyet(timeup))breakloop = .TRUE.
C       write(unit=outstr,fmt=*)firstloop
C       call putcontr(' firstloop: '//outstr)
	      firstloop = .FALSE.
C       changed Jan 99 RW Erwin  to not force a second loop if within tolerance AND check for charging
	      IF (abs(field - c_out) .LT. Iouterr)THEN
		 gotthere = .TRUE.
		 breakloop = .TRUE.
C       6/24/99 DCD added 5 second minimum time for charging time check
		 IF ((iwait .GE. 5) .AND. (.NOT. timeyet(ctimeup)))THEN !magnet NOTcharging,cur too fast
c       IF (.NOT. timeyet(ctimeup))THEN  ! magnet NOT charging, since current went up too fast
		    CALL Shut_mag ! set IOUT to zero
		    CALL Putcontr(' Magnet Error: SHUTDOWN because NOT charging	!')
		    CALL Putcontr(' Verify true status of magnet.')
		    CALL Putcontr(' Magnet device will have to be reset using hdev.')
		    magnctr = .FALSE.     
		    RETURN
		 ENDIF
	      ENDIF
	      IF (.NOT. breakloop)THEN
		 CALL Holds(checktime)
		 IF (READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF
	      ENDIF
	   ENDDO		!until breakloop
C       END OF ROSS LOOP
	   IF (gotthere)THEN
	      fieldlast = field	
	   ELSE
	      command(1:)='IOUT?' !Read back current 
	      lcom = 5
	      CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
	      IF (magshutdown )THEN
		 CALL Putcontr(' Error reading IOUT.') !Debug
		 RETURN
	      ENDIF
	      IF (lreply .NE. 0)THEN
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)rval
		 IF (ios .NE. 0) THEN
		    fieldlast=c_out !from up above in Ross loop
		 ELSE
		    fieldlast=rval
		 ENDIF
	      ENDIF
	      IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		 fieldlast=c_out !from up above in Ross loop
		 CALL Shut_mag
		 CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		 CALL Putcontr(' Verify true status of magnet.')
		 CALL Putcontr(' Magnet device will have to be reset using hdev.')
		 magnctr = .FALSE.     
		 RETURN		 
	      ENDIF
	   ENDIF
	   command(1:)='ERR?'	!check 
	   lcom = 4
	   CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
	   IF (magshutdown )THEN
	      CALL Putcontr(' Error reading back status of ERR.') !Debug
	      RETURN
	   ENDIF
	   IF (lreply .EQ. 3)THEN !got reply of valid form
	      IF (reply(1:lreply) .NE. '000')THEN
		 CALL Shut_mag
		 CALL Putcontr(' Magnet Err: '//reply(1:lreply))
		 CALL Putcontr(' Verify true status of magnet.')
		 CALL Putcontr(' Magnet device will have to be reset using hdev.')
		 magnctr = .FALSE.     
		 RETURN
	      ENDIF
	   ENDIF
C       write(unit=outstr,fmt=*) hfmode
C       CALL Putcontr('after set loop hfmode= '//outstr)
C       write(unit=outstr,fmt=*) magtype
C       CALL Putcontr('and magtype= '//outstr)
	   IF (magtype .eq. 4) THEN !SCmag with persistent switch only
	      IF (hfmode .EQ. 1)THEN !persistent mode
C       RAMP DOWN CURRENT LEADS
		 command(1:)='PSH0'
		 lcom = 4
		 vcommand(1:) = 'PSH?'
		 lvcom = 4
		 CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error turning off PSH.') !Debug
		    RETURN
		 ENDIF
		 wtime = 60.
C       dan dender opts for longer cooldown time. 1-5-01
		 CALL Holds(wtime)
		 IF (READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF

		 str9(3)(1:)=' '
		 WRITE(UNIT=str9(3),FMT='(F9.4)',iostat=ios)field
		 IF (istepstatus .NE. 0)THEN ! use ramp to control current step rate
		    command(1:)='RAMP1,'//str9(3)//',0,8.'
		    lcom = 20
		    vcommand(1:) = 'RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting up RAMP1.') !Debug
		       RETURN
		    ENDIF
		    wtime =3.
		    CALL Holds(wtime)
		    command(1:)='RMP11' 
		    lcom = 5
		    vcommand(1:) = 'RMP?'
		    lvcom = 4
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,1.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error initiating ramp.') !Debug
		       RETURN
		    ENDIF
		 ELSE		! just set current
		    IF (.not. (tinit_done .and. temptype .eq. 13)
	1		 .and. .not. (magtype .eq. 5)   ) THEN !unless lia
C       June 99 change voltage to discharge leads to fixed 1 V
C       voltout = Vcharge(c_out)
		       voltout = 1.5
		       str9(1)(1:)=' '
		       WRITE(UNIT=str9(1),FMT='(F9.4)',iostat=ios)voltout
		       command(1:)='VSET '//str9(1)
		       lcom = 14
		       vcommand(1:) = 'VSET?'
		       lvcom = 5
		       CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,voltout,magshutdown)
		       IF (magshutdown )THEN
			  CALL Putcontr(' Error setting VSET.') !Debug
			  RETURN
		       ENDIF
		       CALL Holds(1.)
		    ENDIF
		    command(1:)='ISET 0' !Start ramp 
		    lcom = 14
		    vcommand(1:) = 'ISET?'
		    lvcom = 5
		    CALL vsc232(command,lcom,vcommand,lvcom,NOREPLY,0.0,magshutdown)
		    IF (magshutdown )THEN
		       CALL Putcontr(' Error setting ISET.') !Debug
		       RETURN
		    ENDIF
		 ENDIF
		 CALL holds(1.)
		 wtime = ABS(fieldlast)/8. + 5.
c       IF (ios .NE. 0)WHAT ROSS?
		 CALL Holds(wtime)
		 command(1:)='IOUT?' !Read back current 
		 lcom = 5
		 CALL vsc232(command,lcom,reply,lreply,GETREPLY,0.0,magshutdown)
		 IF (magshutdown )THEN
		    CALL Putcontr(' Error reading IOUT.') !Debug
		    RETURN
		 ENDIF
		 READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)rval
		 IF (abs(rval) .GT. 0.3)THEN ! not zero yet SO magnet discharges on lowering current
		    CALL Shut_mag
		    CALL Putcontr(' Magnet discharged on lowering current in PS mode forced SHUTDOWN !')
		    CALL Putcontr(' Verify true status of magnet.')
		    CALL Putcontr(' Magnet device will have to be reset using hdev.')
		    magnctr = .FALSE.     
		    RETURN		
		 ENDIF
		 IF (READEF (stop) )THEN !Holds aborted 
		    CALL CLREF(contr_A)
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)
		 ENDIF
	      ENDIF		!Persistent mode
	   ENDIF		!SCmag with persistent switch
	ENDIF			!choice on magtype
	IF (minit_done .and. nsta.eq.-5) CALL CANCELFIELD
 98	RETURN
	END


	REAL*4 FUNCTION Rate(i)
	use tc
	Implicit none
	Real*4     Rate0/.085/, RateImax/.015/, i
	Integer*4  Imax/80/
	Rate = Rate0 + (RateImax - Rate0)*(abs(i)/Imax)
	RETURN
	END
c       charging foltage always positive while sign of Iout determines polarity

	REAL*4 FUNCTION Vcharge(i)
	Implicit none
	Real*4     i
	REAL*4  	Vc0,VIratio
	PARAMETER	(Vc0 = 4.5)
	PARAMETER	(VIratio = 0.0258) ! to reach 124 amps with 1.3 Volts
	Vcharge = Vc0 - VIratio*abs(i)
	RETURN
	END
