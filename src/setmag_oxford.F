#include "icp.h"

C note: if i want contr_a treated like an abort command, need to set aborted
c   and leave contr_a set. To just stop holds, do not leave set.
c  1 notes: this will not work under 1. See (among other things)
c    the calls to ext_to_int_time

      	SUBROUTINE SetMag_Oxford (field,waitmax,aborted) 
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Set magnet to field specified   (in amps)                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use instr
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_hc.h"
#include "c_libwait.h"
	end interface
	REAL*4	     field
	LOGICAL	     aborted
	REAL*4	     waitmax
	CHARACTER*9   newstr, str9(3)
	CHARACTER*12   str12
	CHARACTER*20   STR20
	CHARACTER*80  command,reply
	INTEGER*4     lcom,lreply
	INTEGER*4     hpdev/0/ !used in comm with HP mag controller
	CHARACTER*20  outstr
	INTEGER*4     lstr, ios,  st, iwait
	LOGICAL       READEF   
	LOGICAL       errflg, firstloop, breakloop, timeyet, def_wait, gotthere, sploop
	INTEGER*4     pstatus, status, istepstatus, timeup(2), wait_int(2)
	INTEGER*4     ctimeup(2), cwait_int(2)
	REAL*4	      pshis, checktime, wtime, ratenew, neverup,c_out,voltout,rval
	REAL*4        psoffwait, psonwait, rlast
	REAL*4	      tsample /120./ !120 seconds
	REAL*4        maxtime	!default max wait calculated by this routine
	REAL*4        mintime	!min time to change field on charging calculated below
	INTEGER*4     GETREPLY/1/,NOREPLY/0/
	REAL*4	      rate, Vcharge
	REAL*4        maxfieldlast
	EXTERNAL      timeyet, rate, Vcharge
	REAL*4  	Iouterr
	PARAMETER	(Iouterr = 0.1)

	aborted=.false.
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
	ELSEIF (magtype .eq. 3 .OR. magtype .eq. 4 .OR. magtype .eq. 5) THEN !Scmag without or with PSH
C       RWE Jan 99  if want to do nothing if field == fieldlast use following line
	   IF (field .EQ. fieldlast)THEN

C       For SCmag with persistent switch, in persistent mode:
C       If switch is off, return.
C       if switch is on, turn it off & ramp down leads.    
C       For anything else, just return.

	      IF (magtype .eq. 4 .AND. hfmode .EQ. 1)THEN 
		 command(1:)='X' !get persistent switch heater status
		 lcom = 1
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 pstatus = 0	!default if error reading or decoding 0 = OFF
		 IF (lreply .NE. 0) READ(UNIT=reply(9:9),FMT=*,IOSTAT=ios)pstatus
		 IF( lreply .EQ. 0 .OR. ios .NE. 0)THEN
		    CALL Putcontr(' Error reading in psh. Magnet will be shutdown !') !Debug
		    CALL Shut_mag
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    magnctr = .FALSE.     
		    RETURN		
		 ENDIF
		 IF (pstatus .EQ. 5)THEN ! switch heater fault
		    CALL Putcontr('Controller detects fault in switch heater')
		    CALL Shut_mag
		    CALL Putcontr('Magnet HEATER error forced SHUTDOWN !')
		    magnctr = .FALSE.
		    RETURN
		 ELSE IF (pstatus .EQ. 8)THEN ! no switch fitted
		    CALL Putcontr('Controller indicates switch heater is not fitted')
		    CALL Shut_mag
		    CALL Putcontr('No magnet HEATER present forced SHUTDOWN !')
		    magnctr = .FALSE.
		    RETURN
		 ELSE IF (pstatus .EQ. 0 .OR. pstatus .EQ. 2)THEN !switch is off
		    RETURN
		 ELSE		! switch is on, turn off & ramp down leads
		    command(1:)='H0'
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    wtime = 30.
C       dan dender opts for longer cooldown time. 6-9-99
		    CALL Holds(wtime)
		    IF (.not. READEF (stop)) THEN !Holds aborted 
		       CALL CLREF(contr_A) 
		    ELSE	!normal termination of Holds routine. No control_a
		       CALL CLREF (stop)
		    ENDIF

		    command(1:)='A2' !Go to zero current in leads
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    wtime = ABS(fieldlast)/8. + 5.
c       IF (ios .NE. 0)WHAT ROSS?
		    CALL Holds(wtime)
		    command(1:)='R0' !Read back current 
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
		    IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		       CALL Shut_mag
		       CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		       magnctr = .FALSE.     
		       RETURN		 
		    ENDIF
		    IF (abs(rval) .GT. 0.3)THEN ! not zero yet SO magnet discharges on lowering current
		       CALL Shut_mag
		       CALL Putcontr(' Magnet discharged on lowering current in PS mode forced SHUTDOWN	!')
		       magnctr = .FALSE.     
		       RETURN		
		    ENDIF
		    command(1:)='A0' !Go to hold mode
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    IF (.not. READEF (stop))THEN !Holds aborted 
		       CALL CLREF(contr_A)
		    ELSE	!normal termination of Holds routine. No control_a
		       CALL CLREF (stop)
		    ENDIF
		 ENDIF		!pstatus 
	      ENDIF		!magtype eq 4 and hfmode eq 1
	      RETURN
	   ENDIF		! field eq fieldlast

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
	      command(1:)='X'	!get persistent switch heater status
	      lcom = 1
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      pstatus = 0	!default if error reading or decoding 0 = OFF
	      IF (lreply .NE. 0) READ(UNIT=reply(9:9),FMT=*,IOSTAT=ios)pstatus
	      IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
		 CALL Putcontr(' Error reading in psh. Magnet will be shutdown !') !Debug
		 CALL Shut_mag
		 CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		 magnctr = .FALSE.     
		 RETURN		
	      ENDIF
	      IF (pstatus .EQ. 5)THEN ! switch heater fault
		 CALL Putcontr('Controller detects fault in switch heater')
		 CALL Shut_mag
		 CALL Putcontr('Magnet control disabled	!')
		 magnctr = .FALSE.
		 RETURN
	      ELSE IF (pstatus .EQ. 8)THEN ! no switch fitted
		 CALL Putcontr('Controller indicates switch heater is not fitted')
		 CALL Shut_mag
		 CALL Putcontr('Magnet control disabled	!')
		 magnctr = .FALSE.
		 RETURN
	      ELSE IF (pstatus .EQ. 0 .OR. pstatus .EQ. 2)THEN
		 command(1:)='R16' !get Current setting when PSH was turned off
		 lcom = 3
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 pshis = fieldlast !default if error reading or decoding
		 IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)pshis
		 IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
		    CALL Putcontr(' Error reading in pshis. Magnet will be shutdown !') !Debug
		    CALL Shut_mag
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    magnctr = .FALSE.     
		    RETURN		 
		 ENDIF

		 str9(1)(1:)=' '
		 WRITE(UNIT=str9(1),FMT='(F9.4)',IOSTAT=ios)pshis
		 IF (ios .NE. 0)THEN !Fatal error. Do what, ROSS??
		    CALL Putcontr('Cannot read last known current in magnet PS mode ! SHUTTING DOWN!')
		    CALL Shut_mag
		    magnctr = .FALSE.     
		    RETURN
		 ENDIF
		 command(1:)='R0' !Read back current 
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 rval =0.
		 IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
		 IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		    CALL Shut_mag
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    magnctr = .FALSE.     
		    RETURN		 
		 ENDIF

		 write(unit=str20,fmt=*)fieldlast
		 call putcontr(' using fieldlast as '//str20)
C       

C       set sweep rate at max rate 10A/min, use sweep limiting of supply
C       set target current to last known current in magnet
C       initiate sweep
C<<<<   NCM Changed Max rate per Sung Chang 1/21/10
C       command(1:)='S1.20'
		 command(1:)='S10.0'
		 lcom = 5
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 command(1:)='I'//str9(1) !set target current
		 lcom = 10
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
 		 command(1:)='A1'
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
                 CALL libwait(0.1,status)
C       This wtime should depend on 'switch off' sweep rate.
		 wtime = ABS(pshis-rval)/4. + 10
		 write(unit=str20,fmt=*)wtime
		 call putcontr(' about to hold for '//str20//' secs')
		 CALL Holds(wtime)
		 command(1:)='R0' !Read back current 
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 rval =0.
		 IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
		 IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		    CALL Shut_mag
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    magnctr = .FALSE.     
		    RETURN		 		 
		 ENDIF


		 command(1:)='A0' !Go to hold mode
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 IF (.not. READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF
		 command(1:)='H1' !turn on switch heater, check status
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 wtime = 10.	!wait for switch to go normal
c       dan dender opts for longer warmup time. 6-9-99	      
		 CALL Holds(wtime)
		 command(1:)='X' !get persistent switch heater status
		 lcom = 1
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 pstatus = 0	!default if error reading or decoding 0 = OFF
		 IF (lreply .NE. 0) READ(UNIT=reply(9:9),FMT=*,IOSTAT=ios)pstatus
		 IF( lreply .EQ. 0 .OR. ios .NE. 0)THEN
		    CALL Putcontr(' Error reading in psh. Magnet will be shutdown !') !Debug
		    CALL Shut_mag
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    magnctr = .FALSE.     
		    RETURN		
		 ENDIF
		 IF (pstatus .EQ. 5)THEN ! switch heater fault
		    CALL Putcontr('Controller detects fault in switch heater')
		    CALL Shut_mag
		    CALL Putcontr('Magnet HEATER error forced SHUTDOWN !')
		    magnctr = .FALSE.
		    RETURN
		 ELSE IF (pstatus .EQ. 8)THEN ! no switch fitted
 		    CALL Putcontr('Controller indicates switch heater is not fitted')
		    CALL Shut_mag
		    CALL Putcontr('No magnet HEATER present forced SHUTDOWN !')
		    magnctr = .FALSE.
		    RETURN
		 ELSE IF (pstatus .EQ. 0 .OR. pstatus .EQ. 2)THEN !switch is off
 		    CALL Putcontr('Error reading in psh, or psh turned off ! Magnet will be shutdown!')
		    CALL Shut_mag
		    CALL Putcontr('SWITCH HEATER off forced SHUTDOWN !')
		    magnctr = .FALSE.
		    RETURN
		 ELSE		! switch is on
		    pstatus = 1
		 ENDIF
		 IF (.not. READEF (stop))THEN !Holds aborted
		    CALL CLREF(contr_A) !Just continue 
C       SANS		field = rval  !SANS addition
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop)  
		 ENDIF
	      ENDIF		!pstatus
	   ENDIF		!SCmag with persistent switch
	   command(1:)='X'	!check system status, 00 means all OK
	   lcom = 1
	   CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	   IF (lreply .EQ. 15)THEN !got reply of valid form
	      IF (reply(2:3) .NE. '00')THEN
		 CALL Shut_mag	! set IOUT to zero
		 CALL Putcontr(' Magnet Error forced SHUTDOWN !: '//reply(1:lreply))
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
	   checktime = 5.
	   c_out = fieldlast
	   istepstatus = 1
C       Oxford magnet charging loop. Nov 2000 DCDender using current ramping
	   DO WHILE (.NOT. breakloop)
	      if (c_out .eq. fieldlast) istepstatus = istepstatus + 1
	      if (istepstatus .ge. 10) then
! Remind this controller to go to its setpoint
		 command(1:)='R5'
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 if (lreply .gt. 0.) then
		    READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
		    IF (ABS(rval - field) .gt. iouterr) THEN
		       outstr(1:)=' '
		       WRITE(unit=outstr(1:),fmt=*)field
		       CALL Putcontr(' OXFORD SETPOINT SHOULD BE '//outstr)
		       outstr(1:)=' '
		       WRITE(unit=outstr(1:),fmt=*)rval
		       CALL Putcontr(' OXFORD SETPOINT IS '//outstr)
		    ENDIF
		    CALL libwait(0.1,status)
		 endif

! NCM - Add this directive because ICE does
C<<<<   NCM Change max rate per Sung Chang 1/21/10
C command(1:)='S1.2'
		 command(1:)='S10.0' ! Go to hold mode
		 lcom = 5
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
! NCM - END

		 str9(2)(1:)=' '
		 WRITE(UNIT=str9(2),FMT='(F9.4)',iostat=ios)field
		 command(1:)='I'//str9(2) !Set target current
		 lcom = 10
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 CALL libwait(0.1,status)

		 command(1:)='A1' ! Go to setpoint
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 CALL libwait(0.5,status)
		 istepstatus = 0
	      endif
	      fieldlast = c_out
	      command(1:)='R0'	!Read back current 
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)c_out
	      IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		 c_out = fieldlast
		 CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		 CALL Shut_mag
		 magnctr = .FALSE.     
		 RETURN		 
	      ENDIF

	      IF (firstloop) THEN ! set sweep rate, target current and max wait
	         gotthere=.false.
		 sploop = .true.

! Write setpoint information and make sure it's read
		 DO WHILE (sploop) 
		    sploop = .false.
!command(1:)='S10' ! max sweep rate 10A/min
!lcom = 3
		    command(1:)='S10.0' ! max sweep rate 10A/min	
		    lcom=4
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    if (.not.sploop .and. lreply.eq.0)sploop=.true.
		    CALL LIBWAIT(0.1,status)

		    str9(2)(1:)=' '
		    WRITE(UNIT=str9(2),FMT='(F9.4)',iostat=ios)field
		    command(1:)='I'//str9(2) !Set target current
		    lcom = 10
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    if (.not.sploop .and. lreply.eq.0)sploop=.true.
		    if (lreply.gt.0.and.reply(1:1).ne.'I')sploop=.true.
		    CALL LIBWAIT(0.1,status)

		    command(1:)='A1' !Go to set point
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    if (.not.sploop .and. lreply.eq.0)sploop=.true.
		    if (lreply.gt.0.and.reply(1:1).ne.'A')sploop=.true.
		    CALL LIBWAIT(0.2,status)

! Read back setpoint and make sure it's correct
		    command(1:)='R5'
		    lcom = 2
		    CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		    if (lreply .gt. 0.) then
		       READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
		       IF (ABS(rval - field) .gt. iouterr) sploop=.true.
		    endif
		 ENDDO

		 IF (def_wait)THEN
C       Oxford IPS120-10 is capable of 0.01 A/min ramp rate
		    maxtime = 6000. * abs(field-c_out)
		    iwait = NINT(maxtime)
		    CALL ext_to_int_time(wait_int,iwait,status) !use def max time
		    write(unit=outstr,fmt=*)iwait
		    call putcontr(' Maxtime: '//outstr)
		 ENDIF		!def_wait

		 CALL Sched_Action(timeup,wait_int)
		 mintime = abs(field-c_out)/2.
		 iwait = NINT(mintime)
		 CALL ext_to_int_time(cwait_int,iwait,status)
		 CALL Sched_Action(ctimeup,cwait_int)
	      ENDIF		! firstloop

	      IF (timeyet(timeup))breakloop = .TRUE.
	      firstloop = .FALSE.
C       changed Jan 99 RW Erwin  to not force a second loop if within tolerance AND check for charging
	      IF (abs(field - c_out) .LT. Iouterr)THEN

C       6/13/08 - NCM: Paranoid check to make sure we're really there
                 call libwait(0.5,status)
		 command(1:)='R0' !Read back current 
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
		 IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)c_out
		 IF (lreply .EQ. 0 .OR. ios .NE. 0)THEN
		    c_out = fieldlast
		    CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		    CALL Shut_mag
		    magnctr = .FALSE.     
		    RETURN		 
		 ENDIF
		 IF (abs(field - c_out) .LT. Iouterr)THEN

		    gotthere = .TRUE.
		    breakloop = .TRUE.
C<<<    NCM COMMENTED OUT 1/6/10
C       6/24/99 DCD added 5 second minimum time for charging time check
C       IF ((iwait .GE. 5) .AND. (.NOT. timeyet(ctimeup)))THEN  
C       
C       magnet NOT charging, since current went up too fast
C       CALL Shut_mag ! set IOUT to zero
C       CALL Putcontr(' Magnet Error: SHUTDOWN because NOT charging !')
C       magnctr = .FALSE.     
C       RETURN
C       ENDIF	! charging time check
		 ENDIF          ! Paranoid re-check field - c_out
	      ENDIF		! field - c_out check
	      IF (.NOT. breakloop)THEN
		 CALL Holds(checktime)
		 IF (.not. READEF (stop)) THEN !Holds aborted  
		    CALL CLREF(contr_A) 
		    breakloop = .TRUE.
		 ELSE		!normal termination of Holds routine. No control_a
		    CALL CLREF (stop) 
		 ENDIF
	      ENDIF
	   ENDDO		!until breakloop

C       END OF CURRENT SETTING LOOP
	   IF (gotthere)THEN
	      fieldlast = field	
	   ELSE
	      command(1:)='R0'	!Read back current 
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)fieldlast
	      IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
		 fieldlast=c_out !from up above in Ross loop
		 CALL Shut_mag
		 CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		 magnctr = .FALSE.     
		 RETURN		 
	      ENDIF
	   ENDIF		! gotthere
	   command(1:)='X'	!check system status, '00' means all OK
	   lcom = 1
	   CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	   IF (lreply .EQ. 15)THEN !got reply of valid form
	      IF (reply(2:3) .NE. '00')THEN
		 CALL Shut_mag
		 CALL Putcontr(' Magnet Error forced SHUTDOWN !: '//reply(1:lreply))
		 magnctr = .FALSE.     
		 RETURN
	      ENDIF
	   ENDIF
	   IF (magtype .eq. 4 .AND. hfmode .EQ. 1) THEN	!SCmag in persistent mode
C       RAMP DOWN CURRENT LEADS
	      command(1:)='A0'	!Go to hold mode
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      command(1:)='H0'	! turn switch off
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      wtime = 30.
C       dan dender opts for longer cooldown time. 6-9-99
	      CALL Holds(wtime)
	      IF (.not. READEF (stop))THEN !Holds aborted 
		 CALL CLREF(contr_A) 
	      ELSE		!normal termination of Holds routine. No control_a
		 CALL CLREF (stop)
	      ENDIF

	      command(1:)='A2'	!Go to zero current in leads
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      wtime = ABS(fieldlast)/8. + 5.
c       IF (ios .NE. 0)WHAT ROSS?
	      CALL Holds(wtime)
	      command(1:)='R0'	!Read back current 
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)rval
	      IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
		 CALL Shut_mag
		 CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
		 magnctr = .FALSE.     
		 RETURN		 
	      ENDIF
	      IF (abs(rval) .GT. 0.3)THEN ! not zero yet SO magnet discharges on lowering current
		 CALL Shut_mag
		 CALL Putcontr(' Magnet discharged on lowering current in PS mode forced SHUTDOWN !')
		 magnctr = .FALSE.     
		 RETURN		
	      ENDIF
	      IF (.not. READEF (stop))THEN !Holds aborted 
		 CALL CLREF(contr_A)
	      ELSE		!normal termination of Holds routine. No control_a
		 CALL CLREF (stop)
	      ENDIF

	   ENDIF		!SCmag in persistent mode
	   command(1:)='A0'	!Go to hold mode
	   lcom = 2
	   CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	ENDIF			!choice on magtype
	IF (minit_done .and. nsta.eq.-5) CALL CANCELFIELD
 98	RETURN
	END
