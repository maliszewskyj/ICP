#include "icp.h"
C Yield: wait for all moving motors to stop at their destination
c
	subroutine yield
c
	use instr
	use motors
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_acs.h"
#include "c_pi.h"
#include "c_ez.h"
#include "c_pico.h"
#include "c_vme.h"
#include "c_zeta.h"
#include "c_libwait.h"
	end interface

C       This version of yield is the same as [icp.hard.mot] except:
c       all references to motors other than acs and xray are removed
c       adjpos business at end is removed
c       all calls to lib$wait are changed to call libwait(.5,status)
c       replace setting the motor_wait_time timer with call libwait(1.,status)
c       remove the motor_timer_ast subroutine at the end
c       remove all declarations of sys$
C       change all sys$readef to call readef. Also sys$setef. Declare readef
c       comment out $ssdef
C       Yield: wait for all moving motors to stop at their destination
c       
c       Version = $Id$
c       
c       
	logical*1 all_finished,stoperr
	integer*4 motorno,io_stat,state,mtries, ios, rewrites
	integer*4 ipos, secwait, lsteps, newpos, istat
	integer*4 status,notries
	character*80 command,reply
	character*10  str10(2)
	character*20  str20(2)
	character*2	str2
	integer*4 lcom,lreply
	logical	  bad_reply, rserr,  ckpos(MAXMOTS), adjpos, READEF
	logical   stopped, plus, minus, home, comerr, eichange
	integer*4 iplus,iminus,ihome
	real*4	  rpos, rdangl
	character*28  date

c       
	external drive !,motor_timer_ast
c       
	adjpos = .false. !default is no check on position needed
	eichange = .false.
c       
c       
	IF (READEF(contr_a) ) goto 675

C       Set up position correction flags for motor types ACSF 
C       (needed if limit or abort)
	DO motorno = 1, MAXMOTS
	   IF (mottype(motorno) .EQ. ACSF .AND. moving(motorno))THEN
	      ckpos(motorno) = .TRUE. !check all as per Collin since limits weird
	   ELSE
	      ckpos(motorno) = .FALSE.
	   ENDIF
	ENDDO

	rewrites = 0
c       
 100	all_finished=.true.
	do 600 motorno=1,MAXMOTS !check each motor on instrument
	   IF ( READEF(contr_a) ) goto 675
	   if (.NOT. motfl(motorno)) goto 600
	   if (moving(motorno))then !only continue for motors that were moved
	      if (motorno.eq.2) eichange=.true. ! NCM - test eichange for SPINS
	      mtries  = 10
	      lreply = 0
	      reply(1:) = ' ' ! No valid reply yet
	      IF (mottype(motorno) .eq. PIMOT) THEN
		 ! Is motor still moving?

		 ! Check motor status: moving & limits
		 io_stat = -1
		 do while ((mtries .gt. 0) .and. (io_stat .lt. 0))
		    if (readef(contr_a)) goto 675
		    io_stat = PISTATUS(motorno,istat,status)
		    ! istat - moving
		    ! status - limit status: +1 positive, -1 negative
		    if (io_stat .lt. 0) call libwait(0.5,status)
		    mtries = mtries - 1
		 enddo
		 if (io_stat .lt. 0) then ! Got no valid reply after retries
		    
		 endif

		 if (istat .eq. 0) then ! motor is no longer moving
		    moving(motorno) = .false.
		    ! See if we're on a limit
		    if (status .ne. 0) then
		       call putcontr(' pimotor '//motorstring(motorno)//
	1		    ' on hardware limit!!!!')
 		       if (.not.overshot(motorno)) goto 600 ! give up 

		       ! see if we are ccw of correct position
		       io_stat = PIGETPOS(motorno,ipos)
		       if (ipos .lt. NINT(destination(motorno)*
	1		    pulse(motorno))) then
			  if (readef(contr_a)) goto 675
			  call drive(motorno,destination(motorno))
			  call putcontr(' driving motor off limit to target ')
			  call putcontr(' WARNING: may have backlash problem ')
			  all_finished = .false.
		       endif

		    endif ! on limit

		    ! if we need to perform backlash move, do so now
		    if (overshot(motorno)) then ! intentionally went too far
		       overshot(motorno) = .false.
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		       goto 600
		    endif

		    ! If motion is truly finished
		    io_stat = -1
		    mtries = 10
		    do while ((mtries .gt. 0) .and. io_stat .lt. 0)
                       if (readef(contr_a)) goto 675
		       io_stat = PIGETPOS(motorno,ipos)
		       if (io_stat .lt. 0) call libwait(0.5,status)
		       mtries = mtries - 1
		    enddo

		    ! Check how far we are from target position
		    if (abs(ipos - NINT(destination(motorno)
	1		 *pulse(motorno))) .gt. merr(motorno)) then
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		    endif
		 else
		    all_finished = .false. ! motor is still moving
		 endif

	      ELSE IF(mottype(motorno) .eq. VME) THEN
		 ! Is motor still moving?

		 ! Check motor status: moving & limits
		 io_stat = -1
		 do while ((mtries .gt. 0) .and. (io_stat .lt. 0))
		    if (readef(contr_a)) goto 675
		    io_stat = VMESTATUS(motorno,istat,status)
		    ! istat - moving
		    ! status - limit status: +1 positive, -1 negative
		    if (io_stat .lt. 0) call libwait(0.5,status)
		    mtries = mtries - 1
		 enddo

		 if (mtries .le. 0) then
		    moving(motorno)=.false. !give up on this motor
		    !don't check position. may finish itself.
		    call putcontr(' could not confirm position of motor '//
	1		 motorstring(motorno)) 
		    if (nsta.eq.-11) call VMEDISABLE(motorno)
		    goto 600
		 endif

		 if (istat .eq. 0) then ! motor is no longer moving
		    moving(motorno) = .false.
		    ! See if we're on a limit
		    if (status .ne. 0) then
		       IF ((motorno .eq. TRASHANAL) .and. trashmode) goto 600	
		       call putcontr(' vmemotor '//motorstring(motorno)//
	1		    ' on hardware limit!!!!')
 		       if (.not.overshot(motorno)) then
			  if (nsta .eq. -11) call VMEDISABLE(motorno)
			  goto 600 ! give up 
		       endif

		       ! see if we are ccw of correct position
C		       io_stat = VMEGETPOS(motorno,ipos)
C		       if (ipos .lt. NINT(destination(motorno)*
C	1		    pulse(motorno))) then
C			  if (readef(contr_a)) goto 675
C			  call drive(motorno,destination(motorno))
C			  call putcontr(' driving motor off limit to target ')
C			  call putcontr(' WARNING: may have backlash problem ')
C			  all_finished = .false.
C		       endif
		       ! We should not move this axis if we've stopped.
C		       if (.not.overshot(motorno)) goto 600 ! give up (for all)
		       goto 600 ! give up
		    endif ! on limit

		    ! if we need to perform backlash move, do so now
		    if (overshot(motorno)) then ! intentionally went too far
		       overshot(motorno) = .false.
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		       goto 600
		    endif

		    ! If motion is truly finished
		    io_stat = -1
		    mtries = 10
		    do while ((mtries .gt. 0) .and. io_stat .lt. 0)
                       if (readef(contr_a)) goto 675
		       io_stat = VMEGETPOS(motorno,rpos)
		       if (io_stat .lt. 0) call libwait(0.5,status)
		       mtries = mtries - 1
		    enddo

		    ! Check how far we are from target position
		    ! NOTE: We could loop indefinitely here
		    if (abs(rpos - destination(motorno)).gt.1.0e-8 +
	1		 abs(float(merr(motorno))/
	1		 float(pulse(motorno)))) then
		       if (readef(contr_a)) goto 675
		       IF (rewrites .LT. 10)THEN
			  rewrites = rewrites + 1
			  call libwait(0.5,status)
			  call drive(motorno,destination(motorno))
			  all_finished=.false.
			  goto 600
		       ELSE
			  call putcontr('Giving up after 10 rewrites on vme')
		       ENDIF
		       write(unit=str20(1),fmt=*,iostat=ios)rpos
		       write(unit=str20(2),fmt=*,iostat=ios)
	1		    destination(motorno)
		       call putcontr(' final pos '//
	1		    motorstring(motorno)//': '//str20(1)//
	2		    ' wanted: '//str20(2) )
		       write(unit=str20(1),fmt=*,iostat=ios)
	1		    abs(rpos-destination(motorno))
		       write(unit=str20(2),fmt=*,iostat=ios)
	1		    abs(float(merr(motorno))/float(pulse(motorno)) ) 
		       call putcontr(' diff: '//str20(1)//' allowed err: '//
	1		    str20(2))
		       ! Do not retry, bail out instead
C		       call drive(motorno,destination(motorno))
C		       all_finished = .false.
		    else
		       if (nsta .eq. -11) call VMEDISABLE(motorno)
C SPINS 
		       if (nsta .eq. -5 .and. magnctr .and. motorno .eq. 2) CALL CANCELFIELD
C SPINS
		    endif ! too far off target
		 else
		    all_finished = .false. ! motor is still moving
		 endif

	      ELSE IF(mottype(motorno) .eq. ZETA) THEN
		 ! Is motor still moving?

		 ! Check motor status: moving & limits
		 io_stat = -1
                 status=0
		 do while ((mtries .gt. 0) .and. (io_stat .lt. 0))
		    if (readef(contr_a)) goto 675
		    io_stat = ZETASTATUS(motorno,istat,status)
		    ! istat - moving
		    ! status - limit status: +1 positive, -1 negative	
C
C Short circuit limit status
C
		    if (io_stat .lt. 0) call libwait(0.5,status)
		    mtries = mtries - 1
		 enddo

		 if (istat .eq. 0) then ! motor is no longer moving
		    moving(motorno) = .false.
		    ! See if we're on a limit
		    if (status .ne. 0) then
C		       call putcontr(' zeta '//motorstring(motorno)//
C	1		    ' on hardware limit!!!!')
 		       if (.not.overshot(motorno)) goto 600 ! give up 

		       ! see if we are ccw of correct position
C		       io_stat = ZETAGETPOS(motorno,ipos)
C		       if (ipos .lt. NINT(destination(motorno)*
C	1		    pulse(motorno))) then
C			  if (readef(contr_a)) goto 675
C			  call drive(motorno,destination(motorno))
C			  call putcontr(' driving motor off limit to target ')
C			  call putcontr(' WARNING: may have backlash problem ')
C			  all_finished = .false.
C		       endif
		       ! We should not move this axis if we've stopped.
C		       if (.not.overshot(motorno)) goto 600 ! give up (for all)
		       goto 600 ! give up
		    endif ! on limit

		    ! if we need to perform backlash move, do so now
		    if (overshot(motorno)) then ! intentionally went too far
		       overshot(motorno) = .false.
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		       goto 600
		    endif

		    ! If motion is truly finished
		    io_stat = -1
		    mtries = 10
		    do while ((mtries .gt. 0) .and. io_stat .lt. 0)
                       if (readef(contr_a)) goto 675
		       io_stat = ZETAGETPOS(motorno,ipos)
		       if (io_stat .lt. 0) call libwait(0.5,status)
		       rpos=ipos
		       mtries = mtries - 1
		    enddo

		    ! Check how far we are from target position
		    ! NOTE: We could loop indefinitely here
		    if (abs(rpos - destination(motorno)).gt.1.0e-8 +
	1		 abs(float(merr(motorno))/
	1		 float(pulse(motorno)))) then
		       if (readef(contr_a)) goto 675
		       IF (rewrites .LT. 10)THEN
			  rewrites = rewrites + 1
			  call libwait(0.5,status)
			  call drive(motorno,destination(motorno))
			  all_finished=.false.
			  goto 600
		       ELSE
			  call putcontr('Giving up after 10 rewrites on zeta')
		       ENDIF
		       write(unit=str20(1),fmt=*,iostat=ios)rpos
		       write(unit=str20(2),fmt=*,iostat=ios)
	1		    destination(motorno)
		       call putcontr(' final pos '//
	1		    motorstring(motorno)//': '//str20(1)//
	2		    ' wanted: '//str20(2) )
		       write(unit=str20(1),fmt=*,iostat=ios)
	1		    abs(rpos-destination(motorno))
		       write(unit=str20(2),fmt=*,iostat=ios)
	1		    abs(float(merr(motorno))/float(pulse(motorno)) ) 
		       call putcontr(' diff: '//str20(1)//' allowed err: '//
	1		    str20(2))
		       ! Do not retry, bail out instead
C		       call drive(motorno,destination(motorno))
C		       all_finished = .false.
		    endif ! too far off target
		 else
		    all_finished = .false. ! motor is still moving
		 endif

	      ELSE IF(mottype(motorno) .eq. EZ) THEN
		 ! Is motor still moving?

		 ! Check motor status: moving & limits
		 io_stat = -1
                 status=0
		 do while ((mtries .gt. 0) .and. (io_stat .lt. 0))
		    if (readef(contr_a)) goto 675
		    io_stat = EZSTATUS(motorno,istat,iplus,iminus,ihome)
		    ! istat - moving
		    ! status - limit status: +1 positive, -1 negative	
C
C Short circuit limit status
C
		    if (io_stat .lt. 0) call libwait(0.5,status)
		    mtries = mtries - 1
		 enddo

		 if (istat .eq. 0) then ! motor is no longer moving
		    moving(motorno) = .false.
		    ! See if we're on a limit
		    if (status .ne. 0) then

 		       if (.not.overshot(motorno)) goto 600 ! give up 

		       goto 600 ! give up
		    endif ! on limit

		    ! if we need to perform backlash move, do so now
		    if (overshot(motorno)) then ! intentionally went too far
		       overshot(motorno) = .false.
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		       goto 600
		    endif

		    ! If motion is truly finished
		    io_stat = -1
		    mtries = 10
		    do while ((mtries .gt. 0) .and. io_stat .lt. 0)
                       if (readef(contr_a)) goto 675
		       io_stat = EZGETPOS(motorno,rpos)
                       rpos = rpos / pulse(motorno)
		       if (io_stat .lt. 0) call libwait(0.5,status)
		       mtries = mtries - 1
		    enddo

		 else
		    all_finished = .false. ! motor is still moving
		 endif
	      ELSE IF(mottype(motorno) .eq. PICO) THEN
		 ! Is motor still moving?

		 ! Check motor status: moving & limits
		 io_stat = -1
                 status=0
		 do while ((mtries .gt. 0) .and. (io_stat .lt. 0))
		    if (readef(contr_a)) goto 675
		    io_stat = PICOSTATUS(motorno,istat)
		    ! istat - moving
		    ! status - limit status: +1 positive, -1 negative	
C
		    if (io_stat .lt. 0) call libwait(0.5,status)
		    mtries = mtries - 1
		 enddo

		 if (istat .eq. 0) then ! motor is no longer moving
		    moving(motorno) = .false.
		    ! See if we're on a limit
		    if (status .ne. 0) then

 		       if (.not.overshot(motorno)) goto 600 ! give up 

		       goto 600 ! give up
		    endif ! on limit

		    ! if we need to perform backlash move, do so now
		    if (overshot(motorno)) then ! intentionally went too far
		       overshot(motorno) = .false.
		       if (readef(contr_a)) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished = .false.
		       goto 600
		    endif

		    ! If motion is truly finished
		    if (readef(contr_a)) goto 675
		    rpos = rdangl(motorno)

		 else
		    all_finished = .false. ! motor is still moving
		 endif
	      ELSE IF(mottype(motorno) .eq. ACS) THEN
		 ! Get motor status
		 mtries=10	!retry a given command up to 10 times
		 lreply=0	!no answer yet
		 reply(1:)=' '	!no valid reply yet
		 command(1:3)=motorstring(motorno)//'E'
		 lcom=3	
		 call rs232(command,lcom,reply,lreply) !repeat once
		 bad_reply = lreply.EQ.0 .OR. reply(8:8).NE.'A'
		 do while (mtries.gt.0 .and. bad_reply)
		    call libwait(.5,status)
		    lreply=0	!no answer yet
		    reply(1:)=' ' !no valid reply yet
		    command(1:3)=motorstring(motorno)//'E'
		    lcom=3	
		    call rs232(command,lcom,reply,lreply)
		    bad_reply = lreply.EQ.0 .OR. reply(8:8).NE.'A'
		    if (lreply.lt.0)goto 675 !control a detected
		    mtries=mtries-1
		 enddo
		 if (bad_reply)then !no good reply after 10 tries
		    moving(motorno)=.false. !give up on this motor
		    !don't check position. may finish itself.
		    call putcontr(' could not confirm position of motor '//
	1		 motorstring(motorno)) 
		    if (trackmoves) ckpos(motorno) = .true.
		    IF (nsta .EQ. 8)GOTO 725 !bailout
		    goto 600
		 endif

		 ! Have a valid reply with status so check it out
		 if (reply(9:9).eq.'0')then !motor is no longer moving
		    moving(motorno)=.false. !set flag
		    if (reply(5:5).eq.'-'.or.reply(7:7).eq.'+')then !on limit!
		       IF ((motorno .eq. TRASHANAL) .and. trashmode) goto 600	
		       call putcontr(' motor '//motorstring(motorno)//
	1		    ' on hardware limit!!!!')
		       if (trackmoves) ckpos(motorno) = .true.
		       IF (nsta .EQ. 8) GOTO 725 !bail out
		       if (.not. overshot(motorno)) goto 600 !give up 
		       if (reply(7:7).eq.'+') goto 600       !give up 
			!problem may be extra ccw steps. 
			!see if we can still get to pos
		       mtries=10 !retry a given command up to 10 times
		       lreply=0	!no answer yet
		       reply(1:)=' ' !no valid reply yet
		       do while ((mtries.gt.0).and.(lreply.eq.0.or.reply(3:3).ne.'P'))
			  command(1:3)=motorstring(motorno)//'P'
			  lcom=3
			  IF ( READEF(contr_a) )goto 675
			  call libwait(.5,status) !wait half second and try again
			  call rs232(command,lcom,reply,lreply)
			  mtries=mtries-1
		       enddo
		       if (lreply.eq.0.or.reply(3:3).ne.'P')then !still no good
			  call putcontr(' could not determine position of motor '//
	1		       motorstring(motorno))
			  goto 600
		       endif
		       READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) Ipos
		       if (io_stat.ne.0)then
			  call putcontr(' could not decode position of motor '//
	1		       motorstring(motorno))
			  goto 600
		       endif
		       ! see if we are ccw of correct position
		       if (ipos .lt. NINT(destination(motorno)*
	1		    pulse(motorno)) )then
			  IF (READEF(contr_a) )goto 675
			  call drive(motorno,destination(motorno))
			  call putcontr(' driving motor off limit to target' )
			  call putcontr(' WARNING: may have backlash problem') 
			  all_finished=.false.
		       endif
		       goto 600 
		    endif	!on limit
		    if (overshot(motorno))then !intentionally went too far
		       overshot(motorno)=.false.
		       IF (READEF(contr_a) ) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished=.false.
		       goto 600
		    end if	!overshot

		    ! Get current position using P:
		    mtries=10	!retry a given command up to 10 times
		    lreply=0	!no answer yet
		    reply(1:)=' ' !no valid reply yet
		    command(1:3)=motorstring(motorno)//'P'
		    lcom=3
		    call rs232(command,lcom,reply,lreply)
		    do while ((mtries.gt.0).and.
	1		 (lreply.eq.0.or.reply(3:3).ne.'P'))
		       command(1:3)=motorstring(motorno)//'P'
		       lcom=3
		       call libwait(.5,status) !wait half second and try again here
		       call rs232(command,lcom,reply,lreply)
		       mtries=mtries-1
		    enddo
		    if (lreply.eq.0.or.reply(3:3).ne.'P')then !still no good
		       moving(motorno)=.false. !give up 
		       call putcontr(' could not read final position of motor '//
	1		    motorstring(motorno))
		       if (trackmoves) ckpos(motorno) = .true.
		       IF (nsta. eq. 8) GOTO 725 !bail out
		       goto 600
		    endif
		    READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) Ipos
 50		    format(i12)
		    if (io_stat.ne.0)then
		       call rs232(command,lcom,reply,lreply) !reread once
		       if (lreply.eq.0.or. reply(3:3).ne.'P')then
			  moving(motorno)=.false. !give up on this motor
			  call putcontr(' could not decode final position of motor '//
	1		       motorstring(motorno))
			  IF (nsta. eq. 8) GOTO 725 !bail out
			  goto 600
		       endif
		    endif	!iostat error
		    if (abs(ipos-NINT(destination(motorno)*pulse(motorno))).gt.
	1		 merr(motorno))then
		       IF (READEF(contr_a) ) goto 675
		       call drive(motorno,destination(motorno))
		       all_finished=.false.
		    endif	!too far off target
		 ELSE		!still moving
		    all_finished=.FALSE.
		 ENDIF		!still moving
	      ENDIF		!choice on motor type
	   endif		!moving is .true.
 600	continue		!looping for each motor
c       
	if (all_finished) THEN
	   if (trashmode .and. trashmove) then
	      motorno = TRASHANAL
	      call check_limits(motorno, stopped, plus, minus, home, comerr)
	      if (.not.minus) then
		 ! Send analyzer to lower hardlimit
		 call go_limit(motorno,.TRUE.,comerr)
		 goto 100
	      endif
	      trashmove = .false.
	   endif
	   GOTO 900		!return 
	endif
c       
 660    IF ( READEF(contr_a) ) THEN 
 675	   notries=0
 700	   stoperr=.false.
	   lcom=3
	   do motorno=1,MAXMOTS !stop all
              IF (MOVING(motorno)) THEN
	         command=motorstring(motorno)//'Q'
		 IF (mottype(motorno).EQ. ACSF)THEN
		    call rs232F(command,lcom,reply,lreply)
		    ckpos(motorno)=.TRUE.
		 ELSEIF (mottype(motorno) .EQ. PIMOT) THEN
                    io_stat = PIHALT()
		 ELSEIF (mottype(motorno) .EQ. VME) THEN
		    io_stat = VMEHALT(motorno)
		 ELSEIF (mottype(motorno) .EQ. EZ) THEN
		    io_stat = EZHALT(motorno)
		 ELSEIF (mottype(motorno) .EQ. PICO) THEN
		    io_stat = PICOHALT()
		 ELSE		!ACS
		    call rs232(command,lcom,reply,lreply)
		 ENDIF
	         if (lreply.eq.0)THEN
		    stoperr=.true.
	         ELSE
		    moving(motorno)=.false.
		    if (trackmoves) then
		      ckpos(motorno)=.true.
		      adjpos = .true. !!need to check positions for file
		    endif
	         ENDIF
              end if
	   enddo
	   if (stoperr)then
	      notries=notries+1
	      if (notries.lt.10)then
		 goto 700
	      else
		 goto 800
	      endif
	   else
C       call putcontr(' motor movement aborted by control A')
	      goto 800		!return
	   endif
	END IF
c       
c       Start timer
c	status=sys$setimr(,motor_wait_time,motor_timer_ast,)
c	if (.not.status)CALL LIB$SIGNAL(%VAL(STATUS))
c	STATUS=SYS$HIBER()
c	IF (.NOT.STATUS)CALL LIB$SIGNAL(%VAL(STATUS))
	call libwait(1.,status)
	goto 100

c       BT8 bailout branch. Use contr_a to signal command file must be terminated.
 725	CALL setef(contr_a)	!signal problem as if contr_a
	GOTO 800
C       Err branch
 750	call putcontr(' Error in motor parameters for motor: '//
	1    motorstring(motorno))
c       800	call sys$setef(%val(camerr)) !set error flag
 800	do motorno=1,MAXMOTS
	   moving(motorno)=.false.
	enddo
C       check to see if any ACSF motors need position corrections
C 900	if (nsta.NE.-5)Return	!acsf motors only on Spins
C     check to see if abort was done and any positions need correction in file
 900	if (nsta.NE.-5 .and. .NOT. (trackmoves .and. adjpos) )Return 
	motorno = 1
	adjpos = .FALSE. !now we will use it to remember if any problems
	do while (.NOT. adjpos .AND. motorno .LE. MAXMOTS)
	  IF (ckpos(motorno)) adjpos = .TRUE.
	  motorno = motorno + 1
	enddo
C	IF (magnctr.and.minit_done .and. eichange) CALL CANCELFIELD
	if (.not. adjpos) return
C	Now check for need to adjust position in motpos.buf
	if (.not. trackmoves) return
	do motorno=1,MAXMOTS
	  if (ckpos(motorno) ) then
	    rpos = rdangl(motorno)
	      call memcurpos(motorno,1,rpos)
	  endif
	enddo
	return
	end
C
c
