#include "icp.h"
	SUBROUTINE TrashDrive(targetangle)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     TrashDrive: Move Sample Table and Analyzer Cart in tandem               C
C     Note: BT4(FANS) specific                                                C
C     Axes involved:  2 = monochromator drum                                  C
C                     9 = analyzer cart                                       C
C     Called from: Parse_Contr, Run_Q_Buf, Run_I_Buf, Run_T_Buf, Find_Peak    C
C     Version = $Id$ C
C                                                                             C
C     Nick Maliszewskyj, Feb 2000                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C To do: Make sure that if we go positive, overshoot by a little, dock, and
C        move negative to final position
	use instr
	use motors
	use iso_c_binding
	IMPLICIT none
	interface
#include "c_vme.h"
#include "c_libwait.h"
	end interface
	INTEGER*2    notries
	INTEGER*4    motorno, nsteps, oldpos,newpos,ichange
	INTEGER*4    amotor, isteps, apos, firstmotor, secondmotor
	INTEGER*4    io_stat, status, mtries, retn, movstat, limstat
	REAL*4       targetangle, currentangle, atargetangle, anglecushion, dest
	REAL*4       mback, mback_a, newdest
	CHARACTER*4  integer_fmt
	CHARACTER*1  size
	CHARACTER*10 stepstring
	LOGICAL*1    retry
	logical      stopped, plus, minus, home,comerr
	logical      READEF, dobacklash, gopositive, bad_reply,dockstart

	if (nsta .ne. 4) then
	   call putcontr('This mode of motion only available for FANS')
	   return
	endif

	motorno = TRASHMONO	! Move monochromator drum
	amotor  = TRASHANAL	! Move analyzer cart
	dockstart = .false.
	trashmove = .true.
	mback = back(motorno)/pulse(motorno)
	mback_a = back(amotor)/pulse(amotor)
C       
C       Get current motor position
C       
	notries=10		!try up to 10 times 
	retry  =.true.
	
	notries = 10
	retn = -1
	do while ((notries .gt. 0) .and. (retn .lt. 0)) 
	   retn = VMEGETPOS(motorno,currentangle)
	   oldpos = NINT(currentangle * pulse(motorno))
	   if (retn .lt. 0) call libwait(0.5,status)
	   notries = notries - 1
	enddo
	if (retn .lt. 0) then
	   call putcontr('VMEDRIVE: could not determine position of motor')
	   return
	endif               

C       If notries exceeded, should bail out

C       
C       Set angle of analyzer motor
C       
C       Do this only if analyzer and sample table are in contact

 100	call check_limits(amotor, stopped, plus, minus, home, comerr)
	if (minus) then
	   dockstart = .true.
	   retn = VMESETPOS(amotor, currentangle)
	else
	endif

C       
C       Determine distance and direction of move
C       
	newpos = NINT(targetangle * pulse(motorno))
	ichange = newpos - oldpos
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       If CW motion:  ichange > 0
C       drive analyzer to target first
C       wait until limit switch de-actuated
C       drive monochromator drum to target
C       
C       If CCW motion: ichange < 0
C       drive monochromator drum to target
C       wait until limit switch de-actuated
C       drive analyzer to target
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       
C       Check against deadband
C       
	IF (ABS(ichange) .LE. merr(motorno)) RETURN
	gopositive = (ichange .gt. 0)
	dobacklash = (((ichange .LT. 0) .AND. (back(motorno) .GT. 0))
	1    .OR. ((ichange .GT. 0) .AND. (back(motorno) .LT. 0)))

! Calculate angle cushion for analyzer
	if (gopositive .and. dobacklash) then
	   anglecushion = ABS(mback) * 1.1
	else
	   anglecushion = ABS(mback) * 0.1
	endif

	atargetangle = targetangle + anglecushion
	destination(amotor)  = atargetangle
	destination(motorno) = targetangle

	if(trackmoves) then
	   call memcurpos(motorno,1,targetangle)
	   call memcurpos(amotor,1,atargetangle)
	endif

	if (dobacklash) then
	   targetangle = targetangle - mback ! mback is distance to overshoot
	   overshot(motorno) = .TRUE.
	else
	   overshot(motorno) = .FALSE.
	endif

	if (gopositive) then 
	   firstmotor = amotor
	   secondmotor = motorno
	   dest = atargetangle
	else			! going negative
	   firstmotor = motorno
	   secondmotor = amotor
	   dest = targetangle
	endif

	retn = VMEGOPOS(firstmotor,dest)

	moving(firstmotor) = .true.

C       Should check to make sure first motor actually moving
	
C       Now poll limit status on analyzer
	home = .true.
	notries = 10
	mtries = notries
	do while (home .and. (mtries .gt. 0))
	   call check_limits(firstmotor,stopped,plus,minus,home,comerr)
	   if (firstmotor .eq. motorno) then
	      home = plus
	   else
	      home = minus
	   end if
	   if (home) mtries = mtries - 1
	   IF ( READEF(contr_a) ) goto 675
	   call libwait(0.25,status)
	enddo

	call libwait(0.25,status)

C       If we're going to higher angle, wait until analyzer stops before
C       continuing
	if (gopositive) then
 300	   do while (moving(firstmotor))
	      mtries=10		!retry a given command up to 10 times
	      retn = -1
	      do while (mtries.gt.0 .and. retn .lt. 0)
		 retn = VMESTATUS(firstmotor,movstat,limstat)
		 if (readef(contr_a)) goto 675
		 mtries=mtries-1
		 call libwait(0.25,status)
	      enddo
	      if (retn .lt. 0)then !no good reply after 10 tries
		 moving(firstmotor)=.false. !give up on this motor
!don't check position. may finish itself.
		 call putcontr(' could not confirm position of motor '//
	1	      motorstring(firstmotor)) 
		 goto 675
	      endif

	      if (movstat .eq. 0)then !motor is no longer moving
		 moving(firstmotor)=.false. !set flag
	      endif
	      if (readef(contr_a)) goto 675
	   enddo

! Read position of motor
	   
	else
 400	   home = .true.
	   notries = 10
	   mtries = notries
	   do while (home .and. (mtries .gt. 0))
	      call check_limits(secondmotor,stopped,plus,minus,home,comerr)
	      if (secondmotor .eq. motorno) then
		 home = plus
	      else
		 home = minus
	      end if
	      if (home) mtries = mtries - 1
	      IF ( READEF(contr_a) ) goto 675
	      call libwait(0.25,status)
	   enddo         
	endif

C       Now move second axis to target position
	if (gopositive) then
	   dest = targetangle
	else
! If we're going to run the analyzer positive, don't move at all
	   if (dockstart .and. (atargetangle .ge. currentangle)) then 
	      return
	   endif
	   dest = atargetangle
	endif

	retn = VMEGOPOS(secondmotor,dest)
	moving(secondmotor) = .true.

	RETURN

 550	CALL PUTCONTR(' ERROR CONVERTING POSTION TO OUTPUT FORMAT')
	RETURN

 675	RETURN
C       Should stop motor motion in case of abort      
	trashmove = .false.

	END
