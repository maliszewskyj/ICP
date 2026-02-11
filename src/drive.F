#include "icp.h"
      SUBROUTINE Drive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drive: Move Motor Specified To Physical Angle Indicated                 C
C     Note: Entry Drives no longer moves motor slowly. All drive is same speedC
C     Called from: Parse_Contr, Run_Q_Buf, Run_I_Buf, Run_T_Buf, Find_Peak    C
C                                                                             C
C     Gayle Greene, Beat Schmid, August 1989                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use motors
      use iso_c_binding
      IMPLICIT     none
      interface
#include "c_rs232_acs.h"         
#include "c_libwait.h"
      end interface
      CHARACTER*80 command, reply, gocommand
      INTEGER*4    lcom, lreply, errno, notries, gocom
      INTEGER*4    motorno, nsteps, oldpos,newpos,ichange
      INTEGER*4    io_stat, imove, secwait, status, ios
      REAL*4       targetangle, filepos, filezero
      CHARACTER*4  integer_fmt, errstr
      CHARACTER*1  size, DIRC
      CHARACTER*10 stepstring
      LOGICAL*1    retry,goretry
      LOGICAL	   rserr
      LOGICAL      dobacklash, offval
      CHARACTER*28  date
      CHARACTER*80  GETHOME

C     Treat virtual motor
      IF (mottype(motorno) .EQ. VIRT)THEN
         CALL Vdrive(motorno, targetangle)
         RETURN
      ENDIF

C     Treat Physik Instrumente controllers
      IF (mottype(motorno) .EQ. PIMOT)THEN
         CALL PiDrive(motorno, targetangle)
         RETURN
      ENDIF

C     Treat VME-based OMS58 controllers
      IF (mottype(motorno) .EQ. VME)THEN
         CALL VMEDrive(motorno, targetangle)
         RETURN
      ENDIF

C     Treat Standalone CompuMotors
      IF (mottype(motorno) .EQ. ZETA)THEN
         CALL ZETADrive(motorno, targetangle)
         RETURN
      ENDIF

      IF (mottype(motorno) .EQ. EZ) THEN
         CALL EZDrive(motorno, targetangle)
         RETURN
      ENDIF

      IF (mottype(motorno) .EQ. PICO) THEN
         CALL PICODrive(motorno, targetangle)
         RETURN
      ENDIF

C     Now treat other types of controllers
      errno=0

C     Get newpos in terms of pulses
      newpos = NINT(targetangle*pulse(motorno)) !take nearest no. of steps

C     Get current position into OLDPOS

      IF (mottype(motorno) .EQ. ACSF)THEN !Focusing motors on NG5. Get from file
         errno = 1              !default for err branch opening or reading file
         notries=20             !try up to 10 times 
         retry=.true.
         command(1:3)=motorstring(motorno)//'P'
         lcom=3	
 230     call rs232f(command,lcom,reply,lreply)
         if (reply(3:3).ne.'P'.or.lreply.eq.0)then !error in reply
	    if (retry)then
               if (notries.gt.0)then
                  notries=notries-1
               else
                  retry=.false.
               endif
c     call putcontr(' drive retrying command: '//command(1:lcom))
               call libwait(.5,status)
               goto 230
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
c     call putcontr(' drive decode needs to retry command: '//command(1:lcom))
               call libwait(.5,status)
               goto 230
	    else
               errno=2
               goto 750
      	    endif
         ENDIF
      ELSE         !ACS: standard ACS motor controllers. Send out P command
         notries=20             !try up to 10 times 
         retry=.true.
         command(1:3)=motorstring(motorno)//'P'
         lcom=3	
 130     call rs232(command,lcom,reply,lreply)
         if (reply(3:3).ne.'P'.or.lreply.eq.0)then !error in reply
	    if (retry)then
               if (notries.gt.0)then
                  notries=notries-1
               else
                  retry=.false.
               endif
c     call putcontr(' drive retrying command: '//command(1:lcom))
               call libwait(.5,status)
               goto 130
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
c     call putcontr(' drive decode needs to retry command: '//command(1:lcom))
               call libwait(.5,status)
               goto 130
	    else
               errno=2
               goto 750
      	    endif
         ENDIF
      ENDIF                     !type .EQ. ACS


C     Get change in steps. Compare to acceptable err. 
C     Imove gets steps for ACSF since only incremental drive possible. Imove gets  
C     position for other controllers so a drive to absolute position can be done.
      ichange = newpos-oldpos   ! get change in pulses
      IF (ABS(ichange) .LE. merr(motorno)) RETURN ! Return if motor there
      moving(motorno) = .TRUE.
      destination(motorno) = targetangle
      dobacklash = (((ichange .LT. 0) .AND. (back(motorno) .GT. 0))
     1     .OR. ((ichange .GT. 0) .AND. (back(motorno) .LT. 0)))

      IF (dobacklash) THEN      ! need to overshoot so end going positive
         newpos = newpos-back(motorno) ! back is # steps to overshoot
         overshot(motorno) = .TRUE. ! set flag to indicate intermediate target
      ELSE
         overshot(motorno) = .FALSE.
      END IF
      imove = iabs(newpos)
      IF (newpos.LT.0)THEN
         dirc='-'        !For ACS, this is sign of new position, not direction!
      ELSE
         dirc='+'
      ENDIF
      
C     Write number of steps or position to character string
      nsteps = 1 
      DO WHILE (10**nsteps .LE. imove) ! imove.lt.10**9
         nsteps = nsteps + 1
      END DO
      WRITE(size, FMT= '(i1)', iostat=io_stat) nsteps
      IF (io_stat .NE. 0) GOTO 750
      integer_fmt='(i'//size//')'
      WRITE(stepstring, FMT= integer_fmt, iostat=io_stat) imove 
      IF (io_stat .NE. 0) GOTO 750 !quit if error

C     Set up command string 

      command = motorstring(motorno) //'G'//dirc// stepstring(1:nsteps)
      lcom = 4 + nsteps


C     Send command string out to motor to start moving & treat reply
      if(trackmoves)call memcurpos(motorno,1,targetangle)
      IF (mottype(motorno) .EQ. ACSF)THEN !Focusing motors on NG5. ichange<-steps
         CALL Rs232F(command,lcom,reply,lreply)
         IF (reply(1:2) .NE. motorstring(motorno))THEN !Do not change pos in file
	    CALL Putcontr(' Uncertain if motor '//motorstring(motorno)//
     1           ' moved ')
	    RETURN
         ELSE                   !valid reply so change position if file	
	    OPEN(UNIT=2,FILE=MOTORS_BUF,STATUS='OLD',RECL= MOTBUF_RECL,
     1           ACCESS='DIRECT', FORM='UNFORMATTED',err=780)
	    READ (2,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
     1           back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
     1           vmot2(motorno), vmot3(motorno), vmot4(motorno)
            vmot4(motorno) = float(newpos)
	    WRITE (2,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
     1           back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
     1           vmot2(motorno), vmot3(motorno), vmot4(motorno)
	    CLOSE(2)
         ENDIF
      ELSE                      !ACS: standard ACS motor controllers.
         gocom=lcom
         gocommand(1:gocom)=command(1:lcom)
         goretry=.true.
 600     CALL RS232(gocommand, gocom, reply, lreply) !send out go command
         if (lreply.ne.2)then   !see if valid reply, if not:
	    notries=10          !retry a status command up to 10 times
 	    command(1:3)=motorstring(motorno)//'E'
	    lcom=3	

 	    call rs232(command,lcom,reply,lreply) !get status
	    do while ((notries.gt.0).and.(lreply.eq.0.or.reply(8:8).ne.'A'))
               call libwait(.1,status)
               call rs232(command,lcom,reply,lreply)
               if (lreply.lt.0)return !control a detected
               notries=notries-1
	    enddo
	    if (lreply.eq.0.or.reply(8:8).ne.'A')then !no good reply after 10 tries
               call putcontr(' Motor '//motorstring(motorno)//
     1              ' did not reply. Position should be OK.')
               RETURN
	    endif
	    if (reply(9:9).eq.'0')then !motor not moving
               if (goretry)then
                  goretry=.false.
                  call libwait(.1,status)
                  goto 600      !try sending G command one more time in case never moved
               endif            !if no goretry, just assume ok
	    endif
         ENDIF                  !lreply ne 2
      ENDIF                     !choice on type of controller
      RETURN

C     Treat errors
 750  Write(Unit=errstr,fmt=*,iostat=io_stat)errno
      CALL Putcontr(' Error no. '//errstr//' moving motor: '//motorstring(motorno))
      if (errno.eq.1.or.errno.eq.2) 
     1	   CALL Putcontr(' ***** MOTOR '//motorstring(motorno)//' NOT MOVED *****')
c     CALL setef(camerr)                             ! set error flag
      IF (nsta .EQ. 8)CALL setef(contr_a)
      RETURN
 780  CALL Putcontr(' Error updating position of motor '//motorstring(motorno)//
     1     ' in file')
      CLOSE(1)
      RETURN

      END
