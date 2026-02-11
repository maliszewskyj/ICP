#include "icp.h"
	FUNCTION RdAngl(motorno)
C	************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Angle: Read current position of motor indicated                         C
C	Called from: Parse_Contr                                                C
C                                                                             C
C	Gayle Greene, Beat Schmid, August 1989                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	IMPLICIT     none
	interface
#include "c_pi.h"
#include "c_vme.h"
#include "c_zeta.h"
#include "c_ez.h"
#include "c_libwait.h"
#include "c_rs232_acs.h"
	end interface
	REAL*4       RdAngl
	INTEGER*4    motorno, io_stat, oldpos, secwait, status, oldzero
	REAL*4       rpos,vpos
	CHARACTER*80 command, reply
	INTEGER*4    lcom, lreply, notries, errno
	logical      retry,reretry,rserr, errflag, readef
	character*2  errstr
	CHARACTER*80 GETHOME

	IF (mottype(motorno) .EQ. VIRT)THEN
	   CALL Calc_VirtPos(motorno,vpos,errflag)
	   IF (errflag)THEN 
	      call putcontr(' Err calc virt pos for motor: '//motorstring(motorno))
	      RdAngl = -999.99	! signal error?
	   ELSE			!errflag not set
	      RdAngl = vpos
	   ENDIF
	   Return
	ENDIF			!Virt motor
	IF (mottype(motorno) .EQ. ACSF)THEN !Focusing on NG5
	   notries=2		!try up to 2 times 
	   retry=.true.
	   command(1:3)=motorstring(motorno)//'P'
	   lcom=3	  
 330	   call rs232f(command,lcom,reply,lreply)
	   if (reply(3:3).ne.'P'.or.lreply.eq.0)then !error in reply
	      if (retry)then
		 if (notries.gt.0)then
		    notries=notries-1
		 else
		    retry=.false.
		 endif
c       call putcontr(' drive retrying command: '//command(1:lcom))
		 call libwait(.5,status)
		 goto 330
	      else
		 errno=1
		 goto 750
	      endif
	   endif

	   READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) oldpos
	   IF (io_stat .NE. 0) then
	      if (retry)then
		 retry=.false.
c       call putcontr(' drive decode needs to retry command: '//command(1:lcom))
		 call libwait(.5,status)
		 goto 330
	      else
		 errno=2
		 goto 750
	      endif
	   ENDIF
	   RdAngl = FLOAT(oldpos)/pulse(motorno)
C       Physik Instrumente precision motor controllers
	ELSEIF (mottype(motorno) .eq. PIMOT) THEN
	   notries = 2
	   io_stat = -1
	   do while ((notries .gt. 0) .and. (io_stat.lt.0))
	      io_stat = PIGETPOS(motorno,oldpos)
	      notries = notries - 1
	   enddo
	   if (io_stat .lt. 0) then
	      errno = 2
	      goto 750
	   endif
	   RdAngl = FLOAT(oldpos)/pulse(motorno)
C       VME-based Oregon Microsystems OMS58 Controller
	ELSEIF (mottype(motorno) .eq. VME) THEN
	   notries = 2
	   io_stat = -1
	   do while ((notries .gt. 0) .and. (io_stat.lt.0))
	      io_stat = VMEGETPOS(motorno,RdAngl)
	      notries = notries - 1
	   enddo
	   if (io_stat .lt. 0) then
	      errno = 2
	      goto 750
	   endif
C       Standalone Parker Compumotor
	ELSEIF (mottype(motorno) .eq. ZETA) THEN
	   notries = 2
	   io_stat = -1
	   do while ((notries .gt. 0) .and. (io_stat.lt.0))
	      io_stat = ZETAGETPOS(motorno,oldpos)
	      notries = notries - 1
	   enddo
	   if (io_stat .lt. 0) then
	      errno = 2
	      goto 750
	   endif
	   RdAngl = FLOAT(oldpos)/pulse(motorno)
C       Allmotion EZ10 motor controller
	ELSEIF (mottype(motorno) .eq. EZ) THEN
	   notries = 2
	   io_stat = -1
	   do while ((notries .gt. 0) .and. (io_stat.lt.0))
	      io_stat = EZGETPOS(motorno,rpos)
	      notries = notries - 1
	   enddo
	   if (io_stat .lt. 0) then
	      errno = 2
	      goto 750
	   endif
	   RdAngl = rpos/pulse(motorno)
	ELSEIF (mottype(motorno) .eq. PICO) THEN
	   call readlastpos(motorno, rpos, vpos)
	   RdAngl = rpos

C       Standard Advanced Control Systems controller
	ELSE			!standard ACS
	   reretry = .true.
c       Get current position using P:
	   notries=2		!try up to 2 times 
	   retry=.true.
	   command(1:3)=motorstring(motorno)//'P'
	   lcom=3	  
 230	   call rs232(command,lcom,reply,lreply)
	   if (reply(3:3).ne.'P'.or.lreply.eq.0)then !error in reply
	      if (retry)then
		 if (notries.gt.0)then
		    notries=notries-1
		 else
		    retry=.false.
		 endif
c       call putcontr(' drive retrying command: '//command(1:lcom))
		 call libwait(.5,status)
		 if (readef(contr_a)) then
		    errno=0
		    goto 750
		 endif
		 goto 230
	      else
		 errno=1
		 goto 750
	      endif
	   endif
	   if (reretry) then
	      retry=.true.
	      reretry=.false.
	   endif
	   READ(reply(5:lreply), FMT= '(i12)', iostat=io_stat) oldpos
	   IF (io_stat .NE. 0) then
	      if (retry)then
		 retry=.false.
c       call putcontr(' drive decode needs to retry command: '//command(1:lcom))
		 call libwait(.5,status)
		 goto 230
	      else
		 errno=2
		 goto 750
	      endif
	   ENDIF
	   RdAngl = FLOAT(oldpos)/pulse(motorno)
	ENDIF			!choice on controller type
	RETURN

C       Treat errors
 750    write(unit=errstr,fmt='(i2)',iostat=io_stat)errno
	call putcontr(' Err no '//errstr//' reading motor: '//motorstring(motorno))
    	RdAngl = -999.99	! signal error?
	RETURN


 780	CALL putcontr(' Err reading position of motor '//motorstring(motorno)//
	1    ' from file')
	RdAngl = -999.99	! signal error?
	RETURN

	END

