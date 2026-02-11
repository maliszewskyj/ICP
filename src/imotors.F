#include "icp.h"
	!OPTIONS/extend_source
c SETUP_MOTORS: Set up automatic and manual motor movement parameters
c   (baserate,slewrate,ramp, conversion constant,rate division factor)
c   in MCU registers then write to non_volatile memory.
c
c
	use instr
	use motors
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_cnf.h"
#include "c_debugval.h"
#include "c_pi.h"
#include "c_vme.h"
#include "c_rs232_acs.h"
	end interface
	
	character*80 command,reply
	integer*4 lcom,lreply
	integer*4 io_stat,motorno,nslew,nbase,slewrate,baserate,
     1	  ramprate,pulsedeg,npulse
	character*4 integer_fmt
	character*1 size,rampstring
	character*10 basestring,slewstring,pulsestring
	integer*4    i
	character*80 gethome

C  ascf variables
	real*4 brate,trate,rdum
	integer*4  index, ios,idum,iargc
	logical	    topped
	character*2  str2, mstr2
	character*3   str3
	character*80 arg
	real*4 tabk0(22)/625.,568.2,520.8,500.0,446.4,416.7,357.1,
     1		312.5, 277.8, 250.0, 208.3, 178.6, 156.2, 138.9,
     1	        125.0, 100.0, 83.3, 71.4, 62.5, 55.5, 50.0, 49.0/
	integer*4 kindex(22)/20,22,24,25,28,30,35,40,45,50,60,70,
     1		80,90,100,125,150,175,200,225,250,255/

C  PI variables

c
c
C *** CLEAR MOTOR FLAGS
	DO  I=1,MAXMOTS
	  MOTFL(I)=.FALSE.
	ENDDO

	IF (iargc .gt. 1) THEN
	   call getarg(1,arg)
	   IF (index(arg,'-debug') .gt. 0) CALL DEBUGLEVEL(1)
	ENDIF


	call rdfiles
C	call debuglevel(1)
	if (.not.priv)THEN
	  print *,' This program must be run from instrument control terminal!'
	  CALL EXIT
	ENDIF
	vmedata = .false.
	if (nsta.eq.-11.or.nsta.eq.-7.or.nsta.eq.8.or.nsta.eq.9.or.
	1    nsta.eq.1.or.nsta.eq.-1.or.nsta.eq.-5) vmedata=.true.

	CALL RS232_CONF
	IF (mot_line(acs).ne.-1) CALL RS232_Init
	if (mot_line(PC) .gt.0) then
	   IF (vmedata) THEN 
C	      CALL RS232V_INIT
	   ELSE
C	      CALL RS232PC_INIT
	   ENDIF	   
	endif
	if (nsta.eq.5)    call RS232P_INIT
	if (nsta.eq.-5)   call RS232F_INIT

c  set value of camerr (not used) since it could be set by init_motors
	camerr = 0 !dummy value of camerr in case init_motors sets it.
	call init_motors

	DO motorno=1,MAXMOTS !Set motion parameters for each motor
	  IF (motfl(motorno)) THEN !If motor defined as in use
	   IF (mottype(motorno) .EQ. ACS) THEN
	     baserate=base(motorno)
	     slewrate=top(motorno)
	     ramprate=ramp(motorno)
	     pulsedeg=pulse(motorno)
c
c         Set up character strings with appropriate parameters & send out RS232
c
c         Write baserate to base register as well as jog register 
	     nbase=1
	     do while (10**nbase.le.baserate)  !base.lt.10**9
	       nbase=nbase+1
	     enddo
	     write(size,60,iostat=io_stat)nbase
	     if (io_stat.ne.0)goto 750
 60	     format(i1)
	     integer_fmt='(i'//size//')'
	     write(basestring,fmt=integer_fmt,iostat=io_stat)baserate 
	     if (io_stat.ne.0)goto 750
	     command=motorstring(motorno)//'B='//basestring(1:nbase)
	     lcom=4+nbase
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
	     command=motorstring(motorno)//'J='//basestring(1:nbase) !set Jog speed for manual
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
c
c         write top speed (slewrate) to velocity register for auto (01) & manual (00)
	     nslew=1 !slew rate
	     do while (10**nslew.le.slewrate) !slew .lt.10**9
	       nslew=nslew+1
	     enddo
	     write(size,60,iostat=io_stat)nslew
	     if (io_stat.ne.0)goto 750
	     integer_fmt='(i'//size//')'
	     write(slewstring,fmt=integer_fmt,iostat=io_stat)slewrate !slewrate(1:nslew)<-slew
	     if (io_stat.ne.0)goto 750
	     command=motorstring(motorno)//'V01='//slewstring(1:nslew) !auto mode
	     lcom=6+nslew
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
	     command=motorstring(motorno)//'V00='//slewstring(1:nslew) !manual mode
	     lcom=6+nslew
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
c
C         write ramp for auto (01) and manaul (00)
	     write(rampstring,60,iostat=io_stat)ramprate !ramp .lt. 10
	     if (io_stat.ne.0)goto 750
  	     command=motorstring(motorno)//'R01='//rampstring !for auto mode
	     lcom=7
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
  	     command=motorstring(motorno)//'R00='//rampstring !for manual mode
	     lcom=7
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
c
c	     write conversion constant & rate division constant
c         Write baserate to base register as well as jog register 
	     npulse=1
	     do while (10**npulse.le.pulsedeg)  
	       npulse=npulse+1
	     enddo
	     write(size,60,iostat=io_stat)npulse
	     if (io_stat.ne.0)goto 750
	     integer_fmt='(i'//size//')'
	     write(pulsestring,fmt=integer_fmt,iostat=io_stat)pulsedeg 
	     if (io_stat.ne.0)goto 750
	     command=motorstring(motorno)//'C='//pulsestring(1:npulse)
	     lcom=4+npulse
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
	     command=motorstring(motorno)//'K=1' !No rate division
	     lcom=5
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif

c	     write all motor parameters to non-volatile memory
	     command=motorstring(motorno)//'A'
	     lcom=3
	     call rs232(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',motorstring(motorno)
	       goto 900 !go to next motor if error
	     endif
	     print *,' Parameters written to motor ',motorstring(motorno)
	     goto 950 !jump to endif and next motor
C            Treat errors:
 750	     print *,' Error in motor parameters for motor: ',motorno
 900	     print *,' Error setting parameters for motor ',motorstring(motorno)
 950	     continue
	  ELSEIF (mottype(motorno) .eq. PIMOT) THEN
	     baserate=base(motorno)
	     slewrate=top(motorno)
	     ramprate=ramp(motorno)
	     pulsedeg=pulse(motorno)

C	     io_stat = PISETVEL(motorno,baserate)

C	     io_stat = PISETACC(motorno,ramprate)
	     print *,' Parameters written for motor ',motorstring(motorno)
	   ELSEIF (mottype(motorno) .EQ. ACSF) THEN
	     baserate=base(motorno)
	     slewrate=top(motorno)
	     ramprate=ramp(motorno)
	     pulsedeg=pulse(motorno)

	     write(unit=mstr2,fmt='(i2)',iostat=ios)motorno
c
c         Set up character strings with appropriate parameters & send out RS232
c
c         Write baserate to base register as well as jog register 
	     nbase=1
	     do while (10**nbase.le.baserate)  !base.lt.10**9
	       nbase=nbase+1
	     enddo
	     write(size,60,iostat=io_stat)nbase
	     if (io_stat.ne.0)goto 1750
	     integer_fmt='(i'//size//')'
	     write(basestring,fmt=integer_fmt,iostat=io_stat)baserate 
	     if (io_stat.ne.0)goto 1750
	     command=mstr2//'C='//basestring(1:nbase)
	     lcom=4+nbase
	     call rs232f(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',mstr2
	       goto 1900 !go to next motor if error
	     endif
	     command=mstr2//'J='//basestring(1:nbase) !set Jog speed for manual
	     call rs232f(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',mstr2
	       goto 1900 !go to next motor if error
	     endif
c
c         write top speed (slewrate) to velocity register for auto (01) & manual (00)
	     nslew=1 !slew rate
	     do while (10**nslew.le.slewrate) !slew .lt.10**9
	       nslew=nslew+1
	     enddo
	     write(size,60,iostat=io_stat)nslew
	     if (io_stat.ne.0)goto 1750
	     integer_fmt='(i'//size//')'
	     write(slewstring,fmt=integer_fmt,iostat=io_stat)slewrate !slewrate(1:nslew)<-slew
	     if (io_stat.ne.0)goto 1750
	     command=mstr2//'V='//slewstring(1:nslew) !auto mode
	     lcom=4+nslew
	     call rs232f(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',mstr2
	       goto 1900 !go to next motor if error
	     endif
c
C         write ramp for auto (01) and manaul (00)
	     write(rampstring,60,iostat=io_stat)ramprate !ramp .lt. 10
	     if (io_stat.ne.0)goto 1750
  	     command=mstr2//'R='//rampstring !for auto mode
	     lcom=5
	     call rs232f(command,lcom,reply,lreply)
	     if (lreply.eq.0)then
	       print *,' error writing to motor: ',mstr2
	       goto 1900 !go to next motor if error
	     endif

C       Save to nonvolatile memory
	      command=mstr2//'S'
	      lcom=3
	      call rs232f(command,lcom,reply,lreply)
	      if (lreply.eq.0)then
		 print *,' error writing to motor: ',mstr2
		 goto 1900	!go to next motor if error
	      endif


	     print *,' Parameters written to motor ',mstr2
	     goto 1950 !jump to endif and next motor
C            Treat errors:
 1750	     print *,' Error in motor parameters for motor: ',motorno
 1900	     print *,' Error setting parameters for motor ',motorno
 1950	     continue
	    ENDIF !type is ACSF
	  ENDIF !motfl
	ENDDO !for each motor
c
c  Update motors.buf with current motor settings
	OPEN (UNIT= 1, FILE= trim(gethome())//'MOTORS.BUF', STATUS= 'OLD', 
     1        ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= MOTBUF_RECL)
	DO i = 1, MAXMOTS
	  IF (motfl(i)) THEN !if defined as inuse
	   READ (1,REC=i)idum, idum,idum, idum, 
	1                idum,idum,idum, rdum,
	1                rdum, rdum, vmot4(i)
	    WRITE (1,REC=i) base(i), ramp(i), top(i), pulse(i), back(i),
     1		merr(i), mottype(i), vmot1(i),vmot2(i),vmot3(i),vmot4(i)
	  ENDIF
	END DO
	CLOSE (1)
	end
