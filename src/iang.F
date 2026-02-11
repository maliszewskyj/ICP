#include "icp.h"
	SUBROUTINE Iang(motorno,currentpos)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Iangle: Set motor indicated to currentpos                             C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, Beat Schmid,  August 1989                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
#include "c_pi.h"
#include "c_vme.h"
#include "c_zeta.h"
#include "c_ez.h"
#include "c_pico.h"
#include "c_rs232_acs.h"
	end interface
	INTEGER*4    motorno, io_stat, nsteps, isteps, i
	REAL*4       currentpos, ipos
	CHARACTER*80 command, reply
	INTEGER*4    lcom, lreply
	CHARACTER*20 stepstring, integer_fmt
	CHARACTER*2  size
	LOGICAL	     rserr
	INTEGER*4    secwait
	CHARACTER*80 GETHOME
	
	IF (mottype(motorno) .EQ. VIRT)THEN
	   CALL V_Iang(motorno,currentpos)
	   RETURN
	ENDIF

	IF (mottype(motorno) .EQ. ACSF)THEN
	   isteps = currentpos*pulse(motorno)
	   nsteps = 1                                                 
	   DO WHILE (10**nsteps .LE. IABS(isteps)) ! isteps.lt.10**9
	      nsteps = nsteps + 1
	   ENDDO
	   IF (isteps .LT. 0) nsteps = nsteps + 1
	   WRITE(size, FMT= '(i2)', iostat=io_stat) nsteps
	   IF (io_stat .NE. 0) GOTO 550
	   integer_fmt = '(i'//size//')'
	   WRITE(stepstring, FMT= integer_fmt, iostat=io_stat) isteps 
	   IF (io_stat .NE. 0) GOTO 550
	   IF (isteps .GE. 0) THEN
	      command = motorstring(motorno) //'P=+'// stepstring(1:nsteps)
	      lcom = 5 + nsteps
	   ELSE
	      command = motorstring(motorno) //'P=-'// stepstring(2:nsteps)
	      lcom = 4 + nsteps
	   END IF
	   CALL RS232F(command, lcom, reply, lreply)
c       IF (lreply .EQ. 0) RETURN      ! return with no further action if error	
	   OPEN(UNIT=2,FILE=MOTORS_BUF,STATUS='OLD',RECL= MOTBUF_RECL,
	1	ACCESS='DIRECT', FORM='UNFORMATTED',err=780)
	   READ (2,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
	1	back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
	2	vmot2(motorno), vmot3(motorno), vmot4(motorno)
	   isteps = currentpos*pulse(motorno)
	   vmot4(motorno)=float(isteps)
	   WRITE (2,REC=motorno,err=780) base(motorno), ramp(motorno), top(motorno), pulse(motorno), 
	1	back(motorno), merr(motorno), mottype(motorno), vmot1(motorno),
	2	vmot2(motorno), vmot3(motorno), vmot4(motorno)
	   CLOSE(2)
	ELSEIF (mottype(motorno) .EQ. PIMOT) THEN !PI Motors
	   isteps = currentpos*pulse(motorno)
	   io_stat = PISETPOS(motorno,isteps)
	   if (trackmoves)call memcurpos(motorno,1,currentpos)
	ELSEIF (mottype(motorno) .EQ. VME) THEN !VME Motors
	   if (trackmoves)call memcurpos(motorno,1,currentpos)
	   io_stat = VMESETPOS(motorno,currentpos)
	ELSEIF (mottype(motorno) .EQ. ZETA) THEN !Zeta Motors
	   if (trackmoves)call memcurpos(motorno,1,currentpos)
	   isteps = currentpos*pulse(motorno)
	   io_stat = ZETASETPOS(motorno,isteps)
	ELSEIF (mottype(motorno) .EQ. EZ) THEN
	   if (trackmoves)call memcurpos(motorno,1,currentpos)
	   ipos = currentpos*pulse(motorno)
	   io_stat = EZSETPOS(motorno,ipos)
	ELSEIF (mottype(motorno) .EQ. PICO) THEN
	   call memcurpos(motorno,1,currentpos)
	ELSE			!standard ACS
	   isteps = currentpos*pulse(motorno)
	   nsteps = 1                                                 
	   DO WHILE (10**nsteps .LE. IABS(isteps)) ! isteps.lt.10**9
	      nsteps = nsteps + 1
	   ENDDO
	   IF (isteps .LT. 0) nsteps = nsteps + 1
	   WRITE(size, FMT= '(i2)', iostat=io_stat) nsteps
	   IF (io_stat .NE. 0) GOTO 550
	   integer_fmt = '(i'//size//')'
	   WRITE(stepstring, FMT= integer_fmt, iostat=io_stat) isteps 
	   IF (io_stat .NE. 0) GOTO 550
	   IF (isteps .GE. 0) THEN
	      command = motorstring(motorno) //'P=+'// stepstring(1:nsteps)
	      lcom = 5 + nsteps
	   ELSE
	      command = motorstring(motorno) //'P=-'// stepstring(2:nsteps)
	      lcom = 4 + nsteps
	   END IF
	   CALL RS232(command, lcom, reply, lreply)
c       IF (lreply .EQ. 0) RETURN      ! return with no further action if error
	ENDIF			!choice on type motor controller
	if (trackmoves)call memcurpos(motorno,1,currentpos)
	RETURN

C  Treat errors
 550	CALL PUTCONTR(' ERROR CONVERTING POSTION TO OUTPUT FORMAT')
	RETURN

 780	CALL PUTCONTR(' ERROR UPDATING POSITION IN FILE')

	END
