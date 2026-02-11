#include "icp.h"
c
	subroutine init_motors
c
	use instr
	use motors
	implicit none
	integer*4 io_stat,motorno
	logical   using_virt
c
c
	do motorno=1,MAXMOTS
	  moving(motorno)=.false.
	enddo

	using_virt = .FALSE. !not using virtual motors by default
c
c  Set up strings for motor names for each motor
	do motorno=1,MAXMOTS
         IF (motfl(motorno)) THEN
	   IF (mottype(motorno) .EQ. ACSF)THEN !focusing motors use address
	      write(unit=motorstring(motorno)(1:2),fmt='(I2.2)',
     1		 iostat=io_stat)NINT(vmot3(motorno))
	      if (io_stat.ne.0)goto 750
	   ELSEIF (mottype(motorno) .EQ. PIMOT) THEN
	      write(motorstring(motorno)(1:2),fmt='(I2)',
	1	   iostat=io_stat)motorno
	      if (io_stat.ne.0)goto 750
	   ELSE !not focusing motors
	      write(motorstring(motorno)(1:2),fmt='(I2.2)',
     1		 iostat=io_stat)motorno
	      if (io_stat.ne.0)goto 750
	   ENDIF !focusing motors or not
         END IF
	 IF (mottype(motorno) .EQ. VIRT)using_virt = .TRUE.
	enddo !for each motor
c
	IF (using_virt)THEN
	  IF (nsta .EQ. 8)THEN
	    CALL VINIT_BT8
	  ENDIF
	ENDIF
c
c
c  translate 1 sec to binary for wait time between motor checks

	RETURN
c
 750	CALL PUTCONTR(' Error in motor parameters for motor: '//
     1		motorstring(motorno))
c	call setef(camerr) !set error flag

	RETURN
	end
