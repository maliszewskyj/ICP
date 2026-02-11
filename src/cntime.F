#include "icp.h"
	SUBROUTINE CnTime (time)
C     	*************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to setup and start timer when a count with time is initiated    C
C     Called from: Parsec, Automon                                            C
C                                                                             C
C     Gayle Greene, Beat Schmid,  July 1989                                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use asd
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_tcp_aim.h"
#include "c_vme.h"
#include "c_amscaler.h"
	end interface
	INTEGER*2   idata(0:1)
	INTEGER*4   i4data,d3/3/,d0/0/
	EQUIVALENCE (i4data, idata(0))
      
	REAL*4      time           ! at call, contains time to count in seconds

C  xray variables
	INTEGER*4    itime, mode, io_stat, secwait

C Misc variables not used for XRAY
	INTEGER*4   null4_data,lreg
	INTEGER*2   nscaler, ascaler      ! var for looping through all scalers

	INTEGER*4 btime(2)                              ! time in binary format
	INTEGER*4 status                     ! status variable for system calls

C-------------- Clear HISTO and ROUTER -------------------------------------- C
	IF (PSD) THEN
	   IF (aimpsd) THEN
	      CALL AIM_CLEAR()
	   ENDIF
	ENDIF
	IF (USE_ASD .AND. AIMPSD) CALL AIM_CLEAR()

	CALL ABS_TIME (startsecs)	
	CALL DATE_TIME (start_time)
	IF (USE_ASD .AND. AIMPSD) THEN
	   CALL AIM_ARM()
	ENDIF
	IF (PSD .AND. AIMPSD) CALL AIM_ARM()
	IF (amscaler) THEN ! AM9513
	  t_count = .TRUE.
	  counting = .TRUE.            ! set flag to indicate counting
	  mode = 0                   ! reuse variable for mode=time
	  itime = int(time * 100)      ! calculate preset
	  secwait = itime / 100        ! calculate time to sleep (just in case)
	  CALL SCALERSTART(itime,mode)
 	ELSE IF (vmedata) THEN ! BT4, BT5
 	   t_count = .TRUE.            
 	   counting = .TRUE.           ! set flag to indicate counting
 	   mode = 0                  ! reuse variable for mode=time
 	   itime = int(time)
	   IF (nsta.eq.1) THEN
	      mode=0
	      IF (monobt1 .eq. 'GE311') THEN
		 mode=1
	      ELSEIF (monobt1 .eq. 'GE733') THEN
		 mode=2
	      ENDIF
	      CALL VSCALERSELECT(mode)
	   ENDIF

	   mode = 0
 	   CALL VSCALERSTART(itime,mode)
  	ELSE
          CALL Putcontr(
	1	' Cntime called for other than xray,bt7 or vmedata')
 	ENDIF

 999  	RETURN
	END
