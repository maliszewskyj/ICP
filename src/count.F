#include "icp.h"
	SUBROUTINE Count (moncounts)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to setup and start preset counter when a count with monitor is  C
C     initiated                                                               C
C     Called from: Parse_Contr, Run_I_Buf, Run_Q_Buf, Run_Trash, Run_Polar    C
C                                                                             C
C     Gayle Greene, Beat Schmid, July 1989                                    C
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
	INTEGER*4   i4data, status
	EQUIVALENCE (i4data,idata(0))
	REAL*4      moncounts
	INTEGER*4    null_par /0/ ! dummy variable for CALAM
	INTEGER*4    i, null4_data, d0/0/,d3/3/,lreg
	INTEGER*2    nscaler, ascaler ! var for looping through all scalers
 	INTEGER*4 icounts, mode

	m_count = .TRUE.
	counting = .TRUE.	! set flag for main program
 	icounts = INT(moncounts)

	CALL ABS_TIME (startsecs)

	IF (PSD .AND. AIMPSD) THEN
	      CALL AIM_CLEAR()
	      CALL AIM_ARM()
	ENDIF

	IF (USE_ASD .AND. AIMPSD) THEN
	   CALL AIM_CLEAR()
	   CALL AIM_ARM()
	ENDIF

 	IF (vmedata) THEN
	   IF (nsta.eq.1) THEN
	      mode=0
	      IF (monobt1 .eq. 'GE311') THEN
		 mode=1
	      ELSEIF (monobt1 .eq. 'GE733') THEN
		 mode=2
	      ENDIF
	      CALL VSCALERSELECT(mode)
	   ENDIF
 	   mode = 1		! count for monitor
 	   CALL VSCALERSTART(icounts,mode)
        ELSE IF (amscaler) THEN
           mode = 1		! count for monitor
           CALL SCALERSTART(icounts,mode)
	END IF
	RETURN
	END
	
	
