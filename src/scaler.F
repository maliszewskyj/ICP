#include "icp.h"
	SUBROUTINE Scaler (counts)
C     	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       Routine to wait for scaler to finish count then readout scaler and      C
C       monitor contents.                                                       C
C       Called from: Parse_Contr, Run_I                                         C
C       C
C       Beat Schmid, July 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use asd
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_vme.h"
#include "c_amscaler.h"
#include "c_tcp_aim.h"
	end interface
	INTEGER*4    counts(37),io_stat
	LOGICAL	     READEF

C       misc variables not used on xray
	INTEGER*4    state, alarm(2), status



C--------------------------HIBERNATE IF COUNTING ----------------------------C

 50	io_stat = 1		! poll counters until they are finished
	if (vmedata) then
	   CALL VSCALERSTATUS(io_stat) ! Use VME scalers
	else if (amscaler) then
	   CALL SCALERSTATUS(io_stat) ! Use AM9513 type scalers
	else 
! Some other scaler type
	endif
	if (io_stat .ne. 1) THEN
	   counting = .FALSE.
	   CALL ABS_TIME (stopsecs)
	endif

C--------------------------PROCESS IS AWAKE AGAIN ---------------------------C

 100  	IF (.NOT. counting) THEN ! ast executed

	   IF (nsta .EQ. -5 .AND. use_asd)THEN  
	      IF (m_count)CALL asd_off	
	      CALL Read_asd
	   ENDIF		

	   IF (vmedata.OR.(m_count .OR. t_count).OR. amscaler) CALL ReadScl (counts) ! Read scaler(s)
	   IF (.NOT. (vmedata .or. amscaler) .AND. (.NOT. m_count .or. nsta .eq. -7))
	1	CALL ReadMon (counts) ! Read monitor
	   IF (PSD.or.use_asd.and..not.nsta.eq.-5) Call Readhisto	
	   GOTO 200
	ELSE 
 	   IF (READEF (contr_a)) THEN
 	      IF (vmedata) THEN
 		 CALL VSCALERSTOP
 	      ENDIF
	      IF (aimpsd) CALL AIM_DISARM() ! Stop area detector just in case
	      GOTO 200
	   ELSE
	      GOTO 50
	   END IF
	END IF             

C---------------------------RESET COUNTING FLAGS ----------------------------C

 200  	m_count = .FALSE.
	t_count = .FALSE.
	RETURN
	END

