#include "icp.h"
	SUBROUTINE Refl_TIME (numbuf,isecs,imon)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates time required to run buffer in rbuf.                         C
C       Total seconds returned in isecs if counting on time.                  C
C       Total monitor counts returned in imon if counting on neutrons         C
C     Called from: Howlong                                                    C
C                                                                             C
C     Gayle Greene, May   1993                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use refl
	use instr
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_debugval.h"
	end interface
	INTEGER*4     j, loop, ipol, isecs, imon, numbuf
	REAL*4        qznow
	LOGICAL       varimon
	REAL	      runmon
	INTEGER*4     dbg

	TYPE(r_buf) rbuf

	CALL DEBUGVAL(dbg)
	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD',
	1    ACCESS='DIRECT', RECL=RBUF_RECL,FORM= 'UNFORMATTED')
	READ (1,REC=numbuf) rbuf
	CLOSE (UNIT=1)

	IF (rbuf%mon1 .ne. 0) THEN
	  varimon = .true. !variable count time or neutrons
	ELSE
	  varimon = .false.
	ENDIF

	isecs = 0
	imon  = 0

C--------------------- COUNT-LOOP OVER NUMBER OF POINTS ----------------------C

	DO loop = 0, NINT(rbuf%pts) - 1

	   IF (varimon) THEN
             qznow = abs(rbuf%qz) + abs(rbuf%qzi) * loop
	     runmon =NINT(rbuf%mpf *
     1	      (rbuf%mon0 + (rbuf%mon1 * (qznow**rbuf%exp))))
	     if (DBG.gt.0) print *,'i=',loop,'Qz=',qznow,'time=',runmon	
	   ELSE
	     runmon = NINT(rbuf%mpf) * rbuf%mon0
	     if (DBG.gt.0) print *,'i=',loop,'time=',runmon	
	   ENDIF



	   IF (rbuf%mt .EQ. 'NEUT') THEN
	     imon = imon + runmon
	   ELSE
	     isecs = isecs + runmon
	   ENDIF

	END DO  ! (* Number of total number of points *)

	RETURN
	END
