#include "icp.h"
      SUBROUTINE Calc_RealPos(motorno,targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calc_RealPos: Calculate positions for real motors associated with 
C       virtual motor motorno. Put positions in list realpos                
C                                                                             C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      INTEGER*4	motorno
      REAL*4		targetangle
      LOGICAL		errflag

C     Local Variables
      INTEGER*4	nummots, lpointer


      errflag = .FALSE.         !default is positions calculated OK
C     Calculate positions of real motors represented by virtmot. Put in realpos list.
c     IF (virtpos(motorno) .EQ. targetangle) RETURN !if eq, then already done
c     Always recalculate before moving since once of associated motors may
C     have moved since calculation for limit check.

      IF (nsta .eq. 8) THEN
         IF (motorno .EQ. 21) THEN
	    CALL V21_BT8 (targetangle,errflag)
         ELSEIF (motorno .EQ. 22) THEN
	    CALL V22_BT8 (targetangle,errflag)
         ELSEIF (motorno .EQ. 23) THEN
	    CALL V23_BT8 (targetangle,errflag)
         ELSEIF (motorno .EQ. 24) THEN
	    CALL V24_BT8 (targetangle,errflag)
         ELSEIF (motorno .EQ. 25) THEN
	    CALL V25_BT8 (targetangle,errflag)
         ELSE
	    errflag = .TRUE.
         ENDIF
      ELSE IF (nsta.eq.9) THEN
         IF (motorno .EQ. 24) THEN
	    CALL V24_BT9 (targetangle,errflag)
         ELSEIF (motorno .EQ. 25) THEN
	    CALL V25_BT9 (targetangle,errflag)	
         ELSE
	    errflag = .TRUE.
         ENDIF

      ELSE
         errflag = .TRUE.
      ENDIF

      RETURN

      END
