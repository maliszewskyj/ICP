#include "icp.h"
      SUBROUTINE Calc_Virtpos(motorno,vpos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Calc_VirtPos: Use motorno and nsta to call proper routine to find
C    virtual position of virtual motor number motorno. Return this
C    in vpos. Set errflag if error returned from routine.
C                                                                             C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      IMPLICIT     none

C     Calling parameters
      INTEGER*4	motorno
      REAL*4		vpos
      LOGICAL		errflag

C     Local Variables
      CHARACTER*2	str2
      INTEGER*4	ios

      IF (nsta .EQ. 8)THEN
         IF (motorno .EQ. 21)THEN
	    CALL RdV21_bt8(vpos,errflag)
         ELSEIF (motorno .EQ. 22)THEN
	    CALL RdV22_bt8(vpos,errflag)
         ELSEIF (motorno .EQ. 23)THEN
	    CALL RdV23_bt8(vpos,errflag)
         ELSEIF (motorno .EQ. 24)THEN
	    CALL RdV24_bt8(vpos,errflag)
         ELSEIF (motorno .EQ. 25)THEN
	    CALL RdV25_bt8(vpos,errflag)
         ELSE
	    errflag = .TRUE.
         ENDIF                  !choice on which motor
      ELSE IF (nsta .EQ. 9) THEN
         If (motorno .EQ. 24)THEN
	    CALL RdV24_bt9(vpos,errflag)
         ELSEIF (motorno .EQ. 25)THEN
	    CALL RdV25_bt9(vpos,errflag)
         ELSE
	    errflag = .TRUE.
         ENDIF 
      ELSE                      !not bt8
         errflag = .TRUE.
      ENDIF

      IF (errflag)THEN
         write(unit=str2,fmt='(I2)',iostat=ios)motorno
         CALL PUTCONTR(' Err determining position of virtual motor '//str2)
         IF (nsta .EQ. 8)CALL setef(contr_a)
      ENDIF
      RETURN
      END
