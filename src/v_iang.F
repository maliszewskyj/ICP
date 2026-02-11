#include "icp.h"
      SUBROUTINE V_Iang(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     V_Iang: Set Motors associated with virtual motor motorno to angles
C        corresponding to appropriate functions of targetangle                C
C     Actual display changes are accomplished by calling iang for each 
C       individual motor
C                                                                             C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      INTEGER*4	motorno
      REAL*4		targetangle

C     Local Variables
      INTEGER*4	nummots, lpointer, i, ios
      LOGICAL		errflag
      CHARACTER*2	str2

      errflag = .FALSE.
      IF (nsta .NE. 8.or.nsta.ne.9)THEN
         errflag = .TRUE.
      ELSE
C     Calculate positions of real motors represented by virtmot. Put in realpos list.
         CALL CALC_REALPOS(motorno,targetangle,errflag) !go calc pos of real motors
      ENDIF

      IF (errflag)THEN
         write(unit=str2,fmt='(I2)',iostat=ios)motorno
         CALL PUTCONTR(' Err calculating positions for virtual motor '//str2)
         IF (nsta .EQ. 8)CALL SETEF(contr_a)
      ELSE
         nummots = numreal(motorno)
         lpointer = pntlist(motorno)

C     Note: If BT4 uses virt motors, calc of z might have to be added in.
         do i = lpointer, lpointer + nummots - 1
            CALL IANG (motlist(i),poslist(i)+ z(motlist(i)))
         enddo
      ENDIF                     !errflag
      
      RETURN
      
      END
