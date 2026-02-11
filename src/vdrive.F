#include "icp.h"
      SUBROUTINE VDrive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     VDrive: Move Motors associated with virtual motor motorno to angles
C        corresponding to appropriate functions of targetangle                C
C     Actual moves are accomplished by calling drive for each individual motorC
C     Yield should be called (by calling routine) as usual 
C                                                                             C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use smg_prep
      use virtmot
      IMPLICIT     none

C     Calling parameters
      INTEGER*4	motorno
      REAL*4		targetangle

C     Local Variables
      INTEGER*4	nummots, lpointer, i, ios, motno
      LOGICAL		errflag
      CHARACTER*2	str2
      REAL*4		rpos

C     External functions
      LOGICAL		inlimits


      errflag = .FALSE.         !already calulated
      IF (nsta .ne. 8 .and. nsta.ne.9)THEN
         errflag = .TRUE.
      ELSE
         CALL CALC_REALPOS(motorno,targetangle,errflag) !go calc pos of real motors
      ENDIF

      IF (errflag)THEN
         write(unit=str2,fmt='(I2)',iostat=ios)motorno
         CALL PUTCONTR(' Err calculating positions for virtual motor '//str2)
         IF (nsta .EQ. 8)CALL setef(contr_a)
      ELSE
         nummots = numreal(motorno)
         lpointer = pntlist(motorno)

C     Note: If BT4 uses virt motors, calc of z might have to be added in.
         do i = lpointer, lpointer + nummots - 1
            motno = motlist(i)
            rpos = poslist(i)+z(motno)
            IF (inlimits(motno,rpos,contr))THEN
               CALL DRIVE (motno,rpos)
               CALL YIELD
            ELSE
               CALL Putcontr(' Virtual Drive aborted to avoid exceeding limit')
               CALL setef(contr_a)
            ENDIF
         enddo
      ENDIF                     !errflag

      RETURN

      END
