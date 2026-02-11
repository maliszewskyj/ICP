#include "icp.h"
      SUBROUTINE Gcam 
C     ***************


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     This subroutine issues the KSC library CAM%% call. It checks to verify  C
C     that the transfer of information proceeded successfully and retries if  C
C     not. If the operation cannot be successfully repeated after CAMTRIES    C
C     attempts, this routine sends messages to the terminal and sets event    C
C     flag CAMERR. This routine should not be called from an interrupt ser-   C
C     vice routine as it is not reenterable!                                  C
C     Note: This routine does not actually test for Q=1 after operation.      C
C     To do this, use If (camstat(qxsum).eq.0). (If so,q=1.)                  C
C                                                                             C
C     Called from: Camac_Init, ReadScl, ReadMon, SclInh, MonClr, CnTime,      C
C                  Count                                                      C
C                                                                             C
C     Gayle Greene, Beat Schmid,  July 1989                                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr

      IMPLICIT none
#ifdef USECAMAC
      INTEGER*2 gchan, gc, gn, ga, gf, gdata2
      INTEGER*4 gdata4, gcamstat(6)
      LOGICAL   c16
      LOGICAL*1 qflag
      INTEGER*4 itries

C------------------- ENTRY POINTS ACCORDING TO DATALENGTH --------------------C

      ENTRY Gcam16 (gchan, gc, gn, ga, gf, gdata2, gcamstat,qflag)
      c16 = .TRUE.
      GOTO 100

      ENTRY Gcam24 (gchan, gc, gn, ga, gf, gdata4, gcamstat,qflag)
      c16 = .FALSE.


C----------------------------- START OF EXECUTION ----------------------------C

 100  itries = 0

 101  IF (c16) THEN
         CALL Cam16 (gchan, gc, gn, ga, gf, gdata2, gcamstat)
       ELSE
         CALL Cam24 (gchan, gc, gn, ga, gf, gdata4, gcamstat)
      END IF
      IF (gcamstat(1) .EQ. 1) RETURN !ok

C---------- TREAT ERROR AT RETURN BY RETRYING UNTIL LIMIT REACHED ------------C

      if (gf.eq.10.and.ga.eq.3.or.gf.eq.10.and.ga.eq.4)return !ignore bira no-x
      itries = itries + 1
      IF (itries .LT. camtries) GOTO 101              ! try until limit reached


C-------------------- ABORT ATTEMPT AT EXECUTING COMMAND ---------------------C

      TYPE 200, gf, ga, gn
 200  FORMAT (' Aborting CAMAC command F',i2,' A',i2,' at N',i2)
c      CALL SETEF (camerr)                ! Set CAMAC ERROR event flag
#endif
      RETURN
      END
