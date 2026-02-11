#include "icp.h"
      SUBROUTINE readLastPos(motorno,oldpos,oldzero)
C     ***********************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Read back position information in motpos.buf for motor motorno          C
C                                                                             C
C     Gayle Greene, August 2000                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      IMPLICIT     none
      INTEGER*4    motorno, ios
      REAL*4       oldpos, oldzero
      CHARACTER*80 GETHOME

 20   OPEN (UNIT= 3, FILE= MOTPOS_BUF, STATUS= 'OLD',
     $     ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
     $     RECL=MOTBUF_RECL, IOSTAT=ios)
      if (ios .eq. 0) then
         READ (3,REC=motorno) oldpos,oldzero
         CLOSE (3)
      else
         oldpos = -999.99
         oldzero = 0.
      endif
      RETURN
      END
