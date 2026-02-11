#include "icp.h"
      SUBROUTINE ReadScl (counts)
C     ***************************


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     C
C     Reads the scalers                                                       C
C     Called from: Scaler                                                     C
C     C
C     Gayle Greene, Beat Schmid, July 1989                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use iso_c_binding
      IMPLICIT  none
      interface
#include "c_vme.h"
#include "c_amscaler.h"
      end interface

      INTEGER*2 nscaler, ascaler
      INTEGER*4 i, j, isdata, counts(37)
C     xray variables
      CHARACTER*80	command,reply
      INTEGER*2  lcom,lreply
      LOGICAL    rserr
      INTEGER*4  secwait,io_stat
      REAL*4	 seconds


      DO I=1,37
         COUNTS(I) = 0
      ENDDO
      if (vmedata) then
         CALL VSCALERREAD(seconds,counts)
      elseif (amscaler) then
         ! Should probably eliminate this
         CALL SCALERREAD(seconds,counts(1),counts(2))
      else
         counts(1) = 10         ! Fake monitor counts
         seconds = 1
      endif
      duration = seconds

      RETURN
      END
