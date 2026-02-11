#include "icp.h"
      include COMPILE_OPTS
      SUBROUTINE Sclinh
C     *****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Inhibit all scalers & preset counters via output register               C
C     Called from: Scaler                                                     C
C                                                                             C
C     Gayle Greene, Beat Schmid,  July 1989                                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


      IMPLICIT NONE

      INCLUDE CAUSER_INC
      INCLUDE INSTR_CMN
      INCLUDE CAMAC_CMN

#if defined(VMS)
      outreg(sw_reg)  = outreg(sw_reg) .AND. inh_both1 .OR. inh_both2  
      CALL Gcam24 (cachan, crate, sw_add, sw_reg, f16, outreg(sw_reg) , camstat,
     1	noqcheck)
#endif
      END	
