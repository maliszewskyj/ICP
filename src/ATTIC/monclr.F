#include "icp.h"
      include COMPILE_OPTS
      SUBROUTINE Monclr
C     *****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Clear monitor lams & disable them                                       C
C     Called from: Camac_Init, Scaler                                         C
C                                                                             C
C     Gayle Greene, Beat Schmid,  July 1989                                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


      IMPLICIT none

      INCLUDE CAUSER_INC
      INCLUDE INSTR_CMN
      INCLUDE CAMAC_CMN

#if defined(VMS)
      CALL Gcam16 (cachan,crate,mon_add,a0,f10,null_data,camstat,noqcheck)   ! clr lam 0
      CALL Gcam16 (cachan,crate,mon_add,a1,f10,null_data,camstat,noqcheck)   ! clr lam 1
      CALL Gcam16 (cachan,crate,mon_add,a9,f24,null_data,camstat,noqcheck) !disable both
#endif
      RETURN
      END
