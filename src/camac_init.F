#include "icp.h"
      SUBROUTINE Camac_Init
C     *********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to initialize the CAMAC modules                                 C
C     Called from: Instrini                                                   C
C                                                                             C
C     Gayle Greene, Beat Schmid, July 1989                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use asd
      IMPLICIT none
#ifdef USECAMAC
      LOGICAL*4   lval,d1/1/,d0/0/,d2/2/
      INTEGER*4   i, i4data, iodata,isub, null4_data,lreg
      INTEGER*4   null_par/0/                    !used in booking Lam
      INTEGER*2   nscaler,dlamtype/1/



      IF (nsta .eq. -7.and..not.aimpsd) THEN
         CALL CAMINIT
      ELSE
         RETURN
      END IF
      
C--------------------------- INIT GPIB INTERFACE -----------------------------C

	IF (HISTO_ADD .NE. 0) THEN
C  Disable all Lams in Router and histo
          CALL Gcam24 (cachan,crate,histo_add,a0,f24,d0,camstat,noqcheck) ! disable
          CALL Gcam24 (cachan,crate,rout_add,a0,f24,d0,camstat,noqcheck) ! disable
c Reset router 
          CALL Gcam24 (cachan,crate,rout_add,a0,f9,d0,camstat,noqcheck) ! disable
c Reset histo module and wait for memory contents to be cleared
          CALL Gcam24 (cachan,crate,histo_add,a0,f9,d0,camstat,noqcheck) ! disable
          CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck) ! disable
	  Do While (.not.bjtest(lreg,2))
            CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck) ! disable
	  enddo
c Enable auto increment and front panel histo port of histo module
c          CALL Gcam24 (cachan,crate,histo_add,a0,f25,d0,camstat,noqcheck) ! disable
          CALL Gcam24 (cachan,crate,histo_add,a0,f19,d0,camstat,noqcheck) ! disable
c Write memory group register on router
          CALL Gcam24 (cachan,crate,rout_add,a0,f17,d0,camstat,noqcheck) ! disable
c Write spectrum count register on router (# time slices)
          CALL Gcam24 (cachan,crate,rout_add,a1,f17,d1,camstat,noqcheck) ! disable
c Write dwell clock register
          CALL Gcam24 (cachan,crate,rout_add,a2,f17,d0,camstat,noqcheck) ! disable
          CALL Gcam24 (cachan,crate,rout_add,a3,f17,d2,camstat,noqcheck) ! disable
	ENDIF
#endif
      END	
