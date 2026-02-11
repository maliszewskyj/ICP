#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Count (moncounts)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to setup and start preset counter when a count with monitor is  C
C     initiated                                                               C
C     Called from: Parse_Contr, Run_I_Buf, Run_Q_Buf, Run_Trash, Run_Polar    C
C                                                                             C
C     Gayle Greene, Beat Schmid, July 1989                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT    none
	INTEGER*2   idata(0:1)
	INTEGER*4   i4data, status
	EQUIVALENCE (i4data,idata(0))
	REAL*4      moncounts
	INTEGER*4    null_par /0/                    ! dummy variable for CALAM
	INTEGER*4    i, null4_data, d0/0/,d3/3/,lreg
	INTEGER*2    nscaler, ascaler     ! var for looping through all scalers
#if !defined(VMS)
 	INTEGER*4 icounts, mode
#endif

	INCLUDE CAUSER_INC  ! KSC parameter definition file
	INCLUDE CAMAC_CMN   ! parameter & hardware common variables
	INCLUDE INSTR_CMN   ! module addresses for this instrument
	INCLUDE ASD_CMN
      
	EXTERNAL Preset_Lam    ! Subroutine to execute when preset reaches zero
      
#if !defined(VMS)
	m_count = .TRUE.
	counting = .TRUE.                           ! set flag for main program
 	icounts = INT(moncounts)

	CALL ABS_TIME (startsecs)

	IF (PSD) THEN
	   IF (aimpsd) THEN
	      CALL AIM_CLEAR()
	      CALL AIM_ARM()
	   ELSE
c Inhibit scalers & BIRA via output registers
C	   outreg(sw_reg) = outreg(sw_reg) .AND. inh_both1 .OR. inh_both2 
	      outreg(sw_reg) = 65535
	      CALL Gcam24 (cachan, crate, sw_add, sw_reg, f16, outreg(sw_reg), 
	1	   camstat, noqcheck)

c Reset histo module and wait for memory contents to be cleared
	      CALL Gcam24 (cachan,crate,histo_add,a0,f9,d0,camstat,noqcheck) 
	      CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck)
	      Do While (.not.bjtest(lreg,2))
		 CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck) 
	      ENDDO
	      CALL Gcam24 (cachan,crate,histo_add,a0,f11,d0,camstat,noqcheck)
c       Reset lam bits in router even though lam never enabled
	      CALL Gcam24 (cachan,crate,rout_add,a0,f10,d0,camstat,noqcheck) 
	   ENDIF
	ENDIF

	IF (ASD .AND. AIMPSD) THEN
	   CALL AIM_CLEAR()
	   CALL AIM_ARM()
	ENDIF
	
 	IF (vmedata) THEN
 	   mode = 1 ! count for monitor
 	   CALL VSCALERSTART(icounts,mode)
        ELSE IF (amscaler) THEN
           mode = 1 ! count for monitor
           CALL SCALERSTART(icounts,mode)
 	ELSE
 	   call preset_lam
 	ENDIF

C-------- Write mode control register to router: start it counting
	IF (PSD.AND..not.AIMPSD) THEN
C	   outreg(sw_reg) = outreg(sw_reg) .AND. enb_both1 .OR. enb_both2   
	   outreg(sw_reg) = 0
	   CALL Gcam24 (cachan, crate, sw_add, sw_reg, f16, outreg(sw_reg),
	1	camstat, noqcheck)
	   
	   CALL Gcam24 (cachan, crate, rout_add, a3, f17, d3,
	1	camstat, noqcheck)
	ENDIF

#else      
	m_count = .TRUE.

C-------------- INHIBIT SCALERS(S) & BIRA VIA OUTPUT REGISTERS ---------------C

	outreg(sw_reg) = outreg(sw_reg) .AND. inh_both1 .OR. inh_both2 
	CALL Gcam24 (cachan, crate, sw_add, sw_reg, f16, outreg(sw_reg), 
	1            camstat, noqcheck)
      

      
C--------- CLEAR & DISABLE PRESET LAMS. THEY WILL REMAIN DISABLED ------------C

	CALL Gcam16 (cachan,crate,mon_add,a0,f10,null_data,camstat,noqcheck) ! clear lam 0
	CALL Gcam16 (cachan,crate,mon_add,a1,f10,null_data,camstat,noqcheck) ! clear lam 1
	CALL Gcam16 (cachan,crate,mon_add,a9,f24,null_data,camstat,noqcheck) ! disable both

	
C-------------- RESET INHIBIT OUTPUT ON BIRA TO NOT INHIBITING ---------------C
	IF (nsta .ne. 1)  !only BT1 does not have new bira
     1    CALL gcam16(cachan,crate,mon_add,a4,f10,null_data,camstat,noqcheck)  ! reset inhibit output to off
      
C-------------- Clear Histo and Router ---------------------------------------C
	IF (PSD) THEN
c Reset histo module and wait for memory contents to be cleared
          CALL Gcam24 (cachan,crate,histo_add,a0,f9,d0,camstat,noqcheck) ! disable
          CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck) ! disable
	  Do While (.not.bjtest(lreg,2))
            CALL Gcam24 (cachan,crate,histo_add,a0,f1,lreg,camstat,noqcheck) ! disable
	  enddo
          CALL Gcam24 (cachan,crate,histo_add,a0,f11,d0,camstat,noqcheck) ! disable
c Reset lam bits in router even though lam never enabled
          CALL Gcam24 (cachan,crate,rout_add,a0,f10,d0,camstat,noqcheck) ! disable
	ENDIF
C---------------------------- CLEAR THE SCALERS ------------------------------C

	IF (nsta .EQ. 1) THEN
	   DO i= 0, 2
	      nscaler = scl_add + i
	      CALL Gcam24 (cachan, crate, nscaler, a11, f9, null4_data, camstat,
	1	           noqcheck)
	   END DO
	ELSE                                      ! treat other kind of scalers
	   nscaler = scl_add
	   DO i= 1, scl
	      ascaler = scl_chn + i - 1
	      CALL Gcam24 (cachan,crate,nscaler,ascaler,f9,null4_data,camstat,
	1	           noqcheck)
	   END DO
	   CALL Gcam24 (cachan,crate,nscaler,a0,f10,null4_data,camstat,noqcheck)
	END IF

	
C--------------------- SET UP THE LAM ("BOOK THE LAM") -----------------------C

c	CALL Calam (cachan, crate, mon_add, lamtype, priority, preset_lam,
c	1 	    null_par, mon_add, a1, f27, mon_add, a1, f27, camstat)
c	IF (camstat(err) .NE. 1) THEN
c	   TYPE *, ' error setting up preset lam in COUNT'
c	   CALL Camsg (iand(camstat(err), 'fffffff8'x))
c	   CALL SETEF (camerr)       ! set event flag for camac error
c	END IF

	
C---------------------- WRITE MONITOR COUNTS TO PRESET -----------------------C

	i4data = NINT(moncounts) + 65536
	IF (JMOD(i4data,65536) .LT. 3)  i4data = i4data + 3
	CALL Gcam16 (cachan,crate, mon_add, a0, f16, idata(0), camstat,noqcheck)
	CALL Gcam16 (cachan,crate, mon_add, a1, f16, idata(1), camstat,noqcheck)

	counting = .TRUE.                           ! set flag for main program

      
C------------------------ ENABLE LAM 1 IN THE PRESET -------------------------C

	CALL Gcam16 (cachan,crate,mon_add, a1, f26, null_data, camstat,noqcheck)


C------- NOW REMOVE INHIBIT FROM PRESET & SCALERS VIA OUTPUT REGISTER --------C

	outreg(sw_reg) = outreg(sw_reg) .AND. enb_both1 .OR. enb_both2   
	CALL DATE_TIME (start_time)
	CALL Gcam24 (cachan, crate, sw_add, sw_reg, f16, outreg(sw_reg),
	1            camstat, noqcheck)
      
C-------- Write mode control register to router: start it counting
	IF (PSD) CALL Gcam24 (cachan, crate, rout_add, a3, f17, d3,
	1            camstat, noqcheck)

#endif
	RETURN
	END
      
      
