#include "icp.h"
	SUBROUTINE IGPIB
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Initialize CAMAC GBIP module: Enable Remote. Enable Universal &         C
C     Addressed command inhibit. Set to control mode. Set CAMAC module as     C
C     talker. Unlisten all devices.                                           C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT    none
	integer*2   i2data

#if defined(VMS)
C-------- Switch to command mode. Enable remote & Universal & Addressed ------C

	CALL Gcam16 (cachan,crate,pol_add,a0,f26,null_data,camstat,noqcheck) !command mode
	CALL Gcam16 (cachan,crate,pol_add,a1,f26,null_data,camstat,noqcheck) !enable remote
	CALL Gcam16 (cachan,crate,pol_add,a4,f26,null_data,camstat,noqcheck) !enable univ & add com inh

C--------- Set Camac module as talker & Unlisten all devices -----------------C
	i2data=64 !assumes CAMAC GPIB module is set to base address 0
	CALL Gcam16 (cachan,crate,pol_add,a0,f16,i2data,camstat,noqcheck) !set module as talker
	i2data=63 !unlisten command for all modules
	CALL Gcam16 (cachan,crate,pol_add,a0,f16,i2data,camstat,noqcheck) !unlisten all devices
#endif
	RETURN
	END
      
      
