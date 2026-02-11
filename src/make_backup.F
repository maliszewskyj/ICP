#include "icp.h"
	SUBROUTINE Make_Backup (fname)
C	******************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Subroutine to copy a datafile to the strad                            C
C	Called from: Run_I_Buf, Run_Q_Buf, Run_T_Buf                          C
C                                                                             C
C	Beat Schmid, February 1991                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT	none
	CHARACTER*12    fname
	CHARACTER*3     fcap
	CHARACTER*80    bstr
	INTEGER*4       lbstr

	call upcase(fcap,fname(10:12))
	bstr(1:)=fcap(1:3)//':FINISH'
	lbstr = 10
	call gobroad(lbstr,bstr)

	RETURN	
	END
