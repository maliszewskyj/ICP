#include "icp.h"
	SUBROUTINE Put_Help (line)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO DISPLAY HELP FOR EACH FIELD WHEN EDITING A BUFFER            C
C     Called from: ICP_MAIN, Prepare, Parse_Prep, Updte_I_Buf, Updte_I_File,  C
C                  Updte_Q_Buf, Updte_Q_File, Def_Sequence, Control,          C
C		   Def_Sequence, Parse_RS                                     C
C                                                                             C
C     Beat Schmid,  June 1989                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use smg_prep
	CHARACTER*80 line(3)
	INTEGER i
#ifdef VMS
	CALL CURSOR_ABS (help, 2, 1)
	DO i= 1, 3
	   CALL PUTLINE (help, line(i))
	END DO
#endif
	END


