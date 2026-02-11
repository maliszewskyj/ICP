#include "icp.h"
	SUBROUTINE Timer_Ast     
C     	********************
C probably should not use libsignal subroutine here to keep recursive      
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine Executed When Timer Finishes                                 C
C     Note: No Lams Were Enabled For This Type Of Counting                    C
C                                                                             C
C     Gayle Greene, Beat Schmid, July 1989                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT   none

	counting = .FALSE.                  ! clear flag to signal main program

	END

      
      
	SUBROUTINE Preset_Lam      
C     	*********************
      
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine Executed When Preset Counter Reaches Zero                    C
C                                                                             C
C     Gayle Greene, Beat Schmid, July 1989                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT  none
	INTEGER*4 status, SYS$WAKE, d2/2/,d0/0/

C--------------------------- VERIFY EXPECTING AST ----------------------------C

	IF (.NOT. counting) RETURN
      
	counting = .FALSE.                      ! clear flage for main program

	RETURN
	END


	SUBROUTINE Dsp_Lam      
C     	******************
      
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine Executed When DSP scaler overflows                           C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
	INTEGER*4  null4_data
	INCLUDE INSTR_CMN
	INCLUDE CAMAC_CMN
	INCLUDE CAUSER_INC
      
      

	END
