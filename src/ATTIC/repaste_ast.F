#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Repaste_AST
C     	**********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine to be called by means of an AST when the subprocess completes C
C	The previously pasted virtual displays are repasted and the paste-    C
C	board is updated.                                                     C
C                                                                             C
C	Beat Schmid, May 1990                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
		
	INTEGER*4  d_id(10), d_row(10), d_col(10), window_no, i

	INCLUDE   SMGPREP_CMN
	COMMON    /paste_info/ d_id, d_row, d_col, window_no

#if defined(VMS)
	CALL SMG$BEGIN_PASTEBOARD_UPDATE (paste)
	CALL SMG$ERASE_PASTEBOARD (paste)
	CALL SMG$CREATE_VIRTUAL_KEYBOARD (keyb)
	DO i = 1, window_no
	   CALL SMG$PASTE_VIRTUAL_DISPLAY (d_id(i), paste, d_row(i), d_col(i))
	END DO
	CALL SMG$END_PASTEBOARD_UPDATE (paste)
#endif
	END
