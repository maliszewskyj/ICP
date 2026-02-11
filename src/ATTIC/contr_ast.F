#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Contr_Ast (smg_info)
C     	*******************************
C Probably should not use event flag subroutines here to keep recursive
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine Executed When Contr_A, Contr_P, Contr_W, Contr_Y or          C
C     Contr_C is pressed.        					      C
C     Contr_A Interrupt Is Used To Abort Any Count, Drive, Hold Or Pause.     C
C     Contr_F Interrupt Is Used To Finish Up                                  C
C     Contr_P Interrupt Is Used To Pause The Data Acquisition.                C
C     Contr_W Interrupt Is Used To Refresh the Screen.                        C
C     Contr_Y And Contr_C Interrupts  Are Only Displaying An Error Message    C
C                                                                             C
C     Beat Schmid, February 1990                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
	INTEGER*4  context /0/, status, i, SMG$LIST_PASTING_ORDER
	INTEGER*4  d_id(10), d_row(10), d_col(10), window_no

	INCLUDE   INSTR_CMN
	INCLUDE   SMGPREP_CMN
	RECORD    /SMGDEF/ smg_info
	COMMON    /paste_info/ d_id, d_row, d_col, window_no
	EXTERNAL  Repaste_Ast

	IF (smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLA) THEN
	   CALL SYS$SETEF (%VAL(contr_A))
	   CALL SYS$CLREF (%VAL(pause))!fixes bug when ctrla too soon after ctrlp
	   CALL SYS$WAKE (,)
           CALL PUTCHARS_REV (stat, '*** CTRLA ***', 1, 15)
	   clrstat=.TRUE.
	ELSE IF (smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLW) THEN
	   CALL SMG$REPAINT_SCREEN (smg_info.SMG$L_PASTEBOARD_ID)
	ELSE IF (smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLP) THEN
	   CALL SYS$SETEF (%VAL(pause))
           CALL PUTCHARS_REV (stat, '*** CTRLP ***', 1, 15)
	ELSE IF (smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLF) THEN
           CALL PUTCHARS_REV (stat, '*** CTRLF ***', 1, 15)
	   clrstat=.TRUE.
	   finishup = .TRUE.
	ELSE IF (smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLY .OR.
	1	 smg_info.SMG$B_CHARACTER .EQ. SMG$K_TRM_CTRLC) THEN
	   CALL SMG$FIND_CURSOR_DISPLAY (paste, d_id(1))
	   CALL SMG$RETURN_CURSOR_POS (d_id(1), d_row(1), d_col(1))
	   CALL SMG$PASTE_VIRTUAL_DISPLAY (ctrly, paste, 9, 25)
	   CALL CURSOR_ABS (d_id(1), d_row(1), d_col(1))
	   CALL LIBWAIT (3.0,status)
	   CALL SMG$UNPASTE_VIRTUAL_DISPLAY (ctrly, paste)
	END IF
	END
