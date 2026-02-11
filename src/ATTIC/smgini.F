#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Smgini
C	*****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE SCREEN MANAGEMENT                                 C
C     Called from: ICP_MAIN                                                   C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT   none

#if defined(VMS)
	INTEGER*2  chan, func_code
	INTEGER*4  contr_mask, status

	EXTERNAL   Contr_Ast
	integer*4 logbook
	common/newscreen/logbook
	INCLUDE SMGPREP_CMN
	INCLUDE INSTR_CMN
	INCLUDE REFL_CMN
	INCLUDE DARTZ_CMN
	INCLUDE '($TRMDEF)'
	INCLUDE '($IODEF)'


C--------------------- SCREEN MANAGEMENT INITIALISATION ----------------------C
C                                                                             C
C     Virtual displays, keyboards and pasteboards:                            C
C                                                                             C
C     keyb     Keyboard, thats where you type                                 C
C     paste    Pasteboard, links virtual displays and physical screen         C
C                                                                             C
C     main     Main display with title, version, time and date.               C
C     menu     Main-menu, 4 options                                           C
C     iorq     Menu to select which buffer, 3 options, (4 on BT4)	      C
C     xord     Menu to select which buffer on bt8, 2 options,    	      C
C     pola     Display for polarization setup                                 C
C     prpi     Display for I-buffer editing                                   C
C     prpq     Display for Q-buffer editing                                   C
C     prpb     Display for B-buffer editing                                   C
C     prpt     Display for T-buffer editing                                   C
C     menui    Menu with buffer-contents to select for editing and sequencing C
C     menuq    Menu with buffer-contents to select for editing and sequencing C
C     menub    Menu with buffer-contents to select for editing and sequencing C
C     menut    Menu with buffer-contents to select for editing and sequencing C
C     moni     Display for automonitor utility                                C
C     logbook  Display for sample information screen                          C
C     ascan    Display for automatic selection of changes in q space          C
C     sequ     Display to define a run sequence                               C
C     contr    Display for instrument control (similar as MOP in old vers.    C 
C     stat     Status line in control mode                                    C
C     help     Help-display for help and hints everywhere in the program      C
C     ctrly    Interrupt window for contr_Y                                   C
C     hfdisp   Display for magnet scan  setup                                 C
C                                                                             C
C-----------------------------------------------------------------------------C


	WRITE (5, FMT= *) CHAR(27), '[=0g'            ! Reset tab stops to ev 8
                                                      ! works only on HDS term.
	m$_nenr   = TRM$M_TM_NOEDIT .OR. TRM$M_TM_NORECALL
	m$_nenrcl = TRM$M_TM_NOEDIT .OR. TRM$M_TM_NORECALL .OR. TRM$M_TM_CVTLOW

	CALL SMG$CREATE_VIRTUAL_KEYBOARD (keyb)
	CALL SMG$CREATE_PASTEBOARD (paste)
	CALL SMG$CREATE_VIRTUAL_DISPLAY (3, 14, menu, SMG$M_BORDER)
	IF (nsta .EQ. 4) THEN
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (4, 18, iorq, SMG$M_BORDER)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menui)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpi)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, menuq)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, prpq)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (7, 80, menub)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpb)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, menut)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, prpt)
	ELSE IF (nsta .EQ. -5) THEN
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (4, 18, iorq, SMG$M_BORDER)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menui)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpi)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, menuq)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, prpq)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (7, 80, menub)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpb)
	ELSE IF (nsta .EQ. -7) THEN
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menur)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpr)
	ELSE IF (nsta .EQ. 1) THEN
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, menui)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, prpi)
	ELSEIF (nsta .EQ. 8) THEN
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (4, 18, xord, SMG$M_BORDER)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menux)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpx)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menud)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpd)
	ELSE
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (3, 18, iorq, SMG$M_BORDER)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, menuq)
	   CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 80, prpq)
	   IF (nsta .EQ. 2 .or. nsta .EQ. 9 .or. nsta .EQ. -1)THEN
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (7, 80, menub)
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpb)
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menui) !mot 1 present
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 80, prpi)
	   ELSE
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (8, 80, menub)
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (11, 80, prpb)
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (9, 80, menui)
	     CALL SMG$CREATE_VIRTUAL_DISPLAY (11, 80, prpi)
	   ENDIF
	END IF
	CALL SMG$CREATE_VIRTUAL_DISPLAY (18, 78, main, SMG$M_BORDER)
	CALL SMG$CREATE_VIRTUAL_DISPLAY ( 4, 80, help)
	CALL SMG$CREATE_VIRTUAL_DISPLAY ( 7, 80, sequ)
	CALL SMG$CREATE_VIRTUAL_DISPLAY ( 1, 80, stat)  
	CALL SMG$CREATE_VIRTUAL_DISPLAY (19, 80, contr)
	CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 50, moni, SMG$M_BORDER)
	CALL SMG$CREATE_VIRTUAL_DISPLAY (7,79,logbook, SMG$M_Border)
	CALL SMG$CREATE_VIRTUAL_DISPLAY (12, 50, ascan, SMG$M_BORDER)
	CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 33, pola, SMG$M_BORDER)
C	CALL SMG$CREATE_VIRTUAL_DISPLAY (10, 33, hfdisp, SMG$M_BORDER)
	CALL SMG$CREATE_VIRTUAL_DISPLAY ( 5, 30, ctrly, SMG$M_BORDER)
   	CALL PUTCHARS (ctrly, 'Contr_Y has been disabled', 2, 3)
	CALL PUTCHARS (ctrly, 'Use Contr_A instead', 4, 6) 
	CALL SMG$SET_KEYPAD_MODE (keyb, 0)



C--------------- ENABLE OUT OF BAND CHARS ^A, ^E, ^P, ^W & ^Y ----------------C


	contr_mask = 2**SMG$K_TRM_CTRLA + 2**SMG$K_TRM_CTRLP +
	1            2**SMG$K_TRM_CTRLW + 
	2	     2**SMG$K_TRM_CTRLY + 2**SMG$K_TRM_CTRLC
	IF (nsta .NE. 8)contr_mask = contr_mask + 2**SMG$K_TRM_CTRLF 
	IF (priv)
     1		CALL SMG$SET_OUT_OF_BAND_ASTS (paste, contr_mask, Contr_Ast)
#else

      integer retn,CATCH

      INCLUDE INSTR_CMN    ! module addresses for this instrument
      INCLUDE ICPSIG_CMN

      sigint = 0
      sigquit = 0
      sigtstp = 0
      sigterm = 0

	IF (priv) then
	  retn = CATCH()
c	  write(*,fmt='(a,i)') 'Installed signal handler:',retn
	  sigterm = 0
	endif

#endif
	END      
