#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE PolPrep (flipt)
C     	**************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Select The Different Polarization States                     C
C     Called from: PrepI, PrepQ, PrepB                                        C
C	At call: flipt should be bbuf(k).flip_state                           C
C                                                                             C
C     Beat Schmid, January 1991                                               C
C     Modified by Gayle Greene to make more general                           C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    typ, i, bol_und, rendition, leng, trm
	CHARACTER*1  answ
	CHARACTER*3  str3
	CHARACTER*80 help_line(3)
	LOGICAL      flag(4)
	LOGICAL      flipt(4)  !at call this should be ibuf(k).flip_state

	INCLUDE SMGPREP_CMN     ! Program specific SMG-definitions
	INCLUDE SMGCONST_CMN


C----------------------- COPY FLIPPER STATE TO LOCAL -------------------------C

	DO i = 1,4
	  flag(i)=flipt(i)
	ENDDO

C------------------------------- SETUP WINDOW --------------------------------C

	bol_und = M_BOLD .OR. M_UNDERLINE
	CALL SMG$ERASE_DISPLAY (pola, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (pola, paste, 9, 33)
	CALL PUTCHARS_REND (pola, ' Polarization Setup ', 1, 7,bol_und)
	CALL PUTCHARS (pola, 'Flipper 1', 3, 2)
	CALL PUTCHARS (pola, 'Flipper 2', 3, 13)
	CALL PUTCHARS (pola, 'Execute ?', 3, 24)
	CALL SMG$DRAW_LINE (pola, 4, 1, 4, 33)
	CALL PUTCHARS (pola, flp(1,1), 5, 5)
	CALL PUTCHARS (pola, flp(1,2), 5, 15)
	CALL PUTCHARS (pola, flp(2,1), 6, 5)
	CALL PUTCHARS (pola, flp(2,2), 6, 15)
	CALL PUTCHARS (pola, flp(3,1), 7, 5)
	CALL PUTCHARS (pola, flp(3,2), 7, 15)
	CALL PUTCHARS (pola, flp(4,1), 8, 5)
	CALL PUTCHARS (pola, flp(4,2), 8, 15)
	CALL SMG$DRAW_LINE (pola, 3, 11, 8, 11)
	CALL SMG$DRAW_LINE (pola, 3, 22, 8, 22)
	CALL PUTCHARS_REV (pola, 'RETURN F20 or Ctrl-G', 10, 12)
    	help_line(1) = 
     1	' Select the cross sections to be measured by using keys to toggle'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)


C------------------------------ GET NEW INPUT --------------------------------C

	DO i = 1, 4
	   IF (flag(i)) THEN
	      str3 = 'YES'
	      rendition =  M_REVERSE
	   ELSE
	      str3 = 'NO '
	      rendition = M_NORMAL
	   END IF
	   CALL PUTCHARS_REND (pola, str3, i+4, 26,rendition)		
	END DO
	CALL CURSOR_ON (paste)	

  100   CALL CURSOR_ABS (pola, 5, 26)
	CALL SMG$READ_STRING (keyb, answ,, 1, m$_nenrcl,,, leng, trm, pola)
	IF (leng .GE. 1) THEN
           IF (.NOT. flag(1)) THEN
	      flag(1) = .TRUE.
	      rendition = M_REVERSE
	      str3 = 'YES'
	   ELSE
	      flag(1) = .FALSE.
	      rendition = M_NORMAL
	      str3 = 'NO '
	   END IF
	   CALL PUTCHARS_REND (pola, str3, 5, 26,rendition)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO 110
	IF (trm .EQ. K_TRM_DOWN)   GOTO 110
	IF (trm .EQ. K_TRM_UP)     GOTO 130
	IF (trm .EQ. K_TRM_F20.or.trm.eq.7)    GOTO 200 
	CALL SMG$RING_BELL (pola, 1)
	GOTO 100

  110   CALL CURSOR_ABS (pola, 6, 26)
	CALL SMG$READ_STRING (keyb, answ,, 1, m$_nenrcl,,, leng, trm, pola)
	IF (leng .GE. 1) THEN
           IF (.NOT. flag(2)) THEN
	      flag(2) = .TRUE.
	      rendition = M_REVERSE
	      str3 = 'YES'
	   ELSE
	      flag(2) = .FALSE.
	      rendition = M_NORMAL
	      str3 = 'NO '
	   END IF
	   CALL PUTCHARS_REND (pola, str3, 6, 26,rendition)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO 120
	IF (trm .EQ. K_TRM_DOWN)   GOTO 120
	IF (trm .EQ. K_TRM_UP)     GOTO 100
	IF (trm .EQ. K_TRM_F20.or.trm.eq.7)    GOTO 200 
	CALL SMG$RING_BELL (pola, 1)
	GOTO 110

  120   CALL CURSOR_ABS (pola, 7, 26)
	CALL SMG$READ_STRING (keyb, answ,, 1, m$_nenrcl,,, leng, trm, pola)
	IF (leng .GE. 1) THEN
           IF (.NOT. flag(3)) THEN
	      flag(3) = .TRUE.
	      rendition = M_REVERSE
	      str3 = 'YES'
	   ELSE
	      flag(3) = .FALSE.
	      rendition = M_NORMAL
	      str3 = 'NO '
	   END IF
	   CALL PUTCHARS_REND (pola, str3, 7, 26,rendition)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO 130
	IF (trm .EQ. K_TRM_DOWN)   GOTO 130
	IF (trm .EQ. K_TRM_UP)     GOTO 110
	IF (trm .EQ. K_TRM_F20.or.trm.eq.7)    GOTO 200 
	CALL SMG$RING_BELL (pola, 1)
	GOTO 120

  130   CALL CURSOR_ABS (pola, 8, 26)
	CALL SMG$READ_STRING (keyb, answ,, 1, m$_nenrcl,,, leng, trm, pola)
	IF (leng .GE. 1) THEN
           IF (.NOT. flag(4)) THEN
	      flag(4) = .TRUE.
	      rendition = M_REVERSE
	      str3 = 'YES'
	   ELSE
	      flag(4) = .FALSE.
	      rendition = M_NORMAL
	      str3 = 'NO '
	   END IF
	   CALL  PUTCHARS_REND (pola, str3, 8, 26, rendition)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO 100
	IF (trm .EQ. K_TRM_DOWN)   GOTO 100
	IF (trm .EQ. K_TRM_UP)     GOTO 120
	IF (trm .EQ. K_TRM_F20.or.trm.eq.7)    GOTO 200 
	CALL SMG$RING_BELL (pola, 1)
	GOTO 130


C--------------------------- FINISH UP AND RETURN ----------------------------C

 200	DO i = 1,4
	  flipt(i) = flag(i)
	ENDDO

	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (pola, paste)
	CALL CURSOR_OFF (paste)
	help_line(1) = ' Enter the polarization analysis setup. '
	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
	help_line(3) = ' ' 
	CALL Put_Help (help_line)

	END
