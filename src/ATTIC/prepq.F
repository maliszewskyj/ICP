#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE PrepQ
C	****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C     Routine To Edit The Const.-Q Buffers                                    C
C     Called from: ICP_MAIN, Control                                          C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
#if defined(VMS)

	INCLUDE   SMGPREP_CMN   ! Program specific SMG-definitions
	INCLUDE   INSTR_CMN               ! Common variable blocks
	INCLUDE   SMGCONST_CMN   !Equate keys with SMG constants

	INTEGER*2  com, a, b, c, aa, bb, cc, ec, es, ef, et, hc, kc, lc
	INTEGER*2  hs, ks, ls, hkl1, a1, hkl2, t0, it, tw, th0, th, ter, hf
	INTEGER*2  mon, prf, mt, pts, auto, pose, fkeys

	INTEGER*4  leng, trm, defno, rendition, i, k, index, dummy
	INTEGER*4  SMG$PASTE_VIRTUAL_DISPLAY, SMG$CREATE_MENU
	INTEGER*4  SMG$SELECT_FROM_MENU
	INTEGER*4  ios,current_line

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*7  PutStr
	CHARACTER*8  str8
	CHARACTER*2  str2
	CHARACTER*4  str4

	logical newlog,insert/.true./
	real    fenergy
	character*1 echar

	RECORD /q_buf/ qbuf(nbuf)

	CALL SMG$PASTE_VIRTUAL_DISPLAY (menuq, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpq, paste, 11, 1)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK


C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C

	ASSIGN 100 TO com		! Comment
	ASSIGN 110 TO a  		! Lattice constant a
	ASSIGN 120 TO b  		! Lattice constant b
	ASSIGN 130 TO c  		! Lattice constant c
	ASSIGN 140 TO aa 		! Lattice constant alpha
	ASSIGN 150 TO bb 		! Lattice constant beta
	ASSIGN 160 TO cc 		! Lattice constant gamma
	ASSIGN 170 TO ec 		! Energy center
	ASSIGN 180 TO es 		! Energy step
	ASSIGN 190 TO ef		! Fixed energy
	ASSIGN 200 TO et 		! Fixed energy type
	ASSIGN 230 TO hc 		! H center
	ASSIGN 240 TO kc 		! K center
	ASSIGN 250 TO lc 		! L center
	ASSIGN 260 TO hs 		! H step
	ASSIGN 270 TO ks 		! K step
	ASSIGN 280 TO ls 		! L step
	ASSIGN 290 TO hkl1		! First reflection
	ASSIGN 300 TO a1 		! Angle of first reflection
	ASSIGN 310 TO hkl2		! Second reflection
	ASSIGN 320 TO t0 		! Temperature
	ASSIGN 330 TO it 		! Temperature increment
	ASSIGN 340 TO tw 		! Wait after each temp change
 	ASSIGN 350 TO ter		! Temperature error
	ASSIGN 360 TO th0 		! Hold before first data point
	ASSIGN 1360 TO th 		! Hold before each data point
	ASSIGN 370 TO hf                ! H-Field
 	ASSIGN 380 TO mon 		! Monitor
	ASSIGN 390 TO prf		! Monitor prefactor
	ASSIGN 400 TO mt 		! Monitor type
	ASSIGN 410 TO pts		! Number of points 	
	ASSIGN 420 TO auto		! Automonitor
	ASSIGN 430 TO pose		! Polarization setup
	ASSIGN 450 to fkeys		! Function key emulation
	k = 1                           ! Defno for first menu selection
	

C----------------------------- OPEN QBUFFER.BUF ------------------------------C

  20	OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=QBUF_RECL)
	DO i = 1, nbuf
	   READ (1,REC=i) qbuf(i)   
	   WRITE (line(i),'(i2,1x,a20,1x,3f8.4,f8.2,2f7.2,a1,i7)')
	2       i, qbuf(i).com (1:20), qbuf(i).hc, qbuf(i).kc, qbuf(i).lc,
	3 	qbuf(i).ec, qbuf(i).es, qbuf(i).ef, qbuf(i).eft,
	4 	NINT(qbuf(i).mon)
	END DO
	CLOSE (1)


	CALL CURSOR_ABS (menuq, 1, 1)
	CALL PUTLINE_REVERSE (menuq, ' No Comment *Q-Buffer*       HC      KC 
     1     LC       EC     ES    FixE   Mon  ')


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

	CALL SMG$CREATE_MENU (menuq, line,,, 3)
	CALL SMG$DRAW_LINE (prpq, 1, 1, 1, 80)


C-------------------- SELECT BUFFER OR TAKE OTHER ACTION ---------------------C

   30	CALL SMG$PASTE_VIRTUAL_DISPLAY (stat, paste, 20, 1)
     	CALL CURSOR_ABS (stat, 1, 1)
	CALL PUTLINE_REVERSE (stat, ' NoChg:F16    Cntrl: F17     Cpy: F14   B_Opr: 
	1F18     Seq: F19     Ret: F20  ')
	CALL CURSOR_OFF (paste)
	defno = k
   40 	help_line(1) = ' Select the buffer to edit using the arrow keys! '
	help_line(2) = ' Press ENTER, RETURN, DO or SELECT to edit the
	1 selected buffer!'
	help_line(3) = ' Remote users: Ctrl-G will bring up FKEY field'
	CALL Put_Help (help_line)
	CALL SMG$SELECT_FROM_MENU (keyb, menuq, k, defno, 
	1			       SMG$M_RETURN_IMMED,,, trm)
	IF (trm .EQ. K_TRM_F14) THEN                           ! Super copy
	   CALL CURSOR_ON (paste)
	   GOTO 500
	ELSE IF (trm .EQ. K_TRM_F16) THEN                ! Abort prep mode
	   CALL CURSOR_ON (paste)
	   GOTO 901                                       
	ELSE IF (trm .EQ. K_TRM_F17) THEN                    ! Goto control
	   CALL CURSOR_ON (paste)
	   GOTO 600
	ELSE IF (trm .EQ. K_TRM_F18) THEN               ! Buffer operations
	   CALL CURSOR_ON (paste)
	   GOTO 701
	ELSE IF (trm .EQ. K_TRM_F19) THEN             ! Define run sequence
	   CALL CURSOR_ON (paste)
           GOTO 801                                    
	ELSE IF (trm .EQ. K_TRM_F20) THEN                ! Return to caller
	   CALL CURSOR_ON (paste)
	   GOTO 901                                       
	ELSE IF (trm .NE. K_TRM_CR .AND. trm .NE. K_TRM_DO .AND.
	1 	 trm .NE. K_TRM_SELECT .AND. trm .NE. K_TRM_ENTER) THEN
	  IF (trm .eq. 7) THEN !for control G
	    current_line=2
	    CALL PUTCHARS_REV (prpq, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpq, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpq)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpq, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpq, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpq, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpq, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpq, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpq, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpq, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpq, current_line, 1)
	      goto 40
	    END IF          !length greater than 1
	  ENDIF	   
	  defno = k 
	  GOTO 40                                          ! No visible action
	END IF
	CALL CURSOR_ON (paste)
	CALL PUTLINE_REVERSE (stat, ' NoChg:F16  Updte: F17   Cpy: F14   
	1B_Opr: F18   Seq: F19   Ret: F20 ')


C----------------------------- SETUP INPUT MASK ------------------------------C

	CALL SMG$ERASE_LINE (prpq, 3, 1)

	CALL PUTCHARS_BOLD (prpq, 'Qbuffer No: ', 2, 1)
	WRITE (UNIT= str8(1:2), FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpq, str8(1:2), 2, 13)
	CALL PUTCHARS (prpq, ' Comment: ', 2, 17)
	CALL PUTCHARS_REVEOL (prpq, qbuf(k).com, 2, 27)

	CALL PUTCHARS (prpq, ' Lattice: ', 3, 3)
	CALL PUTCHARS (prpq, ' a:', 3, 13)
	str8(1:7) = PutStr (qbuf(k).a)
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 16)
	CALL PUTCHARS (prpq, '  b:', 3, 23)
	str8(1:7) = PutStr (qbuf(k).b)	
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 27)
	CALL PUTCHARS (prpq, '  c:', 3, 34)
	str8(1:7) = PutStr (qbuf(k).c)
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 38)
	CALL PUTCHARS (prpq, '  aa:', 3, 45)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).aa
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 50)
	CALL PUTCHARS (prpq, '  bb:', 3, 57)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).bb
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 62)
	CALL PUTCHARS (prpq, '  cc:', 3, 69)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).cc
	CALL PUTCHARS_REV (prpq, str8(1:7), 3, 74)

	CALL PUTCHARS (prpq, ' Energy:  ', 4, 3)
	CALL PUTCHARS (prpq, ' EC:', 4, 12)
	str8(1:7) = PutStr (qbuf(k).ec)
	CALL PUTCHARS_REV (prpq, str8(1:7), 4, 16)
	CALL PUTCHARS (prpq, ' ES:', 4, 23)
	str8(1:7) = PutStr (qbuf(k).es)
	CALL PUTCHARS_REV (prpq, str8(1:7), 4, 27)
	CALL PUTCHARS (prpq, ' FE:', 4, 34)
	str8(1:7) = PutStr (qbuf(k).ef)
	CALL PUTCHARS_REV (prpq, str8(1:7), 4, 38)
	CALL PUTCHARS_REV (prpq, '('// qbuf(k).eft //')', 4, 46)
	CALL PUTCHARS (prpq, ' dM=', 4, 52)
	str8(1:7) = PutStr (dm)
	CALL PUTCHARS (prpq, str8(1:7), 4, 56)
	CALL PUTCHARS (prpq, ' dA=', 4, 65)
	str8(1:7) = PutStr (da)
	CALL PUTCHARS (prpq, str8(1:7), 4, 69)


	CALL PUTCHARS (prpq, ' Space: ', 5, 3)
	CALL PUTCHARS (prpq, ' HC:', 5, 12)
	IF (abs(qbuf(k).hc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).hc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).hc
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 16)
	CALL PUTCHARS (prpq, ' KC:', 5, 23)
	IF (abs(qbuf(k).kc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).kc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).kc
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 27)
	CALL PUTCHARS (prpq, ' LC:', 5, 34)
	IF (abs(qbuf(k).lc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).lc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).lc
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 38)
	CALL PUTCHARS (prpq, '  HS:', 5, 45)
	IF (abs(qbuf(k).hs).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).hs
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).hs
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 50)
	CALL PUTCHARS (prpq, '  KS:', 5, 57)
	IF (abs(qbuf(k).ks).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).ks
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).ks
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 62)
	CALL PUTCHARS (prpq, '  LS:', 5, 69)
	IF (abs(qbuf(k).ls).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') qbuf(k).ls
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).ls
	ENDIF
	CALL PUTCHARS_REV (prpq, str8(1:7), 5, 74)

	CALL PUTCHARS (prpq, ' Orient: ', 6, 3)
	CALL PUTCHARS (prpq, ' (h k l):', 6, 12)
	WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)') 
	1   NINT(qbuf(k).hkl1(1)), NINT(qbuf(k).hkl1(2)), NINT(qbuf(k).hkl1(3))	
	CALL PUTCHARS_REV (prpq, str8(1:8), 6, 21)
	CALL PUTCHARS (prpq, '  Angl:', 6, 29)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') qbuf(k).a1
	CALL PUTCHARS_REV (prpq, str8(1:7), 6, 36)
	CALL PUTCHARS (prpq, ' (h k l)'':', 6, 46)
	WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)')
	1   NINT(qbuf(k).hkl2(1)), NINT(qbuf(k).hkl2(2)), NINT(qbuf(k).hkl2(3))	
	CALL PUTCHARS_REV (prpq, str8(1:8), 6, 56)

	CALL PUTCHARS (prpq, ' Temp/H: ', 7, 3)
	CALL PUTCHARS (prpq, ' T0:', 7, 12)
	CALL Putmax(qbuf(k).tmp,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpq, str8(1:6), 7, 16)
	CALL PUTCHARS (prpq, '  Inc-T:', 7, 22)
	CALL Putmax(qbuf(k).it,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpq, str8(1:6), 7, 29)
	CALL PUTCHARS (prpq, '  Wait:', 7, 35)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(qbuf(k).tw)
	CALL PUTCHARS_REV (prpq, str8(1:4), 7, 42)
	CALL PUTCHARS (prpq, '  Err:', 7, 45)
	CALL Putmax(qbuf(k).ter,str8(1:3),3,2)
	CALL PUTCHARS_REV (prpq, str8(1:3), 7, 51)
	CALL PUTCHARS (prpq, '  Hld0:', 7, 54)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(qbuf(k).th0)
	CALL PUTCHARS_REV (prpq, str8(1:3), 7, 61)
	CALL PUTCHARS (prpq, ' Hld:', 7, 65)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(qbuf(k).th)
	CALL PUTCHARS_REV (prpq, str8(1:3), 7, 70)
	str2(1:2)=' '
	CALL PUTCHARS (prpq, str2(1:2), 7, 73)
	CALL PUTCHARS_REV (prpq, 'Field', 7, 75)

      
	mon_line = 8
	CALL PUTCHARS (prpq, ' Time: ', mon_line, 3) 
	CALL PUTCHARS (prpq, ' Monit:', mon_line, 12)
	WRITE (UNIT= str8(1:7), FMT= '(i7)') NINT(qbuf(k).mon)
	CALL PUTCHARS_REV (prpq, str8(1:7), mon_line, 19)
	CALL PUTCHARS (prpq, '  Prefac:', mon_line, 26)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(qbuf(k).mpf)
	CALL PUTCHARS_REV (prpq, str8(1:3), mon_line, 35)
	CALL PUTCHARS (prpq, '  M-typ:', mon_line, 38)
	CALL PUTCHARS_REV (prpq, qbuf(k).mt, mon_line, 46)
	CALL PUTCHARS (prpq, '  #pts:', mon_line, 50)
	WRITE (UNIT= str8(1:4), FMT= '(i4)') NINT(qbuf(k).pts)
	CALL PUTCHARS_REV (prpq, str8(1:4), mon_line, 57)
	CALL PUTCHARS_REV (prpq, ' AMON ', mon_line, 63)
	CALL PUTCHARS_REV (prpq, ' POLA', mon_line, 71)
	CALL PUTCHARS_REV (prpq, 'FKEY', mon_line, 77)



C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

 100	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105	CALL CURSOR_ABS (prpq, 2, 27)                    ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpq)
 105	CALL EDCOMMENT(prpq,keyb,2,27,trm,insert)
	CALL CURSOR_ABS (prpq, 2, 27)                    ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpq,comment)
	   if (qbuf(k).com(1:5).ne.comment(1:5))then
	     newlog=.true.
	     fenergy=qbuf(k).ef
	     echar=qbuf(k).eft(1:1)
	   else
	     newlog=.false.
	   endif
	   qbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REV (prpq, qbuf(k).com, 2, 27)
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)


	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a
	IF (trm .EQ. K_TRM_DOWN)   GOTO a
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO pose
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 105
 110	help_line(1) = ' Lattice constant a in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115	CALL GetString (keyb, prpq, 3, 16, 3, 7, trm, leng, qbuf(k).a)      ! a
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO b
	IF (trm .EQ. K_TRM_DOWN)   GOTO ec
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 115
 120  	help_line(1) = ' Lattice constant b in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpq, 3, 27, 3, 7, trm, leng, qbuf(k).b)      ! b
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO c
	IF (trm .EQ. K_TRM_DOWN)   GOTO es
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 125
 130  	help_line(1) = ' Lattice constant c in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpq, 3, 38, 3, 7, trm, leng, qbuf(k).c)      ! c
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO aa
	IF (trm .EQ. K_TRM_DOWN)   GOTO ef
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO b
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 135
 140  	help_line(1) = ' Lattice constant alpha in degrees!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetString (keyb, prpq, 3, 50, 3, 7, trm, leng, qbuf(k).aa)   ! aa
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO bb
	IF (trm .EQ. K_TRM_DOWN)   GOTO et
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO c
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 145
 150  	help_line(1) = ' Lattice constant beta in degrees! ' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpq, 3, 62, 3, 7, trm, leng, qbuf(k).bb)    ! bb
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO cc
	IF (trm .EQ. K_TRM_DOWN)   GOTO ks
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO aa
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 155
 160  	help_line(1) = ' Lattice constant gamma in degrees!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpq, 3, 74, 3, 7, trm, leng, qbuf(k).cc)    ! cc
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1  trm .EQ. K_TRM_BUFFER_FULL) GOTO ec
	IF (trm .EQ. K_TRM_DOWN)   GOTO ls
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO bb
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 165
 170  	help_line(1) = ' Center of energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpq, 4, 16, 3, 7, trm, leng, qbuf(k).ec)    ! e0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO es
	IF (trm .EQ. K_TRM_DOWN)   GOTO hc
	IF (trm .EQ. K_TRM_UP)     GOTO a
	IF (trm .EQ. K_TRM_LEFT)   GOTO cc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 175
 180  	help_line(1) = ' Energy-step in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpq, 4, 27, 3, 7, trm, leng, qbuf(k).es)    ! es
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ef
	IF (trm .EQ. K_TRM_DOWN)   GOTO kc
	IF (trm .EQ. K_TRM_UP)     GOTO b
	IF (trm .EQ. K_TRM_LEFT)   GOTO ec
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 185
 190  	help_line(1) = ' Fixed energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpq, 4, 38, 3, 7, trm, leng, qbuf(k).ef)    ! ef
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO et
	IF (trm .EQ. K_TRM_DOWN)   GOTO lc
	IF (trm .EQ. K_TRM_UP)     GOTO c
	IF (trm .EQ. K_TRM_LEFT)   GOTO es
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 195
 200  	help_line(1) = ' ''ANA'' for analyzer fixed or ''MON'' for
	1 monochromator fixed!'
	help_line(2) = ' It is sufficient to give the first character.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL CURSOR_ABS (prpq, 4, 47)                              ! et
	CALL SMG$READ_STRING (keyb, str8(1:1),, 3, m$_nenrcl,,, leng, trm, prpq)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'A' .AND. str8(1:1) .NE. 'M')  THEN
	      str8(1:3) = '---'
	      CALL PUTCHARS_BLINK (prpq, str8(1:3), 4, 46)
	      CALL Ring_Bell (prpq, 2)
	      GOTO 205
	   ELSE 
	      IF (str8(1:1) .EQ. 'A') qbuf(k).eft = 'ANA' 
	      IF (str8(1:1) .EQ. 'M') qbuf(k).eft = 'MON' 
	      CALL PUTCHARS_REV (prpq, '('// qbuf(k).eft //')', 4, 46)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hs
	IF (trm .EQ. K_TRM_UP)     GOTO aa
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 205
 230  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' h component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL GetString (keyb, prpq, 5, 16, 3, 7, trm, leng, qbuf(k).hc)    ! hc
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO kc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl1
	IF (trm .EQ. K_TRM_UP)     GOTO ec
	IF (trm .EQ. K_TRM_LEFT)   GOTO et
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 235
 240  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' k component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpq, 5, 27, 3, 7, trm, leng, qbuf(k).kc)    ! kc
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO lc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl1
	IF (trm .EQ. K_TRM_UP)     GOTO es
	IF (trm .EQ. K_TRM_LEFT)   GOTO hc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 245
 250  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' l component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpq, 5, 38, 3, 7, trm, leng, qbuf(k).lc)    ! lc
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hs
	IF (trm .EQ. K_TRM_DOWN)   GOTO a1
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO kc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 255
 260  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) 	= ' h component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
  265	CALL GetString (keyb, prpq, 5, 50, 3, 7, trm, leng, qbuf(k).hs)    ! hs
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ks
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO et
	IF (trm .EQ. K_TRM_LEFT)   GOTO lc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 265
 270  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) = ' k component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275   	CALL GetString (keyb, prpq, 5, 62, 3, 7, trm, leng, qbuf(k).ks)    ! ks
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ls
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO bb
	IF (trm .EQ. K_TRM_LEFT)   GOTO hs
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 275
 280  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) = ' l component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpq, 5, 74, 3, 7, trm, leng, qbuf(k).ls)    ! ls
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hkl1
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO cc
	IF (trm .EQ. K_TRM_LEFT)   GOTO ks
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 285
 290  	help_line(1) = ' Sample orientation: low angle reflection '
	help_line(2) = '  in the positive sense of motor 3'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295 	CALL CURSOR_ABS (prpq, 6, 21)
	CALL SMG$READ_STRING (keyb, str8,, 8, m$_nenr,,, leng, trm,prpq)
	IF (leng .GE. 1) THEN
	   READ (UNIT= str8(1:leng), FMT= *, ERR= 296) 
	1   qbuf(k).hkl1(1), qbuf(k).hkl1(2), qbuf(k).hkl1(3)
	   WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)')
	1   NINT(qbuf(k).hkl1(1)), NINT(qbuf(k).hkl1(2)), NINT(qbuf(k).hkl1(3))	
	   CALL SMG$ERASE_CHARS (prpq, 8, 6, 21)
	   CALL PUTCHARS_REV (prpq, str8(1:8), 6, 21)
	END IF
	GOTO 297
 296  	str8(1:8) = '--------'
	CALL PUTCHARS_BLINK (prpq, str8, 6, 21)
	CALL Ring_Bell (prpq, 2)
	GOTO 295
 297  	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO hc
	IF (trm .EQ. K_TRM_LEFT)   GOTO ls
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 295
 300  	help_line(1) = ' Not currently in use'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpq, 6, 36, 3, 7, trm, leng, qbuf(k).a1)    ! a0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hkl2
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO lc
	IF (trm .EQ. K_TRM_LEFT)   GOTO hkl1
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 305
 310  	help_line(1) = ' Sample orientation: high angle reflection '
	help_line(2) = '  less than 180 degrees above the low angle'
	help_line(3) = '  reflection'
	CALL Put_Help (help_line)
 315 	CALL CURSOR_ABS (prpq, 6, 56)
	CALL SMG$READ_STRING (keyb, str8,, 8, m$_nenr,,, leng, trm,prpq)
	IF (leng .GE. 1) THEN
	   READ (UNIT= str8(1:leng), FMT= *, ERR= 316) 
	1   qbuf(k).hkl2(1), qbuf(k).hkl2(2), qbuf(k).hkl2(3)
	   WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)') 
	1   NINT(qbuf(k).hkl2(1)), NINT(qbuf(k).hkl2(2)), NINT(qbuf(k).hkl2(3))	
	   CALL SMG$ERASE_CHARS (prpq, 8, 6, 56)
	   CALL PUTCHARS_REV (prpq, str8(1:8), 6, 56)
	END IF
	GOTO 317
 316  	str8(1:8) = '--------'
	CALL PUTCHARS_BLINK (prpq, str8, 6, 56)
	CALL Ring_Bell (prpq, 2)
	GOTO 315
 317	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)   GOTO th0
	IF (trm .EQ. K_TRM_UP)     GOTO hs
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 315
 320  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpq, 7, 16, 4, 6, trm, leng, qbuf(k).tmp)  !temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO hkl1
	IF (trm .EQ. K_TRM_LEFT)   GOTO hkl2
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 325
 330  	help_line(1) = ' Increment in temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpq, 7, 29, 4, 6, trm, leng, qbuf(k).it)    ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO a1
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 335
 340  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpq, 7, 42, 2, 4, trm, leng, qbuf(k).tw)    ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 355
 350  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 355  	CALL GetString (keyb, prpq, 7, 51, 6, 3, trm, leng, qbuf(k).ter)  ! ter
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 355
 360  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpq, 7, 61, 2, 4, trm, leng, qbuf(k).th0)   ! th0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO auto
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 365
 1360  	help_line(1) = ' Time in minutes for a hold before each data point.'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1365  	CALL GetString (keyb, prpq, 7, 70, 2, 3, trm, leng, qbuf(k).th)    ! th
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO pose
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 1365
 370  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!' 
	CALL Put_Help (help_line)
  	CALL CURSOR_ABS (prpq, 7, 75) 
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 375  	CALL SMG$CHANGE_RENDITION (prpq, 7, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(qbuf(k).hf,qbuf(k).ihf,qbuf(k).hfw, !field,incr,wait
     1		qbuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpq, 7, 75)
	   GOTO 375
	END IF
	CALL SMG$CHANGE_RENDITION (prpq, 7, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 375
 380 	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 385  	CALL GetString (keyb, prpq, 8, 19, 2, 7, trm, leng, qbuf(k).mon)  ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 385
 390  	help_line(1) = ' Monitor-prefactor: give an integer number or your input
	1 is rounded the next' 
	help_line(2) = ' integer. '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 395  	CALL GetString (keyb, prpq, 8, 35, 2, 3, trm, leng, qbuf(k).mpf)  ! mpf
	qbuf(k).mpf = ANINT (qbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 395
 400  	IF (nsta.eq.0)THEN
	  help_line(1) = ' Monitor-type: TIME only allowed'
	ELSE
	  help_line(1) = ' Monitor-type:  NEUT is neutron based'
	ENDIF
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 405  	CALL CURSOR_ABS (prpq, mon_line, 46)                       ! mt
	CALL SMG$READ_STRING (keyb, str8(1:4),, 4, m$_nenrcl,,, leng, trm,prpq)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'N' .AND. str8(1:1) .NE. 'T')  THEN
	      str8(1:4) = '----'
	      CALL PUTCHARS_BLINK (prpq, str8(1:4), 8, 46)
	      CALL Ring_Bell (prpq, 2)
	      GOTO 405
	   ELSE 
	      IF (str8(1:1) .EQ. 'N') qbuf(k).mt = 'NEUT'
	      IF (str8(1:1) .EQ. 'T'.or.nsta.eq.0) qbuf(k).mt = 'TIME' !xray is time
	      CALL PUTCHARS_REV (prpq, qbuf(k).mt, 8, 46)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO tw
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 405
 410  	help_line(1) = ' Number of points for this run'
	help_line(2) = ' In order to hit the center give an odd number!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 415  	CALL GetString (keyb, prpq, 8, 57, 2, 4, trm, leng, qbuf(k).pts) !# pts
	IF (leng .GE. 1) THEN
	   IF (qbuf(k).pts .NE. 0.) qbuf(k).pts = ABS(qbuf(k).pts)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO auto
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO mt
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 415
  420  	IF (nsta.eq.0)THEN
	  help_line(1)='No monitor on this instrument'
	  help_line(2)=' '
	  help_line(3)=' '
	ELSE
	  help_line(1) = ' You give the time at which this buffer should be
	1   finished. Based on the '
	  help_line(2) = ' current settings and neutron flux, the monitor is
	1   calculated automatically. '
	  help_line(3) = ' Press RETURN, ENTER or DO to activate this feature! '
	ENDIF
	CALL Put_Help (help_line)
	CALL CURSOR_OFF (paste)
  425 	CALL SMG$CHANGE_RENDITION (prpq, mon_line, 63, 1, 6,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	 IF (nsta .ne. 0) THEN
	   CALL AutoMon (2,, qbuf(k),)
	   CALL CURSOR_ABS (prpq, mon_line, 63)
	   GOTO 425
	 ELSE
	   trm = K_TRM_RIGHT
	 ENDIF
	END IF
	CALL SMG$CHANGE_RENDITION (prpq, mon_line, 63, 1, 6,SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO pose
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO pts
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 425
  430  	help_line(1) = ' Enter the polarization analysis setup. '
	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
	help_line(3) = ' ' 
	CALL Put_Help (help_line)
	CALL CURSOR_OFF (paste)
  435 	CALL SMG$CHANGE_RENDITION (prpq, mon_line, 71, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL Pol_Setup (2,, qbuf(k))
	   CALL PolPrep(qbuf(k).flip_state)
	   CALL CURSOR_ABS (prpq, mon_line, 71)
	   GOTO 435
	END IF
	CALL SMG$CHANGE_RENDITION (prpq, mon_line, 71, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th
	IF (trm .EQ. K_TRM_LEFT)  GOTO auto 
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 435
   

 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpq, mon_line, 77)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpq)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpq, 'FKEY', mon_line, 77)
	      CALL Ring_Bell (prpq, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .gt. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpq, 'F14 ', mon_line, 77)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpq, 'F16 ', mon_line, 77)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpq, 'F17 ', mon_line, 77)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpq, 'F18 ', mon_line, 77)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpq, 'F19 ', mon_line, 77)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpq, 'F20 ', mon_line, 77)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpq, 'FKEY', mon_line, 77)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpq, 'FKEY', mon_line, 77)
	        CALL Ring_Bell (prpq, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO com 
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO hf
	IF (trm .EQ. K_TRM_LEFT)  GOTO pose
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_Q_Buf (qbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_Q_Buf (qbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_Q_Buf (qbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_Q_Buf (qbuf(k), k, line, *900)
	CALL Ring_Bell (prpq, 1)
	GOTO 450
   

C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpq, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      qbuf(i)= qbuf(k)
	   END DO
	END IF
	CALL Updte_Q_File (qbuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901          ! Prepare was called from control ==> RETURN
	flip = .TRUE.               ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20 



C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpq, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpq, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$ERASE_LINE (prpq, 9, 1)
	CALL CURSOR_ABS (prpq, 9, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,,, trm, prpq)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR) THEN
	   CALL Parse_Prep (command, *710, 2,, qbuf,,,,,, line)
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpq, 2, 1)
  	   GOTO 30
	END IF         
	CALL Ring_Bell (prpq, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpq, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpq, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpq, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpq, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (menuq, paste)
	CALL SMG$ERASE_DISPLAY (menuq, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpq, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

