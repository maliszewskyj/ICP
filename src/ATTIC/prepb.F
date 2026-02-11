#include "icp.h"
	include COMPILE_OPTS

	SUBROUTINE Prepb
C	****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C     Routine To Edit The BRAGG    Buffers                                    C
C     Called from: ICP_MAIN, Control                                          C
C                                                                             C
C     Gayle Greene, August, 1992                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
#if defined(VMS)

	INCLUDE   SMGPREP_CMN   ! Program specific SMG-definitions
	INCLUDE   INSTR_CMN               ! Common variable blocks
	INCLUDE   SMGCONST_CMN   !Equate keys with SMG constants

	INTEGER*2  com, a, b, c, aa, bb, cc, ec, es, ef, et, hc, kc, lc
	INTEGER*2  hs, ks, ls, hkl1, a1, hkl2, t0, it, tw, th0, th, ter, hf
	INTEGER*2  mon, prf, mt, pts, auto, pose, scn, i3,i4,r3,r4,fkeys

	INTEGER*4  leng, trm, defno, rendition, i, k, index, dummy,ios
	INTEGER*4  SMG$PASTE_VIRTUAL_DISPLAY, SMG$CREATE_MENU
	INTEGER*4  SMG$SELECT_FROM_MENU
	INTEGER*4  current_line,a_line(10),paste_line,mpos(10)

	REAL 	   bpos(10),foc7,foc8,focpos

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*7  PutStr,str7
	CHARACTER*8  str8
	CHARACTER*4  str4
	CHARACTER*6  str6
	CHARACTER*5  str5

	logical newlog,comflag,scrflag,insert/.true./
	real    fenergy,af,arange
	character*1 echar

	RECORD /B_Buf/ bbuf(nbuf)

	IF (nsta .EQ. 4 .or. nsta .eq. -5 .or. nsta .EQ. 2 .or.
     1		nsta .EQ. 9) THEN
	  paste_line=8
	ELSE
	  paste_line=9
	ENDIF
	CALL SMG$PASTE_VIRTUAL_DISPLAY (menub, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpb, paste, paste_line, 1)
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
	ASSIGN 1310 TO scn		! Ascan
	ASSIGN 1330 TO I3		! Inc of motor 3
	ASSIGN 1340 TO R3		! Range of motor 3
	ASSIGN 1350 TO I4		! Inc of motor 4
	ASSIGN 1360 TO R4		! Range of motor 4
	ASSIGN 320 TO t0 		! Temperature
	ASSIGN 330 TO it 		! Temperature increment
	ASSIGN 340 TO tw 		! Wait after each temp change
 	ASSIGN 350 TO ter		! Temperature error
	ASSIGN 360 TO th0 		! Hold before first data point
	ASSIGN 1370 TO th               ! Hold before each data point
	ASSIGN 370 TO hf                ! H-Field
 	ASSIGN 380 TO mon 		! Monitor
	ASSIGN 390 TO prf		! Monitor prefactor
	ASSIGN 400 TO mt 		! Monitor type
	ASSIGN 410 TO pts		! Number of points 	
	ASSIGN 420 TO auto		! Automonitor
	ASSIGN 430 TO pose		! Polarization setup
	ASSIGN 450 to fkeys		! Function key emulation
	k = 1                           ! Defno for first menu selection
	

C----------------------------- OPEN BBUFFER.BUF ------------------------------C

  20	OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=BBUF_RECL)
	DO i = 1, nbuf
	   READ (1,REC=i) bbuf(i)
	   CALL Putmax(bbuf(i).tmp,str5,5,4)   
	   WRITE (line(i),'(i2,1x,a20,1x,3f6.2,f6.2,a1,i7,2f6.2,1x,i3,1x,a5)')
	2       i, bbuf(i).com (1:20), bbuf(i).hc, bbuf(i).kc, bbuf(i).lc,
	3 	bbuf(i).ef, bbuf(i).eft,
	4 	NINT(bbuf(i).mon),bbuf(i).i(3),bbuf(i).i(4),NINT(bbuf(i).pts),
     1		str5
	END DO
	CLOSE (1)


	CALL CURSOR_ABS (menub, 1, 1)
	CALL PUTLINE_REVERSE (menub, ' No Comment *B-Buffer*       HC    KC 
     1   LC     FE    Mon  Inc3  Inc4  PTS  T0 ')


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

	CALL SMG$CREATE_MENU (menub, line,,, 3)
	CALL SMG$DRAW_LINE (prpb, 1, 1, 1, 80)


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
	CALL SMG$SELECT_FROM_MENU (keyb, menub, k, defno, 
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
	    CALL PUTCHARS_REV (prpb, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpb, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpb)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpb, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpb, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpb, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpb, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpb, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpb, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpb, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpb, current_line, 1)
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

	CALL SMG$ERASE_LINE (prpb, 3, 1)

	CALL PUTCHARS_BOLD (prpb, 'Bbuffer No: ', 2, 1)
	WRITE (UNIT= str8(1:2), FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpb, str8(1:2), 2, 13)
	CALL PUTCHARS (prpb, ' Comment: ', 2, 17)
	CALL PUTCHARS_REVEOL (prpb, bbuf(k).com, 2, 27)

	CALL PUTCHARS (prpb, ' Lattice: ', 3, 3)
	CALL PUTCHARS (prpb, ' a:', 3, 13)
	str8(1:7) = PutStr (bbuf(k).lat_a)
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 16)
	CALL PUTCHARS (prpb, '  b:', 3, 23)
	str8(1:7) = PutStr (bbuf(k).b)	
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 27)
	CALL PUTCHARS (prpb, '  c:', 3, 34)
	str8(1:7) = PutStr (bbuf(k).c)
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 38)
	CALL PUTCHARS (prpb, '  aa:', 3, 45)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).aa
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 50)
	CALL PUTCHARS (prpb, '  bb:', 3, 57)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).bb
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 62)
	CALL PUTCHARS (prpb, '  cc:', 3, 69)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).cc
	CALL PUTCHARS_REV (prpb, str8(1:7), 3, 74)

	CALL PUTCHARS (prpb, ' Energy:  ', 4, 3)
	CALL PUTCHARS (prpb, ' EC:', 4, 12)
	str8(1:7) = PutStr (bbuf(k).ec)
	CALL PUTCHARS (prpb, str8(1:7), 4, 16)
c	CALL PUTCHARS (prpb, ' ES:', 4, 23)
c	str8(1:7) = PutStr (bbuf(k).es)
c	CALL PUTCHARS (prpb, str8(1:7), 4, 27)
	CALL PUTCHARS (prpb, ' FE:', 4, 32)
	str8(1:7) = PutStr (bbuf(k).ef)
	CALL PUTCHARS_REV (prpb, str8(1:7), 4, 36)
	CALL PUTCHARS_REV (prpb, '('// bbuf(k).eft //')', 4, 46)
	CALL PUTCHARS (prpb, ' dM=', 4, 52)
	str8(1:7) = PutStr (dm)
	CALL PUTCHARS (prpb, str8(1:7), 4, 56)
	CALL PUTCHARS (prpb, ' dA=', 4, 65)
	str8(1:7) = PutStr (da)
	CALL PUTCHARS (prpb, str8(1:7), 4, 69)


	CALL PUTCHARS (prpb, ' Space: ', 5, 3)
	CALL PUTCHARS (prpb, ' HC:', 5, 12)
	IF (abs(bbuf(k).hc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).hc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).hc
	ENDIF
	CALL PUTCHARS_REV (prpb, str8(1:7), 5, 16)
	CALL PUTCHARS (prpb, ' KC:', 5, 23)
	IF (abs(bbuf(k).kc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).kc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).kc
	ENDIF
	CALL PUTCHARS_REV (prpb, str8(1:7), 5, 27)
	CALL PUTCHARS (prpb, ' LC:', 5, 34)
	IF (abs(bbuf(k).lc).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).lc
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).lc
	ENDIF
	CALL PUTCHARS_REV (prpb, str8(1:7), 5, 38)
	CALL PUTCHARS (prpb, '  HS:', 5, 45)
	IF (abs(bbuf(k).hs).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).hs
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).hs
	ENDIF
	CALL PUTCHARS (prpb, str8(1:7), 5, 50)
	CALL PUTCHARS (prpb, '  KS:', 5, 57)
	IF (abs(bbuf(k).ks).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).ks
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).ks
	ENDIF
	CALL PUTCHARS (prpb, str8(1:7), 5, 62)
	CALL PUTCHARS (prpb, '  LS:', 5, 69)
	IF (abs(bbuf(k).ls).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf(k).ls
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).ls
	ENDIF
	CALL PUTCHARS (prpb, str8(1:7), 5, 74)

	CALL PUTCHARS (prpb, ' Orient: ', 6, 3)
	CALL PUTCHARS (prpb, ' (h k l):', 6, 12)
	WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)') 
	1   NINT(bbuf(k).hkl1(1)), NINT(bbuf(k).hkl1(2)), NINT(bbuf(k).hkl1(3))	
	CALL PUTCHARS_REV (prpb, str8(1:8), 6, 21)
	CALL PUTCHARS (prpb, '  Angl:', 6, 29)
	WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf(k).a1
	CALL PUTCHARS_REV (prpb, str8(1:7), 6, 36)
	CALL PUTCHARS (prpb, ' (h k l)'':', 6, 46)
	WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)')
	1   NINT(bbuf(k).hkl2(1)), NINT(bbuf(k).hkl2(2)), NINT(bbuf(k).hkl2(3))	
	CALL PUTCHARS_REV (prpb, str8(1:8), 6, 56)
	CALL PUTCHARS_REV (prpb,'ASCAN',6, 69)

	CALL PUTCHARS (prpb, ' Temp/H: ', 7, 3)
	CALL PUTCHARS (prpb, ' T0:', 7, 12)
	CALL Putmax(bbuf(k).tmp,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpb, str8(1:6), 7, 16)
	CALL PUTCHARS (prpb, ' Inc-T:', 7, 22)
	CALL Putmax(bbuf(k).it,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpb, str8(1:6), 7, 29)
	CALL PUTCHARS (prpb, '  Wait:', 7, 35)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(bbuf(k).tw)
	CALL PUTCHARS_REV (prpb, str8(1:4), 7, 42)
	CALL PUTCHARS (prpb, '  Err:', 7, 45)
	CALL Putmax(bbuf(k).ter,str8(1:3),3,2)
	CALL PUTCHARS_REV (prpb, str8(1:3), 7, 51)
	CALL PUTCHARS (prpb, '  Hld0:', 7, 54)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(bbuf(k).th0)
	CALL PUTCHARS_REV (prpb, str8(1:3), 7, 61)
	CALL PUTCHARS (prpb, ' Hld:', 7, 65)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(bbuf(k).th)
	CALL PUTCHARS_REV (prpb, str8(1:3), 7, 70)
	str4(1:2)=' '
	CALL PUTCHARS (prpb, str4(1:2), 7, 73)
	CALL PUTCHARS_REV (prpb, 'Field', 7, 75)

	mon_line = 8
	CALL PUTCHARS (prpb, ' Time: ', mon_line, 3) 
	CALL PUTCHARS (prpb, ' Monit:', mon_line, 12)
	WRITE (UNIT= str8(1:7), FMT= '(i7)') NINT(bbuf(k).mon)
	CALL PUTCHARS_REV (prpb, str8(1:7), mon_line, 19)
	CALL PUTCHARS (prpb, '  Prefac:', mon_line, 26)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(bbuf(k).mpf)
	CALL PUTCHARS_REV (prpb, str8(1:3), mon_line, 35)
	CALL PUTCHARS (prpb, '  M-typ:', mon_line, 38)
	CALL PUTCHARS_REV (prpb, bbuf(k).mt, mon_line, 46)
c	CALL PUTCHARS (prpb, '  #pts:', mon_line, 50)
c	WRITE (UNIT= str8(1:4), FMT= '(i4)') NINT(bbuf(k).pts)
c	CALL PUTCHARS_REV (prpb, str8(1:4), mon_line, 57)
	CALL PUTCHARS_REV (prpb, ' AMON ', mon_line, 56)
	CALL PUTCHARS_REV (prpb, ' POLA', mon_line, 71)
	CALL PUTCHARS_REV (prpb, 'FKEY', mon_line, 77)

c************
	current_line = mon_line + 1
	a_line(3) = current_line
	CALL PUTCHARS (prpb, ' A3-center: ', a_line(3), 3)
	mpos(3) = 16
	CALL PUTCHARS (prpb, '  Inc-3: ', a_line(3), 23)
	str7 = PutStr (bbuf(k).i(3)) 
	CALL PUTCHARS_REV (prpb, str7, a_line(3), 32)
	CALL PUTCHARS (prpb, '  A3-range: ', a_line(3), 39)
	IF (bbuf(k).pts .NE. 0.) THEN
	   arange = bbuf(k).pts * bbuf(k).i(3)
	ELSE
	   arange = 0.
	END IF
	str7 = PutStr (arange)
	CALL PUTCHARS_REV (prpb, str7, a_line(3), 51) 
      
	current_line = current_line + 1
	a_line(4) = current_line
	CALL PUTCHARS (prpb, ' A4-center: ', a_line(4), 3)
	mpos(4) = 16
	CALL PUTCHARS (prpb, '  Inc-4: ', a_line(4), 23)
	str7 = PutStr (bbuf(k).i(4)) 
	CALL PUTCHARS_REV (prpb, str7, a_line(4), 32)
	CALL PUTCHARS (prpb, '  A4-range: ', a_line(4), 39)
	IF (bbuf(k).pts .NE. 0.) THEN
	   arange = bbuf(k).pts * bbuf(k).i(4)
	ELSE
	   arange = 0.
	END IF
	str7 = PutStr (arange)
	CALL PUTCHARS_REV (prpb, str7, a_line(4), 51)
	CALL PUTCHARS (prpb, '  #pts: ', a_line(4), 59)
	WRITE (UNIT= str4, FMT= '(i4)') NINT (bbuf(k).pts)
	CALL PUTCHARS_REV (prpb, str4, a_line(4), 66)

	current_line = current_line + 1
	a_line(2) = current_line
	a_line(5) = current_line
	a_line(6) = current_line
	CALL PUTCHARS (prpb, ' A2-position: ', a_line(2), 3)
	mpos(2) = 18
	CALL PUTCHARS (prpb, '  A5-position: ', a_line(5), 25)
	mpos(5) = 41
	CALL PUTCHARS (prpb, '  A6-position: ', a_line(6), 50)
	mpos(6) = 68
 
	IF (nsta .EQ. 4 .or. nsta .EQ. -5 .or. nsta .EQ. 2 .or.
     1	    nsta .EQ. 9) THEN
	  current_line = current_line + 1
	  a_line(1) = current_line
	  a_line(7) = current_line
	  a_line(8) = current_line
	  CALL PUTCHARS (prpb, ' A1-position: ', a_line(1), 3)
	  mpos(1) = 18
	  CALL PUTCHARS (prpb, '  A7-position: ', a_line(7), 25)
	  mpos(7) = 41
	ENDIF
	IF (nsta. EQ. 4) THEN
	  CALL PUTCHARS (prpb, '  A8-position: ', a_line(8), 50)
	  mpos(8) = 68
	ENDIF


	CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)


C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

 100	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105	CALL CURSOR_ABS (prpb, 2, 27)                    ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpb)
 105	CALL EDCOMMENT(prpb,keyb,2,27,trm,insert)
	CALL CURSOR_ABS (prpb, 2, 27)                    ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpb, comment)
	   if (bbuf(k).com(1:5).ne.comment(1:5))then
	     newlog=.true.
	     fenergy=bbuf(k).ef
	     echar=bbuf(k).eft(1:1)
	   else
	     newlog=.false.
	   endif
	   bbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REVEOL (prpb, bbuf(k).com, 2, 27)
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)


	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a
	IF (trm .EQ. K_TRM_DOWN)   GOTO a
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 105
 110	help_line(1) = ' Lattice constant a in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115	CALL GetString (keyb, prpb, 3, 16, 3, 7, trm, leng, bbuf(k).lat_a)      ! a
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO b
	IF (trm .EQ. K_TRM_DOWN)   GOTO hc
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 115
 120  	help_line(1) = ' Lattice constant b in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpb, 3, 27, 3, 7, trm, leng, bbuf(k).b)      ! b
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO c
	IF (trm .EQ. K_TRM_DOWN)   GOTO kc
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 125
 130  	help_line(1) = ' Lattice constant c in '// CHAR(197) // 'ngstrom'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpb, 3, 38, 3, 7, trm, leng, bbuf(k).c)      ! c
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO aa
	IF (trm .EQ. K_TRM_DOWN)   GOTO ef
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO b
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 135
 140  	help_line(1) = ' Lattice constant alpha in degrees!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetString (keyb, prpb, 3, 50, 3, 7, trm, leng, bbuf(k).aa)   ! aa
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO bb
	IF (trm .EQ. K_TRM_DOWN)   GOTO et
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO c
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 145
 150  	help_line(1) = ' Lattice constant beta in degrees! ' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpb, 3, 62, 3, 7, trm, leng, bbuf(k).bb)    ! bb
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO cc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO aa
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 155
 160  	help_line(1) = ' Lattice constant gamma in degrees!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpb, 3, 74, 3, 7, trm, leng, bbuf(k).cc)    ! cc
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1  trm .EQ. K_TRM_BUFFER_FULL) GOTO ef
	IF (trm .EQ. K_TRM_DOWN)   GOTO scn
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO bb
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 165
 170  	help_line(1) = ' Center of energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpb, 4, 16, 3, 7, trm, leng, bbuf(k).ec)    ! e0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO et
	IF (trm .EQ. K_TRM_DOWN)   GOTO hc
	IF (trm .EQ. K_TRM_UP)     GOTO a
	IF (trm .EQ. K_TRM_LEFT)   GOTO cc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 175
 180  	help_line(1) = ' Energy-step in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpb, 4, 27, 3, 7, trm, leng, bbuf(k).es)    ! es
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ef
	IF (trm .EQ. K_TRM_DOWN)   GOTO kc
	IF (trm .EQ. K_TRM_UP)     GOTO b
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 185
 190  	help_line(1) = ' Fixed energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpb, 4, 36, 3, 7, trm, leng, bbuf(k).ef)    ! ef
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO et
	IF (trm .EQ. K_TRM_DOWN)   GOTO lc
	IF (trm .EQ. K_TRM_UP)     GOTO c
	IF (trm .EQ. K_TRM_LEFT)   GOTO cc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 195
 200  	help_line(1) = ' ''ANA'' for analyzer fixed or ''MON'' for
	1 monochromator fixed!'
	help_line(2) = ' It is sufficient to give the first character.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL CURSOR_ABS (prpb, 4, 47)                              ! et
	CALL SMG$READ_STRING (keyb, str8(1:1),, 3, m$_nenrcl,,, leng, trm, prpb)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'A' .AND. str8(1:1) .NE. 'M')  THEN
	      str8(1:3) = '---'
	      CALL PUTCHARS_BLINK (prpb, str8(1:3), 4, 46)
	      CALL Ring_Bell (prpb, 2)
	      GOTO 205
	   ELSE 
	      IF (str8(1:1) .EQ. 'A') bbuf(k).eft = 'ANA' 
	      IF (str8(1:1) .EQ. 'M') bbuf(k).eft = 'MON' 
	      CALL PUTCHARS_REV (prpb, '('// bbuf(k).eft //')', 4, 46)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO aa
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 205
 230  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' h component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL GetString (keyb, prpb, 5, 16, 3, 7, trm, leng, bbuf(k).hc)    ! hc
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO kc
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl1
	IF (trm .EQ. K_TRM_UP)     GOTO a
	IF (trm .EQ. K_TRM_LEFT)   GOTO et
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 235
 240  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' k component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpb, 5, 27, 3, 7, trm, leng, bbuf(k).kc)    ! kc
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO lc
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO b
	IF (trm .EQ. K_TRM_LEFT)   GOTO hc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 245
 250  	help_line(1) = ' Center of scan in Q-space in units of (hkl)!'
	help_line(2) = ' l component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpb, 5, 38, 3, 7, trm, leng, bbuf(k).lc)    ! lc
	IF (leng .GE. 1) THEN
	  CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hkl1
	IF (trm .EQ. K_TRM_DOWN)   GOTO a1
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO kc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 255
 260  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) 	= ' h component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
  265	CALL GetString (keyb, prpb, 5, 50, 3, 7, trm, leng, bbuf(k).hs)    ! hs
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ks
	IF (trm .EQ. K_TRM_DOWN)   GOTO hkl2
	IF (trm .EQ. K_TRM_UP)     GOTO et
	IF (trm .EQ. K_TRM_LEFT)   GOTO lc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 265
 270  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) = ' k component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275   	CALL GetString (keyb, prpb, 5, 62, 3, 7, trm, leng, bbuf(k).ks)    ! ks
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ls
	IF (trm .EQ. K_TRM_DOWN)   GOTO scn
	IF (trm .EQ. K_TRM_UP)     GOTO bb
	IF (trm .EQ. K_TRM_LEFT)   GOTO hs
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 275
 280  	help_line(1) = ' Increment in Q-space in units of (hkl)!'
	help_line(2) = ' l component'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpb, 5, 74, 3, 7, trm, leng, bbuf(k).ls)    ! ls
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hkl1
	IF (trm .EQ. K_TRM_DOWN)   GOTO scn
	IF (trm .EQ. K_TRM_UP)     GOTO cc
	IF (trm .EQ. K_TRM_LEFT)   GOTO ks
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 285
 290  	help_line(1) = ' Sample orientation: low angle reflection '
	help_line(2) = '  in the positive sense of motor 3'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295 	CALL CURSOR_ABS (prpb, 6, 21)
	CALL SMG$READ_STRING (keyb, str8,, 8, m$_nenr,,, leng, trm,prpb)
	IF (leng .GE. 1) THEN
	   READ (UNIT= str8(1:leng), FMT= *, ERR= 296) 
	1   bbuf(k).hkl1(1), bbuf(k).hkl1(2), bbuf(k).hkl1(3)
	   WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)')
	1   NINT(bbuf(k).hkl1(1)), NINT(bbuf(k).hkl1(2)), NINT(bbuf(k).hkl1(3))	
	   CALL SMG$ERASE_CHARS (prpb, 8, 6, 21)
	   CALL PUTCHARS_REV (prpb, str8(1:8), 6, 21)
	   CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	END IF
	GOTO 297
 296  	str8(1:8) = '--------'
	CALL PUTCHARS_BLINK (prpb, str8, 6, 21)
	CALL Ring_Bell (prpb, 2)
	GOTO 295
 297  	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO hc
	IF (trm .EQ. K_TRM_LEFT)   GOTO lc
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 295
 300  	help_line(1) = ' Not currently in use'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpb, 6, 36, 3, 7, trm, leng, bbuf(k).a1)    ! a0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hkl2
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO lc
	IF (trm .EQ. K_TRM_LEFT)   GOTO hkl1
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 305
 310  	help_line(1) = ' Sample orientation: high angle reflection '
	help_line(2) = '  less than 180 degrees above the low angle'
	help_line(3) = '  reflection'
	CALL Put_Help (help_line)
 315 	CALL CURSOR_ABS (prpb, 6, 56)
	CALL SMG$READ_STRING (keyb, str8,, 8, m$_nenr,,, leng, trm,prpb)
	IF (leng .GE. 1) THEN
	   READ (UNIT= str8(1:leng), FMT= *, ERR= 316) 
	1   bbuf(k).hkl2(1), bbuf(k).hkl2(2), bbuf(k).hkl2(3)
	   WRITE (UNIT= str8(1:8), FMT= '(i2,1h,,i2,1h,,i2)') 
	1   NINT(bbuf(k).hkl2(1)), NINT(bbuf(k).hkl2(2)), NINT(bbuf(k).hkl2(3))	
	   CALL SMG$ERASE_CHARS (prpb, 8, 6, 56)
	   CALL PUTCHARS_REV (prpb, str8(1:8), 6, 56)
	   CALL Bragg_Pos(bbuf(k),bpos,a_line,mpos,comflag,.true.)
	END IF
	GOTO 317
 316  	str8(1:8) = '--------'
	CALL PUTCHARS_BLINK (prpb, str8, 6, 56)
	CALL Ring_Bell (prpb, 2)
	GOTO 315
 317	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO scn
	IF (trm .EQ. K_TRM_DOWN)   GOTO ter
	IF (trm .EQ. K_TRM_UP)     GOTO et
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 315


 1310  	help_line(1) = ' You provide value and changes in q are'
	help_line(2) = '   calculated automatically.'
	help_line(3) = '  This feature is not yet implemented.'
c	help_line(3) = ' Press RETURN, ENTER or DO to activate this feature! '
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpb,6,69)
 1315	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL AutoScan (2,bbuf(k))
c	   CALL CURSOR_ABS (prpb, 6, 69)
c	   GOTO 1315
	END IF
	CALL SMG$CHANGE_RENDITION (prpb, 6, 69, 1, 6, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)  GOTO hf
	IF (trm .EQ. K_TRM_UP)    GOTO cc
	IF (trm .EQ. K_TRM_LEFT)  GOTO hkl2
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1315

 1330  	help_line(1) = ' Increment of Motor 3'
	help_line(2) = ' Increment is coupled to Range of Motor 3'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1335  	CALL GetString (keyb, prpb, a_line(3), 32, 1, 7, trm, leng, 
     1	   bbuf(k).i(3))    ! i3
	IF (leng .GE. 1) THEN
	  arange = bbuf(k).pts * bbuf(k).i(3) !update range
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(3), 51) 
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO r3
	IF (trm .EQ. K_TRM_DOWN)   GOTO i4
	IF (trm .EQ. K_TRM_UP)     GOTO prf
	IF (trm .EQ. K_TRM_LEFT)   GOTO fkeys
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1335
 1340  	help_line(1) = ' Range of Motor 3'
	help_line(2) = ' Range of 3 is coupled with number of points'
	help_line(3) = '   and therefore Range of Motor 4'
	CALL Put_Help (help_line)
 1345  	CALL GetString (keyb, prpb, a_line(3), 51, 1, 7, trm, leng, 
     1	   arange)    ! R3
	IF (leng .GE. 1)THEN
	  IF (bbuf(k).i(3) .NE. 0.)THEN
	    bbuf(k).pts = IABS(NINT(arange/bbuf(k).i(3)))
	    arange = bbuf(k).pts * bbuf(k).i(3)
	  ELSE
	    arange = 0
	  ENDIF
	  WRITE (UNIT= str4, FMT= '(i4)') NINT (bbuf(k).pts)
	  CALL PUTCHARS_REV (prpb, str4, a_line(4), 66)
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(3), 51) 
	  arange = bbuf(k).pts * bbuf(k).i(4)
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(4), 51) 
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO i4
	IF (trm .EQ. K_TRM_DOWN)   GOTO r4
	IF (trm .EQ. K_TRM_UP)     GOTO auto
	IF (trm .EQ. K_TRM_LEFT)   GOTO i3
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1345
 1350  	help_line(1) = ' Increment of Motor 4'
	help_line(2) = ' Increment is coupled to Range of Motor 4.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1355  	CALL GetString (keyb, prpb, a_line(4), 32, 1, 7, trm, leng, 
     1	   bbuf(k).i(4))    ! i4
	IF (leng. GE. 1) THEN
	  arange = bbuf(k).pts * bbuf(k).i(4) !change r4
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(4), 51)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO r4
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO i3
	IF (trm .EQ. K_TRM_LEFT)   GOTO r3
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1355
 1360  	help_line(1) = ' Range of Motor 4'
	help_line(2) = ' Range of 4 is coupled with number of points'
	help_line(3) = '   and therefore Range of Motor 3'
	CALL Put_Help (help_line)
 1365  	CALL GetString (keyb, prpb, a_line(4), 51, 1, 7, trm, leng, 
     1	   arange)    ! r4
	IF (leng .GE. 1)THEN
	  IF (bbuf(k).i(4) .NE. 0.)THEN
	    bbuf(k).pts = IABS(NINT(arange/bbuf(k).i(4)))
	    arange = bbuf(k).pts * bbuf(k).i(4)
	  ELSE
	    arange = 0
	  ENDIF
	  WRITE (UNIT= str4, FMT= '(i4)') NINT (bbuf(k).pts)
	  CALL PUTCHARS_REV (prpb, str4, a_line(4), 66)
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(4), 51) 
	  arange = bbuf(k).pts * bbuf(k).i(3)
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(3), 51) 
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO r3
	IF (trm .EQ. K_TRM_LEFT)   GOTO i4
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1365



 320  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpb, 7, 16, 4, 6, trm, leng, bbuf(k).tmp)  !temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO hkl1
	IF (trm .EQ. K_TRM_LEFT)   GOTO scn
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 325
 330  	help_line(1) = ' Increment in temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpb, 7, 29, 4, 6, trm, leng, bbuf(k).it)    ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO kc
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 335
 340  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpb, 7, 42, 2, 4, trm, leng, bbuf(k).tw)    ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO a1
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 355
 350  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 355  	CALL GetString (keyb, prpb, 7, 51, 6, 3, trm, leng, bbuf(k).ter)  ! ter
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 355
 360  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpb, 7, 61, 2, 3, trm, leng, bbuf(k).th0)  ! th0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO auto
	IF (trm .EQ. K_TRM_UP)     GOTO hkl2
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 365

 1370  	help_line(1) = ' Time in minutes for a hold before each data point.'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1375  	CALL GetString (keyb, prpb, 7, 70, 2, 3, trm, leng, bbuf(k).th)    ! th
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO pose
	IF (trm .EQ. K_TRM_UP)     GOTO scn
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 1375

 370  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!' 
	CALL Put_Help (help_line)


  	CALL CURSOR_ABS (prpb, 7, 75) 
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 375  	CALL SMG$CHANGE_RENDITION (prpb, 7, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(bbuf(k).hf,bbuf(k).ihf,bbuf(k).hfw, !field,incr,wait
     1		bbuf(k).hfh) !how controller5 used
	   CALL CURSOR_ABS (prpb, 7, 75)
	   GOTO 375
	END IF
	CALL SMG$CHANGE_RENDITION (prpb, 7, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO scn
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 375
 380 	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 385  	CALL GetString (keyb, prpb, 8, 19, 2, 7, trm, leng, bbuf(k).mon)  ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO i3
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 385
 390  	help_line(1) = ' Monitor-prefactor: give an integer number or your input
	1 is rounded the next' 
	help_line(2) = ' integer. '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 395  	CALL GetString (keyb, prpb, 8, 35, 2, 3, trm, leng, bbuf(k).mpf)  ! mpf
	bbuf(k).mpf = ANINT (bbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO i3
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 395
 400  	IF (nsta.eq.0)THEN
	  help_line(1) = ' Monitor-type:  TIME only allowed'
	ELSE
	  help_line(1) = ' Monitor-type:  NEUT is neutron based'
	ENDIF
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 405  	CALL CURSOR_ABS (prpb, mon_line, 46)                       ! mt
	CALL SMG$READ_STRING (keyb, str8(1:4),, 4, m$_nenrcl,,, leng, trm,prpb)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'N' .AND. str8(1:1) .NE. 'T')  THEN
	      str8(1:4) = '----'
	      CALL PUTCHARS_BLINK (prpb, str8(1:4), 8, 46)
	      CALL Ring_Bell (prpb, 2)
	      GOTO 405
	   ELSE 
	      IF (str8(1:1) .EQ. 'N') bbuf(k).mt = 'NEUT'
	      IF (str8(1:1) .EQ. 'T' .or. nsta.eq.0) bbuf(k).mt = 'TIME'
	      CALL PUTCHARS_REV (prpb, bbuf(k).mt, 8, 46)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO auto
	IF (trm .EQ. K_TRM_DOWN)   GOTO r3
	IF (trm .EQ. K_TRM_UP)     GOTO tw
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 405
 410  	help_line(1) = ' Number of points for this run'
	help_line(2) = ' In order to hit the center give an odd number!'
	help_line(3) = ' Number of points is coupled with Range of 3 & 4.'
	CALL Put_Help (help_line)
 415  	CALL GetString (keyb, prpb, a_line(4), 66, 2, 4, trm, leng, bbuf(k).pts) !# pts
	IF (leng .GE. 1) THEN
	  IF (bbuf(k).pts .NE. 0.) bbuf(k).pts = ABS(bbuf(k).pts)
	  arange = bbuf(k).i(3) * bbuf(k).pts
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(3), 51) 
	  arange = bbuf(k).i(4) * bbuf(k).pts
	  str7 = PutStr (arange)
	  CALL PUTCHARS_REV (prpb, str7, a_line(4), 51) 
	END IF 
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO com
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO pose
	IF (trm .EQ. K_TRM_LEFT)   GOTO r4
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 415
  420	IF (nsta.eq.0)THEN
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
	CALL CURSOR_ABS (prpb, mon_line, 56)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
	CALL CURSOR_OFF (paste)
  425 	CALL SMG$CHANGE_RENDITION (prpb, mon_line, 56, 1, 6,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	 IF (nsta.ne.0)THEN
	  DO i=1,2
	    bbuf(k).a(i) = bpos(i)
	  ENDDO
	  DO i=5,6
	    bbuf(k).a(i) = bpos(i)
	  ENDDO
	  bbuf(k).a(3) = bpos(3) - int(bbuf(k).pts/2.)*bbuf(k).i(3)
	  bbuf(k).a(4) = bpos(4) - int(bbuf(k).pts/2.)*bbuf(k).i(4)
	   CALL AutoMon (5,,, bbuf(k))
	   CALL CURSOR_ABS (prpb, mon_line, 56)
	   GOTO 425
	 ELSE
	   trm = K_TRM_RIGHT
	 ENDIF
	END IF
	CALL SMG$CHANGE_RENDITION (prpb, mon_line, 56, 1, 6, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO pose
	IF (trm .EQ. K_TRM_DOWN)  GOTO r3
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO mt
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 425
  430  	help_line(1) = ' Enter the polarization analysis setup. '
	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
	help_line(3) = ' ' 
	CALL Put_Help (help_line)
	CALL CURSOR_OFF (paste)
  435 	CALL SMG$CHANGE_RENDITION (prpb, mon_line, 71, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL Pol_Setup (2,, bbuf(k))
	   CALL PolPrep (bbuf(k).flip_state)
	   CALL CURSOR_ABS (prpb, mon_line, 71)
	   GOTO 435
	END IF
	CALL SMG$CHANGE_RENDITION (prpb, mon_line, 71, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO pts
	IF (trm .EQ. K_TRM_UP)    GOTO th
	IF (trm .EQ. K_TRM_LEFT)  GOTO auto 
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 435


 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpb, mon_line, 77)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpb)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpb, 'FKEY', mon_line, 77)
	      CALL Ring_Bell (prpb, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpb, 'F14 ', mon_line, 77)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpb, 'F16 ', mon_line, 77)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpb, 'F17 ', mon_line, 77)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpb, 'F18 ', mon_line, 77)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpb, 'F19 ', mon_line, 77)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpb, 'F20 ', mon_line, 77)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpb, 'FKEY', mon_line, 77)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpb, 'FKEY', mon_line, 77)
	        CALL Ring_Bell (prpb, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO I3
	IF (trm .EQ. K_TRM_DOWN)  GOTO pts
	IF (trm .EQ. K_TRM_UP)    GOTO hf
	IF (trm .EQ. K_TRM_LEFT)  GOTO pose
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *700)
	IF (trm .EQ. K_TRM_F19)
	1                             CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_B_Buf (bbuf(k), k, line,
     1	    bpos, *900)
	CALL Ring_Bell (prpb, 1)
	GOTO 450
   

C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpb, 2, 1)
	IF (k .LT. nbuf) THEN
	  DO i=1,2
	    bbuf(k).a(i) = bpos(i)
	  ENDDO
	  DO i=5,6
	    bbuf(k).a(i) = bpos(i)
	  ENDDO
	  bbuf(k).a(3) = bpos(3) - int(bbuf(k).pts/2.)*bbuf(k).i(3)
	  bbuf(k).a(4) = bpos(4) - int(bbuf(k).pts/2.)*bbuf(k).i(4)
	   DO i= k+1, nbuf
	      bbuf(i)= bbuf(k)
	   END DO
	END IF
	CALL Updte_B_File (bbuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901          ! Prepare was called from control ==> RETURN
	flip = .TRUE.               ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20 



C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpb, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpb, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$ERASE_LINE (prpb, 9, 1)
	CALL CURSOR_ABS (prpb, 9, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,,, trm, prpb)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR) THEN
	   CALL Parse_Prep (command, *710, 5,,,,bbuf,,,, line)
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpb, 2, 1)
  	   GOTO 30
	END IF         
	CALL Ring_Bell (prpb, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpb, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpb, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpb, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpb, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (menub, paste)
	CALL SMG$ERASE_DISPLAY (menub, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpb, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

