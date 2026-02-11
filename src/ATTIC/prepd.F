#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE PrepD
C	****************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Edit The Measurement Buffers                                 C
C     Called from: ICP_MAIN                                                   C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	IMPLICIT   none
#if defined(VMS)
	INCLUDE INSTR_CMN      ! Instrument and hardware parameters
	INCLUDE SMGPREP_CMN
	INCLUDE SMGCONST_CMN
	INCLUDE DARTZ_CMN
	INTEGER*2  xyz1,xyz2,xyz3,xyz4,xyz5,xyz6,a1xyz,a2xyz,a3xyz,
     1			a4xyz,a5xyz,a6xyz,a1ixyz,a2ixyz,a3ixyz,a4ixyz,
     1			a5ixyz,a6ixyz,a1exyz,a2exyz,a3exyz,a4exyz,
     1                  a5exyz,a6exyz,psi,phi
	INTEGER*2  zero, com, a1, a1i, a1e, a2, a2i, a2e, a3, a3i, a3e
	INTEGER*2  a4, a4i, a4e, a5, a5i, a5e, a6, a6i, a6e, t0, it, tw, ter
	INTEGER*2  th0, th, hf, mon, prf, mt, pts, auto, pose, fkeys
	INTEGER*2  mn1, mn2, mn3, mn4, mn5, mn6, lpn1, lpn2, lpn3, lpn4,
     1		   lpn5, lpn6, pts1,pts2,pts3,pts4,pts5,pts6
	INTEGER*4  leng, trm, defno, row, rendition, i, k, index, ios
	INTEGER*4  paste_line, a_line(6), com_line, tem_line, current_line
	REAL       af, afn, afxyz
	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*2  str2
	CHARACTER*4  str4
	CHARACTER*5  str5
	CHARACTER*6  str6
	CHARACTER*7  str7, PutStr
	logical newlog, makeline, insert/.true./
	real    fenergy
	character*1 echar
	RECORD /d_buf/ dbuf(nbuf)
C---------------- DEFINE POSITION OF WINDOWS AND STATUS LINE -----------------C
	IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	    nsta .EQ. 9 .OR. nsta .EQ. 8) THEN !motor one is present
	   index = 1
	   paste_line = 9
	ELSE IF (nsta .EQ. 1) THEN
	   index = 3
	   paste_line = 13
	ELSE
	   index = 2
	   paste_line = 10
	END IF
	CALL SMG$PASTE_VIRTUAL_DISPLAY (menud, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpd, paste, paste_line, 1)
C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C
	ASSIGN 100 TO com		! Comment
	ASSIGN 1100 TO lpn1  		! Angle 1: loop #
	ASSIGN 1110 TO mn1  		! Angle 1: mot #
	ASSIGN 110 TO a1  		! Angle 1: start
	ASSIGN 3110 TO xyz1  		! Angle 1: xyz
	ASSIGN 2110 TO a1xyz  		! Angle 1: xyz start
	ASSIGN 120 TO a1i 		! Angle 1: inc
	ASSIGN 2120 TO a1ixyz 		! Angle 1: xyz inc
	ASSIGN 130 TO a1e 		! Angle 1: end
	ASSIGN 2130 TO a1exyz 		! Angle 1: xyz end
	ASSIGN 1120 TO pts1  		! Angle 1: #pts
	ASSIGN 1130 TO lpn2  		! Angle 2: loop #
	ASSIGN 1140 TO mn2  		! Angle 2: mot #
	ASSIGN 140 TO a2  		! Angle 2: start
	ASSIGN 3140 TO xyz2  		! Angle 2: xyz
	ASSIGN 2140 TO a2xyz  		! Angle 2: xyz start
	ASSIGN 150 TO a2i 		! Angle 2: inc
	ASSIGN 2150 TO a2ixyz 		! Angle 2: xyz inc
	ASSIGN 160 TO a2e 		! Angle 2: end
	ASSIGN 2160 TO a2exyz 		! Angle 2: xyz end
	ASSIGN 1150 TO pts2  		! Angle 2: #pts
	ASSIGN 1160 TO lpn3  		! Angle 3: loop #
	ASSIGN 1170 TO mn3  		! Angle 3: mot #
	ASSIGN 170 TO a3  		! Angle 3: start
	ASSIGN 3170 TO xyz3  		! Angle 3: xyz
	ASSIGN 2170 TO a3xyz  		! Angle 3: xyz start
	ASSIGN 180 TO a3i 		! Angle 3: inc
	ASSIGN 2180 TO a3ixyz 		! Angle 3: xyz inc
	ASSIGN 190 TO a3e 		! Angle 3: end
	ASSIGN 2190 TO a3exyz 		! Angle 3: xyz end
	ASSIGN 1180 TO pts3  		! Angle 3: #pts
	ASSIGN 1190 TO lpn4  		! Angle 4: loop #
	ASSIGN 1200 TO mn4  		! Angle 4: mot#
	ASSIGN 200 TO a4  		! Angle 4: start
	ASSIGN 3200 TO xyz4  		! Angle 4: xyz
	ASSIGN 2200 TO a4xyz  		! Angle 4: xyz start
	ASSIGN 210 TO a4i 		! Angle 4: inc
	ASSIGN 2210 TO a4ixyz 		! Angle 4: xyz inc
	ASSIGN 220 TO a4e 		! Angle 4: end
	ASSIGN 2220 TO a4exyz 		! Angle 4: xyz end
	ASSIGN 230 TO pts4		! Angle 4: #pts
	ASSIGN 1230 TO lpn5  		! Angle 5: loop #
	ASSIGN 1240 TO mn5  		! Angle 5: mot#
	ASSIGN 240 TO a5  		! Angle 5: start
	ASSIGN 3240 TO xyz5  		! Angle 5: xyz
	ASSIGN 2240 TO a5xyz  		! Angle 5: xyz start
	ASSIGN 250 TO a5i 		! Angle 5: inc
	ASSIGN 2250 TO a5ixyz 		! Angle 5: xyz inc
	ASSIGN 260 TO a5e 		! Angle 5: end
	ASSIGN 2260 TO a5exyz 		! Angle 5: xyz end
	ASSIGN 1250 TO pts5  		! Angle 5: #pts
	ASSIGN 1260 TO lpn6  		! Angle 6: loop #
	ASSIGN 1270 TO mn6  		! Angle 6: mot#
	ASSIGN 270 TO a6  		! Angle 6: start
	ASSIGN 3270 TO xyz6  		! Angle 6: xyz
	ASSIGN 2270 TO a6xyz  		! Angle 6: xyz start
	ASSIGN 280 TO a6i 		! Angle 6: inc
	ASSIGN 2280 TO a6ixyz 		! Angle 6: xyz inc
	ASSIGN 2290 TO a6exyz 		! Angle 6: xyz end
	ASSIGN 290 TO a6e 		! Angle 6: end
	ASSIGN 1300 TO pts6  		! Angle 6: #pts
	ASSIGN 2300 TO phi  		! Phi
	ASSIGN 3300 TO psi  		! Psi
	ASSIGN 300 TO t0 		! Temperature
	ASSIGN 310 TO it 		! Temperature increment
	ASSIGN 320 TO tw 		! Wait after each temp change
 	ASSIGN 330 TO ter		! Temperature error
	ASSIGN 340 TO th0 		! Hold before first data point
	ASSIGN 1340 TO th 		! Hold before each data point
	ASSIGN 350 TO hf                ! H-Field
 	ASSIGN 360 TO mon 		! Monitor
	ASSIGN 370 TO prf		! Monitor prefactor
	ASSIGN 380 TO mt 		! Monitor type
   	ASSIGN 390 TO auto		! Automonitor
c   	ASSIGN 400 TO pose		! Polarization setup
	ASSIGN 450 TO fkeys		! Function key emulation
	k = 1                           ! Defno for first menu selection
C---------------------------- OPEN 'DBUFFER.BUF' -----------------------------C
   20  	OPEN (UNIT= 1, FILE= 'DBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT',
	1     FORM= 'UNFORMATTED', SHARED,RECL=dbuf_RECL)
	CALL CURSOR_ABS (menud, 1, 1)
	CALL PUTLINE_REVERSE (menud, ' No Comment (1:35)            '//
     1	'          A     A-Beg A-Inc   A-End  Monitor')
	DO i = 1, nbuf
	   READ (1,REC=i) dbuf(i)   
	   af = dbuf(i).a(1) + (dbuf(i).lpts(1) -1)* dbuf(i).i(1)
	   WRITE (line(i),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		iostat=ios)
	2         i, dbuf(i).com(1:35),dbuf(i).mn(1), dbuf(i).a(1), 
     1		  dbuf(i).i(1),af, 
	3         NINT(dbuf(i).mpf), NINT(dbuf(i).mon)
	END DO
	CLOSE (1)
C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C
     	CALL SMG$CREATE_MENU (menud, line,,, 3)
	CALL SMG$DRAW_LINE (prpd, 1, 1, 1, 80)
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
	CALL SMG$SELECT_FROM_MENU (keyb, menud, k, defno, 
	1        		       SMG$M_RETURN_IMMED,,, trm)
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
	1        trm .NE. K_TRM_SELECT .AND. trm .NE. K_TRM_ENTER) THEN
	  IF (trm .eq. 7) THEN !for control G
	    current_line=2
	    CALL PUTCHARS_REV (prpd, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpd, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpd)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpd, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpd, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpd, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpd, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpd, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpd, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpd, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpd, current_line, 1)
	      goto 40
	    END IF          !length greater than 1
	  ENDIF	   
	  defno = k 
	  GOTO 40                                          ! No visible action
	END IF
	CALL CURSOR_ON (paste)
c	CALL PUTLINE_REVERSE (stat, ' NoChg:F16  Updte: F17   Ftch: F13   Cpy: F14   
	CALL PUTLINE (stat, ' NoChg:F16  Updte: F17   Ftch: F13   Cpy: F14   
	1B_Opr: F18   Seq: F19   Ret: F20 ')
C----------------------------- SETUP INPUT MASK ------------------------------C
	CALL SMG$ERASE_LINE (prpd, 3, 1)
	current_line = 2
	com_line = current_line 
	CALL PUTCHARS_BOLD (prpd, ' Buffer No: ', com_line, 1)
	WRITE (UNIT= str2, FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpd, str2, com_line, 13)
	CALL PUTCHARS (prpd, ' Comment: ', com_line, 17)
	CALL PUTCHARS_REVEOL (prpd, dbuf(k).com, com_line, 27)
	current_line = current_line + 1
	CALL PUTCHARS_BOLD (prpd, 
     1	' Loop Mot xyz   a_Beg   a_Inc   a_End '//
     1	'xyz_Beg xyz_Inc xyz_End #pts    phi   psi',
     1	  current_line, 1)
	do i = 1,6
	  makeline = .false.
	  if (i .eq. 1) then
	    IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	      nsta .EQ. 9 .OR. nsta .EQ. 8) makeline=.true. !motor one is present
	  elseif (i .eq. 2 .or. i .eq. 5 .or. i .eq. 6) then
	    IF (nsta .NE. 1) makeline=.true.
	  else
	    makeline=.true.
	  endif
	  if (makeline) THEN
	   current_line = current_line + 1
	   a_line(i) = current_line
	   write(unit=str2,fmt='(I2)',iostat=ios)dbuf(k).lpn(i)
	   CALL PUTCHARS_REV (prpd, str2, a_line(i), 2)  !loop
	   write(unit=str2,fmt='(I2)',iostat=ios)dbuf(k).mn(i)
	   CALL PUTCHARS_REV (prpd, str2, a_line(i), 7)  !mot
	   IF (dbuf(k).xyz(i) .eq. xcoord) THEN
		str7(1:1)='x'
	   ELSEIF (dbuf(k).xyz(i) .eq. ycoord) THEN
		str7(1:1)='y'
	   ELSEIF (dbuf(k).xyz(i) .eq. zcoord) THEN
		str7(1:1)='z'
	   ELSE
		str7(1:1)=' '
	   ENDIF
	   CALL PUTCHARS_REV (prpd, str7(1:1), a_line(i), 12) !xyz
	   str7 = PutStr (dbuf(k).a(i))
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 15)  !a-beg
	   str7 = PutStr (dbuf(k).a_xyz(i))
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 39)  !xyz-beg
	   str7 = PutStr (dbuf(k).i(i))
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 23)  !a-inc
	   str7 = PutStr (dbuf(k).i_xyz(i))
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 47)  !xyz-inc
	   IF (dbuf(k).lpts(i) .NE. 0.) THEN
	      af = dbuf(k).a(i) + (dbuf(k).lpts(i) - 1) * dbuf(k).i(i)
	      afxyz = dbuf(k).a_xyz(i) + (dbuf(k).lpts(i) - 1) * 
     1		dbuf(k).i_xyz(i)
	   ELSE
	      af = dbuf(k).a(i)
	      afxyz = dbuf(k).a_xyz(i)
	   END IF
	   str7 = PutStr (af) 
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 31)  !a-end 
	   str7 = PutStr (afxyz) 
	   CALL PUTCHARS_REV (prpd, str7, a_line(i), 55) 
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(i)
	  CALL PUTCHARS_REV (prpd, str4, a_line(i), 63)
	  if (i .eq. 1) THEN
	    CALL Putmax(dbuf(k).phi,str5,5,3)
	    CALL PUTCHARS_REV (prpd,str5, a_line(i),69)
	    CALL Putmax(dbuf(k).psi,str5,5,3)
	    CALL PUTCHARS_REV (prpd,str5, a_line(i),75)
	  endif
	 END IF !makeline      
	END DO
	current_line = current_line + 1
	tem_line = current_line
	CALL PUTCHARS (prpd, ' Temp/H: ', tem_line, 3)
	CALL PUTCHARS (prpd, ' T0:', tem_line, 12)
	CALL Putmax(dbuf(k).tmp,str6,6,3)
	CALL PUTCHARS_REV (prpd, str6, tem_line, 16)
	CALL PUTCHARS (prpd, ' Inc-T:', tem_line, 22)
	CALL Putmax(dbuf(k).it,str6,6,3)
	CALL PUTCHARS_REV (prpd, str6, tem_line, 29)
	CALL PUTCHARS (prpd, '  Wait:', tem_line, 35)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(dbuf(k).tw)
	CALL PUTCHARS_REV (prpd, str6(1:3), tem_line, 42)
	CALL PUTCHARS (prpd, '  Err:', tem_line, 45)
	CALL Putmax(dbuf(k).ter,str6(1:3),3,2)
	CALL PUTCHARS_REV (prpd, str6(1:3), tem_line, 51)
	CALL PUTCHARS (prpd, '  Hld0:', tem_line, 54)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(dbuf(k).th0)
	CALL PUTCHARS_REV (prpd, str6(1:3), tem_line, 61)
	CALL PUTCHARS (prpd, ' Hld:', tem_line, 65)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(dbuf(k).th)
	CALL PUTCHARS_REV (prpd, str6(1:3), tem_line, 70)
	str6(1:2)=' '
	CALL PUTCHARS (prpd, str6(1:2), tem_line, 73)
	CALL PUTCHARS_REV (prpd, 'Field', tem_line, 75)
	current_line = current_line + 1
	mon_line = current_line
	CALL PUTCHARS (prpd, ' Time:', mon_line, 3)
	CALL PUTCHARS (prpd, ' Monit:', mon_line, 12)
	WRITE (UNIT= str7, FMT= '(i7)') NINT(dbuf(k).mon)
	CALL PUTCHARS_REV (prpd, str7, mon_line, 19)
	CALL PUTCHARS (prpd, '   Prefac:', mon_line, 26)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(dbuf(k).mpf)
	CALL PUTCHARS_REV (prpd, str6, mon_line, 36)
	CALL PUTCHARS (prpd, '   M-typ:', mon_line, 39)
	CALL PUTCHARS_REV (prpd, dbuf(k).mt, mon_line, 48)
	CALL PUTCHARS_REV (prpd, ' AMON ', mon_line, 54)
	CALL PUTCHARS_REV (prpd, 'FKEY', mon_line, 62)
	CALL PUTCHARS (prpd, '#Pts: ', mon_line, 68)
	WRITE (UNIT= str5, FMT= '(i5)',iostat=ios)NINT( dbuf(k).pts)
	CALL PUTCHARS (prpd, str5, mon_line, 75)
C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C
 100  	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105  	CALL CURSOR_ABS (prpd, com_line, 27)                 ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpd)
 105	CALL EDCOMMENT(prpd,keyb,com_line,27,trm,insert)
	CALL CURSOR_ABS (prpd, com_line, 27)                 ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpd,comment)
	   if (dbuf(k).com(1:5).ne.comment(1:5))then
	     newlog=.true.
	     if (wavel.ne.0)then
	       fenergy=81.804/(wavel**2)
	     else
	       fenergy=0.
	     endif
	     echar='A'
	   else
	     newlog=.false.
	   endif
	   dbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REVEOL (prpd, dbuf(k).com, com_line, 27) 
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO (1110, 140, 170) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO (1110, 140, 170) index
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO fkeys
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 105
 1100  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1105  	CALL GetString (keyb,prpd, a_line(1), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(1) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,1,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn1
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1105                                         
 1110  	help_line(1) = ' Number of first motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1115  	CALL GetString (keyb,prpd, a_line(1), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(1) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz1
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1115                                         
 3110  	help_line(1) = ' Coord of first motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3115 	CALL CURSOR_ABS (prpd, a_line(1), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(1), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3110
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(1) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(1) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(1) = zcoord
	      ELSE
		dbuf(k).xyz(1) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(1) = 0.0
		dbuf(k).i_xyz(1) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(1), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(1), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(1), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(1), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1
	IF (trm .EQ. K_TRM_DOWN)   GOTO xyz2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3115                                         
 110  	help_line(1) = ' Starting angle of first motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115  	CALL GetString (keyb,prpd, a_line(1), 15, 1, 7, trm, leng, dbuf(k).a(1))  ! a1
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(1) .NE. 0.) THEN  
	      af = dbuf(k).a(1) + (dbuf(k).lpts(1) - 1) * dbuf(k).i(1)
	   ELSE
	      af = dbuf(k).a(1)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(1), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO xyz1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 115                                         
 2110  	help_line(1) = ' Starting coord of first motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2115  	CALL GetString (keyb,prpd, a_line(1), 39, 1, 7, trm, leng, dbuf(k).a_xyz(1))  ! a1
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(1) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(1) + (dbuf(k).lpts(1) - 1) * dbuf(k).i_xyz(1)
	   ELSE
	      afxyz = dbuf(k).a_xyz(1)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(1), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1ixyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2xyz
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2115                                         
 120  	help_line(1) = ' Increment of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpd, a_line(1), 23, 1, 7, trm, leng, dbuf(k).i(1))  ! i1
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(1) .NE. 0.) THEN
	      af = dbuf(k).a(1) + (dbuf(k).lpts(1) -1)* dbuf(k).i(1)
	   ELSE
	      af = dbuf(k).a(1) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(1), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2i
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 125
 2120  	help_line(1) = 'Coord  Increment of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2125  	CALL GetString (keyb, prpd, a_line(1), 47, 1, 7, trm, leng, dbuf(k).i_xyz(1))  ! i1
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(1) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(1) + (dbuf(k).lpts(1) -1)* dbuf(k).i_xyz(1)
	   ELSE
	      afxyz = dbuf(k).a_xyz(1) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(1), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1exyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2ixyz
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2125
 130  	help_line(1) = ' Final angle of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpd, a_line(1), 31, 1, 7, trm, leng, af)   ! a1f
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(1) .EQ. 0.) THEN
	      dbuf(k).i(1) = 0.05
	      str7 = PutStr (dbuf(k).i(1))
	      CALL PUTCHARS_REV (prpd, str7, a_line(1), 23)
	   END IF
	   dbuf(k).lpts(1) = ANINT ((af - dbuf(k).a(1)) / dbuf(k).i(1) + 1.)
	   IF (dbuf(k).lpts(1) .LT. 0.) THEN
	      dbuf(k).lpts(1)  = ABS(dbuf(k).lpts(1))
	      dbuf(k).i(1) = -1.*dbuf(k).i(1)
	      str7 = PutStr (dbuf(k).i(1))
	      CALL PUTCHARS_REV (prpd, str7, a_line(1), 23)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(1)
	   CALL PUTCHARS_REV (prpd, str4, a_line(1), 63)
	   CALL Update_Dpts(dbuf(k),a_line,1,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2e
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *135)
 	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 135
 2130  	help_line(1) = ' Final coord of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2135  	CALL GetString (keyb, prpd, a_line(1), 55, 1, 7, trm, leng, af)   ! a1f
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(1), 55)
	   IF (dbuf(k).lpts(1) .LE. 1.) THEN
	      dbuf(k).a_xyz(1) = af
	      str7 = PutStr (dbuf(k).a_xyz(1))
	      CALL PUTCHARS_REV (prpd, str7, a_line(1), 39)
	   ELSE
	      dbuf(k).i_xyz(1) = (af - dbuf(k).a_xyz(1))/(dbuf(k).lpts(1)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(1))
	      CALL PUTCHARS_REV (prpd, str7, a_line(1), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts1
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2exyz
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2135)
 	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2135
1120  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1125  	CALL GetString (keyb, prpd, a_line(1), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(1) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,1,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO phi
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1125
 1130  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1135  	CALL GetString (keyb,prpd, a_line(2), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(2) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,2,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn2
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn3
	IF (trm .EQ. K_TRM_UP)     GOTO lpn1
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1135)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1135                                         
 1140  	help_line(1) = ' Number of second motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1145  	CALL GetString (keyb,prpd, a_line(2), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(2) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz2
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn3
	IF (trm .EQ. K_TRM_UP)     GOTO mn1
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1145                                         
 

 3140  	help_line(1) = ' Coord of second motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3145 	CALL CURSOR_ABS (prpd, a_line(2), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(2), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3140
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(2) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(2) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(2) = zcoord
	      ELSE
		dbuf(k).xyz(2) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(2) = 0.0
		dbuf(k).i_xyz(2) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(2), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(2), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(2), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(2), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2
	IF (trm .EQ. K_TRM_DOWN)   GOTO xyz3
	IF (trm .EQ. K_TRM_UP)     GOTO (3110, 3110, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (1140, 100, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3145
 140  	help_line(1) = ' Starting angle of second motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetString (keyb, prpd, a_line(2), 15, 1, 7, trm, leng, dbuf(k).a(2))  ! a2
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(2) .NE. 0.) THEN  
	      af = dbuf(k).a(2) + (dbuf(k).lpts(2) - 1) * dbuf(k).i(2)
	   ELSE
	      af = dbuf(k).a(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(2), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3
	IF (trm .EQ. K_TRM_UP)     GOTO (110, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (3140, 100, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 145
 2140  	help_line(1) = ' Starting coord of second motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2145  	CALL GetString (keyb, prpd, a_line(2), 39, 1, 7, trm, leng, dbuf(k).a_xyz(2))  ! a2
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(2) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(2) + (dbuf(k).lpts(2) - 1) * dbuf(k).i_xyz(2)
	   ELSE
	      afxyz = dbuf(k).a_xyz(2)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(2), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2ixyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3xyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2110, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2145
 150  	help_line(1) = ' Increment of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpd, a_line(2), 23, 1, 7, trm, leng, dbuf(k).i(2))  ! i2
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(2) .NE. 0.) THEN
	      af = dbuf(k).a(2) + (dbuf(k).lpts(2) -1)* dbuf(k).i(2)
	   ELSE
	      af = dbuf(k).a(2) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(2), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3i
	IF (trm .EQ. K_TRM_UP)     GOTO a1i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 155
 2150  	help_line(1) = ' Coord Increment of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2155  	CALL GetString (keyb, prpd, a_line(2), 47, 1, 7, trm, leng, dbuf(k).i_xyz(2))  ! i2
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(2) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(2) + (dbuf(k).lpts(2) -1)* dbuf(k).i_xyz(2)
	   ELSE
	      afxyz = dbuf(k).a_xyz(2) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(2), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2exyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3ixyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2120, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2155
 160  	help_line(1) = ' Final angle of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpd, a_line(2), 31, 1, 7, trm, leng, af)   ! af2
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(2) .EQ. 0.) THEN
	      dbuf(k).i(2) = 0.05
	      str7 = PutStr (dbuf(k).i(2))
	      CALL PUTCHARS_REV (prpd, str7, a_line(2), 23)
	   END IF
	   dbuf(k).lpts(2) = ANINT ((af - dbuf(k).a(2)) / dbuf(k).i(2) + 1.)
	   IF (dbuf(k).lpts(2) .LT. 0.) THEN
	      dbuf(k).lpts(2)  = ABS(dbuf(k).lpts(2))
	      dbuf(k).i(2) = -1.*dbuf(k).i(2)
	      str7 = PutStr (dbuf(k).i(2))
	      CALL PUTCHARS_REV (prpd, str7, a_line(2), 23)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(2)
	   CALL PUTCHARS_REV (prpd, str4, a_line(2), 63)
	   CALL Update_Dpts(dbuf(k),a_line,2,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3e
	IF (trm .EQ. K_TRM_UP)     GOTO (130, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 165
 2160  	help_line(1) = ' Final coord of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2165  	CALL GetString (keyb, prpd, a_line(2), 55, 1, 7, trm, leng, af)   ! af2
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(2), 55)
	   IF (dbuf(k).lpts(2) .LE. 1.) THEN
	      dbuf(k).a_xyz(2) = af
	      str7 = PutStr (dbuf(k).a_xyz(2))
	      CALL PUTCHARS_REV (prpd, str7, a_line(2), 39)
	   ELSE
	      dbuf(k).i_xyz(2) = (af - dbuf(k).a_xyz(2))/(dbuf(k).lpts(2)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(2))
	      CALL PUTCHARS_REV (prpd, str7, a_line(2), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts2
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3exyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2130, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2165
1150  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1155  	CALL GetString (keyb, prpd, a_line(2), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(2) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,2,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn3
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts3
	IF (trm .EQ. K_TRM_UP)     GOTO pts1
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1155
 1160  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1165  	CALL GetString (keyb,prpd, a_line(3), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(3) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,3,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn3
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn4
	IF (trm .EQ. K_TRM_UP)     GOTO lpn2
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1165                                         
 1170  	help_line(1) = ' Number of third motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1175  	CALL GetString (keyb,prpd, a_line(3), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(3) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz3
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn4
	IF (trm .EQ. K_TRM_UP)     GOTO mn2
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1175                                         
 3170  	help_line(1) = ' Coord of third motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3175 	CALL CURSOR_ABS (prpd, a_line(3), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(3), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3170
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(3) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(3) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(3) = zcoord
	      ELSE
		dbuf(k).xyz(3) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(3) = 0.0
		dbuf(k).i_xyz(3) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(3), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(3), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(3), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(3), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3
	IF (trm .EQ. K_TRM_DOWN)   GOTO xyz4
	IF (trm .EQ. K_TRM_UP)     GOTO (3140, 3140, 100) index
c	IF (trm .EQ. K_TRM_LEFT)   GOTO (1170, 1170, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3175
 170  	help_line(1) = ' Starting angle of third motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpd, a_line(3), 15, 1, 7, trm, leng, dbuf(k).a(3))  ! a3
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(3) .NE. 0.) THEN  
	      af = dbuf(k).a(3) + (dbuf(k).lpts(3) - 1) * dbuf(k).i(3)
	   ELSE
	      af = dbuf(k).a(3)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(3), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4
	IF (trm .EQ. K_TRM_UP)     GOTO (140, 140, 100) index
c	IF (trm .EQ. K_TRM_LEFT)   GOTO (1170, 1170, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO xyz3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 175
 2170  	help_line(1) = ' Starting coord of third motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2175  	CALL GetString (keyb, prpd, a_line(3), 39, 1, 7, trm, leng, dbuf(k).a_xyz(3))  ! a3
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(3) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(3) + (dbuf(k).lpts(3) - 1) * dbuf(k).i_xyz(3)
	   ELSE
	      afxyz = dbuf(k).a_xyz(3)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(3), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3ixyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4xyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2140, 2140, 100) index
c	IF (trm .EQ. K_TRM_LEFT)   GOTO (1170, 1170, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2175
 180  	help_line(1) = ' Increment of third motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpd, a_line(3), 23, 1, 7, trm, leng, dbuf(k).i(3))  ! i3
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(3) .NE. 0.) THEN
	      af = dbuf(k).a(3) + (dbuf(k).lpts(3) -1)* dbuf(k).i(3)
	   ELSE
	      af = dbuf(k).a(3) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(3), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4i
	IF (trm .EQ. K_TRM_UP)     GOTO (150, 150, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 185
 2180  	help_line(1) = ' Coord Increment of third motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2185  	CALL GetString (keyb, prpd, a_line(3), 47, 1, 7, trm, leng, dbuf(k).i_xyz(3))  ! i3
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(3) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(3) + (dbuf(k).lpts(3) -1)* dbuf(k).i_xyz(3)
	   ELSE
	      afxyz = dbuf(k).a_xyz(3) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(3), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3exyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4ixyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2150, 2150, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2185
 190  	help_line(1) = ' Final angle of third motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpd, a_line(3), 31, 1, 7, trm, leng, af)     ! af3
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(3) .EQ. 0.) THEN
	      dbuf(k).i(3) = 0.05
	      str7 = PutStr (dbuf(k).i(3))
	      CALL PUTCHARS_REV (prpd, str7, a_line(3), 23)
	   END IF
	   dbuf(k).lpts(3) = ANINT ((af - dbuf(k).a(3)) / dbuf(k).i(3) + 1.)
	   IF (dbuf(k).lpts(3) .LT. 0.) THEN
	      dbuf(k).lpts(3)  = ABS(dbuf(k).lpts(3))
	      dbuf(k).i(3) = -1.*dbuf(k).i(3)
	      str7 = PutStr (dbuf(k).i(3))
	      CALL PUTCHARS_REV (prpd, str7, a_line(3), 23)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(3)
	   CALL PUTCHARS_REV (prpd, str4, a_line(3), 63)
	   CALL Update_Dpts(dbuf(k),a_line,3,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4e
	IF (trm .EQ. K_TRM_UP)     GOTO (160, 160, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 195
 2190  	help_line(1) = ' Final coord of third motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2195  	CALL GetString (keyb, prpd, a_line(3), 55, 1, 7, trm, leng, af)     ! af3
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(3), 55)
	   IF (dbuf(k).lpts(3) .LE. 1.) THEN
	      dbuf(k).a_xyz(3) = af
	      str7 = PutStr (dbuf(k).a_xyz(3))
	      CALL PUTCHARS_REV (prpd, str7, a_line(3), 39)
	   ELSE
	      dbuf(k).i_xyz(3) = (af - dbuf(k).a_xyz(3))/(dbuf(k).lpts(3)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(3))
	      CALL PUTCHARS_REV (prpd, str7, a_line(3), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts3
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4exyz
	IF (trm .EQ. K_TRM_UP)     GOTO (2160, 2160, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2195
1180  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1185  	CALL GetString (keyb, prpd, a_line(3), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(3) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,3,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn4
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts4
	IF (trm .EQ. K_TRM_UP)     GOTO pts2
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1185
2300  	help_line(1) = ' Phi'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
2305  	CALL GetString (keyb, prpd, a_line(1), 69, 4, 5, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).phi = afn
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO psi
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2305
 3300  	help_line(1) = ' Psi'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3305  	CALL GetString (keyb, prpd, a_line(1), 75, 4, 5, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).psi = afn
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn2
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO phi
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3305
 1190  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1195  	CALL GetString (keyb,prpd, a_line(4), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(4) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,4,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn4
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn5
	IF (trm .EQ. K_TRM_UP)     GOTO lpn3
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1195                                         
 1200  	help_line(1) = ' Number of fourth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1205  	CALL GetString (keyb,prpd, a_line(4), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(4) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz4
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn5
	IF (trm .EQ. K_TRM_UP)     GOTO mn3
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1205                                         

 3200  	help_line(1) = ' Coord of fourth motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3205 	CALL CURSOR_ABS (prpd, a_line(4), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(4), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3200
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(4) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(4) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(4) = zcoord
	      ELSE
		dbuf(k).xyz(4) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(4) = 0.0
		dbuf(k).i_xyz(4) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(4), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(4), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(4), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(4), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4
c	IF (trm .EQ. K_TRM_DOWN)   GOTO (1240, 1240, 300) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO xyz5
	IF (trm .EQ. K_TRM_UP)     GOTO xyz3
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3205
 200  	help_line(1) = ' Starting angle of fourth motor '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL GetString (keyb, prpd, a_line(4), 15, 1, 7, trm, leng, dbuf(k).a(4))  ! a4
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(4) .NE. 0.) THEN  
	      af = dbuf(k).a(4) + (dbuf(k).lpts(4) - 1) * dbuf(k).i(4)
	   ELSE
  	      af = dbuf(k).a(4)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(4), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4i
	IF (trm .EQ. K_TRM_DOWN)   GOTO (240, 240, 300) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3
	IF (trm .EQ. K_TRM_LEFT)   GOTO xyz4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 205
 2200  	help_line(1) = ' Starting coord of fourth motor '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2205  	CALL GetString (keyb, prpd, a_line(4), 39, 1, 7, trm, leng, dbuf(k).a_xyz(4))  ! a4
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(4) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(4) + (dbuf(k).lpts(4) - 1) * dbuf(k).i_xyz(4)
	   ELSE
  	      afxyz = dbuf(k).a_xyz(4)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(4), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4ixyz
c	IF (trm .EQ. K_TRM_DOWN)   GOTO (1240, 1240, 300) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO a5xyz
	IF (trm .EQ. K_TRM_UP)     GOTO a3xyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2205
 210  	help_line(1) = ' Increment of fourth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!' 
	help_line(3) = ' '
	CALL Put_Help (help_line)
 215  	CALL GetString (keyb, prpd, a_line(4), 23, 1, 7, trm, leng, dbuf(k).i(4))  ! i4
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(4) .NE. 0.) THEN
	      af = dbuf(k).a(4) + (dbuf(k).lpts(4) -1)* dbuf(k).i(4)
	   ELSE
	      af = dbuf(k).a(4) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(4), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4e
	IF (trm .EQ. K_TRM_DOWN)   GOTO (250, 250, 320) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *215)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 215
 2210  	help_line(1) = ' Coord Increment of fourth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!' 
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2215  	CALL GetString (keyb, prpd, a_line(4), 47, 1, 7, trm, leng, dbuf(k).i_xyz(4))  ! i4
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(4) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(4) + (dbuf(k).lpts(4) -1)* dbuf(k).i_xyz(4)
	   ELSE
	      afxyz = dbuf(k).a_xyz(4) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(4), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4exyz
c	IF (trm .EQ. K_TRM_DOWN)   GOTO (250, 250, 320) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO a5ixyz
	IF (trm .EQ. K_TRM_UP)     GOTO a3ixyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2215)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2215
 220  	help_line(1) = ' Final angle of fourth motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
      	CALL Put_Help (help_line)
 225  	CALL GetString (keyb, prpd, a_line(4), 31, 1, 7, trm, leng, af)   ! af4
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(4) .EQ. 0.) THEN
	      dbuf(k).i(4) = 0.05
	      str7 = PutStr (dbuf(k).i(4))
	      CALL PUTCHARS_REV (prpd, str7, a_line(4), 23)
	   END IF
	   dbuf(k).lpts(4) = ANINT ((af - dbuf(k).a(4)) / dbuf(k).i(4) + 1.)
	   IF (dbuf(k).lpts(4) .LT. 0.) THEN
	      dbuf(k).lpts(4)  = ABS(dbuf(k).lpts(4))
	      dbuf(k).i(4) = -1.*dbuf(k).i(4)
	      str7 = PutStr (dbuf(k).i(4))
	      CALL PUTCHARS_REV (prpd, str7, a_line(4), 23)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(4)
	   CALL PUTCHARS_REV (prpd, str4, a_line(4), 63)
	   CALL Update_Dpts(dbuf(k),a_line,4,mon_line)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO (260, 260, 340) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *225)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 225
 2220  	help_line(1) = ' Final coord of fourth motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
      	CALL Put_Help (help_line)
 2225  	CALL GetString (keyb, prpd, a_line(4), 55, 1, 7, trm, leng, af)   ! af4
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(4), 55)
	   IF (dbuf(k).lpts(4) .LE. 1.) THEN
	      dbuf(k).a_xyz(4) = af
	      str7 = PutStr (dbuf(k).a_xyz(4))
	      CALL PUTCHARS_REV (prpd, str7, a_line(4), 39)
	   ELSE
	      dbuf(k).i_xyz(4) = (af - dbuf(k).a_xyz(4))/(dbuf(k).lpts(4)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(4))
	      CALL PUTCHARS_REV (prpd, str7, a_line(4), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts4
c	IF (trm .EQ. K_TRM_DOWN)   GOTO (260, 260, 340) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO a5exyz
	IF (trm .EQ. K_TRM_UP)     GOTO a3exyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2225)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2225
 230  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL GetString (keyb, prpd, a_line(4), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(4) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,4,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn5
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts5
	IF (trm .EQ. K_TRM_UP)     GOTO pts3
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 235
 1230  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1235  	CALL GetString (keyb,prpd, a_line(5), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(5) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,5,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn5
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn6
	IF (trm .EQ. K_TRM_UP)     GOTO lpn4
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1235                                         
 1240  	help_line(1) = ' Number of fifth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1245  	CALL GetString (keyb,prpd, a_line(5), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(5) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz5
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn6
	IF (trm .EQ. K_TRM_UP)     GOTO mn4
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1245                                         


 3240  	help_line(1) = ' Coord of fifth motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3245 	CALL CURSOR_ABS (prpd, a_line(5), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(5), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3240
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(5) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(5) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(5) = zcoord
	      ELSE
		dbuf(k).xyz(5) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(5) = 0.0
		dbuf(k).i_xyz(5) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(5), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(5), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(5), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(5), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5
	IF (trm .EQ. K_TRM_DOWN)   GOTO xyz6
	IF (trm .EQ. K_TRM_UP)     GOTO xyz4
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3245
 240  	help_line(1) = ' Starting angle of fifth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpd, a_line(5), 15, 1, 7, trm, leng, dbuf(k).a(5))  ! a5
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(5) .NE. 0.) THEN  
	      af = dbuf(k).a(5) + (dbuf(k).lpts(5) - 1) * dbuf(k).i(5)
	   ELSE
	      af = dbuf(k).a(5)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(5), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6
	IF (trm .EQ. K_TRM_UP)     GOTO a4
	IF (trm .EQ. K_TRM_LEFT)   GOTO xyz5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 245
 2240  	help_line(1) = ' Starting coord of fifth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2245  	CALL GetString (keyb, prpd, a_line(5), 39, 1, 7, trm, leng, dbuf(k).a_xyz(5))  ! a5
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(5) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(5) + (dbuf(k).lpts(5) - 1) * dbuf(k).i_xyz(5)
	   ELSE
	      afxyz = dbuf(k).a_xyz(5)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(5), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5ixyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6xyz
	IF (trm .EQ. K_TRM_UP)     GOTO a4xyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2245
 250  	help_line(1) = ' Increment of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpd, a_line(5), 23, 1, 7, trm, leng, dbuf(k).i(5))  ! i5
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(5) .NE. 0.) THEN
	      af = dbuf(k).a(5) + (dbuf(k).lpts(5) -1)* dbuf(k).i(5)
	   ELSE
	      af = dbuf(k).a(5) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(5), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6i
	IF (trm .EQ. K_TRM_UP)     GOTO a4i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 255
 2250  	help_line(1) = ' Coord Increment of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2255  	CALL GetString (keyb, prpd, a_line(5), 47, 1, 7, trm, leng, dbuf(k).i_xyz(5))  ! i5
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(5) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(5) + (dbuf(k).lpts(5) -1)* dbuf(k).i_xyz(5)
	   ELSE
	      afxyz = dbuf(k).a_xyz(5) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(5), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5exyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6ixyz
	IF (trm .EQ. K_TRM_UP)     GOTO a4ixyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2255
 260  	help_line(1) = ' Final angle of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 265  	CALL GetString (keyb, prpd, a_line(5), 31, 1, 7, trm, leng, af)   ! a5f
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(5) .EQ. 0.) THEN
	      dbuf(k).i(5) = 0.05
	      str7 = PutStr (dbuf(k).i(5))
	      CALL PUTCHARS_REV (prpd, str7, a_line(5), 23)
	   END IF
	   dbuf(k).lpts(5) = ANINT ((af - dbuf(k).a(5)) / dbuf(k).i(5) + 1.)
	   IF (dbuf(k).lpts(5) .LT. 0.) THEN
	      dbuf(k).lpts(5)  = ABS(dbuf(k).lpts(5))
	      dbuf(k).i(5) = -1.*dbuf(k).i(5)
	      str7 = PutStr (dbuf(k).i(5))
	      CALL PUTCHARS_REV (prpd, str7, a_line(5), 23)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(5)
	   CALL PUTCHARS_REV (prpd, str4, a_line(5), 63)
	   CALL Update_Dpts(dbuf(k),a_line,5,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6e
	IF (trm .EQ. K_TRM_UP)     GOTO a4e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 265
 2260  	help_line(1) = ' Final coord of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2265  	CALL GetString (keyb, prpd, a_line(5), 55, 1, 7, trm, leng, af)   ! a5f
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(5), 55)
	   IF (dbuf(k).lpts(5) .LE. 1.) THEN
	      dbuf(k).a_xyz(5) = af
	      str7 = PutStr (dbuf(k).a_xyz(5))
	      CALL PUTCHARS_REV (prpd, str7, a_line(5), 39)
	   ELSE
	      dbuf(k).i_xyz(5) = (af - dbuf(k).a_xyz(5))/(dbuf(k).lpts(5)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(5))
	      CALL PUTCHARS_REV (prpd, str7, a_line(5), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts5
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6exyz
	IF (trm .EQ. K_TRM_UP)     GOTO a4exyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2265
1250  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1255  	CALL GetString (keyb, prpd, a_line(5), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(5) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,5,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn6
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts6
	IF (trm .EQ. K_TRM_UP)     GOTO pts4
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1255
 1260  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1265  	CALL GetString (keyb,prpd, a_line(6), 2, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpn(6) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,6,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn6
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO lpn5
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1265                                         
 1270  	help_line(1) = ' Number of sixth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1275  	CALL GetString (keyb,prpd, a_line(6), 7, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)dbuf(k).mn(6) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO xyz6
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO mn5
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1275                                         


 3270  	help_line(1) = ' Coord of sixth motor'
	help_line(2) = ' Enter X, Y, Z, or blank for none'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 3275 	CALL CURSOR_ABS (prpd, a_line(6), 12)                       !xyz1
	CALL SMG$READ_STRING (keyb, str4,, 1, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'X' .AND. str4(1:1) .NE. 'Y' 
     1		.AND. str4(1:1) .NE. 'Z' .AND. str4(1:1) .NE. ' ')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4(1:1), a_line(6), 12)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 3270
	   ELSE 
	      IF (str4(1:1) .EQ. 'X') THEN
		dbuf(k).xyz(6) = xcoord
	      ELSEIF (str4(1:1) .EQ. 'Y') THEN
		dbuf(k).xyz(6) = ycoord
	      ELSEIF (str4(1:1) .EQ. 'Z') THEN
		dbuf(k).xyz(6) = zcoord
	      ELSE
		dbuf(k).xyz(6) = nocoord
		str4(1:1)=' '
		dbuf(k).a_xyz(6) = 0.0
		dbuf(k).i_xyz(6) = 0.0
		afxyz = 0.0
	   	str7 = PutStr (afxyz)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(6), 23)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(6), 39)
	   	CALL PUTCHARS_REV (prpd, str7, a_line(6), 55)
	      ENDIF
	      CALL PUTCHARS_REV (prpd, str4(1:1), a_line(6), 12)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO xyz5
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *3275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 3275
 270  	help_line(1) = ' Starting angle of sixth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275  	CALL GetString (keyb, prpd, a_line(6), 15, 1, 7, trm, leng, dbuf(k).a(6))  ! a6
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(6) .NE. 0.) THEN  
	      af = dbuf(k).a(6) + (dbuf(k).lpts(6) - 1) * dbuf(k).i(6)
	   ELSE
	      af = dbuf(k).a(6)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(6), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6i
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO a5
	IF (trm .EQ. K_TRM_LEFT)   GOTO xyz6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 275
 2270  	help_line(1) = ' Starting coord of sixth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2275  	CALL GetString (keyb, prpd, a_line(6), 39, 1, 7, trm, leng, dbuf(k).a_xyz(6))  ! a6
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(6) .NE. 0.) THEN  
	      afxyz = dbuf(k).a_xyz(6) + (dbuf(k).lpts(6) - 1) * dbuf(k).i_xyz(6)
	   ELSE
	      afxyz = dbuf(k).a_xyz(6)
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV (prpd, str7, a_line(6), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6ixyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO a5xyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2275
 280  	help_line(1) = ' Increment of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpd, a_line(6), 23, 1, 7, trm, leng, dbuf(k).i(6))  ! i6
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(6) .NE. 0.) THEN
	      af = dbuf(k).a(6) + (dbuf(k).lpts(6) -1)* dbuf(k).i(6)
	   ELSE
	      af = dbuf(k).a(6) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV  (prpd, str7, a_line(6), 31)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6e
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO a5i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 285
 2280  	help_line(1) = ' Coord Increment of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2285  	CALL GetString (keyb, prpd, a_line(6), 47, 1, 7, trm, leng, dbuf(k).i_xyz(6))  ! i6
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).lpts(6) .NE. 0.) THEN
	      afxyz = dbuf(k).a_xyz(6) + (dbuf(k).lpts(6) -1)* dbuf(k).i_xyz(6)
	   ELSE
	      afxyz = dbuf(k).a_xyz(6) 
	   END IF
	   str7 = PutStr (afxyz)
	   CALL PUTCHARS_REV  (prpd, str7, a_line(6), 55)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6exyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO ter
	IF (trm .EQ. K_TRM_UP)     GOTO a5ixyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6xyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2285
 290  	help_line(1) = ' Final angle of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295  	CALL GetString (keyb, prpd, a_line(6), 31, 1, 7, trm, leng, af)   ! af6
	IF (leng .GE. 1) THEN
	   IF (dbuf(k).i(6) .EQ. 0.) THEN
	      dbuf(k).i(6) = 0.05
	      str7 = PutStr (dbuf(k).i(6))
	      CALL PUTCHARS_REV  (prpd, str7, a_line(6), 23)
	   END IF
	   dbuf(k).lpts(6) = ANINT ((af - dbuf(k).a(6)) / dbuf(k).i(6) + 1.)
	   IF (dbuf(k).lpts(6) .LT. 0.) THEN
	      dbuf(k).lpts(6)  = ABS(dbuf(k).lpts(6))
	      dbuf(k).i(6) = -1.*dbuf(k).i(6)
	      str7 = PutStr (dbuf(k).i(6))
	      CALL PUTCHARS_REV  (prpd, str7, a_line(6), 23)
  	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') dbuf(k).lpts(6)
	   CALL PUTCHARS_REV (prpd, str4, a_line(6), 63)
	   CALL Update_Dpts(dbuf(k),a_line,6,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6xyz
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO a5e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 295
 2290  	help_line(1) = ' Final coord of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 2295  	CALL GetString (keyb, prpd, a_line(6), 55, 1, 7, trm, leng, af)   ! af6
	IF (leng .GE. 1) THEN
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpd, str7, a_line(6), 55)
	   IF (dbuf(k).lpts(6) .LE. 1.) THEN
	      dbuf(k).a_xyz(6) = af
	      str7 = PutStr (dbuf(k).a_xyz(6))
	      CALL PUTCHARS_REV (prpd, str7, a_line(6), 39)
	   ELSE
	      dbuf(k).i_xyz(6) = (af - dbuf(k).a_xyz(6))/(dbuf(k).lpts(6)-1.)
	      str7 = PutStr (dbuf(k).i_xyz(6))
	      CALL PUTCHARS_REV (prpd, str7, a_line(6), 47)
	   ENDIF
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts6
	IF (trm .EQ. K_TRM_DOWN)   GOTO th0
	IF (trm .EQ. K_TRM_UP)     GOTO a5exyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6ixyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *2295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 2295
1300  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1305  	CALL GetString (keyb, prpd, a_line(6), 63, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  dbuf(k).lpts(6) = NINT(afn)
	  CALL Update_Dpts(dbuf(k),a_line,6,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)   GOTO th
	IF (trm .EQ. K_TRM_UP)     GOTO pts5
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6exyz
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *1305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1305
 300  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpd, tem_line, 16, 4, 6, trm, leng, dbuf(k).tmp) ! temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO (270, 270, 200) index
c	IF (trm .EQ. K_TRM_UP)     GOTO (1300, 1300, 200) index
c	IF (trm .EQ. K_TRM_LEFT)   GOTO (290, 290, 230) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (1300, 1300, 230) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 305
 310  	help_line(1) = ' Increment of the temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 315  	CALL GetString (keyb, prpd, tem_line, 29, 4, 6, trm, leng, dbuf(k).it)   ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO (280, 280, 200) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *315)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 315
 320  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpd, tem_line, 42, 2, 3, trm, leng, dbuf(k).tw)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO a6xyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *325)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 325
 330  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpd, tem_line, 51, 6, 3, trm, leng, dbuf(k).ter)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO a6ixyz
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *335)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 335
 340  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed once at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpd, tem_line, 61, 2, 3, trm, leng, dbuf(k).th0)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO auto
	IF (trm .EQ. K_TRM_UP)     GOTO (2290,2290, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 345
 1340  	help_line(1) = ' Time in minutes for a hold before each data point'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1345  	CALL GetString (keyb, prpd, tem_line, 70, 2, 3, trm, leng, dbuf(k).th)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
c	IF (trm .EQ. K_TRM_UP)     GOTO (290, 290, 220) index
	IF (trm .EQ. K_TRM_UP)     GOTO (1300, 1300, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 1345
 350  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!'
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpd, tem_line, 75)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 355	CALL SMG$CHANGE_RENDITION (prpd, tem_line, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(dbuf(k).hf,dbuf(k).ihf,dbuf(k).hfw, !field,incr,wait
     1	        dbuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpd, tem_line, 75)
	   GOTO 355
	ENDIF
	CALL SMG$CHANGE_RENDITION (prpd, tem_line, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT)  GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO pts6
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *355)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 355                 
 360  	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpd, mon_line, 19, 2, 7, trm, leng, dbuf(k).mon) ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *365)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 365
 370  	help_line(1) = ' Monitor-prefactor: give an integer or your input
	1 is rounded.' 
	help_line(2) = ' Prefactors are not allowed with PSD except when'
	help_line(3) = ' Transducer Board in use. Max then is 256.'
	CALL Put_Help (help_line)
 375  	CALL GetString (keyb, prpd, mon_line, 36, 2, 3, trm, leng, dbuf(k).mpf) ! mpf
	dbuf(k).mpf = ANINT (dbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *375)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 375
 380  	IF (nsta.eq.0)THEN
	  help_line(1) = ' Monitor-type: TIME only allowed'
	ELSE
	  help_line(1) = ' Monitor-type:  NEUT is neutron based'
	ENDIF
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 385  	CALL CURSOR_ABS (prpd, mon_line, 48)                       ! mt
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpd)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'N' .AND. str4(1:1) .NE. 'T')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpd, str4, mon_line, 48)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 385
	   ELSE 
	      IF (str4(1:1) .EQ. 'N') dbuf(k).mt = 'NEUT'
	      IF (str4(1:1) .EQ. 'T'.or.nsta.eq.0) dbuf(k).mt = 'TIME' !xray is time
	      CALL PUTCHARS_REV (prpd, dbuf(k).mt, mon_line, 48)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO auto
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_D_Angles (dbuf(k), a_line, *385)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 385
c 390  	IF (nsta.eq.0)THEN
c	  help_line(1)='No monitor on this instrument'
c	  help_line(2)=' '
c	  help_line(3)=' '
c	ELSE
c	  help_line(1) = ' You give the time at which this buffer should be
c	1   finished. Based on the '
c	  help_line(2) = ' current settings and neutron flux, the monitor is
c	1   calculated automatically. '
c	  help_line(3) = ' Press RETURN, ENTER or DO to activate this feature! '
c	ENDIF
 390	help_line(1) = 'AUTOMON NOT IMPLEMENTED FOR BT8'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpd, mon_line, 54)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 395  	CALL SMG$CHANGE_RENDITION (prpd, mon_line, 54, 1, 6,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	 IF (nsta .ne. 0) THEN
c	   CALL AutoMon (1,dbuf(k),,)
c	   CALL CURSOR_ABS (prpd, mon_line, 54)
c	   GOTO 395
c	 ELSE
c	   TRM = K_TRM_RIGHT
c	 ENDIF
c	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) TRM = K_TRM_RIGHT
	CALL SMG$CHANGE_RENDITION (prpd, mon_line, 54, 1, 6, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO mt
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_D_Angles (dbuf(k), a_line, *395)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 395
c 400  	help_line(1) = ' Enter the polarization analysis setup!'
c	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
c	help_line(3) = ' NOT IMPLEMENTED FOR BT8'
c	CALL Put_Help (help_line)
c	CALL CURSOR_ABS (prpd, mon_line, 61)
c	CALL CURSOR_OFF (paste)
c	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
c 405  	CALL SMG$CHANGE_RENDITION (prpd, mon_line, 61, 1, 6,, rendition)
c	CALL SMG$READ_KEYSTROKE (keyb, trm)
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL Pol_Setup (1,dbuf(k),)
c	   CALL PolPrep (dbuf(k).flip_state)
c	   CALL CURSOR_ABS (prpd, mon_line, 61)
c	   GOTO 405
c	END IF
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) TRM = K_TRM_RIGHT
c	CALL SMG$CHANGE_RENDITION (prpd, mon_line, 61, 1, 6, SMG$M_REVERSE)
c	CALL CURSOR_ON (paste)
c	IF (trm .EQ. K_TRM_RIGHT) GOTO com
c	IF (trm .EQ. K_TRM_DOWN)  GOTO com
c	IF (trm .EQ. K_TRM_UP)    GOTO th0
c	IF (trm .EQ. K_TRM_LEFT)  GOTO auto
c	IF (trm .EQ. K_TRM_F13)   CALL Fetch_D_Angles (dbuf(k), a_line, *405)
c	IF (trm .EQ. K_TRM_F14)   GOTO 500
c	IF (trm .EQ. K_TRM_F16)    GOTO 901
c	IF (trm .EQ. K_TRM_F17)   CALL updte_D_Buf (dbuf(k), k, line, *30)
c	IF (trm .EQ. K_TRM_F18)   CALL updte_D_Buf (dbuf(k), k, line, *700)
c	IF (trm .EQ. K_TRM_F19)   CALL updte_D_Buf (dbuf(k), k, line, *800)
c	IF (trm .EQ. K_TRM_F20)   CALL updte_D_Buf (dbuf(k), k, line, *900)
c	CALL Ring_Bell (prpd, 1)
c	GOTO 405
 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpd, mon_line, 62)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpd)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpd, 'FKEY', mon_line, 62)
	      CALL Ring_Bell (prpd, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 13 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 13) THEN
	          CALL PUTCHARS_REV (prpd, 'F13 ', mon_line, 62)
	  	  trm = K_TRM_F13
		ELSEIF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpd, 'F14 ', mon_line, 62)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpd, 'F16 ', mon_line, 62)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpd, 'F17 ', mon_line, 62)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpd, 'F18 ', mon_line, 62)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpd, 'F19 ', mon_line, 62)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpd, 'F20 ', mon_line, 62)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpd, 'FKEY', mon_line, 62)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpd, 'FKEY', mon_line, 62)
	        CALL Ring_Bell (prpd, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO com
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO auto
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_D_Angles (dbuf(k), a_line, *455)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL updte_D_Buf (dbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL updte_D_Buf (dbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL updte_D_Buf (dbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL updte_D_Buf (dbuf(k), k, line, *900)
	CALL Ring_Bell (prpd, 1)
	GOTO 450
C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C
 500  	CALL SMG$ERASE_DISPLAY (prpd, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      dbuf(i)= dbuf(k)
	   END DO
	END IF
	CALL updte_D_File (dbuf, line, *30)
C--------------------------- GO INTO CONTROL MODE ----------------------------C
 600  	IF (flip) GOTO 901         ! Prepare was called from control ==> RETURN
	flip = .TRUE.              ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20
C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 700  	CALL PUTCHARS (prpd, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpd, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$GET_DISPLAY_ATTR (prpd, row)
	CALL SMG$ERASE_LINE (prpd, row-1, 1)
	CALL CURSOR_ABS (prpd, row-1, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,, leng, trm, prpd)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR .AND. leng .GE. 2) THEN
	   CALL Parse_Prep (command, *710,8,,,,,,, dbuf,line) 
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpd, 2, 1)
	   GOTO 30
	END IF         
	CALL Ring_Bell (prpd, 1)
	GOTO 710
C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 
 800  	CALL PUTCHARS (prpd, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpd, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30
C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C
 900  	CALL PUTCHARS (prpd, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpd, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (menud, paste)
	CALL SMG$ERASE_DISPLAY (menud, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpd, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END
