#include "icp.h"
	include COMPILE_OPTS

	SUBROUTINE PrepR
C	****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Edit The Reflectometer Buffers                               C
C     Called from: ICP_MAIN                                                   C
C                                                                             C
C     Gayle Greene, December, 1992                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
#if defined(VMS)

	INCLUDE   SMGPREP_CMN   ! Program specific SMG-definitions
	INCLUDE   INSTR_CMN               ! Common variable blocks
	INCLUDE   SMGCONST_CMN   !Equate keys with SMG constants
	INCLUDE   REFL_CMN   

	INTEGER*2  zero, com, qz, qzi, qze, qx, qxi, qxe, s1, s1i, s1e
	INTEGER*2  s2, s2i, s2e, s3, s3i, s3e, s4, s4i, s4e, t0, it, tw, ter
	INTEGER*2  th0, th, hf, mon0,mon1,exp, prf, mt, pts,  pose, smode
	INTEGER*2  sx,sxi,sxe,sxnum, fkeys

	INTEGER*4  leng, trm, defno, row, rendition, i, k, ios
	INTEGER*4  paste_line, a_line(6), com_line, tem_line, current_line,
     1	           wv_line
	REAL       af, afn,qzf,qxf

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*2  str2
	CHARACTER*4  str4
	CHARACTER*6  str6
	CHARACTER*7  str7, PutStr
	CHARACTER*8  str8
	CHARACTER*9  str9 
	CHARACTER*10  str10 

	logical newlog,insert/.true./
	real    fenergy
	character*1 echar

	RECORD /r_buf/ rbuf(nbuf)


C---------------- DEFINE POSITION OF WINDOWS AND STATUS LINE -----------------C

	   paste_line = 9

	CALL SMG$PASTE_VIRTUAL_DISPLAY (MENUR, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpr, paste, paste_line, 1)


C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C

	ASSIGN 100 TO com		! Comment
	ASSIGN 110 TO qz  		! Angle 1: start
	ASSIGN 120 TO qzi 		! Angle 1: inc
	ASSIGN 130 TO qze 		! Angle 1: end
	ASSIGN 140 TO qx  		! Angle 2: start
	ASSIGN 150 TO qxi 		! Angle 2: inc
	ASSIGN 160 TO qxe 		! Angle 2: end
	ASSIGN 170 TO s1  		! Slit 1: start
	ASSIGN 180 TO s1i 		! Slit 1: inc
	ASSIGN 190 TO s1e 		! Slit 1: end
	ASSIGN 200 TO s2  		! Slit 2: start
	ASSIGN 210 TO s2i 		! Slit 2: inc
	ASSIGN 220 TO s2e 		! Slit 2: end
	ASSIGN 230 TO pts		! Number of points
	ASSIGN 240 TO s3  		! Slit 3: start
	ASSIGN 250 TO s3i 		! Slit 3: inc
	ASSIGN 260 TO s3e 		! Slit 3: end
	ASSIGN 270 TO s4  		! Slit 4: start
	ASSIGN 280 TO s4i 		! Slit 4: inc
	ASSIGN 290 TO s4e 		! Slit 4: end
	ASSIGN 1260 TO sxnum  		! Xmotor: which number motor
	ASSIGN 1270 TO sx  		! Xmotor: start
	ASSIGN 1280 TO sxi 		! Xmotor: inc
	ASSIGN 1290 TO sxe 		! Xmotor: end
	ASSIGN 1300 TO smode		! Scan mode
	ASSIGN 300 TO t0 		! Temperature
	ASSIGN 310 TO it 		! Temperature increment
	ASSIGN 320 TO tw 		! Wait after each temp change
 	ASSIGN 330 TO ter		! Temperature error
	ASSIGN 340 TO th0 		! Hold before first data point
	ASSIGN 1340 TO th 		! Hold before each data point
	ASSIGN 350 TO hf                ! H-Field
 	ASSIGN 360 TO mon0 		! Mon0
	ASSIGN 1360 TO mon1		! Mon1
	ASSIGN 1370 TO exp		! Exp
	ASSIGN 370 TO prf		! Monitor prefactor
	ASSIGN 380 TO mt 		! Monitor type
   	ASSIGN 400 TO pose		! Polarization setup
	ASSIGN 450 to fkeys		! Function key emulation
	k = 1                           ! Defno for first menu selection
	

C---------------------------- OPEN 'RBUFFER.BUF' -----------------------------C

   20  	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT',
	1     FORM= 'UNFORMATTED', SHARED)

	CALL CURSOR_ABS (MENUR, 1, 1)
	CALL PUTLINE_REVERSE (MENUR, ' No Comment (1:20)         qz-beg    qz-end   qx-beg    qx-end
	1     Monitor  NPTS')
	DO i = 1, nbuf
	   READ (1,REC=i) rbuf(i)
	   qxf = rbuf(i).qx + (rbuf(i).pts -1)* rbuf(i).qxi
	   qzf = rbuf(i).qz + (rbuf(i).pts -1)* rbuf(i).qzi
	   IF (rbuf(i).mon1 .eq. 0.) THEN
	     write(str8,fmt='(i8)')NINT(rbuf(i).mon0)
	   ELSE
	     str8='  varies'
	   ENDIF
	   WRITE (line(i),'(i2,1x,a20,f9.6,f10.6,f9.6,f10.6,i3,''*'',a8,i5)')
	2         i, rbuf(i).com(1:20), rbuf(i).qz, qzf, rbuf(i).qx,qxf, 
	3         NINT(rbuf(i).mpf), str8, NINT(rbuf(i).pts) 
	END DO
	CLOSE (1)


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

     	CALL SMG$CREATE_MENU (MENUR, line,,, 3)
	CALL SMG$DRAW_LINE (prpr, 1, 1, 1, 80)


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
	CALL SMG$SELECT_FROM_MENU (keyb, MENUR, k, defno, 
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
	   CALL CURSOR_ON(paste)
	   GOTO 701
	ELSE IF (trm .EQ. K_TRM_F19) THEN             ! Define run sequence
	   CALL CURSOR_ON (paste)
	   GOTO 801                                    
	ELSE IF (trm .EQ. K_TRM_F20) THEN                ! Return to caller
	   CALL CURSOR_ON(paste)
	   GOTO 901                                       
	ELSE IF (trm .NE. K_TRM_CR .AND. trm .NE. K_TRM_DO .AND.
	1        trm .NE. K_TRM_SELECT .AND. trm .NE. K_TRM_ENTER) THEN
	  IF (trm .eq. 7) THEN !for control G
	    current_line=2
	    CALL PUTCHARS_REV (prpr, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpr, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpr)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpr, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpr, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpr, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpr, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpr, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpr, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpr, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpr, current_line, 1)
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

	CALL SMG$ERASE_LINE (prpr, 3, 1)
	current_line = 2

	com_line = current_line 
	CALL PUTCHARS_BOLD (prpr, ' Buffer No: ', com_line, 1, 0)
	WRITE (UNIT= str2, FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpr, str2, com_line, 13, 0)
	CALL PUTCHARS (prpr, ' Comment: ', com_line, 17)
	CALL PUTCHARS_REVEOL (prpr, rbuf(k).com, com_line, 27)

	   current_line = current_line + 1
	   a_line(1) = current_line
	   CALL PUTCHARS (prpr, ' qz-beg: ', a_line(1), 17)
c	   str9 = PutStr9 (rbuf(k).qz)
	   CALL Putmax(rbuf(k).qz,str9,9,6)
	   CALL PUTCHARS_REV (prpr, str9, a_line(1), 27)
	   CALL PUTCHARS (prpr, '  Inc-qz: ', a_line(1), 36)
c	   str9 = PutStr9 (rbuf(k).qzi)
	   CALL Putmax(rbuf(k).qzi,str9,9,6)
	   CALL PUTCHARS_REV (prpr, str9, a_line(1), 45)
	   CALL PUTCHARS (prpr, '  qz-end: ', a_line(1), 54)
	   IF (rbuf(k).pts .NE. 0.) THEN
	      qzf = rbuf(k).qz + (rbuf(k).pts - 1) * rbuf(k).qzi
	   ELSE
	      qzf = rbuf(k).qz
	   END IF
	   WRITE (UNIT=str10,FMT='(f10.6)')qzf
	   CALL PUTCHARS_REV (prpr, str10, a_line(1), 64) 

	   current_line = current_line + 1
	   a_line(2) = current_line
	   CALL PUTCHARS (prpr, ' qx-beg: ', a_line(2), 17)
c	   str9 = PutStr9 (rbuf(k).qx) 
	   CALL Putmax(rbuf(k).qx,str9,9,6)
	   CALL PUTCHARS_REV (prpr, str9, a_line(2), 27)
	   CALL PUTCHARS (prpr, '  Inc-qx: ', a_line(2), 36)
c	   str9 = PutStr9 (rbuf(k).qxi) 
	   CALL Putmax(rbuf(k).qxi,str9,9,6)
	   CALL PUTCHARS_REV (prpr, str9, a_line(2), 45)
	   CALL PUTCHARS (prpr, '  qx-end: ', a_line(2), 54)
	   IF (rbuf(k).pts .NE. 0.) THEN
	      qxf = rbuf(k).qx + (rbuf(k).pts - 1) * rbuf(k).qxi
	   ELSE
	      qxf = rbuf(k).qx
	   END IF
	   WRITE(UNIT=str10,fmt='(f10.6)')qxf
	   CALL PUTCHARS_REV (prpr, str10, a_line(2), 64) 

	current_line = current_line + 1
	a_line(3) = current_line
	CALL PUTCHARS (prpr, '  Slit', a_line(3), 3)
	CALL PUTCHARS (prpr, ' S1-beg: ', a_line(3), 17)
	str7 = PutStr (rbuf(k).s(1)) 
	CALL PUTCHARS_REV (prpr, str7, a_line(3), 27)
	CALL PUTCHARS (prpr, '  Inc-S1: ', a_line(3), 34)
	str7 = PutStr (rbuf(k).i(1)) 
	CALL PUTCHARS_REV (prpr, str7, a_line(3), 43)
	CALL PUTCHARS (prpr, '  S1-end: ', a_line(3), 50)
	IF (rbuf(k).pts .NE. 0.) THEN
	   af = rbuf(k).s(1) + (rbuf(k).pts - 1) * rbuf(k).i(1)
	ELSE
	   af = rbuf(k).s(1)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpr, str7, a_line(3), 60) 
      
	current_line = current_line + 1
	a_line(4) = current_line
	CALL PUTCHARS (prpr, ' Openings:', a_line(4), 2)
	CALL PUTCHARS (prpr, ' S2-beg: ', a_line(4), 17)
	str7 = PutStr (rbuf(k).s(2)) 
	CALL PUTCHARS_REV (prpr, str7, a_line(4), 27)
	CALL PUTCHARS (prpr, '  Inc-S2: ', a_line(4), 34)
	str7 = PutStr (rbuf(k).i(2)) 
	CALL PUTCHARS_REV (prpr, str7, a_line(4), 43)
	CALL PUTCHARS (prpr, '  S2-end: ', a_line(4), 50)
	IF (rbuf(k).pts .NE. 0.) THEN
	   af = rbuf(k).s(2) + (rbuf(k).pts - 1) * rbuf(k).i(2)
	ELSE
	   af = rbuf(k).s(2)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	CALL PUTCHARS (prpr, '  #pts: ', a_line(4), 67)
	WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)

	   current_line = current_line + 1
	   a_line(5) = current_line
	   CALL PUTCHARS (prpr, ' S3-beg: ', a_line(5), 17)
	   str7 = PutStr (rbuf(k).s(3)) 
	   CALL PUTCHARS_REV (prpr, str7, a_line(5), 27)
	   CALL PUTCHARS (prpr, '  Inc-S3: ', a_line(5), 34)
	   str7 = PutStr (rbuf(k).i(3)) 
	   CALL PUTCHARS_REV (prpr, str7, a_line(5), 43)
	   CALL PUTCHARS (prpr, '  S3-end: ', a_line(5), 50)
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).s(3) + (rbuf(k).pts - 1) * rbuf(k).i(3)
	   ELSE
	      af = rbuf(k).s(3)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(5), 60) 

	   current_line = current_line + 1
	   a_line(6) = current_line
	   CALL PUTCHARS (prpr, ' S4-beg: ', a_line(6), 17)
	   str7 = PutStr (rbuf(k).s(4)) 
	   CALL PUTCHARS_REV (prpr, str7, a_line(6), 27)
	   CALL PUTCHARS (prpr, '  Inc-S4: ', a_line(6), 34)
	   str7 = PutStr (rbuf(k).i(4)) 
	   CALL PUTCHARS_REV (prpr, str7, a_line(6), 43)
 	   CALL PUTCHARS (prpr, '  S4-end: ', a_line(6), 50)
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).s(4) + (rbuf(k).pts - 1) * rbuf(k).i(4)
	   ELSE
	      af = rbuf(k).s(4)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(6), 60) 
	CALL PUTCHARS (prpr, ' scan_mode: ', a_line(6), 67)
	WRITE (UNIT= str4, FMT= '(i1)') NINT (rbuf(k).scan_mode)
	CALL PUTCHARS_REV (prpr, str4(1:1), a_line(6), 78)

	current_line = current_line + 1
	wv_line = current_line
	CALL PUTCHARS (prpr, ' XM-num: ', wv_line, 2)
	write(unit=str7(1:2),fmt='(I2)')rbuf(k).numx 
	CALL PUTCHARS_REV (prpr, str7(1:2), wv_line, 12)
	CALL PUTCHARS (prpr, ' XM-beg: ', wv_line, 17)
	str7 = PutStr (rbuf(k).sx) 
	CALL PUTCHARS_REV (prpr, str7, wv_line, 27)
	CALL PUTCHARS (prpr, '  Inc-XM: ', wv_line, 34)
	str7 = PutStr (rbuf(k).ix) 
	CALL PUTCHARS_REV (prpr, str7, wv_line, 43)
 	CALL PUTCHARS (prpr, '  XM-end: ', wv_line, 50)
	IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).sx + (rbuf(k).pts - 1) * rbuf(k).ix
	ELSE
	      af = rbuf(k).sx
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpr, str7, wv_line, 60) 
	CALL PUTCHARS_REV (prpr, ' POLA', wv_line, 71)
	CALL PUTCHARS_REV (prpr, 'FKEY', wv_line, 77)


	current_line = current_line + 1
	tem_line = current_line
	CALL PUTCHARS (prpr, ' Temp/H: ', tem_line, 2)
	CALL PUTCHARS (prpr, ' T0:', tem_line, 12)
	CALL Putmax(rbuf(k).tmp,str6,6,3)
	CALL PUTCHARS_REV (prpr, str6, tem_line, 16)
	CALL PUTCHARS (prpr, ' Inc-T:', tem_line, 22)
	CALL Putmax(rbuf(k).it,str6,6,3)
	CALL PUTCHARS_REV (prpr, str6, tem_line, 29)
	CALL PUTCHARS (prpr, '  Wait:', tem_line, 35)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(rbuf(k).tw)
	CALL PUTCHARS_REV (prpr, str6(1:3), tem_line, 42)
	CALL PUTCHARS (prpr, '  Err:', tem_line, 45)
	CALL Putmax(rbuf(k).ter,str6(1:3),3,2)
	CALL PUTCHARS_REV (prpr, str6(1:3), tem_line, 51)
	CALL PUTCHARS (prpr, '  Hld0:', tem_line, 54)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(rbuf(k).th0)
	CALL PUTCHARS_REV (prpr, str6(1:3), tem_line, 61)
	CALL PUTCHARS (prpr, '  Hld:', tem_line, 65)
	WRITE (UNIT= str6(1:3), FMT= '(i3)')NINT( rbuf(k).th)
	CALL PUTCHARS_REV (prpr, str6(1:3), tem_line, 70)
	str2(1:2)=' '
	CALL PUTCHARS (prpr, str2(1:2), tem_line, 73)
	CALL PUTCHARS_REV (prpr, 'Field', tem_line, 75)

	current_line = current_line + 1
	mon_line = current_line
	CALL PUTCHARS (prpr, ' Time:', mon_line, 2)
	CALL PUTCHARS (prpr, 'Prefac:', mon_line, 10)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(rbuf(k).mpf)
	CALL PUTCHARS_REV (prpr, str6(1:3), mon_line, 18)
	CALL PUTCHARS (prpr, 'Mon0:', mon_line, 24)
	WRITE (UNIT= str8, FMT= '(i8)') NINT(rbuf(k).mon0)
	CALL PUTCHARS_REV (prpr, str8, mon_line, 29)
	CALL PUTCHARS (prpr, 'Mon1:', mon_line, 41)
	WRITE (UNIT= str8, FMT= '(i8)') NINT(rbuf(k).mon1)
	CALL PUTCHARS_REV (prpr, str8, mon_line, 46)
	CALL PUTCHARS (prpr, '  Exp:', mon_line, 54)
	WRITE (UNIT= str6, FMT= '(f6.2)') rbuf(k).exp
	CALL PUTCHARS_REV (prpr, str6, mon_line, 60)
	CALL PUTCHARS (prpr, ' M-typ:', mon_line, 67)
	CALL PUTCHARS_REV (prpr, rbuf(k).mt, mon_line, 74)

	

C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

      
 100  	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105  	CALL CURSOR_ABS (prpr, com_line, 27)                 ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpr)
 105	CALL EDCOMMENT(prpr,keyb,com_line,27,trm,insert)
	CALL CURSOR_ABS(prpr,com_line,27)
	CALL SMG$READ_FROM_DISPLAY(prpr,comment)
	   if (rbuf(k).com(1:5).ne.comment(1:5))then
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
	   rbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REVEOL (prpr, rbuf(k).com, com_line, 27) 
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qz
	IF (trm .EQ. K_TRM_DOWN)   GOTO QZ
	IF (trm .EQ. K_TRM_UP)     GOTO mon0
	IF (trm .EQ. K_TRM_LEFT)   GOTO mt
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 105
 110  	help_line(1) = ' Starting value for qz'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115  	CALL GetStringl(keyb,prpr, a_line(1), 2 7, 1, 9, trm, leng, rbuf(k).qz)  ! qz
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      qzf = rbuf(k).qz + (rbuf(k).pts - 1) * rbuf(k).qzi
	   ELSE
	      qzf = rbuf(k).qz
	   END IF
	   WRITE (UNIT=str10,FMT='(f10.6)')qzf
	   CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qzi
	IF (trm .EQ. K_TRM_DOWN)   GOTO qx
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 115                                         
 120  	help_line(1) = ' Increment of qz'
	help_line(2) = ' NOTE: Inc-qz and qz-end are coupled to qz-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetStringl(keyb, prpr, a_line(1), 45, 1, 9, trm, leng, rbuf(k).qzi)  ! qzi
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      qzf = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	   ELSE
	      qzf = rbuf(k).qz 
	   END IF
	   WRITE (UNIT=str10,FMT='(f10.6)')qzf
	   CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qze
	IF (trm .EQ. K_TRM_DOWN)   GOTO qxi
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO qz
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 125
 130  	help_line(1) = ' Final qz'
	help_line(2) = ' NOTE: Inc-qz and qz-end are coupled to qz-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetStringf(keyb, prpr, a_line(1), 64, 1, 9, trm, leng, qzf)   ! qzf
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).qzi .EQ. 0.) THEN
	      rbuf(k).qzi = 0.05
c	      str9 = PutStr9 (rbuf(k).qzi)
	      CALL Putmax(rbuf(k).qzi,str9,9,6) 
	      CALL PUTCHARS_REV (prpr, str9, a_line(1), 45)
	   END IF
	   rbuf(k).pts = ANINT ((qzf - rbuf(k).qz) / rbuf(k).qzi + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).qzi = -1.*rbuf(k).qzi
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      qzf = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')qzf
   	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      qxf = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')qxf
   	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	   DO i = 1, 4
	      af = rbuf(k).s(i) + (rbuf(k).pts -1)* rbuf(k).i(i)
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, a_line(i+2), 60)
	   END DO 
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qx
	IF (trm .EQ. K_TRM_DOWN)   GOTO qxe
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO qzi
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *135)
 	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 135
 140  	help_line(1) = ' Starting angle of qx'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetStringl(keyb, prpr, a_line(2), 27, 1, 9, trm, leng, rbuf(k).qx)  ! qx
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      qxf = rbuf(k).qx + (rbuf(k).pts - 1) * rbuf(k).qxi
	   ELSE
	      qxf = rbuf(k).qx
	   END IF
	   WRITE(UNIT=str10,fmt='(f10.6)')qxf
	   CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qxi
	IF (trm .EQ. K_TRM_DOWN)   GOTO s1
	IF (trm .EQ. K_TRM_UP)     GOTO QZ
	IF (trm .EQ. K_TRM_LEFT)   GOTO QZe
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 145
 150  	help_line(1) = ' Increment of qx'
	help_line(2) = ' NOTE: Inc-2 and qx-end are coupled to qx-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetStringl(keyb, prpr, a_line(2), 45, 1, 9, trm, leng, rbuf(k).qxi)  ! qxi
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      qxf = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	   ELSE
	      qxf = rbuf(k).qx 
	   END IF
	   WRITE(UNIT=str10,fmt='(f10.6)')qxf
	   CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO qxe
	IF (trm .EQ. K_TRM_DOWN)   GOTO s1i
	IF (trm .EQ. K_TRM_UP)     GOTO QZi
	IF (trm .EQ. K_TRM_LEFT)   GOTO qx
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 155
 160  	help_line(1) = ' Final qx'
	help_line(2) = ' NOTE: Inc-2 and qx-end are coupled to qx-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetStringf(keyb, prpr, a_line(2), 64, 1, 10, trm, leng, af)   ! af2
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).qxi .EQ. 0.) THEN
	      rbuf(k).qxi = 0.05
c	      str9 = PutStr9 (rbuf(k).qxi)
	      CALL Putmax(rbuf(k).qxi,str9,9,6)
	      CALL PUTCHARS_REV (prpr, str9, a_line(2), 45)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).qx) / rbuf(k).qxi + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).qxi = -1.*rbuf(k).qxi
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      qzf = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')qzf
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      qxf = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')qxf
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
 	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s1
	IF (trm .EQ. K_TRM_DOWN)   GOTO s1e
	IF (trm .EQ. K_TRM_UP)     GOTO qze
	IF (trm .EQ. K_TRM_LEFT)   GOTO qxi
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 165
 170  	help_line(1) = ' Starting angle of Slit 1 '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpr, a_line(3), 27, 1, 7, trm, leng, rbuf(k).s(1))  ! s1
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      af = rbuf(k).s(1) + (rbuf(k).pts - 1) * rbuf(k).i(1)
	   ELSE
	      af = rbuf(k).s(1)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s1i
	IF (trm .EQ. K_TRM_DOWN)   GOTO s2
	IF (trm .EQ. K_TRM_UP)     GOTO Qx
	IF (trm .EQ. K_TRM_LEFT)   GOTO Qxe
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 175
 180  	help_line(1) = ' Increment of Slit 1 '
	help_line(2) = ' NOTE: Inc-1 and S1-end are coupled to S1-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpr, a_line(3), 43, 1, 7, trm, leng, rbuf(k).i(1))  ! i3
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   ELSE
	      af = rbuf(k).s(1) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s1e
	IF (trm .EQ. K_TRM_DOWN)   GOTO s2i
	IF (trm .EQ. K_TRM_UP)     GOTO QXi
	IF (trm .EQ. K_TRM_LEFT)   GOTO s1
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 185
 190  	help_line(1) = ' Final angle of Slit 1 '
	help_line(2) = ' NOTE: Inc-1 and S1-end are coupled to S1-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpr, a_line(3), 60, 1, 7, trm, leng, af)     ! af3
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).i(1) .EQ. 0.) THEN
	      rbuf(k).i(1) = 0.05
	      str7 = PutStr (rbuf(k).i(1))
	      CALL PUTCHARS_REV (prpr, str7, a_line(3), 43)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).s(1)) / rbuf(k).i(1) + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).i(1) = -1.*rbuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      af = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      af = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)	      
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s2
	IF (trm .EQ. K_TRM_DOWN)   GOTO s2e
	IF (trm .EQ. K_TRM_UP)     GOTO QXe
	IF (trm .EQ. K_TRM_LEFT)   GOTO s1i
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 195
 200  	help_line(1) = ' Starting angle of Slit 2 '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL GetString (keyb, prpr, a_line(4), 27, 1, 7, trm, leng, rbuf(k).s(2))  ! s2
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      af = rbuf(k).s(2) + (rbuf(k).pts - 1) * rbuf(k).i(2)
	   ELSE
  	      af = rbuf(k).s(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s2i
	IF (trm .EQ. K_TRM_DOWN)   GOTO s3
	IF (trm .EQ. K_TRM_UP)     GOTO s1
	IF (trm .EQ. K_TRM_LEFT)   GOTO s1e
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 205
 210  	help_line(1) = ' Increment of Slit 2 '
	help_line(2) = ' NOTE: Inc-2 and S2-end are coupled to S2-beg and #pts!!' 
	help_line(3) = ' '
	CALL Put_Help (help_line)
 215  	CALL GetString (keyb, prpr, a_line(4), 43, 1, 7, trm, leng, rbuf(k).i(2))  ! i4
	IF (leng .GE. 1) THEN
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s2e
	IF (trm .EQ. K_TRM_DOWN)   GOTO s3i
	IF (trm .EQ. K_TRM_UP)     GOTO s1i
	IF (trm .EQ. K_TRM_LEFT)   GOTO s2
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *215)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 215
 220  	help_line(1) = ' Final angle for Slit 2 '
	help_line(2) = ' NOTE: Inc-2 and S2-end are coupled to S2-beg and #pts!!'
	help_line(3) = ' '
      	CALL Put_Help (help_line)
 225  	CALL GetString (keyb, prpr, a_line(4), 60, 1, 7, trm, leng, af)   ! af4
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).i(2) .EQ. 0.) THEN
	      rbuf(k).i(2) = 0.05
	      str7 = PutStr (rbuf(k).i(2))
	      CALL PUTCHARS_REV (prpr, str7, a_line(4), 43)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).s(2)) / rbuf(k).i(2) + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).i(1) = -1.*rbuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      af = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      af = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
  	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts
	IF (trm .EQ. K_TRM_DOWN)   GOTO s3e
	IF (trm .EQ. K_TRM_UP)     GOTO s1e
	IF (trm .EQ. K_TRM_LEFT)   GOTO s2i
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *225)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 225
 230  	help_line(1) = ' Number of points for this run'
	help_line(2) = ' NOTE: #pts is coupled to S2-beg and S2-end'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL GetString (keyb, prpr, a_line(4), 75, 2, 4, trm, leng, rbuf(k).pts) ! # of points
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      rbuf(k).pts = ABS(rbuf(k).pts)
	      qzf = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')qzf
  	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      qxf = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')qxf
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      DO i = 1,4
	        af = rbuf(k).s(i) + (rbuf(k).pts -1)* rbuf(k).i(i)
	        str7 = PutStr (af)
	        CALL PUTCHARS_REV (prpr, str7, a_line(i+2), 60)
	      ENDDO
	        af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	        str7 = PutStr (af)
   	        CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	   END IF ! (* rbuf(k).pts .NE. 0*)
	END IF ! (* leng .GE. 1*)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s3
	IF (trm .EQ. K_TRM_DOWN)   GOTO smode
	IF (trm .EQ. K_TRM_UP)     GOTO s1e
	IF (trm .EQ. K_TRM_LEFT)   GOTO s2e
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 235
 240  	help_line(1) = ' Starting angle of Slit 3'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpr, a_line(5), 27, 1, 7, trm, leng, rbuf(k).s(3))  ! s3
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      af = rbuf(k).s(3) + (rbuf(k).pts - 1) * rbuf(k).i(3)
	   ELSE
	      af = rbuf(k).s(3)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s3i
	IF (trm .EQ. K_TRM_DOWN)   GOTO s4
	IF (trm .EQ. K_TRM_UP)     GOTO s2
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 245
 250  	help_line(1) = ' Increment of Slit 3'
	help_line(2) = ' NOTE: Inc-3 and S3-end are coupled to S3-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpr, a_line(5), 43, 1, 7, trm, leng, rbuf(k).i(3))  ! i5
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	   ELSE
	      af = rbuf(k).s(3) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s3e
	IF (trm .EQ. K_TRM_DOWN)   GOTO s4i
	IF (trm .EQ. K_TRM_UP)     GOTO s2i
	IF (trm .EQ. K_TRM_LEFT)   GOTO s3
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 255
 260  	help_line(1) = ' Final angle of Slit 3'
	help_line(2) = ' NOTE: Inc-3 and S3-end are coupled to S3-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 265  	CALL GetString (keyb, prpr, a_line(5), 60, 1, 7, trm, leng, af)   ! s3f
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).i(3) .EQ. 0.) THEN
	      rbuf(k).i(3) = 0.05
	      str7 = PutStr (rbuf(k).i(3))
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 43)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).s(3)) / rbuf(k).i(3) + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).i(3) = -1.*rbuf(k).i(3)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      af = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      af = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s4
	IF (trm .EQ. K_TRM_DOWN)   GOTO s4e
	IF (trm .EQ. K_TRM_UP)     GOTO s2e
	IF (trm .EQ. K_TRM_LEFT)   GOTO s3i
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 265
 270  	help_line(1) = ' Starting angle of Slit 4'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275  	CALL GetString (keyb, prpr, a_line(6), 27, 1, 7, trm, leng, rbuf(k).s(4))  ! s4
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      af = rbuf(k).s(4) + (rbuf(k).pts - 1) * rbuf(k).i(4)
	   ELSE
	      af = rbuf(k).s(4)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s4i
	IF (trm .EQ. K_TRM_DOWN)   GOTO sx
	IF (trm .EQ. K_TRM_UP)     GOTO s3
	IF (trm .EQ. K_TRM_LEFT)   GOTO s3e
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 275
 280  	help_line(1) = ' Increment of Slit 4'
	help_line(2) = ' NOTE: Inc-4 and S4-end are coupled to S4-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpr, a_line(6), 43, 1, 7, trm, leng, rbuf(k).i(4))  ! i6
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	   ELSE
	      af = rbuf(k).s(4) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO s4e
	IF (trm .EQ. K_TRM_DOWN)   GOTO sxi
	IF (trm .EQ. K_TRM_UP)     GOTO s3i
	IF (trm .EQ. K_TRM_LEFT)   GOTO s4
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 285
 290  	help_line(1) = ' Final angle of Slit 4'
	help_line(2) = ' NOTE: Inc-4 and S4-end are coupled to S4-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295  	CALL GetString (keyb, prpr, a_line(6), 60, 1, 7, trm, leng, af)   ! af6
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).i(4) .EQ. 0.) THEN
	      rbuf(k).i(4) = 0.05
	      str7 = PutStr (rbuf(k).i(4))
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 43)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).s(4)) / rbuf(k).i(4) + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).i(4) = -1.*rbuf(k).i(4)
  	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      af = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      af = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO smode
	IF (trm .EQ. K_TRM_DOWN)   GOTO sxe
	IF (trm .EQ. K_TRM_UP)     GOTO s3e
	IF (trm .EQ. K_TRM_LEFT)   GOTO s4i
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 295
 1260  	help_line(1) = ' Motor number of X motor:'
	help_line(2) = ' Must be higher than 18 and defined in instr.cfg.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1265  	CALL GetString (keyb, prpr, wv_line, 12,  2, 2, trm, leng, af) 
	rbuf(k).numx = ANINT(af)
	IF (leng .GE. 1) THEN
	  IF (rbuf(k).numx .gt. maxmots .or. rbuf(k).numx .lt. 0)THEN 
	    rbuf(k).numx = 0
	    CALL Ring_Bell (prpr, 1)
	    GOTO 1266
	  ENDIF
	  DO i = 1,pgmove
	    IF (rbuf(k).numx .eq. pglist(i))THEN
	      rbuf(k).numx = 0
	      CALL Ring_Bell (prpr, 1)
	      GOTO 1266
	    ENDIF
	  ENDDO
	  DO i = 1,bemove
	    IF (rbuf(k).numx .eq. belist(i))THEN
	      rbuf(k).numx = 0
	      CALL Ring_Bell (prpr, 1)
	      GOTO 1266
	    ENDIF
	  ENDDO
	  DO i = 1,4
	    IF (rbuf(k).numx .eq. slitlist(i))THEN
	      rbuf(k).numx = 0
	      CALL Ring_Bell (prpr, 1)
	      GOTO 1266
	    ENDIF
	  ENDDO
	  IF (rbuf(k).numx .NE. 0.) THEN  
	     IF (.not. motfl(rbuf(k).numx))THEN
		rbuf(k).numx = 0 !motor not in use
		CALL Ring_Bell (prpr, 1)
	        GOTO 1266
	     ENDIF
	  END IF
	END IF !something entered
 1266	write(unit=str7(1:2),fmt='(I2)')rbuf(k).numx 
	CALL PUTCHARS_REV (prpr, str7(1:2), wv_line, 12)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO sx
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO s4
	IF (trm .EQ. K_TRM_LEFT)   GOTO smode
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1265
 1270  	help_line(1) = ' Starting angle of Xmotor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1275  	CALL GetString (keyb, prpr, wv_line, 27, 1, 7, trm, leng, rbuf(k).sx)  ! s4
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN  
	      af = rbuf(k).sx + (rbuf(k).pts - 1) * rbuf(k).ix
	   ELSE
	      af = rbuf(k).sx
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO sxi
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO s4
	IF (trm .EQ. K_TRM_LEFT)   GOTO sxnum
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1275
 1280  	help_line(1) = ' Increment of Xmotor'
	help_line(2) = ' NOTE: Inc-X and X-end are coupled to X-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1285  	CALL GetString (keyb, prpr, wv_line, 43, 1, 7, trm, leng, rbuf(k).ix)  ! i6
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).pts .NE. 0.) THEN
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	   ELSE
	      af = rbuf(k).sx 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO sxe
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO s4i
	IF (trm .EQ. K_TRM_LEFT)   GOTO sx
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1285
 1290  	help_line(1) = ' Final angle of Xmotor'
	help_line(2) = ' NOTE: Inc-X and X-end are coupled to X-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1295  	CALL GetString (keyb, prpr, wv_line, 60, 1, 7, trm, leng, af)   ! afx
	IF (leng .GE. 1) THEN
	   IF (rbuf(k).ix .EQ. 0.) THEN
	      rbuf(k).ix = 0.05
	      str7 = PutStr (rbuf(k).ix)
	      CALL PUTCHARS_REV (prpr, str7, wv_line, 43)
	   END IF
	   rbuf(k).pts = ANINT ((af - rbuf(k).sx) / rbuf(k).ix + 1.)
	   IF (rbuf(k).pts .LT. 0.) THEN
	      rbuf(k).pts  = ABS(rbuf(k).pts)
	      rbuf(k).ix = -1.*rbuf(k).ix
  	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (rbuf(k).pts)
	   CALL PUTCHARS_REV (prpr, str4, a_line(4), 75)
	      af = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	      WRITE (UNIT=str10,FMT='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(1), 64)
	      af = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	      WRITE(UNIT=str10,fmt='(f10.6)')af
	      CALL PUTCHARS_REV (prpr, str10, a_line(2), 64)
	      af = rbuf(k).s(3) + (rbuf(k).pts -1)* rbuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(5), 60)
	      af = rbuf(k).s(4) + (rbuf(k).pts -1)* rbuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, a_line(6), 60)
	      af = rbuf(k).sx + (rbuf(k).pts -1)* rbuf(k).ix
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpr, str7, wv_line, 60)
	   af = rbuf(k).s(1) + (rbuf(k).pts -1)* rbuf(k).i(1)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(3), 60)
	   af = rbuf(k).s(2) + (rbuf(k).pts -1)* rbuf(k).i(2)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpr, str7, a_line(4), 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pose
	IF (trm .EQ. K_TRM_DOWN)   GOTO th0
	IF (trm .EQ. K_TRM_UP)     GOTO s4e
	IF (trm .EQ. K_TRM_LEFT)   GOTO sxi
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1295
 1300  	help_line(1) = ' Scan Mode'
	help_line(2) = ' Determines how motor positions are caculated based'
	help_line(3) = '   on qz and qx'
	CALL Put_Help (help_line)
 1305  	CALL GetString (keyb, prpr, a_line(6), 78, 2, 1, trm, leng, rbuf(k).scan_mode)   !
	rbuf(k).scan_mode = ANINT (rbuf(k).scan_mode)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO sxnum
	IF (trm .EQ. K_TRM_DOWN)   GOTO pose
	IF (trm .EQ. K_TRM_UP)     GOTO pts
	IF (trm .EQ. K_TRM_LEFT)   GOTO s4e
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1305
 300  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpr, tem_line, 16, 4, 6, trm, leng, rbuf(k).tmp) ! temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO sx
	IF (trm .EQ. K_TRM_LEFT)   GOTO fkeys
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 305
 310  	help_line(1) = ' Increment of the temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 315  	CALL GetString (keyb, prpr, tem_line, 29, 4, 6, trm, leng, rbuf(k).it)   ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon0
	IF (trm .EQ. K_TRM_UP)     GOTO sx
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *315)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 315
 320  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will it wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpr, tem_line, 42, 2, 4, trm, leng, rbuf(k).tw)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon1
	IF (trm .EQ. K_TRM_UP)     GOTO sxi
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *325)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 325
 330  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpr, tem_line, 51, 6, 3, trm, leng, rbuf(k).ter)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon1
	IF (trm .EQ. K_TRM_UP)     GOTO sxi
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *335)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 335
 340  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed once at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpr, tem_line, 61, 2, 3, trm, leng, rbuf(k).th)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO exp
	IF (trm .EQ. K_TRM_UP)     GOTO sxe
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 345
 1340  	help_line(1) = ' Time in minutes for a hold before each data point'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1345  	CALL GetString (keyb, prpr, tem_line, 70, 2, 3, trm, leng, rbuf(k).th)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO pose
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1345
 350  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
     1	 selected via HDEV!'
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpr, tem_line, 75)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 355	CALL SMG$CHANGE_RENDITION (prpr,tem_line,75,1,5,, rendition)
	CALL SMG$READ_KEYSTROKE(keyb,trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(rbuf(k).hf,rbuf(k).ihf,rbuf(k).hfw, !field,incr,wait
     1		rbuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpr, tem_line, 75)
	   GOTO 205
	END IF
	CALL SMG$CHANGE_RENDITION (prpr, tem_line, 75, 1, 5,SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO fkeys
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *355)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 355
 360  	help_line(1) = ' Time = Prefac * (Mon0 + Mon1*(qz**Exp))'
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' Counting by NEUTRONS disabled on this instrument'
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpr, mon_line, 29, 2, 8, trm, leng, rbuf(k).mon0) ! mon0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mon1
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *365)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 365
 1360  	help_line(1) = ' Time = Prefac * (Mon0 + Mon1*(qz**Exp))'
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' Enter 0 for constant time '
	CALL Put_Help (help_line)
 1365  	CALL GetString (keyb, prpr, mon_line, 46, 2, 8, trm, leng, rbuf(k).mon1) ! mon1
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO exp
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon0
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1365)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1365
 370  	help_line(1) = ' Time = Prefac * (Mon0 + Mon1*(qz**Exp))'
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' Counting by NEUTRONS disabled on this instrument'
	CALL Put_Help (help_line)
 375  	CALL GetString (keyb, prpr, mon_line, 18, 2, 3, trm, leng, rbuf(k).mpf) ! mpf
	rbuf(k).mpf = ANINT (rbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mon0
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *375)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 375
 1370  	help_line(1) = ' Time = Prefac * (Mon0 + Mon1*(qz**Exp))'
	help_line(2) = ' Real number in the range 0 - 2'
	help_line(3) = ' Enter 0 for constant time'
	CALL Put_Help (help_line)
 1375  	CALL GetString (keyb, prpr, mon_line, 60, 4, 6, trm, leng, rbuf(k).exp) ! temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO th0
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon1
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *1375)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 1375
 380  	help_line(1) = ' Monitor-type:  TIME is time based on seconds'
	help_line(2) = ' '
	help_line(3) = ' Counting by NEUTRONS disabled on this instrument'
	CALL Put_Help (help_line)
 385  	CALL CURSOR_ABS (prpr, mon_line, 74)                       ! mt
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpr)
	IF (leng .GE. 1) THEN
c	   IF (str4(1:1) .NE. 'N' .AND. str4(1:1) .NE. 'T')  THEN
	   IF (str4(1:1) .NE. 'T')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpr, str4, mon_line, 74)
	      CALL Ring_Bell (prpr, 2)
	      GOTO 385
	   ELSE 
c	      IF (str4(1:1) .EQ. 'N') rbuf(k).mt = 'NEUT'
	      IF (str4(1:1) .EQ. 'T') rbuf(k).mt = 'TIME'
	      CALL PUTCHARS_REV (prpr, rbuf(k).mt, mon_line, 74)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO com
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO hf
	IF (trm .EQ. K_TRM_LEFT)   GOTO exp
c	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (rbuf(k), a_line, *385)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 385
 400  	help_line(1) = ' Enter the polarization analysis setup!'
	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpr, wv_line,71)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 405  	CALL SMG$CHANGE_RENDITION (prpr, wv_line, 71, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL PolPrep (rbuf(k).flip_state)
	   CALL CURSOR_ABS (prpr, wv_line, 71)
	   GOTO 405
	END IF
	CALL SMG$CHANGE_RENDITION (prpr, wv_line, 71, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO th
	IF (trm .EQ. K_TRM_UP)    GOTO smode
	IF (trm .EQ. K_TRM_LEFT)  GOTO sxe
c	IF (trm .EQ. K_TRM_F13)   CALL Fetch_Angles (rbuf(k), a_line, *405)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 405

 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpr, wv_line, 77)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpr)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpr, 'FKEY', wv_line, 77)
	      CALL Ring_Bell (prpr, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpr, 'F14 ', wv_line, 77)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpr, 'F16 ', wv_line, 77)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpr, 'F17 ', wv_line, 77)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpr, 'F18 ', wv_line, 77)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpr, 'F19 ', wv_line, 77)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpr, 'F20 ', wv_line, 77)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpr, 'FKEY', wv_line, 77)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpr, 'FKEY', wv_line, 77)
	        CALL Ring_Bell (prpr, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)  GOTO hf
	IF (trm .EQ. K_TRM_UP)    GOTO smode
	IF (trm .EQ. K_TRM_LEFT)  GOTO pose
c	IF (trm .EQ. K_TRM_F13)   CALL Fetch_Angles (rbuf(k), a_line, *455)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_R_Buf (rbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_R_Buf (rbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL Updte_R_Buf (rbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_R_Buf (rbuf(k), k, line, *900)
	CALL Ring_Bell (prpr, 1)
	GOTO 450


C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpr, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      rbuf(i)= rbuf(k)
	   END DO
	END IF
	CALL Updte_R_File (rbuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901         ! Prepare was called from control ==> RETURN
	flip = .TRUE.              ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20
      

C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpr, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpr, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$GET_DISPLAY_ATTR (prpr, row)
	CALL SMG$ERASE_LINE (prpr, row-1, 1)
	CALL CURSOR_ABS (prpr, row-1, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,, leng, trm, prpr)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR .AND. leng .GE. 2) THEN
	   CALL Parse_Prep (command, *710,6,,,,,rbuf,,, line) 
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpr, 2, 1)
	   GOTO 30
	END IF         
	CALL Ring_Bell (prpr, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpr, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpr, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpr, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpr, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (MENUR, paste)
	CALL SMG$ERASE_DISPLAY (MENUR, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpr, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

 


