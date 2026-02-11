#include "icp.h"
	include COMPILE_OPTS

	SUBROUTINE PrepX
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

	INCLUDE   SMGPREP_CMN   ! Program specific SMG-definitions
	INCLUDE   INSTR_CMN               ! Common variable blocks
	INCLUDE   SMGCONST_CMN   !Equate keys with SMG constants
	INCLUDE   DARTZ_CMN   

	INTEGER*2  zero, com, a1, a1i, a1e, a2, a2i, a2e, a3, a3i, a3e
	INTEGER*2  a4, a4i, a4e, a5, a5i, a5e, a6, a6i, a6e, t0, it, tw, ter
	INTEGER*2  th0, th, hf, mon, prf, mt, pts, auto, pose, fkeys
	INTEGER*2  mn1, mn2, mn3, mn4, mn5, mn6, lpn1, lpn2, lpn3, lpn4,
     1		   lpn5, lpn6, pts1,pts2,pts3,pts4,pts5,pts6

	INTEGER*4  leng, trm, defno, row, rendition, i, k, index, ios
	INTEGER*4  paste_line, a_line(6), com_line, tem_line, current_line
	REAL       af, afn

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*2  str2
	CHARACTER*4  str4
	CHARACTER*5  str5
	CHARACTER*6  str6
	CHARACTER*7  str7, PutStr

	logical newlog, insert/.true./
	real    fenergy
	character*1 echar

	RECORD /x_buf/ xbuf(nbuf)


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

	CALL SMG$PASTE_VIRTUAL_DISPLAY (menux, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpx, paste, paste_line, 1)


C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C

	ASSIGN 100 TO com		! Comment
	ASSIGN 1100 TO lpn1  		! Angle 1: loop #
	ASSIGN 1110 TO mn1  		! Angle 1: mot #
	ASSIGN 110 TO a1  		! Angle 1: start
	ASSIGN 120 TO a1i 		! Angle 1: inc
	ASSIGN 130 TO a1e 		! Angle 1: end
	ASSIGN 1120 TO pts1  		! Angle 1: #pts
	ASSIGN 1130 TO lpn2  		! Angle 2: loop #
	ASSIGN 1140 TO mn2  		! Angle 2: mot #
	ASSIGN 140 TO a2  		! Angle 2: start
	ASSIGN 150 TO a2i 		! Angle 2: inc
	ASSIGN 160 TO a2e 		! Angle 2: end
	ASSIGN 1150 TO pts2  		! Angle 2: #pts
	ASSIGN 1160 TO lpn3  		! Angle 3: loop #
	ASSIGN 1170 TO mn3  		! Angle 3: mot #
	ASSIGN 170 TO a3  		! Angle 3: start
	ASSIGN 180 TO a3i 		! Angle 3: inc
	ASSIGN 190 TO a3e 		! Angle 3: end
	ASSIGN 1180 TO pts3  		! Angle 3: #pts
	ASSIGN 1190 TO lpn4  		! Angle 4: loop #
	ASSIGN 1200 TO mn4  		! Angle 4: mot#
	ASSIGN 200 TO a4  		! Angle 4: start
	ASSIGN 210 TO a4i 		! Angle 4: inc
	ASSIGN 220 TO a4e 		! Angle 4: end
	ASSIGN 230 TO pts4		! Angle 4: #pts
	ASSIGN 1230 TO lpn5  		! Angle 5: loop #
	ASSIGN 1240 TO mn5  		! Angle 5: mot#
	ASSIGN 240 TO a5  		! Angle 5: start
	ASSIGN 250 TO a5i 		! Angle 5: inc
	ASSIGN 260 TO a5e 		! Angle 5: end
	ASSIGN 1250 TO pts5  		! Angle 5: #pts
	ASSIGN 1260 TO lpn6  		! Angle 6: loop #
	ASSIGN 1270 TO mn6  		! Angle 6: mot#
	ASSIGN 270 TO a6  		! Angle 6: start
	ASSIGN 280 TO a6i 		! Angle 6: inc
	ASSIGN 290 TO a6e 		! Angle 6: end
	ASSIGN 1300 TO pts6  		! Angle 6: #pts
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
	

C---------------------------- OPEN 'XBUFFER.BUF' -----------------------------C

   20  	OPEN (UNIT= 1, FILE= 'XBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT',
	1     FORM= 'UNFORMATTED', SHARED,RECL=xbuf_RECL)

	CALL CURSOR_ABS (menux, 1, 1)
	CALL PUTLINE_REVERSE (menux, ' No Comment (1:35)            '//
     1	'          A     A-Beg A-Inc   A-End  Monitor')
	DO i = 1, nbuf
	   READ (1,REC=i) xbuf(i)   
	   af = xbuf(i).a(1) + (xbuf(i).lpts(1) -1)* xbuf(i).i(1)
	   WRITE (line(i),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		iostat=ios)
	2         i, xbuf(i).com(1:35),xbuf(i).mn(1), xbuf(i).a(1), 
     1		  xbuf(i).i(1),af, 
	3         NINT(xbuf(i).mpf), NINT(xbuf(i).mon)
	END DO
	CLOSE (1)


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

     	CALL SMG$CREATE_MENU (menux, line,,, 3)
	CALL SMG$DRAW_LINE (prpx, 1, 1, 1, 80)


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
	CALL SMG$SELECT_FROM_MENU (keyb, menux, k, defno, 
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
	    CALL PUTCHARS_REV (prpx, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpx, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpx)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpx, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpx, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpx, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpx, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpx, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpx, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpx, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpx, current_line, 1)
	      goto 40
	    END IF          !length greater than 1
	  ENDIF	   
	  defno = k 
	  GOTO 40                                          ! No visible action
	END IF
	CALL CURSOR_ON (paste)
	CALL PUTLINE_REVERSE (stat, ' NoChg:F16  Updte: F17   Ftch: F13   Cpy: F14   
	1B_Opr: F18   Seq: F19   Ret: F20 ')


C----------------------------- SETUP INPUT MASK ------------------------------C

	CALL SMG$ERASE_LINE (prpx, 3, 1)
	current_line = 2

	com_line = current_line 
	CALL PUTCHARS_BOLD (prpx, ' Buffer No: ', com_line, 1)
	WRITE (UNIT= str2, FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpx, str2, com_line, 13)
	CALL PUTCHARS (prpx, ' Comment: ', com_line, 17)
	CALL PUTCHARS_REVEOL (prpx, xbuf(k).com, com_line, 27)

	IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	    nsta .EQ. 9 .OR. nsta .EQ. 8) THEN !motor one is present
	   current_line = current_line + 1
	   a_line(1) = current_line
	   CALL PUTCHARS (prpx, 'Loop: ', a_line(1), 5)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(1)
	   CALL PUTCHARS_REV (prpx, str2, a_line(1), 11)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(1)
	   CALL PUTCHARS_REV (prpx, str2, a_line(1), 18)
	   CALL PUTCHARS (prpx, '-Beg: ', a_line(1), 20)
	   str7 = PutStr (xbuf(k).a(1))
	   CALL PUTCHARS_REV (prpx, str7, a_line(1), 27)
	   CALL PUTCHARS (prpx, '  Inc  : ', a_line(1), 34)
	   str7 = PutStr (xbuf(k).i(1))
	   CALL PUTCHARS_REV (prpx, str7, a_line(1), 43)
	   CALL PUTCHARS (prpx, '     End: ', a_line(1), 50)
	   IF (xbuf(k).lpts(1) .NE. 0.) THEN
	      af = xbuf(k).a(1) + (xbuf(k).lpts(1) - 1) * xbuf(k).i(1)
	   ELSE
	      af = xbuf(k).a(1)
	   END IF
	   str7 = PutStr (af) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(1), 60) 
	   CALL PUTCHARS (prpx, '  #pts: ', a_line(1), 67)
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(1)
	  CALL PUTCHARS_REV (prpx, str4, a_line(1), 75)
	END IF      

	IF (nsta .NE. 1) THEN
	   current_line = current_line + 1
	   a_line(2) = current_line
	   CALL PUTCHARS (prpx, 'Loop: ', a_line(2), 5)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(2)
	   CALL PUTCHARS_REV (prpx, str2, a_line(2), 11)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(2)
	   CALL PUTCHARS_REV (prpx, str2, a_line(2), 18)
	   CALL PUTCHARS (prpx, '-Beg: ', a_line(2), 20)
	   str7 = PutStr (xbuf(k).a(2)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(2), 27)
	   CALL PUTCHARS (prpx, '  Inc  : ', a_line(2), 34)
	   str7 = PutStr (xbuf(k).i(2)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(2), 43)
	   CALL PUTCHARS (prpx, '     End: ', a_line(2), 50)
	   IF (xbuf(k).lpts(2) .NE. 0.) THEN
	      af = xbuf(k).a(2) + (xbuf(k).lpts(2) - 1) * xbuf(k).i(2)
	   ELSE
	      af = xbuf(k).a(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(2), 60) 
	   CALL PUTCHARS (prpx, '  #pts: ', a_line(2), 67)
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(2)
	   CALL PUTCHARS_REV (prpx, str4, a_line(2), 75)
	END IF      

	current_line = current_line + 1
	a_line(3) = current_line
	CALL PUTCHARS (prpx, 'Loop: ', a_line(3), 5)
	write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(3)
	CALL PUTCHARS_REV (prpx, str2, a_line(3), 11)
	write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(3)
	CALL PUTCHARS_REV (prpx, str2, a_line(3), 18)
	CALL PUTCHARS (prpx, '-Beg: ', a_line(3), 20)
	str7 = PutStr (xbuf(k).a(3)) 
	CALL PUTCHARS_REV (prpx, str7, a_line(3), 27)
	CALL PUTCHARS (prpx, '  Inc  : ', a_line(3), 34)
	str7 = PutStr (xbuf(k).i(3)) 
	CALL PUTCHARS_REV (prpx, str7, a_line(3), 43)
	CALL PUTCHARS (prpx, '     End: ', a_line(3), 50)
	IF (xbuf(k).lpts(3) .NE. 0.) THEN
	   af = xbuf(k).a(3) + (xbuf(k).lpts(3) - 1) * xbuf(k).i(3)
	ELSE
	   af = xbuf(k).a(3)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpx, str7, a_line(3), 60) 
	CALL PUTCHARS (prpx, '  #pts: ', a_line(3), 67)
   	WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(3)
	CALL PUTCHARS_REV (prpx, str4, a_line(3), 75)
   
	current_line = current_line + 1
	a_line(4) = current_line
	CALL PUTCHARS (prpx, 'Loop: ', a_line(4), 5)
	write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(4)
	CALL PUTCHARS_REV (prpx, str2, a_line(4), 11)
	write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(4)
	CALL PUTCHARS_REV (prpx, str2, a_line(4), 18)
	CALL PUTCHARS (prpx, '-Beg: ', a_line(4), 20)
	str7 = PutStr (xbuf(k).a(4)) 
	CALL PUTCHARS_REV (prpx, str7, a_line(4), 27)
	CALL PUTCHARS (prpx, '  Inc  : ', a_line(4), 34)
	str7 = PutStr (xbuf(k).i(4)) 
	CALL PUTCHARS_REV (prpx, str7, a_line(4), 43)
	CALL PUTCHARS (prpx, '     End: ', a_line(4), 50)
	IF (xbuf(k).lpts(4) .NE. 0.) THEN
	   af = xbuf(k).a(4) + (xbuf(k).lpts(4) - 1) * xbuf(k).i(4)
	ELSE
	   af = xbuf(k).a(4)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpx, str7, a_line(4), 60)
	CALL PUTCHARS (prpx, '  #pts: ', a_line(4), 67)
	WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(4)
	CALL PUTCHARS_REV (prpx, str4, a_line(4), 75)

	IF (nsta .NE. 1) THEN
	   current_line = current_line + 1
	   a_line(5) = current_line
	   CALL PUTCHARS (prpx, 'Loop: ', a_line(5), 5)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(5)
	   CALL PUTCHARS_REV (prpx, str2, a_line(5), 11)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(5)
	   CALL PUTCHARS_REV (prpx, str2, a_line(5), 18)
	   CALL PUTCHARS (prpx, '-Beg: ', a_line(5), 20)
	   str7 = PutStr (xbuf(k).a(5)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(5), 27)
	   CALL PUTCHARS (prpx, '  Inc  : ', a_line(5), 34)
	   str7 = PutStr (xbuf(k).i(5)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(5), 43)
	   CALL PUTCHARS (prpx, '     End: ', a_line(5), 50)
	   IF (xbuf(k).lpts(5) .NE. 0.) THEN
	      af = xbuf(k).a(5) + (xbuf(k).lpts(5) - 1) * xbuf(k).i(5)
	   ELSE
	      af = xbuf(k).a(5)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(5), 60) 
	   CALL PUTCHARS (prpx, '  #pts: ', a_line(5), 67)
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(5)
	  CALL PUTCHARS_REV (prpx, str4, a_line(5), 75)

	   current_line = current_line + 1
	   a_line(6) = current_line
	   CALL PUTCHARS (prpx, 'Loop: ', a_line(6), 5)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).lpn(6)
	   CALL PUTCHARS_REV (prpx, str2, a_line(6), 11)
	   write(unit=str2,fmt='(I2)',iostat=ios)xbuf(k).mn(6)
	   CALL PUTCHARS_REV (prpx, str2, a_line(6), 18)
	   CALL PUTCHARS (prpx, '-Beg: ', a_line(6), 20)
	   str7 = PutStr (xbuf(k).a(6)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(6), 27)
	   CALL PUTCHARS (prpx, '  Inc  : ', a_line(6), 34)
	   str7 = PutStr (xbuf(k).i(6)) 
	   CALL PUTCHARS_REV (prpx, str7, a_line(6), 43)
 	   CALL PUTCHARS (prpx, '     End: ', a_line(6), 50)
	   IF (xbuf(k).lpts(6) .NE. 0.) THEN
	      af = xbuf(k).a(6) + (xbuf(k).lpts(6) - 1) * xbuf(k).i(6)
	   ELSE
	      af = xbuf(k).a(6)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(6), 60) 
	   CALL PUTCHARS (prpx, '  #pts: ', a_line(6), 67)
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(6)
	   CALL PUTCHARS_REV (prpx, str4, a_line(6), 75)
	END IF      

	current_line = current_line + 1
	tem_line = current_line
	CALL PUTCHARS (prpx, ' Temp/H: ', tem_line, 3)
	CALL PUTCHARS (prpx, ' T0:', tem_line, 12)
	CALL Putmax(xbuf(k).tmp,str6,6,3)
	CALL PUTCHARS_REV (prpx, str6, tem_line, 16)
	CALL PUTCHARS (prpx, ' Inc-T:', tem_line, 22)
	CALL Putmax(xbuf(k).it,str6,6,3)
	CALL PUTCHARS_REV (prpx, str6, tem_line, 29)
	CALL PUTCHARS (prpx, '  Wait:', tem_line, 35)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(xbuf(k).tw)
	CALL PUTCHARS_REV (prpx, str6(1:3), tem_line, 42)
	CALL PUTCHARS (prpx, '  Err:', tem_line, 45)
	CALL Putmax(xbuf(k).ter,str6(1:3),3,2)
	CALL PUTCHARS_REV (prpx, str6(1:3), tem_line, 51)
	CALL PUTCHARS (prpx, '  Hld0:', tem_line, 54)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(xbuf(k).th0)
	CALL PUTCHARS_REV (prpx, str6(1:3), tem_line, 61)
	CALL PUTCHARS (prpx, ' Hld:', tem_line, 65)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(xbuf(k).th)
	CALL PUTCHARS_REV (prpx, str6(1:3), tem_line, 70)
	str6(1:2)=' '
	CALL PUTCHARS (prpx, str6(1:2), tem_line, 73)
	CALL PUTCHARS_REV (prpx, 'Field', tem_line, 75)

	current_line = current_line + 1
	mon_line = current_line
	CALL PUTCHARS (prpx, ' Time:', mon_line, 3)
	CALL PUTCHARS (prpx, ' Monit:', mon_line, 12)
	WRITE (UNIT= str7, FMT= '(i7)') NINT(xbuf(k).mon)
	CALL PUTCHARS_REV (prpx, str7, mon_line, 19)
	CALL PUTCHARS (prpx, '   Prefac:', mon_line, 26)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(xbuf(k).mpf)
	CALL PUTCHARS_REV (prpx, str6, mon_line, 36)
	CALL PUTCHARS (prpx, '   M-typ:', mon_line, 39)
	CALL PUTCHARS_REV (prpx, xbuf(k).mt, mon_line, 48)
	CALL PUTCHARS_REV (prpx, ' AMON ', mon_line, 54)
	CALL PUTCHARS_REV (prpx, 'FKEY', mon_line, 62)
	CALL PUTCHARS (prpx, '#Pts: ', mon_line, 68)
	WRITE (UNIT= str5, FMT= '(i5)',iostat=ios)NINT( xbuf(k).pts)
	CALL PUTCHARS (prpx, str5, mon_line, 75)
	

C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

      
 100  	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105  	CALL CURSOR_ABS (prpx, com_line, 27)                 ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpx)
 105	CALL EDCOMMENT(prpx,keyb,com_line,27,trm,insert)
	CALL CURSOR_ABS (prpx, com_line, 27)                 ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpx,comment)
	   if (xbuf(k).com(1:5).ne.comment(1:5))then
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
	   xbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REVEOL (prpx, xbuf(k).com, com_line, 27) 
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO (1110, 140, 170) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO (1110, 140, 170) index
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO fkeys
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 105
 1100  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1105  	CALL GetString (keyb,prpx, a_line(1), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(1) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,1,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn1
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1105                                         
 1110  	help_line(1) = ' Number of first motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1115  	CALL GetString (keyb,prpx, a_line(1), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(1) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1115                                         
 110  	help_line(1) = ' Starting angle of first motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115  	CALL GetString (keyb,prpx, a_line(1), 27, 1, 7, trm, leng, xbuf(k).a(1))  ! a1
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(1) .NE. 0.) THEN  
	      af = xbuf(k).a(1) + (xbuf(k).lpts(1) - 1) * xbuf(k).i(1)
	   ELSE
	      af = xbuf(k).a(1)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(1), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 115                                         
 120  	help_line(1) = ' Increment of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpx, a_line(1), 43, 1, 7, trm, leng, xbuf(k).i(1))  ! i1
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(1) .NE. 0.) THEN
	      af = xbuf(k).a(1) + (xbuf(k).lpts(1) -1)* xbuf(k).i(1)
	   ELSE
	      af = xbuf(k).a(1) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(1), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2i
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 125
 130  	help_line(1) = ' Final angle of first motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpx, a_line(1), 60, 1, 7, trm, leng, af)   ! a1f
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(1) .EQ. 0.) THEN
	      xbuf(k).i(1) = 0.05
	      str7 = PutStr (xbuf(k).i(1))
	      CALL PUTCHARS_REV (prpx, str7, a_line(1), 43)
	   END IF
	   xbuf(k).lpts(1) = ANINT ((af - xbuf(k).a(1)) / xbuf(k).i(1) + 1.)
	   IF (xbuf(k).lpts(1) .LT. 0.) THEN
	      xbuf(k).lpts(1)  = ABS(xbuf(k).lpts(1))
	      xbuf(k).i(1) = -1.*xbuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(1)
	   CALL PUTCHARS_REV (prpx, str4, a_line(1), 75)
	   CALL Update_Xpts(xbuf(k),a_line,1,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts1
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2e
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *135)
 	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 135
1120  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1125  	CALL GetString (keyb, prpx, a_line(1), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(1) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,1,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn2
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1125
 1130  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1135  	CALL GetString (keyb,prpx, a_line(2), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(2) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,2,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn2
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn3
	IF (trm .EQ. K_TRM_UP)     GOTO lpn1
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1135)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1135                                         
 1140  	help_line(1) = ' Number of second motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1145  	CALL GetString (keyb,prpx, a_line(2), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(2) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn3
	IF (trm .EQ. K_TRM_UP)     GOTO mn1
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1145                                         
 140  	help_line(1) = ' Starting angle of second motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetString (keyb, prpx, a_line(2), 27, 1, 7, trm, leng, xbuf(k).a(2))  ! a2
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(2) .NE. 0.) THEN  
	      af = xbuf(k).a(2) + (xbuf(k).lpts(2) - 1) * xbuf(k).i(2)
	   ELSE
	      af = xbuf(k).a(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(2), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3
	IF (trm .EQ. K_TRM_UP)     GOTO (110, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (1140, 100, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 145
 150  	help_line(1) = ' Increment of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpx, a_line(2), 43, 1, 7, trm, leng, xbuf(k).i(2))  ! i2
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(2) .NE. 0.) THEN
	      af = xbuf(k).a(2) + (xbuf(k).lpts(2) -1)* xbuf(k).i(2)
	   ELSE
	      af = xbuf(k).a(2) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(2), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3i
	IF (trm .EQ. K_TRM_UP)     GOTO (120, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 155
 160  	help_line(1) = ' Final angle of second motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpx, a_line(2), 60, 1, 7, trm, leng, af)   ! af2
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(2) .EQ. 0.) THEN
	      xbuf(k).i(2) = 0.05
	      str7 = PutStr (xbuf(k).i(2))
	      CALL PUTCHARS_REV (prpx, str7, a_line(2), 43)
	   END IF
	   xbuf(k).lpts(2) = ANINT ((af - xbuf(k).a(2)) / xbuf(k).i(2) + 1.)
	   IF (xbuf(k).lpts(2) .LT. 0.) THEN
	      xbuf(k).lpts(2)  = ABS(xbuf(k).lpts(2))
	      xbuf(k).i(2) = -1.*xbuf(k).i(2)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(2)
	   CALL PUTCHARS_REV (prpx, str4, a_line(2), 75)
	   CALL Update_Xpts(xbuf(k),a_line,2,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts2
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3e
	IF (trm .EQ. K_TRM_UP)     GOTO (130, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 165
1150  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1155  	CALL GetString (keyb, prpx, a_line(2), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(2) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,2,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn3
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts3
	IF (trm .EQ. K_TRM_UP)     GOTO pts1
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1155
 1160  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1165  	CALL GetString (keyb,prpx, a_line(3), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(3) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,3,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn3
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn4
	IF (trm .EQ. K_TRM_UP)     GOTO lpn2
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1165                                         
 1170  	help_line(1) = ' Number of third motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1175  	CALL GetString (keyb,prpx, a_line(3), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(3) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn4
	IF (trm .EQ. K_TRM_UP)     GOTO mn2
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1175                                         
 170  	help_line(1) = ' Starting angle of third motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpx, a_line(3), 27, 1, 7, trm, leng, xbuf(k).a(3))  ! a3
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(3) .NE. 0.) THEN  
	      af = xbuf(k).a(3) + (xbuf(k).lpts(3) - 1) * xbuf(k).i(3)
	   ELSE
	      af = xbuf(k).a(3)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4
	IF (trm .EQ. K_TRM_UP)     GOTO (140, 140, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (1170, 1170, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 175
 180  	help_line(1) = ' Increment of third motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpx, a_line(3), 43, 1, 7, trm, leng, xbuf(k).i(3))  ! i3
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(3) .NE. 0.) THEN
	      af = xbuf(k).a(3) + (xbuf(k).lpts(3) -1)* xbuf(k).i(3)
	   ELSE
	      af = xbuf(k).a(3) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4i
	IF (trm .EQ. K_TRM_UP)     GOTO (150, 150, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 185
 190  	help_line(1) = ' Final angle of third motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpx, a_line(3), 60, 1, 7, trm, leng, af)     ! af3
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(3) .EQ. 0.) THEN
	      xbuf(k).i(3) = 0.05
	      str7 = PutStr (xbuf(k).i(3))
	      CALL PUTCHARS_REV (prpx, str7, a_line(3), 43)
	   END IF
	   xbuf(k).lpts(3) = ANINT ((af - xbuf(k).a(3)) / xbuf(k).i(3) + 1.)
	   IF (xbuf(k).lpts(3) .LT. 0.) THEN
	      xbuf(k).lpts(3)  = ABS(xbuf(k).lpts(3))
	      xbuf(k).i(1) = -1.*xbuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(3)
	   CALL PUTCHARS_REV (prpx, str4, a_line(3), 75)
	   CALL Update_Xpts(xbuf(k),a_line,3,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts3
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4e
	IF (trm .EQ. K_TRM_UP)     GOTO (160, 160, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 195
1180  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1185  	CALL GetString (keyb, prpx, a_line(3), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(3) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,3,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn4
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts4
	IF (trm .EQ. K_TRM_UP)     GOTO pts2
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1185
 1190  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1195  	CALL GetString (keyb,prpx, a_line(4), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(4) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,4,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn4
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn5
	IF (trm .EQ. K_TRM_UP)     GOTO lpn3
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1195                                         
 1200  	help_line(1) = ' Number of fourth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1205  	CALL GetString (keyb,prpx, a_line(4), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(4) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn5
	IF (trm .EQ. K_TRM_UP)     GOTO mn3
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1205                                         
 200  	help_line(1) = ' Starting angle of fourth motor '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL GetString (keyb, prpx, a_line(4), 27, 1, 7, trm, leng, xbuf(k).a(4))  ! a4
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(4) .NE. 0.) THEN  
	      af = xbuf(k).a(4) + (xbuf(k).lpts(4) - 1) * xbuf(k).i(4)
	   ELSE
  	      af = xbuf(k).a(4)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4i
	IF (trm .EQ. K_TRM_DOWN)   GOTO (1240, 1240, 300) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 205
 210  	help_line(1) = ' Increment of fourth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!' 
	help_line(3) = ' '
	CALL Put_Help (help_line)
 215  	CALL GetString (keyb, prpx, a_line(4), 43, 1, 7, trm, leng, xbuf(k).i(4))  ! i4
	IF (leng .GE. 1) THEN
	   af = xbuf(k).a(4) + (xbuf(k).lpts(4) -1)* xbuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4e
	IF (trm .EQ. K_TRM_DOWN)   GOTO (250, 250, 320) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *215)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 215
 220  	help_line(1) = ' Final angle of fourth motor '
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
      	CALL Put_Help (help_line)
 225  	CALL GetString (keyb, prpx, a_line(4), 60, 1, 7, trm, leng, af)   ! af4
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(4) .EQ. 0.) THEN
	      xbuf(k).i(4) = 0.05
	      str7 = PutStr (xbuf(k).i(4))
	      CALL PUTCHARS_REV (prpx, str7, a_line(4), 43)
	   END IF
	   xbuf(k).lpts(4) = ANINT ((af - xbuf(k).a(4)) / xbuf(k).i(4) + 1.)
	   IF (xbuf(k).lpts(4) .LT. 0.) THEN
	      xbuf(k).lpts(4)  = ABS(xbuf(k).lpts(4))
	      xbuf(k).i(1) = -1.*xbuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(4)
	   CALL PUTCHARS_REV (prpx, str4, a_line(4), 75)
	   CALL Update_Xpts(xbuf(k),a_line,4,mon_line)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts4
	IF (trm .EQ. K_TRM_DOWN)   GOTO (260, 260, 340) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *225)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 225
230  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
235  	CALL GetString (keyb, prpx, a_line(4), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(4) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,4,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO (1240, 1240, 300) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts5
	IF (trm .EQ. K_TRM_UP)     GOTO pts3
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 235
 1230  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1235  	CALL GetString (keyb,prpx, a_line(5), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(5) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,5,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn5
	IF (trm .EQ. K_TRM_DOWN)   GOTO lpn6
	IF (trm .EQ. K_TRM_UP)     GOTO lpn4
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1235                                         
 1240  	help_line(1) = ' Number of fifth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1245  	CALL GetString (keyb,prpx, a_line(5), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(5) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5
	IF (trm .EQ. K_TRM_DOWN)   GOTO mn6
	IF (trm .EQ. K_TRM_UP)     GOTO mn4
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1245                                         
 240  	help_line(1) = ' Starting angle of fifth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpx, a_line(5), 27, 1, 7, trm, leng, xbuf(k).a(5))  ! a5
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(5) .NE. 0.) THEN  
	      af = xbuf(k).a(5) + (xbuf(k).lpts(5) - 1) * xbuf(k).i(5)
	   ELSE
	      af = xbuf(k).a(5)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6
	IF (trm .EQ. K_TRM_UP)     GOTO a4
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 245
 250  	help_line(1) = ' Increment of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpx, a_line(5), 43, 1, 7, trm, leng, xbuf(k).i(5))  ! i5
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(5) .NE. 0.) THEN
	      af = xbuf(k).a(5) + (xbuf(k).lpts(5) -1)* xbuf(k).i(5)
	   ELSE
	      af = xbuf(k).a(5) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6i
	IF (trm .EQ. K_TRM_UP)     GOTO a4i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 255
 260  	help_line(1) = ' Final angle of fifth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 265  	CALL GetString (keyb, prpx, a_line(5), 60, 1, 7, trm, leng, af)   ! a5f
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(5) .EQ. 0.) THEN
	      xbuf(k).i(5) = 0.05
	      str7 = PutStr (xbuf(k).i(5))
	      CALL PUTCHARS_REV (prpx, str7, a_line(5), 43)
	   END IF
	   xbuf(k).lpts(5) = ANINT ((af - xbuf(k).a(5)) / xbuf(k).i(5) + 1.)
	   IF (xbuf(k).lpts(5) .LT. 0.) THEN
	      xbuf(k).lpts(5)  = ABS(xbuf(k).lpts(5))
	      xbuf(k).i(5) = -1.*xbuf(k).i(5)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(5)
	   CALL PUTCHARS_REV (prpx, str4, a_line(5), 75)
	   CALL Update_Xpts(xbuf(k),a_line,5,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts5
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6e
	IF (trm .EQ. K_TRM_UP)     GOTO a4e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 265
1250  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1255  	CALL GetString (keyb, prpx, a_line(5), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(5) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,5,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn6
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts6
	IF (trm .EQ. K_TRM_UP)     GOTO pts4
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1255
 1260  	help_line(1) = ' 1 unless moving motors in nested loops. If nesting: '//
     1	 'specify loop in which this'
	help_line(2) = ' motor is moved. Loop 1 is outermost '//
     1	 'loop. Loop 2 motors are moved through all'
	help_line(3) = ' positions for each position of Loop 1 motors.'//
     1	 ' Continues up to 6 loops max.'
	CALL Put_Help (help_line)
 1265  	CALL GetString (keyb,prpx, a_line(6), 11, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpn(6) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,6,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mn6
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO lpn5
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1265                                         
 1270  	help_line(1) = ' Number of sixth motor to scan'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1275  	CALL GetString (keyb,prpx, a_line(6), 18, 2, 2, trm, leng, afn)
	IF (leng .GE. 1)xbuf(k).mn(6) = NINT(afn)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO mn5
	IF (trm .EQ. K_TRM_LEFT)   GOTO lpn6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1275                                         
 270  	help_line(1) = ' Starting angle of sixth motor'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275  	CALL GetString (keyb, prpx, a_line(6), 27, 1, 7, trm, leng, xbuf(k).a(6))  ! a6
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(6) .NE. 0.) THEN  
	      af = xbuf(k).a(6) + (xbuf(k).lpts(6) - 1) * xbuf(k).i(6)
	   ELSE
	      af = xbuf(k).a(6)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpx, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6i
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO a5
	IF (trm .EQ. K_TRM_LEFT)   GOTO mn6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 275
 280  	help_line(1) = ' Increment of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpx, a_line(6), 43, 1, 7, trm, leng, xbuf(k).i(6))  ! i6
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).lpts(6) .NE. 0.) THEN
	      af = xbuf(k).a(6) + (xbuf(k).lpts(6) -1)* xbuf(k).i(6)
	   ELSE
	      af = xbuf(k).a(6) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV  (prpx, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6e
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO a5i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 285
 290  	help_line(1) = ' Final angle of sixth motor'
	help_line(2) = ' NOTE: Inc and end are coupled to beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295  	CALL GetString (keyb, prpx, a_line(6), 60, 1, 7, trm, leng, af)   ! af6
	IF (leng .GE. 1) THEN
	   IF (xbuf(k).i(6) .EQ. 0.) THEN
	      xbuf(k).i(6) = 0.05
	      str7 = PutStr (xbuf(k).i(6))
	      CALL PUTCHARS_REV  (prpx, str7, a_line(6), 43)
	   END IF
	   xbuf(k).lpts(6) = ANINT ((af - xbuf(k).a(6)) / xbuf(k).i(6) + 1.)
	   IF (xbuf(k).lpts(6) .LT. 0.) THEN
	      xbuf(k).lpts(6)  = ABS(xbuf(k).lpts(6))
	      xbuf(k).i(6) = -1.*xbuf(k).i(6)
  	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') xbuf(k).lpts(6)
	   CALL PUTCHARS_REV (prpx, str4, a_line(6), 75)
	   CALL Update_Xpts(xbuf(k),a_line,6,mon_line)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts6
	IF (trm .EQ. K_TRM_DOWN)   GOTO th0
	IF (trm .EQ. K_TRM_UP)     GOTO a5e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 295
1300  	help_line(1) = ' Number of positions scanned by this motor'
	help_line(2) = ' NOTE: #pts is coupled to beg and end for this motor'
	help_line(3) = ' '
	CALL Put_Help (help_line)
1305  	CALL GetString (keyb, prpx, a_line(6), 75, 2, 4, trm, leng, afn)
	IF (leng .GE. 1)THEN
	  xbuf(k).lpts(6) = NINT(afn)
	  CALL Update_Xpts(xbuf(k),a_line,6,mon_line)
	ENDIF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)   GOTO hf
	IF (trm .EQ. K_TRM_UP)     GOTO pts5
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *1305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1305
 300  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpx, tem_line, 16, 4, 6, trm, leng, xbuf(k).tmp) ! temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO (1270, 1270, 200) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (290, 290, 230) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 305
 310  	help_line(1) = ' Increment of the temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 315  	CALL GetString (keyb, prpx, tem_line, 29, 4, 6, trm, leng, xbuf(k).it)   ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO (270, 270, 200) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *315)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 315
 320  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpx, tem_line, 42, 2, 3, trm, leng, xbuf(k).tw)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO (280, 280, 210) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *325)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 325
 330  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpx, tem_line, 51, 6, 3, trm, leng, xbuf(k).ter)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO (280, 280, 210) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *335)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 335
 340  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed once at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpx, tem_line, 61, 2, 3, trm, leng, xbuf(k).th0)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO auto
	IF (trm .EQ. K_TRM_UP)     GOTO (290, 290, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 345
 1340  	help_line(1) = ' Time in minutes for a hold before each data point'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1345  	CALL GetString (keyb, prpx, tem_line, 70, 2, 3, trm, leng, xbuf(k).th)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO (290, 290, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 1345
 350  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!'
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpx, tem_line, 75)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 355	CALL SMG$CHANGE_RENDITION (prpx, tem_line, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(xbuf(k).hf,xbuf(k).ihf,xbuf(k).hfw, !field,incr,wait
     1	        xbuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpx, tem_line, 75)
	   GOTO 355
	ENDIF
	CALL SMG$CHANGE_RENDITION (prpx, tem_line, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT)  GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO pts6
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *355)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 355                 
 360  	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpx, mon_line, 19, 2, 7, trm, leng, xbuf(k).mon) ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *365)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 365
 370  	help_line(1) = ' Monitor-prefactor: give an integer or your input
	1 is rounded.' 
	help_line(2) = ' Prefactors are not allowed with PSD except when'
	help_line(3) = ' Transducer Board in use. Max then is 256.'
	CALL Put_Help (help_line)
 375  	CALL GetString (keyb, prpx, mon_line, 36, 2, 3, trm, leng, xbuf(k).mpf) ! mpf
	xbuf(k).mpf = ANINT (xbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *375)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 375
 380  	IF (nsta.eq.0)THEN
	  help_line(1) = ' Monitor-type: TIME only allowed'
	ELSE
	  help_line(1) = ' Monitor-type:  NEUT is neutron based'
	ENDIF
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 385  	CALL CURSOR_ABS (prpx, mon_line, 48)                       ! mt
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpx)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'N' .AND. str4(1:1) .NE. 'T')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpx, str4, mon_line, 48)
	      CALL Ring_Bell (prpx, 2)
	      GOTO 385
	   ELSE 
	      IF (str4(1:1) .EQ. 'N') xbuf(k).mt = 'NEUT'
	      IF (str4(1:1) .EQ. 'T'.or.nsta.eq.0) xbuf(k).mt = 'TIME' !xray is time
	      CALL PUTCHARS_REV (prpx, xbuf(k).mt, mon_line, 48)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO auto
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_X_Angles (xbuf(k), a_line, *385)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
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
	CALL CURSOR_ABS (prpx, mon_line, 54)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 395  	CALL SMG$CHANGE_RENDITION (prpx, mon_line, 54, 1, 6,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	 IF (nsta .ne. 0) THEN
c	   CALL AutoMon (1,xbuf(k),,)
c	   CALL CURSOR_ABS (prpx, mon_line, 54)
c	   GOTO 395
c	 ELSE
c	   TRM = K_TRM_RIGHT
c	 ENDIF
c	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) TRM = K_TRM_RIGHT
	CALL SMG$CHANGE_RENDITION (prpx, mon_line, 54, 1, 6, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO mt
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_X_Angles (xbuf(k), a_line, *395)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 395

c 400  	help_line(1) = ' Enter the polarization analysis setup!'
c	help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
c	help_line(3) = ' NOT IMPLEMENTED FOR BT8'
c	CALL Put_Help (help_line)
c	CALL CURSOR_ABS (prpx, mon_line, 61)
c	CALL CURSOR_OFF (paste)
c	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
c 405  	CALL SMG$CHANGE_RENDITION (prpx, mon_line, 61, 1, 6,, rendition)
c	CALL SMG$READ_KEYSTROKE (keyb, trm)
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL Pol_Setup (1,xbuf(k),)
c	   CALL PolPrep (xbuf(k).flip_state)
c	   CALL CURSOR_ABS (prpx, mon_line, 61)
c	   GOTO 405
c	END IF
c	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) TRM = K_TRM_RIGHT
c	CALL SMG$CHANGE_RENDITION (prpx, mon_line, 61, 1, 6, SMG$M_REVERSE)
c	CALL CURSOR_ON (paste)
c	IF (trm .EQ. K_TRM_RIGHT) GOTO com
c	IF (trm .EQ. K_TRM_DOWN)  GOTO com
c	IF (trm .EQ. K_TRM_UP)    GOTO th0
c	IF (trm .EQ. K_TRM_LEFT)  GOTO auto
c	IF (trm .EQ. K_TRM_F13)   CALL Fetch_X_Angles (xbuf(k), a_line, *405)
c	IF (trm .EQ. K_TRM_F14)   GOTO 500
c	IF (trm .EQ. K_TRM_F16)    GOTO 901
c	IF (trm .EQ. K_TRM_F17)   CALL updte_X_Buf (xbuf(k), k, line, *30)
c	IF (trm .EQ. K_TRM_F18)   CALL updte_X_Buf (xbuf(k), k, line, *700)
c	IF (trm .EQ. K_TRM_F19)   CALL updte_X_Buf (xbuf(k), k, line, *800)
c	IF (trm .EQ. K_TRM_F20)   CALL updte_X_Buf (xbuf(k), k, line, *900)
c	CALL Ring_Bell (prpx, 1)
c	GOTO 405


 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpx, mon_line, 62)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpx)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpx, 'FKEY', mon_line, 62)
	      CALL Ring_Bell (prpx, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 13 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 13) THEN
	          CALL PUTCHARS_REV (prpx, 'F13 ', mon_line, 62)
	  	  trm = K_TRM_F13
		ELSEIF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpx, 'F14 ', mon_line, 62)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpx, 'F16 ', mon_line, 62)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpx, 'F17 ', mon_line, 62)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpx, 'F18 ', mon_line, 62)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpx, 'F19 ', mon_line, 62)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpx, 'F20 ', mon_line, 62)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpx, 'FKEY', mon_line, 62)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpx, 'FKEY', mon_line, 62)
	        CALL Ring_Bell (prpx, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO com
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO auto
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_X_Angles (xbuf(k), a_line, *455)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL updte_X_Buf (xbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL updte_X_Buf (xbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL updte_X_Buf (xbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL updte_X_Buf (xbuf(k), k, line, *900)
	CALL Ring_Bell (prpx, 1)
	GOTO 450


C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpx, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      xbuf(i)= xbuf(k)
	   END DO
	END IF
	CALL updte_X_File (xbuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901         ! Prepare was called from control ==> RETURN
	flip = .TRUE.              ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20
      

C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpx, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpx, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$GET_DISPLAY_ATTR (prpx, row)
	CALL SMG$ERASE_LINE (prpx, row-1, 1)
	CALL CURSOR_ABS (prpx, row-1, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,, leng, trm, prpx)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR .AND. leng .GE. 2) THEN
	   CALL Parse_Prep (command, *710,7,,,,,, xbuf,,line) 
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpx, 2, 1)
	   GOTO 30
	END IF         
	CALL Ring_Bell (prpx, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpx, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpx, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpx, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpx, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (menux, paste)
	CALL SMG$ERASE_DISPLAY (menux, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpx, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

