#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE PrepI
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
	INCLUDE   XRAY_CMN   !for POLY

	INTEGER*2  zero, com, a1, a1i, a1e, a2, a2i, a2e, a3, a3i, a3e
	INTEGER*2  a4, a4i, a4e, a5, a5i, a5e, a6, a6i, a6e, t0, it, tw, ter
	INTEGER*2  th0, th, hf, mon, prf, mt, pts, auto, pose, fkeys

	INTEGER*4  leng, trm, defno, row, rendition, i, k, index, ios
	INTEGER*4  paste_line, a_line(6), com_line, tem_line, current_line
	REAL       af, rval

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*2  str2
	CHARACTER*4  str4
	CHARACTER*6  str6, str6b
	CHARACTER*7  str7, PutStr

	logical newlog, insert/.true./
	real    fenergy
	character*1 echar

	logical collim7, collim15
	parameter (collim7 = .true.)
	parameter (collim15 = .false.)

	RECORD /i_buf/ ibuf(nbuf)


C---------------- DEFINE POSITION OF WINDOWS AND STATUS LINE -----------------C

	IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	    nsta .EQ. 9 .OR. nsta .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	   index = 1
	   paste_line = 9
	ELSE IF (nsta .EQ. 1) THEN
	   index = 3
	   paste_line = 13
	ELSE
	   index = 2
	   paste_line = 10
	END IF

	CALL SMG$PASTE_VIRTUAL_DISPLAY (MENUI, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpi, paste, paste_line, 1)


C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C

	ASSIGN 100 TO com		! Comment
	ASSIGN 110 TO a1  		! Angle 1: start
	ASSIGN 120 TO a1i 		! Angle 1: inc
	ASSIGN 130 TO a1e 		! Angle 1: end
	ASSIGN 140 TO a2  		! Angle 2: start
	ASSIGN 150 TO a2i 		! Angle 2: inc
	ASSIGN 160 TO a2e 		! Angle 2: end
	ASSIGN 170 TO a3  		! Angle 3: start
	ASSIGN 180 TO a3i 		! Angle 3: inc
	ASSIGN 190 TO a3e 		! Angle 3: end
	ASSIGN 200 TO a4  		! Angle 4: start
	ASSIGN 210 TO a4i 		! Angle 4: inc
	ASSIGN 220 TO a4e 		! Angle 4: end
	ASSIGN 230 TO pts		! Number of points
	ASSIGN 240 TO a5  		! Angle 5: start
	ASSIGN 250 TO a5i 		! Angle 5: inc
	ASSIGN 260 TO a5e 		! Angle 5: end
	ASSIGN 270 TO a6  		! Angle 6: start
	ASSIGN 280 TO a6i 		! Angle 6: inc
	ASSIGN 290 TO a6e 		! Angle 6: end
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
   	ASSIGN 400 TO pose		! Polarization setup
	ASSIGN 450 TO fkeys		! Function key emulation
	k = 1                           ! Defno for first menu selection
	

C---------------------------- OPEN 'IBUFFER.BUF' -----------------------------C

   20  	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT',
	1     FORM= 'UNFORMATTED', SHARED,RECL=IBUF_RECL)

	CALL CURSOR_ABS (MENUI, 1, 1)
	CALL PUTLINE_REVERSE (MENUI, ' No Comment *I-Buffer*    A4-beg  A4-end  Inc
	1    Monitor   Temp  Itemp   Twait  ')
	DO i = 1, nbuf
	   READ (1,REC=i) ibuf(i)   
	   CALL Putmax(ibuf(i).tmp,str6b,6,5)
	   CALL Putmax(ibuf(i).it,str6,6,5)
	   af = ibuf(i).a(4) + (ibuf(i).pts -1)* ibuf(i).i(4)
	   WRITE (line(i),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,1x,a6,1x,a6,f7.0)')
	2         i, ibuf(i).com(1:20), ibuf(i).a(4), af, ibuf(i).i(4), 
	3         NINT(ibuf(i).mpf), NINT(ibuf(i).mon), str6b, 
	4	  str6, ibuf(i).tw
	END DO
	CLOSE (1)


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

     	CALL SMG$CREATE_MENU (MENUI, line,,, 3)
	CALL SMG$DRAW_LINE (prpi, 1, 1, 1, 80)


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
	CALL SMG$SELECT_FROM_MENU (keyb, MENUI, k, defno, 
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
	    CALL PUTCHARS_REV (prpi, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpi, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpi)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpi, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpi, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 13 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpi, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpi, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpi, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpi, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpi, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpi, current_line, 1)
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

	CALL SMG$ERASE_LINE (prpi, 3, 1)
	current_line = 2

	IF (nsta .EQ. 1 .AND. autolim)THEN !reset motor 4 to default scan
	  IF (monobt1 .EQ. 'CU311' .OR. monobt1 .EQ. 'GE733')THEN
	    ibuf(k).a(4) = 3.0
	    ibuf(k).i(4) = .05
	    ibuf(k).pts = 201
	  ELSEIF (monobt1 .EQ. 'GE311')THEN
	    ibuf(k).a(4) = 1.3
	    ibuf(k).i(4) = .05
	    ibuf(k).pts = 201
	  ENDIF !which mono
c	  ibuf(k).flip_state(1) = collim15  !using flip_state to hold collim
	ENDIF

	com_line = current_line 
	CALL PUTCHARS_BOLD (prpi, 'IBuffer No: ', com_line, 1)
	WRITE (UNIT= str2, FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpi, str2, com_line, 13)
	CALL PUTCHARS (prpi, ' Comment: ', com_line, 17)
	CALL PUTCHARS_REVEOL (prpi, ibuf(k).com, com_line, 27)

	IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	    nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	   current_line = current_line + 1
	   a_line(1) = current_line
	   CALL PUTCHARS (prpi, ' A1-beg: ', a_line(1), 17)
	   str7 = PutStr (ibuf(k).a(1))
	   CALL PUTCHARS_REV (prpi, str7, a_line(1), 27)
	   CALL PUTCHARS (prpi, '  Inc-1: ', a_line(1), 34)
	   str7 = PutStr (ibuf(k).i(1))
	   CALL PUTCHARS_REV (prpi, str7, a_line(1), 43)
	   CALL PUTCHARS (prpi, '  A1-end: ', a_line(1), 50)
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(1) + (ibuf(k).pts - 1) * ibuf(k).i(1)
	   ELSE
	      af = ibuf(k).a(1)
	   END IF
	   str7 = PutStr (af) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(1), 60) 
	END IF      

	IF (nsta .NE. 1) THEN
	   current_line = current_line + 1
	   a_line(2) = current_line
	   CALL PUTCHARS (prpi, ' A2-beg: ', a_line(2), 17)
	   str7 = PutStr (ibuf(k).a(2)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(2), 27)
	   CALL PUTCHARS (prpi, '  Inc-2: ', a_line(2), 34)
	   str7 = PutStr (ibuf(k).i(2)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(2), 43)
	   CALL PUTCHARS (prpi, '  A2-end: ', a_line(2), 50)
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts - 1) * ibuf(k).i(2)
	   ELSE
	      af = ibuf(k).a(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(2), 60) 
	END IF      

	current_line = current_line + 1
	a_line(3) = current_line
	CALL PUTCHARS (prpi, ' A3-beg: ', a_line(3), 17)
	str7 = PutStr (ibuf(k).a(3)) 
	CALL PUTCHARS_REV (prpi, str7, a_line(3), 27)
	CALL PUTCHARS (prpi, '  Inc-3: ', a_line(3), 34)
	str7 = PutStr (ibuf(k).i(3)) 
	CALL PUTCHARS_REV (prpi, str7, a_line(3), 43)
	CALL PUTCHARS (prpi, '  A3-end: ', a_line(3), 50)
	IF (ibuf(k).pts .NE. 0.) THEN
	   af = ibuf(k).a(3) + (ibuf(k).pts - 1) * ibuf(k).i(3)
	ELSE
	   af = ibuf(k).a(3)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpi, str7, a_line(3), 60) 
      
	current_line = current_line + 1
	a_line(4) = current_line
	CALL PUTCHARS (prpi, ' Angles:', a_line(4), 3)
	CALL PUTCHARS (prpi, ' A4-beg: ', a_line(4), 17)
	str7 = PutStr (ibuf(k).a(4)) 
	CALL PUTCHARS_REV (prpi, str7, a_line(4), 27)
	CALL PUTCHARS (prpi, '  Inc-4: ', a_line(4), 34)
	str7 = PutStr (ibuf(k).i(4)) 
	CALL PUTCHARS_REV (prpi, str7, a_line(4), 43)
	CALL PUTCHARS (prpi, '  A4-end: ', a_line(4), 50)
	IF (ibuf(k).pts .NE. 0.) THEN
	   af = ibuf(k).a(4) + (ibuf(k).pts - 1) * ibuf(k).i(4)
	ELSE
	   af = ibuf(k).a(4)
	END IF
	str7 = PutStr (af)
	CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	CALL PUTCHARS (prpi, '  #pts: ', a_line(4), 67)
	WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)

	IF (nsta .NE. 1) THEN
	   current_line = current_line + 1
	   a_line(5) = current_line
	   CALL PUTCHARS (prpi, ' A5-beg: ', a_line(5), 17)
	   str7 = PutStr (ibuf(k).a(5)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(5), 27)
	   CALL PUTCHARS (prpi, '  Inc-5: ', a_line(5), 34)
	   str7 = PutStr (ibuf(k).i(5)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(5), 43)
	   CALL PUTCHARS (prpi, '  A5-end: ', a_line(5), 50)
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(5) + (ibuf(k).pts - 1) * ibuf(k).i(5)
	   ELSE
	      af = ibuf(k).a(5)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(5), 60) 

	   current_line = current_line + 1
	   a_line(6) = current_line
	   CALL PUTCHARS (prpi, ' A6-beg: ', a_line(6), 17)
	   str7 = PutStr (ibuf(k).a(6)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(6), 27)
	   CALL PUTCHARS (prpi, '  Inc-6: ', a_line(6), 34)
	   str7 = PutStr (ibuf(k).i(6)) 
	   CALL PUTCHARS_REV (prpi, str7, a_line(6), 43)
 	   CALL PUTCHARS (prpi, '  A6-end: ', a_line(6), 50)
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(6) + (ibuf(k).pts - 1) * ibuf(k).i(6)
	   ELSE
	      af = ibuf(k).a(6)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(6), 60) 
	END IF      

	current_line = current_line + 1
	tem_line = current_line
	CALL PUTCHARS (prpi, ' Temp/H: ', tem_line, 3)
	CALL PUTCHARS (prpi, ' T0:', tem_line, 12)
	CALL Putmax(ibuf(k).tmp,str6,6,5)
	CALL PUTCHARS_REV (prpi, str6, tem_line, 16)
	CALL PUTCHARS (prpi, ' Inc-T:', tem_line, 22)
	CALL Putmax(ibuf(k).it,str6,6,5)
	CALL PUTCHARS_REV (prpi, str6, tem_line, 29)
	CALL PUTCHARS (prpi, '  Wait:', tem_line, 35)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(ibuf(k).tw)
	CALL PUTCHARS_REV (prpi, str6(1:3), tem_line, 42)
	CALL PUTCHARS (prpi, '  Err:', tem_line, 45)
	CALL Putmax(ibuf(k).ter,str6(1:3),3,2)
	CALL PUTCHARS_REV (prpi, str6(1:3), tem_line, 51)
	CALL PUTCHARS (prpi, '  Hld0:', tem_line, 54)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(ibuf(k).th0)
	CALL PUTCHARS_REV (prpi, str6(1:3), tem_line, 61)
	CALL PUTCHARS (prpi, ' Hld:', tem_line, 65)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(ibuf(k).th)
	CALL PUTCHARS_REV (prpi, str6(1:3), tem_line, 70)
	str6(1:2)=' '
	CALL PUTCHARS (prpi, str6(1:2), tem_line, 73)
	CALL PUTCHARS_REV (prpi, 'Field', tem_line, 75)

	current_line = current_line + 1
	mon_line = current_line
	CALL PUTCHARS (prpi, ' Time:', mon_line, 3)
	CALL PUTCHARS (prpi, ' Monit:', mon_line, 12)
	WRITE (UNIT= str7, FMT= '(i7)') NINT(ibuf(k).mon)
	CALL PUTCHARS_REV (prpi, str7, mon_line, 19)
	CALL PUTCHARS (prpi, '   Prefac:', mon_line, 26)
	WRITE (UNIT= str6(1:3), FMT= '(i3)') NINT(ibuf(k).mpf)
	CALL PUTCHARS_REV (prpi, str6, mon_line, 36)
	CALL PUTCHARS (prpi, '   M-typ:', mon_line, 39)
	CALL PUTCHARS_REV (prpi, ibuf(k).mt, mon_line, 48)
	CALL PUTCHARS_REV (prpi, ' AMON ', mon_line, 56)
c	IF (nsta. ne. 1) THEN
	  CALL PUTCHARS_REV (prpi, ' POLA ', mon_line, 68)
c	ELSE !nsta .eq. 1
c	  IF (ibuf(k).flip_state(1) .eq. collim7)THEN
c	    str6 = '     7'
c	  ELSE
c	    str6 = '    15'
c	  ENDIF
c	  CALL PUTCHARS (prpi, '  Col:', mon_line, 62)
c	  CALL PUTCHARS_REV (prpi, str6, mon_line, 68)
c	ENDIF
	CALL PUTCHARS_REV (prpi, 'FKEY', mon_line, 75)
	

C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

      
 100  	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105  	CALL CURSOR_ABS (prpi, com_line, 27)                 ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpi)
 105	CALL EDCOMMENT(prpi,keyb,com_line,27,trm,insert)
	CALL CURSOR_ABS (prpi, com_line, 27)                 ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpi,comment)
	   if (ibuf(k).com(1:5).ne.comment(1:5))then
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
	   ibuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REVEOL (prpi, ibuf(k).com, com_line, 27) 
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO (110, 140, 170) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO (110, 140, 170) index
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO fkeys
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *105)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 105
 110  	help_line(1) = ' Starting angle of the monochromator crystal!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115  	CALL GetString (keyb,prpi, a_line(1), 27, 1, 7, trm, leng, ibuf(k).a(1))  ! a1
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(1) + (ibuf(k).pts - 1) * ibuf(k).i(1)
	   ELSE
	      af = ibuf(k).a(1)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO com
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *115)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 115                                         
 120  	help_line(1) = ' Increment of the monochromator crystal!'
	help_line(2) = ' NOTE: Inc-1 and A1-end are coupled to A1-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpi, a_line(1), 43, 1, 7, trm, leng, ibuf(k).i(1))  ! i1
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	   ELSE
	      af = ibuf(k).a(1) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a1e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2i
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *125)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 125
 130  	help_line(1) = ' Final angle of the monochromator crystal!'
	help_line(2) = ' NOTE: Inc-1 and A1-end are coupled to A1-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpi, a_line(1), 60, 1, 7, trm, leng, af)   ! a1f
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(1) .EQ. 0.) THEN
	      ibuf(k).i(1) = 0.05
	      str7 = PutStr (ibuf(k).i(1))
	      CALL PUTCHARS_REV (prpi, str7, a_line(1), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(1)) / ibuf(k).i(1) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(1) = -1.*ibuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   DO i = 1, 6
	      af = ibuf(k).a(i) + (ibuf(k).pts -1)* ibuf(k).i(i)
	      str7 = PutStr (af)
   	      CALL PUTCHARS_REV (prpi, str7, a_line(i), 60)
	   END DO 
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2
	IF (trm .EQ. K_TRM_DOWN)   GOTO a2e
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO a1i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *135)
 	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 135
 140  	help_line(1) = ' Starting angle of the monochromator drum!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL GetString (keyb, prpi, a_line(2), 27, 1, 7, trm, leng, ibuf(k).a(2))  ! a2
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(2) + (ibuf(k).pts - 1) * ibuf(k).i(2)
	   ELSE
	      af = ibuf(k).a(2)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3
	IF (trm .EQ. K_TRM_UP)     GOTO (110, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (130, 100, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *145)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 145
 150  	help_line(1) = ' Increment of the monochromator drum!'
	help_line(2) = ' NOTE: Inc-2 and A2-end are coupled to A2-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpi, a_line(2), 43, 1, 7, trm, leng, ibuf(k).i(2))  ! i2
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	   ELSE
	      af = ibuf(k).a(2) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a2e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3i
	IF (trm .EQ. K_TRM_UP)     GOTO (120, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *155)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 155
 160  	help_line(1) = ' Final angle of the monochromator drum!'
	help_line(2) = ' NOTE: Inc-2 and A2-end are coupled to A2-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpi, a_line(2), 60, 1, 7, trm, leng, af)   ! af2
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(2) .EQ. 0.) THEN
	      ibuf(k).i(2) = 0.05
	      str7 = PutStr (ibuf(k).i(2))
	      CALL PUTCHARS_REV (prpi, str7, a_line(2), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(2)) / ibuf(k).i(2) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(2) = -1.*ibuf(k).i(2)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	       nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	   END IF
	   IF (nsta .NE. 1) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
 	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	   END IF
	   af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3
	IF (trm .EQ. K_TRM_DOWN)   GOTO a3e
	IF (trm .EQ. K_TRM_UP)     GOTO (130, 100, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a2i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *165)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 165
 170  	help_line(1) = ' Starting angle of the sample table! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpi, a_line(3), 27, 1, 7, trm, leng, ibuf(k).a(3))  ! a3
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(3) + (ibuf(k).pts - 1) * ibuf(k).i(3)
	   ELSE
	      af = ibuf(k).a(3)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4
	IF (trm .EQ. K_TRM_UP)     GOTO (140, 140, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (160, 160, 100) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *175)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 175
 180  	help_line(1) = ' Increment of the sample table! '
	help_line(2) = ' NOTE: Inc-3 and A3-end are coupled to A3-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpi, a_line(3), 43, 1, 7, trm, leng, ibuf(k).i(3))  ! i3
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   ELSE
	      af = ibuf(k).a(3) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a3e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4i
	IF (trm .EQ. K_TRM_UP)     GOTO (150, 150, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *185)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 185
 190  	help_line(1) = ' Final angle of the sample table! '
	help_line(2) = ' NOTE: Inc-3 and A3-end are coupled to A3-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpi, a_line(3), 60, 1, 7, trm, leng, af)     ! af3
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(3) .EQ. 0.) THEN
	      ibuf(k).i(3) = 0.05
	      str7 = PutStr (ibuf(k).i(3))
	      CALL PUTCHARS_REV (prpi, str7, a_line(3), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(3)) / ibuf(k).i(3) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(1) = -1.*ibuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	       nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	   END IF
	   IF (nsta .NE. 1) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	      str7 = PutStr (af)	      
	      CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	      str7 = PutStr (af)	      
	      CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	   END IF
	   af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4
	IF (trm .EQ. K_TRM_DOWN)   GOTO a4e
	IF (trm .EQ. K_TRM_UP)     GOTO (160, 160, 100) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *195)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 195
 200  	help_line(1) = ' 2-theta starting angle of the first detector! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL GetString (keyb, prpi, a_line(4), 27, 1, 7, trm, leng, ibuf(k).a(4))  ! a4
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(4) + (ibuf(k).pts - 1) * ibuf(k).i(4)
	   ELSE
  	      af = ibuf(k).a(4)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4i
	IF (trm .EQ. K_TRM_DOWN)   GOTO (240, 240, 300) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3
	IF (trm .EQ. K_TRM_LEFT)   GOTO a3e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *205)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 205
 210  	help_line(1) = ' Increment of the detectors in 2-theta! '
	help_line(2) = ' NOTE: Inc-4 and A4-end are coupled to A4-beg and #pts!!' 
	help_line(3) = ' '
	CALL Put_Help (help_line)
 215  	CALL GetString (keyb, prpi, a_line(4), 43, 1, 7, trm, leng, ibuf(k).i(4))  ! i4
	IF (leng .GE. 1) THEN
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a4e
	IF (trm .EQ. K_TRM_DOWN)   GOTO (250, 250, 320) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *215)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 215
 220  	help_line(1) = ' Final 2-theta angle for the first detector! '
	help_line(2) = ' NOTE: Inc-4 and A4-end are coupled to A4-beg and #pts!!'
	help_line(3) = ' '
      	CALL Put_Help (help_line)
 225  	CALL GetString (keyb, prpi, a_line(4), 60, 1, 7, trm, leng, af)   ! af4
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(4) .EQ. 0.) THEN
	      ibuf(k).i(4) = 0.05
	      str7 = PutStr (ibuf(k).i(4))
	      CALL PUTCHARS_REV (prpi, str7, a_line(4), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(4)) / ibuf(k).i(4) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(1) = -1.*ibuf(k).i(1)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	       nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	   END IF
   	   IF (nsta .NE. 1) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	   END IF
	   af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
  	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts
	IF (trm .EQ. K_TRM_DOWN)   GOTO (260, 260, 340) index
	IF (trm .EQ. K_TRM_UP)     GOTO a3e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *225)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 225
 230  	help_line(1) = ' Number of points for this run'
	help_line(2) = ' NOTE: #pts is coupled to A4-beg and A4-end'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL GetString (keyb, prpi, a_line(4), 75, 2, 4, trm, leng, ibuf(k).pts) ! # of points
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      ibuf(k).pts = ABS(ibuf(k).pts)
	      IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	          nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	         af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	         str7 = PutStr (af)
  	         CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	      END IF
	      af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	      af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	      IF (nsta .NE. 1) THEN            
	         af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	         str7 = PutStr (af)
		 CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
		 af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	         str7 = PutStr (af)
		 CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
		 af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	         str7 = PutStr (af)
		 CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	      END IF ! (* nsta .NE. 1*)
	   END IF ! (* ibuf(k).pts .NE. 0*)
	END IF ! (* leng .GE. 1*)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO (240, 240, 300) index
	IF (trm .EQ. K_TRM_DOWN)   GOTO hf
	IF (trm .EQ. K_TRM_UP)     GOTO a3e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a4e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *235)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 235
 240  	help_line(1) = ' Starting angle of the analyzer crystal!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpi, a_line(5), 27, 1, 7, trm, leng, ibuf(k).a(5))  ! a5
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(5) + (ibuf(k).pts - 1) * ibuf(k).i(5)
	   ELSE
	      af = ibuf(k).a(5)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5i
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6
	IF (trm .EQ. K_TRM_UP)     GOTO a4
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *245)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 245
 250  	help_line(1) = ' Increment of the analyzer crystal!'
	help_line(2) = ' NOTE: Inc-5 and A5-end are coupled to A5-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL GetString (keyb, prpi, a_line(5), 43, 1, 7, trm, leng, ibuf(k).i(5))  ! i5
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	   ELSE
	      af = ibuf(k).a(5) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a5e
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6i
	IF (trm .EQ. K_TRM_UP)     GOTO a4i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *255)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 255
 260  	help_line(1) = ' Final angle of the analyzer crystal!'
	help_line(2) = ' NOTE: Inc-5 and A5-end are coupled to A5-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 265  	CALL GetString (keyb, prpi, a_line(5), 60, 1, 7, trm, leng, af)   ! a5f
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(5) .EQ. 0.) THEN
	      ibuf(k).i(5) = 0.05
	      str7 = PutStr (ibuf(k).i(5))
	      CALL PUTCHARS_REV (prpi, str7, a_line(5), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(5)) / ibuf(k).i(5) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(5) = -1.*ibuf(k).i(5)
	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	       nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(1), 60)
	   END IF
	   IF (nsta .NE. 1) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(2), 60)
	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(5), 60)
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	   END IF
	   af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(3), 60)
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(4), 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6
	IF (trm .EQ. K_TRM_DOWN)   GOTO a6e
	IF (trm .EQ. K_TRM_UP)     GOTO a4e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *265)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 265
 270  	help_line(1) = ' Starting angle of the analyzer drum!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 275  	CALL GetString (keyb, prpi, a_line(6), 27, 1, 7, trm, leng, ibuf(k).a(6))  ! a6
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN  
	      af = ibuf(k).a(6) + (ibuf(k).pts - 1) * ibuf(k).i(6)
	   ELSE
	      af = ibuf(k).a(6)
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV (prpi, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6i
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0
	IF (trm .EQ. K_TRM_UP)     GOTO a5
	IF (trm .EQ. K_TRM_LEFT)   GOTO a5e
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *275)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 275
 280  	help_line(1) = ' Increment of the analyzer drum!'
	help_line(2) = ' NOTE: Inc-6 and A6-end are coupled to A6-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 285  	CALL GetString (keyb, prpi, a_line(6), 43, 1, 7, trm, leng, ibuf(k).i(6))  ! i6
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).pts .NE. 0.) THEN
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	   ELSE
	      af = ibuf(k).a(6) 
	   END IF
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV  (prpi, str7, a_line(6), 60)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO a6e
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO a5i
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *285)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 285
 290  	help_line(1) = ' Final angle of the analyzer drum!'
	help_line(2) = ' NOTE: Inc-6 and A6-end are coupled to A6-beg and #pts!!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 295  	CALL GetString (keyb, prpi, a_line(6), 60, 1, 7, trm, leng, af)   ! af6
	IF (leng .GE. 1) THEN
	   IF (ibuf(k).i(6) .EQ. 0.) THEN
	      ibuf(k).i(6) = 0.05
	      str7 = PutStr (ibuf(k).i(6))
	      CALL PUTCHARS_REV  (prpi, str7, a_line(6), 43)
	   END IF
	   ibuf(k).pts = ANINT ((af - ibuf(k).a(6)) / ibuf(k).i(6) + 1.)
	   IF (ibuf(k).pts .LT. 0.) THEN
	      ibuf(k).pts  = ABS(ibuf(k).pts)
	      ibuf(k).i(6) = -1.*ibuf(k).i(6)
  	   END IF
	   WRITE (UNIT= str4, FMT= '(i4)') NINT (ibuf(k).pts)
	   CALL PUTCHARS_REV (prpi, str4, a_line(4), 75)
	   IF (nsta .EQ. 4 .OR. NSTA .EQ. -5 .OR. nsta .EQ. 2 .OR.
     1	       nsta .EQ. 9 .OR. NSTA .EQ. -1 .OR.
     1	    (nsta .EQ. 0 .AND. poly)) THEN !motor one is present
	      af = ibuf(k).a(1) + (ibuf(k).pts -1)* ibuf(k).i(1)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV  (prpi, str7, a_line(1), 60)
	   END IF
	   IF (nsta .NE. 1) THEN
	      af = ibuf(k).a(2) + (ibuf(k).pts -1)* ibuf(k).i(2)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV  (prpi, str7, a_line(2), 60)
	      af = ibuf(k).a(5) + (ibuf(k).pts -1)* ibuf(k).i(5)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV  (prpi, str7, a_line(5), 60)
	      af = ibuf(k).a(6) + (ibuf(k).pts -1)* ibuf(k).i(6)
	      str7 = PutStr (af)
	      CALL PUTCHARS_REV  (prpi, str7, a_line(6), 60)
	   END IF
	   af = ibuf(k).a(3) + (ibuf(k).pts -1)* ibuf(k).i(3)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV  (prpi, str7, a_line(3), 60)
	   af = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   str7 = PutStr (af)
	   CALL PUTCHARS_REV  (prpi, str7, a_line(4), 60)
	END IF                                                      
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO t0
	IF (trm .EQ. K_TRM_DOWN)   GOTO th0
	IF (trm .EQ. K_TRM_UP)     GOTO a5e
	IF (trm .EQ. K_TRM_LEFT)   GOTO a6i
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *295)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 295
 300  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 305  	CALL GetString (keyb, prpi, tem_line, 16, 4, 6, trm, leng, ibuf(k).tmp) ! temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO (270, 270, 200) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO (290, 290, 230) index
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *305)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 305
 310  	help_line(1) = ' Increment of the temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 315  	CALL GetString (keyb, prpi, tem_line, 29, 4, 6, trm, leng, ibuf(k).it)   ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO (270, 270, 200) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *315)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 315
 320  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 325  	CALL GetString (keyb, prpi, tem_line, 42, 2, 3, trm, leng, ibuf(k).tw)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO (280, 280, 210) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *325)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 325
 330  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 335  	CALL GetString (keyb, prpi, tem_line, 51, 6, 3, trm, leng, ibuf(k).ter)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO mt
	IF (trm .EQ. K_TRM_UP)     GOTO (280, 280, 210) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *335)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 335
 340  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed once at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 345  	CALL GetString (keyb, prpi, tem_line, 61, 2, 3, trm, leng, ibuf(k).th0)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO auto
	IF (trm .EQ. K_TRM_UP)     GOTO (290, 290, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 345
 1340  	help_line(1) = ' Time in minutes for a hold before each data point'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1345  	CALL GetString (keyb, prpi, tem_line, 70, 2, 3, trm, leng, ibuf(k).th)  ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO pose
	IF (trm .EQ. K_TRM_UP)     GOTO (290, 290, 220) index
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *345)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 1345
 350  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!'
	CALL Put_Help (help_line)
	CALL CURSOR_ABS (prpi, tem_line, 75)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 355	CALL SMG$CHANGE_RENDITION (prpi, tem_line, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(ibuf(k).hf,ibuf(k).ihf,ibuf(k).hfw, !field,incr,wait
     1	        ibuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpi, tem_line, 75)
	   GOTO 355
	ENDIF
	CALL SMG$CHANGE_RENDITION (prpi, tem_line, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT)  GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO pts
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *355)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 355                 
 360  	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 365  	CALL GetString (keyb, prpi, mon_line, 19, 2, 7, trm, leng, ibuf(k).mon) ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *365)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 365
 370  	help_line(1) = ' Monitor-prefactor: give an integer number or your input
	1 is rounded the next' 
	help_line(2) = ' integer. '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 375  	CALL GetString (keyb, prpi, mon_line, 36, 2, 3, trm, leng, ibuf(k).mpf) ! mpf
	ibuf(k).mpf = ANINT (ibuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *375)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 375
 380  	IF (nsta.eq.0)THEN
	  help_line(1) = ' Monitor-type: TIME only allowed'
	ELSE
	  help_line(1) = ' Monitor-type:  NEUT is neutron based'
	ENDIF
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 385  	CALL CURSOR_ABS (prpi, mon_line, 48)                       ! mt
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpi)
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'N' .AND. str4(1:1) .NE. 'T')  THEN
	      str4 = '----'
	      CALL PUTCHARS_BLINK (prpi, str4, mon_line, 48)
	      CALL Ring_Bell (prpi, 2)
	      GOTO 385
	   ELSE 
	      IF (str4(1:1) .EQ. 'N') ibuf(k).mt = 'NEUT'
	      IF (str4(1:1) .EQ. 'T'.or.nsta.eq.0) ibuf(k).mt = 'TIME' !xray is time
	      CALL PUTCHARS_REV (prpi, ibuf(k).mt, mon_line, 48)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO auto
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *385)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 385
 390  	IF (nsta.eq.0)THEN
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
	CALL CURSOR_ABS (prpi, mon_line, 56)
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 395  	CALL SMG$CHANGE_RENDITION (prpi, mon_line, 56, 1, 6,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	 IF (nsta .ne. 0) THEN
	   CALL AutoMon (1,ibuf(k),,)
	   CALL CURSOR_ABS (prpi, mon_line, 56)
	   GOTO 395
	 ELSE
	   TRM = K_TRM_RIGHT
	 ENDIF
	END IF
	CALL SMG$CHANGE_RENDITION (prpi, mon_line, 56, 1, 6, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_RIGHT) GOTO pose
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th0
	IF (trm .EQ. K_TRM_LEFT)  GOTO mt
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_Angles (ibuf(k), a_line, *395)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 395
c 400  	IF (nsta .ne. 1)THEN
 400	  help_line(1) = ' Enter the polarization analysis setup!'
	  help_line(2) = ' Press RETURN, ENTER or DO to activate this feature! '
c	ELSE
c	  help_line(1) = ' Enter collimation '
c	  help_line(2) = '    7 or 15'
c	ENDIF
	help_line(3) = ' '
	CALL Put_Help (help_line)
c	IF (nsta .ne. 1)THEN
	  CALL CURSOR_ABS (prpi, mon_line, 68)
	  CALL CURSOR_OFF (paste)
	  rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 405  	  CALL SMG$CHANGE_RENDITION (prpi, mon_line, 68, 1, 6,, rendition)
	  CALL SMG$READ_KEYSTROKE (keyb, trm)
	  IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
c	   CALL Pol_Setup (1,ibuf(k),)
	   CALL PolPrep (ibuf(k).flip_state)
	   CALL CURSOR_ABS (prpi, mon_line, 68)
	   GOTO 405
	  END IF
	  CALL SMG$CHANGE_RENDITION (prpi, mon_line, 68, 1, 6, SMG$M_REVERSE)
	  CALL CURSOR_ON (paste)
c	ELSE !bt1
c 	  CALL GetString (keyb, prpi, mon_line, 68, 2, 6, trm, leng, rval) 
c	  i = NINT (rval)                     ! nearest integer
c	  IF (i .eq. 7) THEN
c	    ibuf(k).flip_state(1) = collim7
c	  ELSEIF (i .eq. 15) THEN
c	    ibuf(k).flip_state(1) = collim15
c	  ELSE
c	    CALL PUTCHARS_BLINK (prpi, '------', mon_line, 68)
c	    CALL Ring_Bell (prpi, 1)
c	    GOTO 400
c	  ENDIF  
c	  IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) GOTO com
c	ENDIF
	IF (trm .EQ. K_TRM_RIGHT) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO th
	IF (trm .EQ. K_TRM_LEFT)  GOTO auto
	IF (trm .EQ. K_TRM_F13)   CALL Fetch_Angles (ibuf(k), a_line, *400)
	IF (trm .EQ. K_TRM_F14)   GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)   CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)   CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19)   CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)   CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 400

 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpi, mon_line, 75)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpi)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpi, 'FKEY', mon_line, 75)
	      CALL Ring_Bell (prpi, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 13 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 13) THEN
	          CALL PUTCHARS_REV (prpi, 'F13 ', mon_line, 75)
	  	  trm = K_TRM_F13
		ELSEIF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpi, 'F14 ', mon_line, 75)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpi, 'F16 ', mon_line, 75)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpi, 'F17 ', mon_line, 75)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpi, 'F18 ', mon_line, 75)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpi, 'F19 ', mon_line, 75)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpi, 'F20 ', mon_line, 75)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpi, 'FKEY', mon_line, 75)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpi, 'FKEY', mon_line, 75)
	        CALL Ring_Bell (prpi, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_RIGHT) GOTO com
	IF (trm .EQ. K_TRM_DOWN)  GOTO com
	IF (trm .EQ. K_TRM_UP)    GOTO hf
	IF (trm .EQ. K_TRM_LEFT)  GOTO pose
	IF (trm .EQ. K_TRM_F13)    CALL Fetch_Angles (ibuf(k), a_line, *455)
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_I_Buf (ibuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_I_Buf (ibuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_I_Buf (ibuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_I_Buf (ibuf(k), k, line, *900)
	CALL Ring_Bell (prpi, 1)
	GOTO 450


C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpi, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      ibuf(i)= ibuf(k)
	   END DO
	END IF
	CALL Updte_I_File (ibuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901         ! Prepare was called from control ==> RETURN
	flip = .TRUE.              ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20
      

C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpi, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpi, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$GET_DISPLAY_ATTR (prpi, row)
	CALL SMG$ERASE_LINE (prpi, row-1, 1)
	CALL CURSOR_ABS (prpi, row-1, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,, leng, trm, prpi)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR .AND. leng .GE. 2) THEN
	   CALL Parse_Prep (command, *710, 1, ibuf,,,,,,, line) 
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpi, 2, 1)
	   GOTO 30
	END IF         
	CALL Ring_Bell (prpi, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpi, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpi, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpi, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpi, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (MENUI, paste)
	CALL SMG$ERASE_DISPLAY (MENUI, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpi, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

 


	include COMPILE_OPTS
	SUBROUTINE Put_Help (line)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO DISPLAY HELP FOR EACH FIELD WHEN EDITING A BUFFER            C
C     Called from: ICP_MAIN, Prepare, Parse_Prep, Updte_I_Buf, Updte_I_File,  C
C                  Updte_Q_Buf, Updte_Q_File, Def_Sequence, Control,          C
C		   Def_Sequence, Parse_RS                                     C
C                                                                             C
C     Beat Schmid,  June 1989                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	CHARACTER*80 line(3)
	INTEGER i
	INCLUDE SMGPREP_CMN
#if defined(VMS)
	CALL SMG$ERASE_DISPLAY (help, 2, 1)
	CALL CURSOR_ABS (help, 2, 1)
	DO i= 1, 3
	   CALL PUTLINE (help, line(i))
	END DO
#endif
	END


