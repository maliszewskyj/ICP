#include "icp.h"
	include COMPILE_OPTS

	SUBROUTINE PrepT
C	****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C     Routine To Edit The Trash Buffers                                       C
C     Called from: ICP_MAIN, Control                                          C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT   none
#if defined(VMS)

	INCLUDE   SMGPREP_CMN   ! Program specific SMG-definitions
	INCLUDE   INSTR_CMN               ! Common variable blocks
	INCLUDE   SMGCONST_CMN   !Equate keys with SMG constants

	INTEGER*2  com, ec, es, ef, m3, t0, it, tw, th0, th, ter, hf
	INTEGER*2  mon, prf, mt, pts, fkeys

	INTEGER*4  leng, trm, defno, rendition, i, k, t_line, e_line
	INTEGER*4  ios,current_line

	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*50 comment, command
	CHARACTER*7  PutStr, str7
	CHARACTER*8  str8
	CHARACTER*2  str2
	CHARACTER*4  str4

	logical newlog, insert/.true./
	real fenergy
	character*1 echar

	RECORD /t_buf/ tbuf(nbuf)

	CALL SMG$PASTE_VIRTUAL_DISPLAY (menut, paste, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (prpt, paste, 13, 1)


C-------------------- ASSIGN VALUES TO THE INPUT FIELDS ----------------------C

	ASSIGN 100 TO com		! Comment
	ASSIGN 110 TO ec 		! Energy center
	ASSIGN 120 TO es 		! Energy step
	ASSIGN 130 TO ef		! Fixed energy
	ASSIGN 140 TO m3 		! Move angle 3 during scan
	ASSIGN 150 TO t0 		! Temperature
	ASSIGN 160 TO it 		! Temperature increment
	ASSIGN 170 TO tw 		! Wait after each temp change
 	ASSIGN 180 TO ter		! Temperature error
	ASSIGN 190 TO th0 		! Hold before first data point
	ASSIGN 1190 TO th 		! Hold before each data point
	ASSIGN 200 TO hf                ! H-Field
 	ASSIGN 210 TO mon 		! Monitor
	ASSIGN 220 TO prf		! Monitor prefactor
	ASSIGN 230 TO mt 		! Monitor type
	ASSIGN 240 TO pts		! Number of points 	
	ASSIGN 450 TO fkeys		! Funciton key emulation
	k = 1                           ! Defno for first menu selection
	

C----------------------------- OPEN TBUFFER.BUF ------------------------------C

  20	OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=TBUF_RECL)
	DO i = 1, nbuf
	   READ (1,REC=i) tbuf(i)   
	   CALL Putmax(tbuf(i).tmp,str7,7,5)
	   WRITE (line(i),'(i2,1x,a20,1x,f8.2,2f7.2,i7,1x,a7,i7)')
	2       i, tbuf(i).com (1:20), tbuf(i).ec, tbuf(i).es, tbuf(i).ef, 
	3	NINT(tbuf(i).mon), str7, NINT(tbuf(i).pts)
	END DO
	CLOSE (1)


	CALL CURSOR_ABS (menut, 1, 1)
	CALL PUTLINE_REVERSE (menut, ' No Comment *T-Buffer*        EC     ES    
	1FixE   Mon     Tmp     Pts ',)


C--------------------- CREATE MENU WITH BUFFER CONTENTS ----------------------C

	CALL SMG$CREATE_MENU (menut, line,,, 3)
	CALL SMG$DRAW_LINE (prpt, 1, 1, 1, 80)


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
	CALL SMG$SELECT_FROM_MENU (keyb, menut, k, defno, 
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
	1        trm .NE. K_TRM_SELECT .AND. trm .NE. K_TRM_ENTER) THEN
	  IF (trm .eq. 7) THEN !for control G
	    current_line=2
	    CALL PUTCHARS_REV (prpt, 'FKEY', current_line, 5)
 50	    help_line(1) = ' Type function sequence '
	    help_line(2) = '     Example, type the three keys F 2 0'
	    help_line(3) = '     to get same effect as typing Function key'
	    CALL Put_Help (help_line)
 55  	    CALL CURSOR_ABS (prpt, current_line, 5)                       ! Func
	    CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpt)
	    i = 0 !default value in case no key series entered
	    IF (leng .GE. 1) THEN
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	        CALL PUTCHARS_REV (prpt, 'FKEY', current_line, 5)
	        CALL Ring_Bell (prpt, 2)
	        GOTO 55
	      ELSE 
	        IF (str4(2:4) .EQ. 'KEY') GOTO 55
	        READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	        IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		  IF (i .eq. 14) THEN !supercopy
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 500
		  ELSEIF (i .eq. 16) THEN                ! Abort prep mode
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
		    CALL CURSOR_ON (paste)
	 	    GOTO 901                                       
		  ELSEIF (i .eq. 17) THEN                    ! Goto control
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 600
		  ELSEIF (i .eq. 18) THEN               ! Buffer operations
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 701
		  ELSEIF (i .eq. 19) THEN             ! Define run sequence
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 801                                    
		  ELSEIF (i .eq. 20) THEN                ! Return to caller
	      	    CALL SMG$ERASE_LINE (prpt, current_line, 1)
	   	    CALL CURSOR_ON (paste)
	   	    GOTO 901                                       
		  ENDIF
	          CALL PUTCHARS_REV (prpt, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpt, 2)
	          GOTO 55
	        ELSE !invalid value read
	          CALL PUTCHARS_REV (prpt, 'FKEY', current_line, 5)
	          CALL Ring_Bell (prpt, 2)
	          GOTO 55
	        ENDIF !i was a valid number
	      END IF ! Found an F as first character
	    ELSE !length not greater than 1
	      CALL SMG$ERASE_LINE (prpt, current_line, 1)
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

	CALL SMG$ERASE_LINE (prpt, 3, 1)

	CALL PUTCHARS_BOLD (prpt, 'TBuffer No: ', 2, 1)
	WRITE (UNIT= str8(1:2), FMT= '(i2)') k
	CALL PUTCHARS_BOLD (prpt, str8(1:2), 2, 13)
	CALL PUTCHARS (prpt, ' Comment: ', 2, 17)
	CALL PUTCHARS_REVEOL (prpt, tbuf(k).com, 2, 27)

	e_line = 3
	CALL PUTCHARS (prpt, ' Energy:  ', e_line, 3)
	CALL PUTCHARS (prpt, ' EC:', e_line, 12)
	str8(1:7) = PutStr (tbuf(k).ec)
	CALL PUTCHARS_REV (prpt, str8(1:7), e_line, 16)
	CALL PUTCHARS (prpt, ' ES:', e_line, 23)
	str8(1:7) = PutStr (tbuf(k).es)
	CALL PUTCHARS_REV (prpt, str8(1:7), e_line, 27)
	CALL PUTCHARS (prpt, ' FE:', e_line, 34)
	str8(1:7) = PutStr (tbuf(k).ef)
	CALL PUTCHARS_REV (prpt, str8(1:7), e_line, 38)
	CALL PUTCHARS (prpt, ' dM=', e_line, 52)
	str8(1:7) = PutStr (dm)
	CALL PUTCHARS (prpt, str8(1:7), e_line, 56)
	CALL PUTCHARS (prpt, ' dA=', e_line, 65)
	str8(1:7) = PutStr (da)
	CALL PUTCHARS (prpt, str8(1:7), e_line, 69)

	CALL PUTCHARS (prpt, ' Angle 3:  ', 4, 3)
	CALL PUTCHARS (prpt, ' M3:', 4, 12)
	IF (tbuf(k).m3) THEN 
	   str8(1:3) = 'YES'
	ELSE
    	   STR8(1:3) = 'NO '
	END IF
	CALL PUTCHARS_REV (prpt, str8(1:3), 4, 16)

	t_line = 5
	CALL PUTCHARS (prpt, ' Temp/H: ', t_line, 3)
	CALL PUTCHARS (prpt, ' T0:', t_line, 12)
	CALL Putmax(tbuf(k).tmp,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpt, str8(1:6), t_line, 16)
	CALL PUTCHARS (prpt, '  Inc-T:', t_line, 22)
	CALL Putmax(tbuf(k).it,str8(1:6),6,5)
	CALL PUTCHARS_REV (prpt, str8(1:6), t_line, 29)
	CALL PUTCHARS (prpt, '  Wait:', t_line, 35)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(tbuf(k).tw)
	CALL PUTCHARS_REV (prpt, str8(1:3), t_line, 42)
	CALL PUTCHARS (prpt, '  Err:', t_line, 45)
	CALL Putmax(tbuf(k).ter,str8(1:3),3,2)
	CALL PUTCHARS_REV (prpt, str8(1:3), t_line, 51)
	CALL PUTCHARS (prpt, '  Hld0:', t_line, 54)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(tbuf(k).th0)
	CALL PUTCHARS_REV (prpt, str8(1:3), t_line, 61)
	CALL PUTCHARS (prpt, ' Hld:', t_line, 65)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(tbuf(k).th)
	CALL PUTCHARS_REV (prpt, str8(1:3), t_line, 70)
	str2(1:2)=' '
	CALL PUTCHARS (prpt, str2(1:2), t_line, 73)
	CALL PUTCHARS_REV (prpt, 'Field', t_line, 75)

      
	mon_line = 6
	CALL PUTCHARS (prpt, ' Time: ', mon_line, 3) 
	CALL PUTCHARS (prpt, ' Monit:', mon_line, 12)
	WRITE (UNIT= str8(1:7), FMT= '(i7)') NINT(tbuf(k).mon)
	CALL PUTCHARS_REV (prpt, str8(1:7), mon_line, 19)
	CALL PUTCHARS (prpt, '  Prefac:', mon_line, 26)
	WRITE (UNIT= str8(1:3), FMT= '(i3)') NINT(tbuf(k).mpf)
	CALL PUTCHARS_REV (prpt, str8(1:3), mon_line, 35)
	CALL PUTCHARS (prpt, '  M-typ:', mon_line, 38)
	CALL PUTCHARS_REV (prpt, tbuf(k).mt, mon_line, 46)
	CALL PUTCHARS (prpt, '  #pts:', mon_line, 50)
	WRITE (UNIT= str8(1:4), FMT= '(i4)') NINT(tbuf(k).pts)
	CALL PUTCHARS_REV (prpt, str8(1:4), mon_line, 57)
	CALL PUTCHARS_REV (prpt, 'FKEY', mon_line, 65)



C---------------------- GET NEW INPUT FOR THE BUFFERS ------------------------C

 100	help_line(1) = ' Give the comment for this buffer. Maximum length: 50
	1 characters '
	help_line(2) = ' NOTE: THE FIRST FIVE (5) CHARACTERS DEFINE YOUR DATA
	2 FILE !!! '
	help_line(3) = ' '
	CALL Put_Help (help_line)
c 105	CALL CURSOR_ABS (prpt, 2, 27)                    ! Comment
c	CALL SMG$READ_STRING (keyb, comment,, 50, m$_nenr,,, leng, trm, prpt)
 105	CALL EDCOMMENT(prpt,keyb,2,27,trm,insert)
	CALL CURSOR_ABS (prpt, 2, 27)                    ! Comment
	CALL SMG$READ_FROM_DISPLAY(prpt,comment)
	   if (tbuf(k).com(1:5).ne.comment(1:5))then
	     newlog=.true.
	     fenergy=tbuf(k).ef
	     echar='A'
	   else
	     newlog=.false.
	   endif
	   tbuf(k).com(1:) = comment(1:)
	   CALL PUTCHARS_REV (prpt, tbuf(k).com, 2, 27)
	   if (newlog)call getlog(comment(1:5),fenergy,echar)
	   CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ec
	IF (trm .EQ. K_TRM_DOWN)   GOTO ec
	IF (trm .EQ. K_TRM_UP)     GOTO mon
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 105
 110  	help_line(1) = ' Center of energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 115  	CALL GetString (keyb, prpt, e_line, 16, 3, 7, trm, leng, tbuf(k).ec)    ! e0
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO es
	IF (trm .EQ. K_TRM_DOWN)   GOTO m3
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 115
 120  	help_line(1) = ' Energy-step in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 125  	CALL GetString (keyb, prpt, e_line, 27, 3, 7, trm, leng, tbuf(k).es)    ! es
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ef
	IF (trm .EQ. K_TRM_DOWN)   GOTO it
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO ec
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 125
 130  	help_line(1) = ' Fixed energy in meV!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 135  	CALL GetString (keyb, prpt, e_line, 38, 3, 7, trm, leng, tbuf(k).ef)    ! ef
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO m3
	IF (trm .EQ. K_TRM_DOWN)   GOTO tw
	IF (trm .EQ. K_TRM_UP)     GOTO com
	IF (trm .EQ. K_TRM_LEFT)   GOTO es
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 135
 140  	help_line(1) = ' Move Angle 3 during scan in order to maintain Q-
	1 direction!'
	help_line(2) = ' Type ''YES'' or ''NO''!'                    
	help_line(3) = ' '
	CALL Put_Help (help_line)
 145  	CALL CURSOR_ABS (prpt, 4, 16)                              ! m3
	CALL SMG$READ_STRING (keyb, str8(1:3),, 3, m$_nenrcl,,, leng, trm)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'N' .AND. str8(1:1) .NE. 'Y')  THEN
	      str8(1:3) = '---'
	      CALL PUTCHARS_BLINK (prpt, str8(1:3), 4, 16)
	      CALL Ring_Bell (prpt, 2)
	      GOTO 145
	   ELSE 
	      IF (str8(1:1) .EQ. 'N') THEN
		 tbuf(k).m3 = .FALSE.
		 str8(1:3)  = 'NO '
	      ELSE 
		 tbuf(k).m3 = .TRUE.
		 str8(1:3)  = 'YES'
	      END IF
	      CALL PUTCHARS_REV (prpt, str8(1:3), 4, 16)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO t0 
	IF (trm .EQ. K_TRM_DOWN)   GOTO t0 
	IF (trm .EQ. K_TRM_UP)     GOTO ec
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef 
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 235
 150  	help_line(1) = ' Temperature set-point for the temp-controller! '
	help_line(2) = ' Note: The value 0.0 is a special flag.'
	help_line(3) = ' Temp will not be changed in buffer if 0.0 is here'
	CALL Put_Help (help_line)
 155  	CALL GetString (keyb, prpt, t_line, 16, 4, 6, trm, leng, tbuf(k).tmp)  !temp
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO it
	IF (trm .EQ. K_TRM_DOWN)   GOTO mon
	IF (trm .EQ. K_TRM_UP)     GOTO m3
	IF (trm .EQ. K_TRM_LEFT)   GOTO ef  
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 155
 160  	help_line(1) = ' Increment in temperature! '
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 165  	CALL GetString (keyb, prpt, t_line, 29, 4, 6, trm, leng, tbuf(k).it)    ! it
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO tw
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO es
	IF (trm .EQ. K_TRM_LEFT)   GOTO t0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 165
 170  	help_line(1) = ' Max-time in minutes to pause the data aquisition, every
	1 time the temperature is '
	help_line(2) = ' changed by the controller!  The program will wait
	1 until Max-time expires or the'
	help_line(3) = ' temperature is within the given error of the setpoint.'
	CALL Put_Help (help_line)
 175  	CALL GetString (keyb, prpt, t_line, 42, 2, 4, trm, leng, tbuf(k).tw)    ! tw
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO ter
	IF (trm .EQ. K_TRM_DOWN)   GOTO prf
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO it
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 175
 180  	help_line(1) = ' Error in degrees. If temperature is within
	1 this error of the setpoint, ICP'
	help_line(2) = ' continues to take data even if Max-time has not
	1 expired yet.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 185  	CALL GetString (keyb, prpt, t_line, 51, 6, 3, trm, leng, tbuf(k).ter)  ! ter
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th0
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO tw
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 185
 190  	help_line(1) = ' Time in minutes for a single hold before any data is
	1 taken, i.e. the hold is'
	help_line(2) = ' executed at the very beginning of the run.'
	help_line(3) = ' Maximum hold time is 999 minutes (about 16 hours)'
	CALL Put_Help (help_line)
 195  	CALL GetString (keyb, prpt, t_line, 61, 2, 3, trm, leng, tbuf(k).th0)    ! th
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO th
	IF (trm .EQ. K_TRM_DOWN)   GOTO pts
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO ter
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 195
 1190  	help_line(1) = ' Time in minutes for a hold before each data point'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 1195  	CALL GetString (keyb, prpt, t_line, 70, 2, 3, trm, leng, tbuf(k).th)    ! th
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO hf
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO th0
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 1195

 200  	help_line(1) = ' Magnetic Field'
	help_line(2) = ' Hit ENTER to setup magnet control parameters.'
	help_line(3) = ' Make sure you have the correct configuration
	1 selected via HDEV!' 
	CALL Put_Help (help_line)
  	CALL CURSOR_ABS (prpt, t_line, 75) 
	CALL CURSOR_OFF (paste)
	rendition = SMG$M_REVERSE .OR. SMG$M_BOLD .OR. SMG$M_BLINK
 205  	CALL SMG$CHANGE_RENDITION (prpt, t_line, 75, 1, 5,, rendition)
	CALL SMG$READ_KEYSTROKE (keyb, trm)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_DO) THEN
	   CALL HFIELD(tbuf(k).hf,tbuf(k).ihf,tbuf(k).hfw, !field,incr,wait
     1		tbuf(k).hfh) !how controller used
	   CALL CURSOR_ABS (prpt, t_line, 75)
	   GOTO 205
	END IF
	CALL SMG$CHANGE_RENDITION (prpt, t_line, 75, 1, 5, SMG$M_REVERSE)
	CALL CURSOR_ON (paste)
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mon
	IF (trm .EQ. K_TRM_DOWN)   GOTO fkeys
	IF (trm .EQ. K_TRM_UP)     GOTO ef
	IF (trm .EQ. K_TRM_LEFT)   GOTO th
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 205
 210 	help_line(1) = ' Monitor for each point! '
	help_line(2) = ' Give an integer number or your value will be rounded
	1 to the next integer.'
	help_line(3) = ' NOTE: YOUR TOTAL MONITOR IS  monitor*prefactor. '
	CALL Put_Help (help_line)
 215  	CALL GetString (keyb, prpt, mon_line, 19, 2, 7, trm, leng, tbuf(k).mon)  ! mon
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO prf
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO t0
	IF (trm .EQ. K_TRM_LEFT)   GOTO hf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 215
 220  	help_line(1) = ' Monitor-prefactor: give an integer number or your input
	1 is rounded the next' 
	help_line(2) = ' integer. '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 225  	CALL GetString (keyb, prpt, mon_line, 35, 2, 3, trm, leng, tbuf(k).mpf)  ! mpf
	tbuf(k).mpf = ANINT (tbuf(k).mpf)                     ! nearest integer
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO mt
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO it
	IF (trm .EQ. K_TRM_LEFT)   GOTO mon
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 225
 230  	help_line(1) = ' Monitor-type:  NEUT is neutron based'
	help_line(2) = '                TIME is time based is seconds'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 235  	CALL CURSOR_ABS (prpt, mon_line, 46)                       ! mt
	CALL SMG$READ_STRING (keyb, str8(1:4),, 4, m$_nenrcl,,, leng, trm)
	IF (leng .GE. 1) THEN
	   IF (str8(1:1) .NE. 'N' .AND. str8(1:1) .NE. 'T')  THEN
	      str8(1:4) = '----'
	      CALL PUTCHARS_BLINK (prpt, str8(1:4), mon_line, 46)
	      CALL Ring_Bell (prpt, 2)
	      GOTO 235
	   ELSE 
	      IF (str8(1:1) .EQ. 'N') tbuf(k).mt = 'NEUT'
	      IF (str8(1:1) .EQ. 'T') tbuf(k).mt = 'TIME'
	      CALL PUTCHARS_REV (prpt, tbuf(k).mt, mon_line, 46)
	   END IF
	END IF          
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO pts
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO tw
	IF (trm .EQ. K_TRM_LEFT)   GOTO prf
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 235
 240  	help_line(1) = ' Number of points for this run'
	help_line(2) = ' In order to hit the center give an odd number!'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 245  	CALL GetString (keyb, prpt, mon_line, 57, 2, 4, trm, leng, tbuf(k).pts) !# pts
	IF (leng .GE. 1) THEN
	   IF (tbuf(k).pts .NE. 0.) tbuf(k).pts = ABS(tbuf(k).pts)
	END IF
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO fkeys
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO ter
	IF (trm .EQ. K_TRM_LEFT)   GOTO mt
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 245


 450	help_line(1) = ' Type function sequence '
	help_line(2) = '     Example, type the three keys F 2 0'
	help_line(3) = '     to get same effect as typing Function key'
	CALL Put_Help (help_line)
 455  	CALL CURSOR_ABS (prpt, mon_line, 65)                       ! Func
	CALL SMG$READ_STRING (keyb, str4,, 4, m$_nenrcl,,, leng, trm, prpt)
	i = 0 !default value in case no key series entered
	IF (leng .GE. 1) THEN
	   IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'F')  THEN
	      CALL PUTCHARS_REV (prpt, 'FKEY', mon_line, 65)
	      CALL Ring_Bell (prpt, 2)
	      GOTO 455
	   ELSE 
	      IF (str4(2:4) .EQ. 'KEY') GOTO 455
	      READ(UNIT=STR4(2:),fmt=*,iostat=ios)i
	      IF (ios .eq. 0  .and. i .ge. 14 .and. i .le. 20 .and.
     1			i .ne. 15) THEN
		IF (i .eq. 14) THEN
	          CALL PUTCHARS_REV (prpt, 'F14 ', mon_line, 65)
	  	  trm = K_TRM_F14
		ELSEIF (i .eq. 16) THEN
	          CALL PUTCHARS_REV (prpt, 'F16 ', mon_line, 65)
	  	  trm = K_TRM_F16
		ELSEIF (i .eq. 17) THEN
	          CALL PUTCHARS_REV (prpt, 'F17 ', mon_line, 65)
	  	  trm = K_TRM_F17
		ELSEIF (i .eq. 18) THEN
	          CALL PUTCHARS_REV (prpt, 'F18 ', mon_line, 65)
	  	  trm = K_TRM_F18
		ELSEIF (i .eq. 19) THEN
	          CALL PUTCHARS_REV (prpt, 'F19 ', mon_line, 65)
	  	  trm = K_TRM_F19
		ELSEIF (i .eq. 20) THEN
	          CALL PUTCHARS_REV (prpt, 'F20 ', mon_line, 65)
	  	  trm = K_TRM_F20
		ENDIF
	        CALL PUTCHARS_REV (prpt, 'FKEY', mon_line, 65)
	      ELSE !invalid value read
	        CALL PUTCHARS_REV (prpt, 'FKEY', mon_line, 65)
	        CALL Ring_Bell (prpt, 2)
	        GOTO 455
	      ENDIF !i was a valid number
	   END IF ! Found an F as first character
	END IF          !length greater than 1
	IF (trm .EQ. K_TRM_CR .OR. trm .EQ. K_TRM_RIGHT .OR.
	1   trm .EQ. K_TRM_BUFFER_FULL) GOTO com
	IF (trm .EQ. K_TRM_DOWN)   GOTO com
	IF (trm .EQ. K_TRM_UP)     GOTO th
	IF (trm .EQ. K_TRM_LEFT)   GOTO pts
	IF (trm .EQ. K_TRM_F14)    GOTO 500
	IF (trm .EQ. K_TRM_F16)    GOTO 901
	IF (trm .EQ. K_TRM_F17)    CALL Updte_T_Buf (tbuf(k), k, line, *30)
	IF (trm .EQ. K_TRM_F18)    CALL Updte_T_Buf (tbuf(k), k, line, *700)
	IF (trm .EQ. K_TRM_F19 .OR. trm .EQ. K_TRM_DO)
	1                              CALL Updte_T_Buf (tbuf(k), k, line, *800)
	IF (trm .EQ. K_TRM_F20)    CALL Updte_T_Buf (tbuf(k), k, line, *900)
	CALL Ring_Bell (prpt, 1)
	GOTO 450


C---------- COPY CURRENT BUFFER TO ALL BUFFERS WITH HIGHER NUMBER ------------C

 500  	CALL SMG$ERASE_DISPLAY (prpt, 2, 1)
	IF (k .LT. nbuf) THEN
	   DO i= k+1, nbuf
	      tbuf(i)= tbuf(k)
	   END DO
	END IF
	CALL Updte_T_File (tbuf, line, *30)
      

C--------------------------- GO INTO CONTROL MODE ----------------------------C

 600  	IF (flip) GOTO 901          ! Prepare was called from control ==> RETURN
	flip = .TRUE.               ! Prepare was called from main    ==> CALL
	CALL Control
	GOTO 20 



C---------------------- DO OPERATIONS ON WHOLE BUFFERS -----------------------C
 
 700  	CALL PUTCHARS (prpt, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 701  	CALL SMG$ERASE_DISPLAY (prpt, 2, 1)
	CALL SMG$ERASE_DISPLAY (stat)
	CALL PUTCHARS_REV  (stat, ' Return: F20 or Command>F20', 1, 33)
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 710  	CALL SMG$ERASE_LINE (prpt, 7, 1)
	CALL CURSOR_ABS (prpt, 7, 1)
	CALL SMG$READ_STRING (keyb, command, ' Command> ', 50,,,,, trm, prpt)
	IF  ( (command(1:1) .EQ. 'F' .or. command(1:1) .eq. 'f') .and.
     1        command(2:3).eq.'20')  trm = SMG$K_TRM_F20
	IF (trm .EQ. K_TRM_CR) THEN
	   CALL Parse_Prep (command, *710, 4,,, tbuf,,,,, line)
	ELSE IF (trm .EQ. K_TRM_F20) THEN
	   CALL CURSOR_OFF (paste)
	   CALL SMG$ERASE_DISPLAY (prpt, 2, 1)
  	   GOTO 30
	END IF         
	CALL Ring_Bell (prpt, 1)
	GOTO 710


C--------------------------- DEFINE RUN_SEQUENCE -----------------------------C 

 800  	CALL PUTCHARS (prpt, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
	CALL SMG$ERASE_LINE (prpt, 3, 1)
 801  	CALL Def_Sequence
	GOTO 30


C--------- ERASE AND UNPASTE DISPLAY THEN RETURN TO CALLING ROUTINE ----------C

 900  	CALL PUTCHARS (prpt, ' First, buffer is updated', 3, 28)
	CALL LIB$WAIT (2.0)
 901  	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (prpt, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (stat, paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (menut, paste)
	CALL SMG$ERASE_DISPLAY (menut, 1, 1)
	CALL SMG$ERASE_DISPLAY (prpt, 1, 1)
	flip = .FALSE. 
#endif
	RETURN
	END

