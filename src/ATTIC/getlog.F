#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Getlog(coms,fenergy,echar)
C     	************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Request Sample Information from user when ne comment         C
C     Called from: Prepi, Prepq, Prept                                        C
C                                                                             C
C     Gayle Greene, Oct. 1991
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    leng, pos, trm
	INTEGER*4    context/0/
	INTEGER*4    status, maxtry,ios,notries/10/,openfai/30/ !from $forder
	integer*4    i
	CHARACTER*28 sys_time
	CHARACTER*80 help_line(3),dummy
	Character*11 fname(3)/'IBUFFER.BUF','QBUFFER.BUF','TBUFFER.BUF'/
	integer*4 userline,stochline,powerline,monoline,colline,blankline,
     1	  retline,left,right
	integer*2 exp,stoic,quant,high,wide,mono,anal,filt,col,energ,rpow

	character*10 power,beamheight,beamwidth,monostr,
     1	   analstr,filter,estr
	character*50 expers
	character*20 colstr
	character*33 stostr
	character*5 coms, !to write com to file
     1	  qstr
	real fenergy
	character*1 echar
	CHARACTER*80 dclfile,ret_file

	INCLUDE SMGPREP_CMN               ! Program specific SMG-definitions
	INCLUDE INSTR_CMN  !for logbuf_recl

	integer*4 logbook
	common/newscreen/logbook

#if defined(VMS)



C------------Set up fixed energy info----------------------------------C
	write(unit=estr(1:9),fmt='(f7.2)',err=50)fenergy
	estr(10:10)=echar
	goto 60
 50	estr(1:)=' '

C------------Get date and time and current subdirectory----------------------C
 60	CALL DATE_TIME (sys_time)
	do i=1,3
	  call find_file(fname(i),ret_file,context,status)
	  if (status)goto 70
	enddo
	left=1
	right=20
	dclfile(1:)=' '
	goto 80
 70	dclfile=ret_file
	right = INDEX (ret_file,']')
	left = INDEX (ret_file,'[')
 80	call find_file_end(context)

C-------------------------- CHECK FOR logbook.buf-----------------------------C
	
  	Call FIND_FILE (LOGBOOK_BUF,ret_file, context,status)
	IF (.NOT. status)THEN
	  expers='experimenters'
	  stostr='H2O'
	  qstr='10 '
	  power='15'
	  beamheight='2'
	  beamwidth='1'
	  monostr='PG(002)'
	  analstr='PG(002)'
	  filter='PG'
	  colstr='40 40 40 40'
 10 	  OPEN (UNIT= 1, FILE= LOGBOOK_BUF, STATUS= 'NEW', 
	1        ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= LOGBUF_RECL, SHARED,
     1	         ERR=30)
c	  WRITE (1,REC=1) sys_time(1:17),dclfile(left:right),coms
	  WRITE (1,REC=2) expers
	  WRITE (1,REC=3) stostr,qstr
	  WRITE (1,REC=4) power,beamheight,beamwidth
	  WRITE (1,REC=5) monostr,analstr,filter
	  WRITE (1,REC=6) colstr
	  CLOSE (1)
        END IF
C --------------------Read in last values -----------------------------C
	maxtry=0
 	OPEN (UNIT= 1, FILE= LOGBOOK_BUF, STATUS= 'OLD', ACCESS= 
	1     'DIRECT', FORM= 'UNFORMATTED', SHARED, RECL=LOGBUF_RECL,IOSTAT=ios)
	DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	  CALL libwait(.2,status)
  	  OPEN (UNIT= 1, FILE= LOGBOOK_BUF, STATUS= 'OLD', ACCESS= 
	1     'DIRECT', FORM= 'UNFORMATTED', SHARED, RECL=LOGBUF_RECL,IOSTAT=ios)
	  maxtry = maxtry+1
	ENDDO
	IF (ios.NE.0)THEN !error opening file
	    expers='experimenters'
	    stostr='H2O'
	    qstr='10 '
	    power='15'
	    beamheight='2'
	    beamwidth='1'
	    monostr='PG(002)'
	    analstr='PG(002)'
	    filter='PG'
	    colstr='40 40 40 40'
	    goto 30
	  endif
 25	  READ (1,REC=2) expers
	  READ (1,REC=3) stostr,qstr
	  READ (1,REC=4) power,beamheight,beamwidth
	  READ (1,REC=5) monostr,analstr,filter
	  READ (1,REC=6) colstr
	CLOSE (1)
 30	call find_file_end(context)
C-----------Assign line numbers to the input fields --------------------C
	assign 100 to exp
	assign 150 to stoic
	assign 200 to quant
	assign 225 to rpow
	assign 250 to high
	assign 300 to wide
	assign 350 to mono
	assign 400 to anal
	assign 450 to filt
	assign 500 to col
	assign 550 to energ

C----------------------------- GENERAL DISPLAY -------------------------------C

	CALL SMG$ERASE_DISPLAY (logbook, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (logbook, paste, 2, 2)
c	line='Sample Information for Permanent Logbook'
c	CALL PUTCHARS_BOLD (logbook, line(1:50) , 1, 1)

	userline = 1
	CALL PUTCHARS_REV (logbook, coms(1:5),userline, 1)
	CALL PUTCHARS (logbook, '    Experimenters: ', userline, 6)
	CALL SMG$PUT_LINE (logbook, expers, userline, 25, SMG$M_ERASE_TO_EOL,
	1                   SMG$M_REVERSE)
	stochline=userline+1
	   CALL PUTCHARS (logbook, 'Sample stochiometry: ',stochline, 1)
	   CALL PUTCHARS_REV (logbook,stostr, stochline, 22)
	   CALL PUTCHARS (logbook, 'Quantity (grams): ', stochline, 56)
	   CALL PUTCHARS_REV (logbook, qstr, stochline, 74) 

	powerline=stochline+1
	   CALL PUTCHARS (logbook, 'Reactor Power: ', powerline, 1)
	   CALL PUTCHARS_REV (logbook, power, powerline, 16)
	   CALL PUTCHARS (logbook, 'Beam Height (in.): ', powerline, 27)
	   CALL PUTCHARS_REV (logbook, beamheight, powerline, 45)
	   CALL PUTCHARS (logbook, 'Beam width: ', powerline, 56)
	   CALL PUTCHARS_REV (logbook, beamwidth, powerline, 69)

	monoline=powerline+1
	   CALL PUTCHARS (logbook, 'Monochromator: ', monoline, 1)
	   CALL PUTCHARS_REV (logbook, monostr, monoline, 16)
	   CALL PUTCHARS (logbook, 'Analyzer: ', monoline, 27)
	   CALL PUTCHARS_REV (logbook, analstr, monoline, 45)
	   CALL PUTCHARS (logbook, 'Filter: ', monoline, 56)
	   CALL PUTCHARS_REV (logbook, filter, monoline, 69)

	colline=monoline+1
	   CALL PUTCHARS (logbook, 'Collimation: ', colline, 1)
	   CALL PUTCHARS_REV (logbook, colstr, colline, 16)
	   CALL PUTCHARS (logbook, 'Fixed energy (meV): ', colline, 40)
	   CALL PUTCHARS_REV (logbook, estr, colline, 61)

	blankline=colline+1
	   CALL PUTCHARS (logbook, ' ',blankline, 1)
	  
	retline=blankline+1
	CALL PUTCHARS_REV (logbook, 'RETURN: F20 or Ctrl-G', retline, 20)

	CALL Cursor_Abs (logbook, 1, 25)
	   

 100  	help_line(1) = ' Give the experimenters names. (50 chars max.)'
	help_line(2) = ' Please include NIST contact.'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 105  	CALL Cursor_Abs (logbook, userline, 25)          !Experimentors
	CALL SMG$READ_STRING (keyb, dummy,, 50, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   expers=dummy
	   CALL SMG$PUT_CHARS (logbook,expers, userline, 25, 
	1                      SMG$M_ERASE_TO_EOL, SMG$M_REVERSE)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO stoic
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO stoic
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO col
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO energ
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 105

 150  	help_line(1) = 'Chemical formula or fraction of each element.'
	help_line(2) =  ' Example: H2O'
	help_line(3) = ' '
	CALL Put_Help (help_line)
 155  	CALL Cursor_Abs (logbook, stochline, 22)          !Stochiometry
	CALL SMG$READ_STRING (keyb, dummy,, 33, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   stostr=dummy
	   CALL PUTCHARS_REV (logbook,stostr, stochline, 22) 
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO quant
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO rpow
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO exp
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO exp
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 155


 200  	help_line(1) = ' If other than grams, give units.'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 205  	CALL Cursor_Abs (logbook, stochline, 74)          !Quantity
	CALL SMG$READ_STRING (keyb, dummy,, 5, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   qstr=dummy
	   CALL PUTCHARS_REV (logbook,qstr, stochline, 74)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO rpow
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO high
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO exp
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO stoic
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 205

 225  	help_line(1) = ' Megawatts'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 230  	CALL Cursor_Abs (logbook, powerline, 16)          !R Power
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   power=dummy
	   CALL PUTCHARS_REV (logbook,power, powerline, 16)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO high
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO mono
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO stoic
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO quant
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 230

 250  	help_line(1) = ' If other than inches, give units.'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 255  	CALL Cursor_Abs (logbook, powerline, 45)          !Height
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   beamheight=dummy
	   CALL PUTCHARS_REV (logbook,beamheight, powerline, 45) 
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO wide
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO anal
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO quant
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO rpow
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 255

 300  	help_line(1) = ' If other than inches, give units.'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 305  	CALL Cursor_Abs (logbook, powerline, 69)          !Width
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   beamwidth=dummy
	   CALL PUTCHARS_REV (logbook,beamwidth, powerline, 69)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO mono
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO filt
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO quant
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO high
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 305

 350  	help_line(1) = ' Crystal and Reflection'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 355  	CALL Cursor_Abs (logbook, monoline, 16)          !Mono
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   monostr=dummy
	   CALL PUTCHARS_REV (logbook,monostr, monoline, 16) 
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO anal
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO col
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO rpow
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO wide
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 355

 400  	help_line(1) = ' Crystal and Reflection'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 405  	CALL Cursor_Abs (logbook, monoline, 45)          !Anal
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   analstr=dummy
	   CALL PUTCHARS (logbook,analstr, monoline, 45) 
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO filt
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO energ
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO high
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO mono
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 405

 450  	help_line(1) = ' Crystal'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 455  	CALL Cursor_Abs (logbook, monoline, 69)          !filter
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   filter=dummy
	   CALL PUTCHARS (logbook,filter, monoline, 69)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO col
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO energ
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO wide
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO anal
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 455

 500  	help_line(1) = ' Reactor to detector in minutes'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 505  	CALL Cursor_Abs (logbook, colline, 16)          !collim
	CALL SMG$READ_STRING (keyb, dummy,, 20, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   colstr=dummy
	   CALL PUTCHARS (logbook,colstr, colline, 16)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO energ
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO exp
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO mono
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO filt
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 505

 550  	help_line(1) = ' If other than meV, give units.'
	help_line(2) = ' Follow with M or A to indicate'
	help_line(3) = '   monochromater or analyzer'
	CALL Put_Help (help_line)
 555  	CALL Cursor_Abs (logbook, colline, 61)          !energy
	CALL SMG$READ_STRING (keyb, dummy,, 10, m$_nenr,,, leng, trm, logbook)
	IF (leng.ge.1)then
	   estr=dummy
	   CALL PUTCHARS (logbook,estr, colline, 61) 
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_RIGHT .OR.
     1	  trm .EQ. SMG$K_TRM_BUFFER_FULL) GOTO exp
	IF (trm .EQ. SMG$K_TRM_DOWN)   GOTO exp
	IF (trm .EQ. SMG$K_TRM_UP)     GOTO anal
	IF (trm .EQ. SMG$K_TRM_LEFT)   GOTO col
	IF (trm .EQ. SMG$K_TRM_F20 .or. trm .eq. 7)    goto 99
	CALL SMG$RING_BELL (logbook, 1)
	GOTO 555

  99	OPEN (UNIT= 1, FILE= LOGBOOK_BUF, STATUS= 'OLD', ACCESS= 
	1     'DIRECT', RECL=LOGBUF_RECL,FORM= 'UNFORMATTED')
	  write (1,REC=2) expers
	  write (1,REC=3) stostr,qstr
	  write (1,REC=4) power,beamheight,beamwidth
	  write (1,REC=5) monostr,analstr,filter
	  write (1,REC=6) colstr
	CLOSE (1)

	
           maxtry = 0
 	   OPEN (UNIT=1, NAME= MONITOR_REC, FORM= 'FORMATTED', 
     1	    ACCESS= 'APPEND', STATUS='UNKNOWN',
     1      CARRIAGECONTROL= 'LIST', SHARED, IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	     CALL libwait(.2,status)
	     OPEN (UNIT=1, NAME= MONITOR_REC, FORM= 'FORMATTED', 
     1	      ACCESS= 'APPEND', STATUS='UNKNOWN',
     1        CARRIAGECONTROL= 'LIST', SHARED, IOSTAT=ios)
	     maxtry = maxtry+1
	   ENDDO
	   IF (ios .NE. 0)GOTO 2209
	   write(unit=1,fmt='(1x,a17,1x,a,1x,a)',err=2209)sys_time(1:17),
     1	     dclfile(left:right),coms
	  write (unit=1,fmt='(5x,a,a)') 'Experimenters: ',expers
	  write (unit=1,fmt='(5x,a,a,1x,a,a))') 
     1	    'Sample: ',stostr,' Quantity (gms.): ',qstr
	  write (unit=1,fmt='(5x,6a)') 
     1	    'Reactor power: ',power,' Beam Height (in.): ',beamheight,
     1	     ' Width: ',beamwidth
	  write (unit=1,fmt='(5x,6a)') 
     1	     'Monochromater: ',monostr,' Analyzer: ',analstr,
     1	     ' Filter: ',filter
	  write (unit=1,fmt='(5x,4a))')'Collimator: ', colstr,
     1	     ' Fixed Energy (meV): ',estr
 2209	  close(unit=1,err=2210)

 2210   	CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (logbook, paste)
#endif
	END
