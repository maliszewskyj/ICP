#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Parse_Prep (command, *, typ, ibuf, qbuf, tbuf, bbuf, 
     1		rbuf, xbuf, dbuf,line)
C	***************************************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Checks Spelling And Syntax Of The Given Commands. If Ok,        C
C     Executes  Command And Returns Control To Statement Label * In The       C
C     Calling Program.                                                        C
C     Called from: PrepI, PrepQ, PrepT, PrepB, PrepR, Prepx		      C
C      At call, typ =
C	 1 : ibuffer
C	 2 : qbuffer
C	 4 : tbuffer
C	 5 : Bbuffer
C        6 : rbuffer
C        7 : xbuffer
C        8 : dbuffer
C                                                                             C
C     Beat Schmid, June 1989    		                              C
C     Gayle Greene, modifications
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	IMPLICIT   none
	INCLUDE INSTR_CMN      ! Instrument and hardware parameters
	INCLUDE SMGPREP_CMN
	INCLUDE REFL_CMN
	INCLUDE DARTZ_CMN
	INTEGER*4     i, j, k, k1, k2, typ, kj,jr,jk,iscreen,status
	REAL          data, af
	LOGICAL       isdigit, comflag
	CHARACTER*4   str4
	CHARACTER*50  command
	CHARACTER*80  help_line(3), line(nbuf)
	logical newlog,redopos
	real fenergy
	character*1 echar
	real*4 bpos(10)
	RECORD /i_buf/ ibuf(nbuf), defi_buf
	RECORD /q_buf/ qbuf(nbuf), defq_buf
	RECORD /t_buf/ tbuf(nbuf), deft_buf
	RECORD /b_buf/ bbuf(nbuf), defb_buf
	RECORD /r_buf/ rbuf(nbuf), defr_buf
	RECORD /x_buf/ xbuf(nbuf), defx_buf
	RECORD /d_buf/ dbuf(nbuf), defd_buf
	redopos = .false.
C------------------ IF COMMENT THEN GO DIRECTLY TO C-LABEL -------------------C
	IF (command(1:3) .EQ. 'COM' .OR. command(1:3) .EQ. 'com') GOTO 30
C---------------------- REMOVE ALL BLANKS FROM STRING ------------------------C
 500	j = 1
	DO i = 1, 50
	   IF (command(i:i) .NE. ' ') THEN
	      command(j:j) = command(i:i)
	      j = j + 1
	   END IF
	END DO
	command(j:50) = ' '                               ! blank is terminator
	CALL STR$UPCASE (command, command)
	command = ',' // command           ! comma is for correct loop handling
C------------------ FIND NEXT COMMA OR FIND CR AND RETURN --------------------C
 520	i = 1 
 521	DO WHILE (command(i:i) .NE. ',' .AND. command(i:i) .NE. ' ')
	   i = i + 1
	END DO         
	IF (command(i:i) .EQ. ' ' .OR. ( command(i:i) .EQ. ',' .AND.
	1                                command(i+1:i+1) .EQ. ' ')) THEN
 522	   IF (typ .EQ. 1) THEN
	      CALL Updte_I_File (ibuf, line, *523)         
	   ELSE IF (typ .EQ. 2) THEN
	      CALL Updte_Q_File (qbuf, line, *523)         
	   ELSE IF (typ .EQ. 5) THEN
	     IF (redopos) THEN
	     DO k = 1,nbuf
	       CALL Bragg_Pos(bbuf(k),bpos,,,comflag,.false.)
	       DO i=1,2
	        bbuf(k).a(i) = bpos(i)
	       ENDDO
	       DO i=5,6
	         bbuf(k).a(i) = bpos(i)
	       ENDDO
	       bbuf(k).a(3) = bpos(3) - int(bbuf(k).pts/2.)*bbuf(k).i(3)
	       bbuf(k).a(4) = bpos(4) - int(bbuf(k).pts/2.)*bbuf(k).i(4)
	     ENDDO
	     ENDIF
	      CALL Updte_B_File (bbuf, line, *523)         
	   ELSE IF (typ .EQ. 4) THEN
	      CALL Updte_T_File (tbuf, line, *523)         
	   ELSE IF (typ .EQ. 6) THEN
	      CALL Updte_R_File (rbuf, line, *523)         
	   ELSE IF (typ .EQ. 7) THEN
	      CALL Updte_X_File (xbuf, line, *523)         
	   ELSE IF (typ .EQ. 8) THEN
	      CALL Updte_D_File (dbuf, line, *523)         
	   END IF
 523	   RETURN 1
	ELSE
	command(1:) = command (i+1:50)
	END IF
C----------------------- DETERMINE NATURE OF COMMAND -------------------------C
	j = ICHAR (command(1:1))                  ! determine ascii-val of char
	j = j - 64                                  ! 'A'...'Z'  <==>  [1...26]
	IF (j .LT. 1 .OR. j .GT. 26) GOTO 270
	GOTO (  10,  20,  30, 999,  50, 60, 999,  80,  90, 999,
	1      110, 120, 130, 999, 999, 160, 999, 180, 190, 200,
	2      999, 999, 999, 999, 999, 999) j
C----------------------------- A: A AA  An  AnF ------------------------------C
  10	IF (command(2:2) .EQ. '=') THEN                               ! Latt: a
	   IF (typ .EQ. 2) THEN
	      READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         qbuf(k).a = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         bbuf(k).lat_a = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 310
	   END IF
	ELSE IF (command(2:3) .EQ. 'A=') THEN                        ! Latt: aa
	   IF (typ .EQ. 2) THEN
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         qbuf(k).aa = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN  !Bragg
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         bbuf(k).aa = data
	      END DO				
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 310
	   END IF
	ELSE IF (command (3:3) .EQ. '=') THEN                           ! An
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      help_line(2) = ' '
	      help_line(3) = ' '
	      CALL Put_Help (help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSEIF (nsta .eq. 1 .AND. (j.ne.3 .and. j.ne.4) ) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
     1		' not moved in buffer'
	      help_line(2) =  ' '
	      help_line(3) =  ' '
	      CALL Put_Help  (help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSE
	     READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	     IF (typ .EQ. 7)THEN !X buffer
	       DO k = 1, nbuf
		 DO jk = 1,6
		   jr = xbuf(k).mn(jk)
 		   IF (jr .EQ. j) xbuf(k).a(jk) = data
		 ENDDO !jk
	       END DO !k
	     ELSEIF (typ .EQ. 8)THEN !D buffer
	       DO k = 1, nbuf
		 DO jk = 1,6
		   jr = dbuf(k).mn(jk)
 		   IF (jr .EQ. j) dbuf(k).a(jk) = data
		 ENDDO !jk
	       END DO !k
	     ELSEIF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).a(j) = data
	      END DO
	    ELSE
	      GOTO 310
	    ENDIF !typ
	   END IF
	   GOTO 520            
	ELSE IF (command (3:4) .EQ. 'F=') THEN                            ! AnF
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      help_line(2) = ' '
	      help_line(3) = ' '
	      CALL Put_Help (help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSEIF (nsta .eq. 1 .AND. j.ne.3 .and. j.ne.4 ) THEN
	      help_line(1) = ' Motor no. '// command(2:2)//
     1		' not moved in buffer'
	      help_line(2) =' '
	      help_line(3) =' '
	      CALL Put_Help(help_line)
	      CALL libwait(2.0,status)
	   ELSE
	     READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	     IF (typ .EQ. 7) THEN !x buffer
	      DO k = 1, nbuf
		DO jk = 1,6
		  jr = xbuf(k).mn(jk)
		  IF (jr .EQ. j) THEN
		    af = data
		    IF (xbuf(k).i(jk) .EQ. 0.) xbuf(k).i(jk) = 0.05
		    xbuf(k).pts = ANINT ((af - xbuf(k).a(jk)) / xbuf(k).i(jk) + 1.)
		  ENDIF !jr
	        ENDDO !jk
	      END DO !k
	     ELSEIF (typ .EQ. 8)THEN !d buffer
	      DO k = 1, nbuf
		DO jk = 1,6
		  jr = dbuf(k).mn(jk)
		  IF (jr .EQ. j) THEN
		    af = data
		    IF (dbuf(k).i(jk) .EQ. 0.) dbuf(k).i(jk) = 0.05
		    dbuf(k).pts = ANINT ((af - dbuf(k).a(jk)) / dbuf(k).i(jk) + 1.)
		  ENDIF !jr
	        ENDDO !jk
	      END DO !k
	     ELSEIF (typ .EQ. 1)THEN
	      DO k = 1, nbuf
		 af = data
		 IF (ibuf(k).i(j) .EQ. 0.) ibuf(k).i(j) = 0.05
		 ibuf(k).pts = ANINT ((af - ibuf(k).a(j)) / ibuf(k).i(j) + 1.)
	      END DO
	     ELSE
	       GOTO 310
	     ENDIF !typ
	   END IF
	   GOTO 520
	END IF
	GOTO 270
C--------------------------------- B: B BB -----------------------------------C
  20	IF (command(2:2) .EQ. '=') THEN                               ! Latt: b
           IF (typ .EQ. 2) THEN
	      READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         qbuf(k).b = data
	      END DO				
	      GOTO 520
           ELSE IF (typ .EQ. 5) THEN !Bragg
	      READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         bbuf(k).b = data
	      END DO				
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 310
	   END IF
	ELSE IF (command(2:3) .EQ. 'B=') THEN                        ! Latt: bb
           IF (typ .EQ. 2) THEN
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         qbuf(k).bb = data
	      END DO				
	      GOTO 520
           ELSE IF (typ .EQ. 5) THEN  !Bragg
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = 1, nbuf
	         bbuf(k).bb = data
	      END DO				
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 310
	   END IF
	END IF
	GOTO 270
C---------------------------- C: C CC  COM  COPY -----------------------------C
  30	IF (command (1:4) .EQ. 'COPY') THEN
	   i = INDEX (command, '-')
	   IF (i .EQ. 0) THEN                                     ! Single copy
	      READ (UNIT= command(5:50), FMT= *, ERR=270) k1, j
	      IF (k1 .GT. nbuf .OR. j .GT. nbuf) GOTO 270
	      k2 = k1
	      i = INDEX (command, ',')
	      command(1:) = command (i+1:)
	   ELSE                                                    ! Range copy
	      READ (UNIT= command(5:i-1), FMT= *, ERR= 270) k1
	      READ (UNIT= command(i+1:), FMT= *, ERR= 270) k2
	      i = INDEX (command, ',')
	      command(1:) = command(i+1:)
	      READ (UNIT= command(1:), FMT= *, ERR= 270) j
	      IF (k1 .GT. nbuf .OR. k2 .GT. nbuf) GOTO 270
	      IF (j+(k2-k1) .GT. nbuf) THEN
		 help_line(1) = ' Arithmetic error. Try again'
		 help_line(2) = ' '
		 help_line(3) = ' '
		 CALL Put_Help (help_line)
		 RETURN	1
	      END IF
	   END IF
	   DO i= k1, k2
	      IF (typ .EQ. 1) THEN
		 ibuf(j) = ibuf(i)
	      ELSE IF (typ .EQ. 2) THEN
		 qbuf(j) = qbuf(i)
	      ELSE IF (typ .EQ. 5) THEN !Bragg
		 bbuf(j) = bbuf(i)
	      ELSE IF (typ .EQ. 6) THEN
		 rbuf(j) = rbuf(i)
	      ELSE IF (typ .EQ. 7) THEN
		 xbuf(j) = xbuf(i)
	      ELSE IF (typ .EQ. 8) THEN
		 dbuf(j) = dbuf(i)
	      ELSE IF (typ .EQ. 4) THEN
		 tbuf(j) = tbuf(i)
	      ELSE
		 GOTO 310
	      END IF
	      j = j + 1
	   END DO
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'OM=' .OR. command(2:4) .EQ. 'om=') THEN
	   j = 5				       	             ! comment
	   DO WHILE (command(j:j) .EQ. ' ')
	      j = j + 1
	   END DO
	   newlog = .false.
	   echar='A' !defaults if newlog
	   if (wavel.ne.0)then
	     fenergy=81.804/(wavel**2)
	   else
	     fenergy=0.
	   endif
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.ibuf(k).com(1:5))newlog=.true.
		 ibuf(k).com(1:) = command(j:)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.qbuf(k).com(1:5))newlog=.true.
		 qbuf(k).com(1:) = command(j:)
	      END DO
	      IF (newlog) THEN
	        fenergy=qbuf(1).ef
	        echar=qbuf(1).eft(1:1)
	      ENDIF
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.bbuf(k).com(1:5))newlog=.true.
		 bbuf(k).com(1:) = command(j:)
	      END DO
	      IF (newlog) THEN
	        fenergy=bbuf(1).ef
	        echar=bbuf(1).eft(1:1)
	      ENDIF
	   ELSEIF (typ .EQ. 6) THEN !rbuffer
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.rbuf(k).com(1:5))newlog=.true.
		 rbuf(k).com(1:) = command(j:)
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !xbuffer
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.xbuf(k).com(1:5))newlog=.true.
		 xbuf(k).com(1:) = command(j:)
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !dbuffer
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.dbuf(k).com(1:5))newlog=.true.
		 dbuf(k).com(1:) = command(j:)
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !t buffer
	      DO k = 1, nbuf
	         if (command(j:j+4).ne.tbuf(k).com(1:5))newlog=.true.
		 tbuf(k).com(1:) = command(j:)
	      END DO
	      IF (newlog) fenergy=tbuf(1).ef
	   ELSE
	     GOTO 310
	   END IF
	   IF (newlog) THEN
	     CALL getlog(command(j:j+4),fenergy,echar)
	     CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	   ENDIF
	   GOTO 522
	ELSE IF (command(2:2) .EQ. '=') THEN                         ! Latt: c
	   READ (UNIT= command(3:), FMT= *, ERR= 270) data
           IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
	         qbuf(k).c = data
	      END DO				
           ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
	         bbuf(k).c = data
	      END DO				
	      redopos = .true.			
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'C=') THEN                        ! Latt: cc
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
           IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
	         qbuf(k).cc = data
	      END DO				
           ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
	         bbuf(k).cc = data
	      END DO				
	      redopos = .true.			
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	END IF
	GOTO 270
C---------------------------- E: EC  ES EF  EFT -----------------------------C
  50	IF (command(2:3) .EQ. 'C=') THEN
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = 1, nbuf
	         qbuf(k).ec = data
	      END DO				
	   ELSEIF (typ .EQ. 4)THEN
	      DO k = 1, nbuf
	         tbuf(k).ec = data
	      END DO
	   ELSE
	     GOTO 310		
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'S=') THEN
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = 1, nbuf
	         qbuf(k).es = data
	      END DO				
	   ELSEIF (typ .EQ. 4)THEN
	      DO k = 1, nbuf
	         tbuf(k).es = data
	      END DO	
	   ELSE
	      GOTO 310		
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'FT=') THEN
	   IF (typ .EQ. 2) THEN
	     IF (command(5:5) .EQ. 'A') THEN
	      str4(1:3) = 'ANA'		
	     ELSE IF (command(5:5) .EQ. 'M') THEN
	      str4(1:3) = 'MON'
	     ELSE
	      GOTO 270
	     END IF
	     DO k = 1, nbuf
	      qbuf(k).eft = str4(1:3)
	     END DO
	   ELSE
	     GOTO 310
	   ENDIF
	   GOTO 520
	END IF			
	GOTO 270		            
C---------------------------- F: FE  -----------------------------C
 60	IF (command(2:3) .EQ. 'E=') THEN
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = 1, nbuf
	         qbuf(k).ef = data
	      END DO				
	   ELSE IF (typ .EQ. 5) THEN 
	      DO k = 1, nbuf
	         bbuf(k).ef = data
	      END DO				
	      redopos = .true.			
	   ELSEIF (typ .EQ. 4)THEN
	      DO k = 1, nbuf
	         tbuf(k).ef = data
	      END DO		
	   ELSE
	      GOTO 310		
	   END IF
	   GOTO 520
	ENDIF
	GOTO 270
C-------------------------- H:   HELP  HC  HS  HF ---------------------------C
  80	IF (command(2:4) .EQ. 'ELP') THEN
	   IF (typ .EQ. 1) THEN
	      iscreen = prpi
	   ELSE IF (typ .EQ. 2) THEN
	      iscreen = prpq
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      iscreen = prpb
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      iscreen = prpr
	   ELSE IF (typ .EQ. 7) THEN !X buffer
	      iscreen = prpx
	   ELSE IF (typ .EQ. 8) THEN !D buffer
	      iscreen = prpd
	   ELSE  !T buffer
	      iscreen = prpt
	   ENDIF
	   CALL Cursor_Abs (iscreen, 2, 1)
 	   CALL PUTLINE (iscreen, ' SYNTAX: field = value                         field: COM  PTS  M3')
	   CALL PUTLINE (iscreen, ' COPY n1[-n2],m (n1 to n2 into m, m+1, ...)           EC ES FE EFT  MON MPF MT')
	   CALL PUTLINE (iscreen, ' RESET n1[-n2]  (Reset to default values)             T IT TW TH TER  HF')
	   CALL PUTLINE (iscreen, ' Separate multiple instructions by commas ')
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'C=') THEN                        ! H-center
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 5) THEN !Bragg
	     DO k = 1, nbuf
	       bbuf(k).hc = data
	     END DO
	      redopos = .true.			
	   ELSEIF (typ .EQ. 2) THEN
	     DO k = 1, nbuf
	       qbuf(k).hc = data
	     END DO
	   ELSE
	     GOTO 310
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'S=') THEN                          ! H-step
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN
	     DO k = 1, nbuf
	       qbuf(k).hs = data
	     END DO
	   ELSE 
	     GOTO 310
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'F=') THEN                         ! H-field
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X buffer
	      DO k = 1, nbuf
		 xbuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D buffer
	      DO k = 1, nbuf
		 dbuf(k).hf = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T buffer
	      DO k = 1, nbuf
		 tbuf(k).hf = data
	      END DO
	   ELSE
	     GOTO 310
	   END IF
	   GOTO 520
	END IF
	GOTO 270
C------------------------------- I:   In  IT ---------------------------------C
  90	IF (command(2:3) .EQ. 'T=') THEN	                        ! Inc-T
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).it = ABS(data)
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command (3:3) .EQ. '=') THEN                      ! Inc-angle
	  READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	  IF (typ .EQ. 6) THEN !refl
	    kj = slitlist(j)
	    IF (.NOT. motfl(kj)) THEN
	      help_line(1) = ' Slit no. '// command(2:2) //
     1		' is not served on this instrument'
	      help_line(2) =  ' '
	      help_line(3) =  ' '
	      CALL Put_Help  (help_line)
	      CALL LIBWAIT (2.0,status)
	    ELSE !motfl
	      IF (j .LT. 1 .OR. j .GT. 4) GOTO 310
	      READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	        DO k = 1, nbuf
		  rbuf(k).i(j) = data
	        END DO
	    ENDIF !motfl
	    GOTO 520
	  ELSE !typ ne 6
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      help_line(2) = ' '
	      help_line(3) = ' '
	      CALL Put_Help (help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSEIF (nsta .eq. 1 .AND. (j.ne.3 .and. j.ne.4) ) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
     1		' not moved in buffer'
	      help_line(2) =' '
	      help_line(3) =' '
	      CALL Put_Help(help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSE !motfl
	      READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	      IF (typ .EQ. 5) THEN !Bragg
	        IF (j .NE. 3 .AND. j .NE. 4) goto 310
	        DO k = 1, nbuf
		  bbuf(k).i(j) = data
	        END DO
	      ELSEIF (typ .EQ. 7) THEN !XBUF
	        DO k = 1, nbuf
		  DO jk = 1,6
		    jr = xbuf(k).mn(jk)
		    IF (jr .EQ. j) THEN
		      xbuf(k).i(jk) = data
		    ENDIF
		  ENDDO
	        END DO
	      ELSEIF (typ .EQ. 8) THEN !DBUF
	        DO k = 1, nbuf
		  DO jk = 1,6
		    jr = dbuf(k).mn(jk)
		    IF (jr .EQ. j) THEN
		      dbuf(k).i(jk) = data
		    ENDIF
		  ENDDO
	        END DO
	      ELSEIF (typ .EQ. 1) THEN !Ibuf
	        DO k = 1, nbuf
		  ibuf(k).i(j) = data
	        END DO
	      ELSE
		GOTO 310
	      ENDIF !typ
	   END IF !motfl
	  ENDIF !typ eq 7
	   GOTO 520
	END IF
	GOTO 270
C------------------------------- K:   KC  KS ---------------------------------C
 110	IF (command(2:3) .EQ. 'C=') THEN		             ! K-center
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 5)THEN  !Bragg
	     DO k = 1, nbuf
	       bbuf(k).kc = data
	     END DO
	      redopos = .true.			
	   ELSEIF (typ .EQ. 2) THEN
	     DO k = 1, nbuf
	       qbuf(k).kc = data
	     END DO
	   ELSE
	     GOTO 310
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'S=') THEN                          ! K-step
	   IF (typ .EQ. 2) THEN
	     READ (UNIT= command(4:), FMT= *, ERR= 270) data
	     DO k = 1, nbuf
	      qbuf(k).ks = data
	     END DO
	     GOTO 520
	   ELSE
	     GOTO 310
	   ENDIF
	END IF
	GOTO 270
C------------------------------- L:   LC  LS ---------------------------------C
 120	IF (command(2:3) .EQ. 'C=') THEN                             ! L-center
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 5) THEN !Bragg
	     DO k = 1, nbuf
	       bbuf(k).lc = data
	     END DO
	      redopos = .true.			
	   ELSEIF (typ .EQ. 2) THEN
	     DO k = 1, nbuf
	       qbuf(k).lc = data
	     END DO
	   ELSE
	     GOTO 310
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'S=') THEN                          ! L-step
	   IF (typ .EQ. 2) THEN
	     READ (UNIT= command(4:), FMT= *, ERR= 270) data
	     DO k = 1, nbuf
	       qbuf(k).ls = data
	     END DO
	     GOTO 520
	   ELSE
	     GOTO 310
	   ENDIF
	END IF
	GOTO 270
C---------------------------- M:   MON  MPF  MT ------------------------------C
 130	IF (command (2:4) .EQ. 'ON=') THEN                            ! Monitor
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).mon = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).mon = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).mon = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).mon0 = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !Xbuffer
	      DO k = 1, nbuf
		 xbuf(k).mon = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !Dbuffer
	      DO k = 1, nbuf
		 dbuf(k).mon = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN ! T buffer
	      DO k = 1, nbuf
		 tbuf(k).mon = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520            
	ELSE IF (command (2:4) .EQ. 'PF=') THEN                           ! Mpf
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).mpf = ANINT(data)
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'T=') THEN                    ! Monitor type
	   IF (command(4:4) .EQ. 'N' .AND. nsta.ne.0) THEN
	      str4 = 'NEUT'
	   ELSE IF (command(4:4) .EQ. 'T' .OR. nsta.eq.0) THEN
	      str4 = 'TIME'
	   ELSE
	      GOTO 270
	   END IF
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !Xbuffer
	      DO k = 1, nbuf
		 xbuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !Dbuffer
	      DO k = 1, nbuf
		 dbuf(k).mt = str4
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !Tbuffer
	      DO k = 1, nbuf
		 tbuf(k).mt = str4
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. '3=' .AND. nsta .EQ. 4) THEN  ! Move Angle 3
	   IF (typ .NE. 4) GOTO 310
	   IF (command(4:4) .EQ. 'N') THEN
	      DO k = 1, nbuf
		 tbuf(k).m3 = .FALSE.
	      END DO
	   ELSE
	      DO k = 1, nbuf
		 tbuf(k).m3 = .TRUE.
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270
C------------------------------- P:   PTS    ---------------------------------C
 160	IF (command(2:4) .EQ. 'TS=') THEN                                 ! Pts
	   READ (UNIT= command(5:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).pts = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).pts = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	END IF
	GOTO 270
C-------------------------------- R:   RESET ---------------------------------C
 180	IF (command(2:3) .EQ. 'ES') THEN                                ! Reset
	   i = 4
	   DO WHILE (.NOT. isdigit(command(i:i)) .AND. j .LT. 50)
	      i = i + 1
	   END DO   
	   j = INDEX (command,'-')
	   IF (j .EQ. 0) THEN
	      READ (UNIT= command(i:), FMT= *, ERR= 270) k1         
	      k2 = k1		
	   ELSE
	      READ (UNIT= command(i:j-1), FMT= *, ERR= 270) k1         
	      READ (UNIT= command(j+1:), FMT= *, ERR= 270) k2
	   END IF
	   IF (k1 .GT. nbuf .OR. k2 .GT. nbuf) GOTO 270
	   IF (typ .EQ. 1) THEN
	      DO k = k1, k2
		 ibuf(k) = defi_buf
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = k1, k2
		 qbuf(k) = defq_buf
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = k1, k2
		 bbuf(k) = defb_buf
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = k1, k2
		 rbuf(k) = defr_buf
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = k1, k2
		 xbuf(k) = defx_buf
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = k1, k2
		 dbuf(k) = defd_buf
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = k1, k2
		 tbuf(k) = deft_buf
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	END IF 
	GOTO 270
C---------------------------- S: Sn Snf --------------------------------------C
 190	IF (command (3:3) .EQ. '=') THEN                           ! Sn
	   IF (typ .NE. 6) GOTO 310 !Refl only
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j !which slit
	   kj = slitlist(j)
	   IF (.NOT. motfl(kj)) THEN
	      help_line(1) = ' Slit no. '// command(2:2) //
     1		' is not served on this instrument'
	      help_line(2) =  ' '
	      help_line(3) =  ' '
	      CALL Put_Help  (help_line)
	      CALL LIBWAIT (2.0,status)
	   ELSE !motfl(kj)
	      READ (UNIT= command(4:50), FMT= *, ERR= 270) data !
	      DO k = 1, nbuf
		 rbuf(k).s(j) = data
	      END DO
	   ENDIF
	   GOTO  520            
	ELSE IF (command (3:4) .EQ. 'F=') THEN                            ! SnF
	   IF (typ .NE. 6) GOTO 310 !Refl only
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j !which slit
	   kj = slitlist(j)
	   IF (.NOT. motfl(kj)) THEN
	      help_line(1) = ' Slit no. '// command(2:2) //
     1	      ' is not served on this instrument'
	      help_line(2) =  ' '
	      help_line(3) =  ' '
	      CALL Put_Help(help_line)
	      CALL libwait(2.0,status)
	   ELSE !motfl(kj)
	      READ (UNIT= command(5:50), FMT= *, ERR= 270) data !
	      DO k = 1, nbuf !
		 af = data
		 IF (rbuf(k).i(j) .EQ. 0.) rbuf(k).i(j) = 0.05
		 rbuf(k).pts = ANINT ((af - rbuf(k).s(j)) / rbuf(k).i(j) + 1.)
	      END DO !1,nbuf
	   END IF !motfl
	   GOTO 520
	END IF
	GOTO 270
C---------------------------- T:   T  TW TH TER ------------------------------C
 200	IF (command(2:2) .EQ. '=') THEN                                   ! Tmp
	   READ (UNIT= command(3:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).tmp = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).tmp = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'W=') THEN                          ! T-wait
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).tw = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T BUffer
	      DO k = 1, nbuf
		 tbuf(k).tw = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'H=') THEN                          ! T-Hold
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Brag
	      DO k = 1, nbuf
		 bbuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).th = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).th = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ER=') THEN                          ! T-err
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = 1, nbuf
		 ibuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = 1, nbuf
		 qbuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = 1, nbuf
		 bbuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !Refl
	      DO k = 1, nbuf
		 rbuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 7) THEN !X Buffer
	      DO k = 1, nbuf
		 xbuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 8) THEN !D Buffer
	      DO k = 1, nbuf
		 dbuf(k).ter = data
	      END DO
	   ELSE IF (typ .EQ. 4) THEN !T Buffer
	      DO k = 1, nbuf
		 tbuf(k).ter = data
	      END DO
	   ELSE
	      GOTO 310
	   END IF
	   GOTO 520
	END IF
	GOTO 270
 310	help_line(1) = ' Operation not allowed on this buffer!'
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	GOTO 520
 999	CONTINUE
 270	IF (typ .EQ. 1) THEN
	   CALL Updte_I_File (ibuf, line)         
	ELSE IF (typ .EQ. 2) THEN
	   CALL Updte_Q_File (qbuf, line)
	ELSE IF (typ .EQ. 5) THEN !Bragg
	   IF (redopos) THEN
	   DO k = 1,nbuf
	     CALL Bragg_Pos(bbuf(k),bpos,,,comflag,.false.)
	     DO i=1,2
	      bbuf(k).a(i) = bpos(i)
	     ENDDO
	     DO i=5,6
	       bbuf(k).a(i) = bpos(i)
	     ENDDO
	     bbuf(k).a(3) = bpos(3) - int(bbuf(k).pts/2.)*bbuf(k).i(3)
	     bbuf(k).a(4) = bpos(4) - int(bbuf(k).pts/2.)*bbuf(k).i(4)
	   ENDDO
	   ENDIF
	   CALL Updte_B_File (bbuf, line)
	ELSE IF (typ .EQ. 6) THEN !Refl
	   CALL Updte_R_File (rbuf, line)
	ELSE IF (typ .EQ. 7) THEN !X Buffer
	   CALL Updte_X_File (xbuf, line)
	ELSE IF (typ .EQ. 8) THEN !D Buffer
	   CALL Updte_D_File (dbuf, line)
	ELSE IF (typ .EQ. 4) THEN !T Buffer
	   CALL Updte_T_File (tbuf, line)
	END IF
	help_line(1) = ' Unrecognized command: ' // command(1:50)
	help_line(2) = ' Type HELP for command list and syntax '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	RETURN 1
	END
