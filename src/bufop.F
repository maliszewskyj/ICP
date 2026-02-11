#include "icp.h"
	SUBROUTINE bufop (origcommand, iend, typ)
C	***************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     Routine to change fields in buffers indicated from Control mode         C
C     Called from: Parse_control                                              C
C                                                                             C
C     Gayle Greene August 14,1992                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use refl
	use smg_prep
	IMPLICIT   none

	INTEGER*4     i, j, k,  typ, iend, l, m, istart, idum(10)
	REAL          data, af
	REAL*4        bpos(10)
	LOGICAL       redopos, comflag
	CHARACTER*4   str4
	CHARACTER*50  command,origcommand
	CHARACTER*80  help_line(3)

	TYPE(i_buf) ibuf(NBUF)
	TYPE(q_buf) qbuf(NBUF)
	TYPE(t_buf) tbuf(NBUF)
	TYPE(b_buf) bbuf(NBUF)
	TYPE(r_buf) rbuf(NBUF)

	command(1:)=origcommand(1:iend)
	redopos = .false.	!no need to recalculate bragg buffer motor positions

c       Set up range of buffers to be treated

	j=7
	DO WHILE ( lge(command(j:j),'0') .and. lle(command(j:j),'9') ) !isdigit?
	   j=j+1
	ENDDO
	IF (command(j:j).ne.'-')THEN !not a range of buffers, so point to first
	   istart=7
	ELSE			!point to start of last buffer
	   istart=j+1
	ENDIF
	DO WHILE (lge(command(istart:istart),'0') .and. 
	1  lle(command(istart:istart),'9') .and.
	2  istart .lt. iend)	!do while istart points to digit
	   istart=istart+1
	ENDDO
	IF (command(j:j).ne.'-')THEN ! (* one buffer only *)
	   READ (UNIT= command(7:istart-1), FMT= *, ERR= 270) l        
	   m = l
	ELSE			! (* print range of buffers *)
	   READ (UNIT= command(7:j-1), FMT= *, ERR= 270) l        
	   READ (UNIT= command(j+1:istart-1),  FMT= *, ERR= 270) m
	END IF
	IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1    m .LT. 1) GOTO 270
	IF (l .GT. m) THEN	! Exchange l & m
	   k = l
	   l = m
	   m = k
	END IF

	command(1:)=','//command(istart:iend)

	IF (TYP .EQ. 1) THEN
	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT', FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	   DO k = l, m
	      READ (1,REC=k) ibuf(k)
	   ENDDO
	ELSE IF (TYP .EQ. 2) THEN
	   OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT', FORM= 'UNFORMATTED',RECL=QBUF_RECL)
	   DO k = l, m
	      READ (1,REC=k) qbuf(k)
	   ENDDO
	ELSE IF (TYP .EQ. 4) THEN
	   OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT', FORM= 'UNFORMATTED',RECL=TBUF_RECL)
	   DO k = l, m
	      READ (1,REC=k) tbuf(k)
	   ENDDO
	ELSE IF (TYP .EQ. 5) THEN
	   OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT', FORM= 'UNFORMATTED',RECL=BBUF_RECL)
	   DO k = l, m
	      READ (1,REC=k) bbuf(k)
	   ENDDO
	ELSE IF (TYP .EQ. 6) THEN
	   OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT', FORM= 'UNFORMATTED', RECL=RBUF_RECL)
	   DO k = l, m
	      READ (1,REC=k) rbuf(k)
	   ENDDO
	ENDIF
	CLOSE(1)



C------------------FIND NEXT COMMA OR FIND CR AND RETURN --------------------C

 520	i = 1 
 521	DO WHILE (command(i:i) .NE. ',' .AND. command(i:i) .NE. ' ')
	   i = i + 1
	END DO         
	IF (command(i:i) .EQ. ' ' .OR. ( command(i:i) .EQ. ',' .AND.
	1    command(i+1:i+1) .EQ. ' ')) THEN
 522	   IF (typ .EQ. 1) THEN
	      OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	   'DIRECT', FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	      DO k = l, m
		 WRITE (1,REC=k) ibuf(k)
	      ENDDO
	   ELSE IF (typ .EQ. 2) THEN
	      OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	   'DIRECT', FORM= 'UNFORMATTED',RECL=QBUF_RECL)
	      DO k = l, m
		 WRITE (1,REC=k) qbuf(k)
	      ENDDO
	   ELSE IF (typ .EQ. 4) THEN
	      OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	   'DIRECT', FORM= 'UNFORMATTED',RECL=TBUF_RECL)
	      DO k = l, m
		 WRITE (1,REC=k) tbuf(k)
	      ENDDO
	   ELSE IF (typ .EQ. 5) THEN
	      IF (redopos) THEN
		 DO k = l, m
		    CALL Bragg_Pos(bbuf(k),bpos,idum,idum,comflag,.false.)
		    IF (comflag)THEN
		       help_line(1)(1:)='Q-vector not in scattering plane'
		       goto 260
		    ENDIF
		    DO i=1,2
		       bbuf(k)%a(i) = bpos(i)
		    ENDDO
		    DO i=5,6
		       bbuf(k)%a(i) = bpos(i)
		    ENDDO
		    bbuf(k)%a(3) = bpos(3) - int(bbuf(k)%pts/2.)*bbuf(k)%i(3)
		    bbuf(k)%a(4) = bpos(4) - int(bbuf(k)%pts/2.)*bbuf(k)%i(4)
		 ENDDO
	      ENDIF		!redopos
	      OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	   'DIRECT', FORM= 'UNFORMATTED',RECL=BBUF_RECL)
	      DO k = l, m
		 WRITE (1,REC=k) bbuf(k)
	      ENDDO
	   ELSE IF (typ .EQ. 6) THEN
	      OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	   'DIRECT', FORM= 'UNFORMATTED',RECL=RBUF_RECL)
	      DO k = l, m
		 WRITE (1,REC=k) rbuf(k)
	      ENDDO
	   ENDIF
	   CLOSE(1)
 523	   RETURN 
	ELSE
	   command(1:) = command (i+1:50)
	END IF


C-----------------------DETERMINE NATURE OF COMMAND -------------------------C

	j = ICHAR (command(1:1)) ! determine ascii-val of char
	j = j - 64		! 'A'...'Z'  <==>  [1...26]
	IF (j .LT. 1 .OR. j .GT. 26) GOTO 270

	GOTO (  10,  20,  30, 270,  50, 60, 270,  80,  90, 270,
	1    110, 120, 130, 270, 270, 160, 270, 180, 270, 200,
	2    270, 270, 270, 270, 270, 270) j



C-----------------------------A: A AA  An  AnF ------------------------------C

 10	IF (command(2:2) .EQ. '=') THEN	! Latt: a
	   IF (typ .EQ. 2) THEN
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      CALL Getval(command(3:),data,*270)
	      DO k = l, m
	         qbuf(k)%a = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      CALL Getval(command(3:),data,*270)
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      DO k = l, m
	         bbuf(k)%lat_a = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
	ELSE IF (command(2:3) .EQ. 'A=') THEN ! Latt: aa
	   IF (typ .EQ. 2) THEN
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         qbuf(k)%aa = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         bbuf(k)%aa = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
        ELSE IF (typ .GE. 2 ) THEN ! Remainder for Ibuf only
	   GOTO 270
	ELSE IF (command (3:3) .EQ. '=') THEN ! An
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' is not served on this instrument'
	      GOTO 260		!error. stop execution of bufop command.
	   ELSEIF (nsta .eq. 1 .AND. (j.ne.3 .and. j.ne.4) ) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' not moved in buffer'
	      GOTO 260		!error - stop execution of bufop command.
	   ELSE
	      READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	      DO k = l, m
		 ibuf(k)%a(j) = data
c       IF (ibuf(k)%i(j) .NE. 0.) THEN
c       af = ibuf(k)%a(j) + (ibuf(k)%pts - 1) * ibuf(k)%i(j)
c       ELSE
c       af = ibuf(k)%a(j)
c       END IF
	      END DO
	   END IF
	   GOTO 520            
	ELSE IF (command (3:4) .EQ. 'F=') THEN ! AnF
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' is not served on this instrument'
	      GOTO 260
	   ELSEIF (nsta .eq. 1 .AND. (j.ne.3 .and. j.ne.4) ) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' not moved in buffer'
	      GOTO 260		!error - stop execution of bufop command.
	   ELSE
	      READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	      DO k = l, m
		 af = data
		 IF (ibuf(k)%i(j) .EQ. 0.) ibuf(k)%i(j) = 0.05
		 ibuf(k)%pts = ANINT ((af - ibuf(k)%a(j)) / ibuf(k)%i(j) + 1.)
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270


C---------------------------------B: B BB -----------------------------------C

 20	IF (command(2:2) .EQ. '=') THEN	! Latt: b
           IF (typ .EQ. 2) THEN
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      CALL Getval(command(3:),data,*270)
	      DO k = l, m
	         qbuf(k)%b = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      CALL Getval(command(3:),data,*270)
	      DO k = l, m
	         bbuf(k)%b = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
	ELSE IF (command(2:3) .EQ. 'B=') THEN ! Latt: bb
           IF (typ .EQ. 2) THEN
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         qbuf(k)%bb = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         bbuf(k)%bb = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
	END IF
	GOTO 270


C----------------------------C: C CC  COM  COPY -----------------------------C

 30	IF (command (1:4) .EQ. 'COPY') THEN
	   help_line(1)=' Copy command not permitted with BUFOP'
	   GOTO 260		!COPY NOT PERMITTED
	ELSE IF (command(2:4) .EQ. 'OM=' .OR. command(2:4) .EQ. 'om=') THEN
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%com(1:) = command(5:50)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%com(1:) = command(5:50)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%com(1:) = command(5:50)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%com(1:) = command(5:50)
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%com(1:) = command(5:50)
	      END DO
	   END IF
	   GOTO 520		!CHANGE IN COMMENT NOT PERMITTED CAUSE OF LOG FILE
	ELSE IF (command(2:2) .EQ. '=') THEN ! Latt: c
           IF (typ .EQ. 2) THEN
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      CALL Getval(command(3:),data,*270)
	      DO k = l, m
	         qbuf(k)%c = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
c       READ (UNIT= command(3:), FMT= *, ERR= 270) data
	      CALL Getval(command(3:),data,*270)
	      DO k = l, m
	         bbuf(k)%c = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
	ELSE IF (command(2:3) .EQ. 'C=') THEN ! Latt: cc
           IF (typ .EQ. 2) THEN
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         qbuf(k)%cc = data
	      END DO				
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      READ (UNIT= command(4:), FMT= *, ERR= 270) data
	      DO k = l, m
	         bbuf(k)%cc = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   END IF
	END IF
	GOTO 270


C----------------------------E: EC  ES   EFT -----------------------------C

 50	IF (typ .EQ. 1 .or. typ .EQ. 6) GOTO 270 ! not ibuffers,rbuffers
	IF (command(2:3) .EQ. 'C=') THEN
	   IF (typ .EQ. 5)GOTO 270 !not bbuffer
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = l, m
	         qbuf(k)%ec = data
	      END DO				
	   ELSE
	      DO k = l, m
	         tbuf(k)%ec = data
	      END DO				
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'S=') THEN
	   IF (typ .EQ. 5)GOTO 270 !not bbuffer
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = l, m
	         qbuf(k)%es = data
	      END DO				
	   ELSE
	      DO k = l, m
	         tbuf(k)%es = data
	      END DO				
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'FT=') THEN
	   IF (typ .GE. 3) GOTO 270
	   IF (command(5:5) .EQ. 'A') THEN
	      str4(1:3) = 'ANA'		
	   ELSE IF (command(5:5) .EQ. 'M') THEN
	      str4(1:3) = 'MON'
	   ELSE
	      GOTO 270
	   END IF
	   DO k = l, m
	      qbuf(k)%eft = str4(1:3)
	   END DO
	   GOTO 520
	END IF			
	GOTO 270		            

C--------------------------F:  FE -----------------C
 60	IF (command(2:3) .EQ. 'E=') THEN
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2) THEN 
	      DO k = l, m
	         qbuf(k)%ef = data
	      END DO				
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = l, m
	         bbuf(k)%ef = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      DO k = l, m
	         tbuf(k)%ef = data
	      END DO				
	   END IF
	   GOTO 520
	END IF			
	GOTO 270		            

C--------------------------H:   HELP  HC  HS   HF HFW H0 HOLD-----------------C

 80	IF (command(2:4) .EQ. 'ELP') THEN ! Help command not permitted 
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'C=') THEN ! H-center
	   IF (typ .EQ. 1 .OR. typ .EQ. 3 .OR. typ .EQ. 4 .OR. typ .EQ. 6) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ.eq.2)THEN
	      DO k = l, m
		 qbuf(k)%hc = data
	      END DO
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = l, m
	         bbuf(k)%hc = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   ENDIF
	ELSE IF (command(2:3) .EQ. 'S=') THEN ! H-step
	   IF (typ .EQ. 1 .OR. typ .GE. 3) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   DO k = l, m
	      qbuf(k)%hs = data
	   END DO
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'F=') THEN ! H-field
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%hf = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%hf = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%hf = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%hf = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%hf = data
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. '0=') THEN ! Hold 1st
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%th0 = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%th0 = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%th0 = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%th0 = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%th0 = data
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'OLD=') THEN	! Hold
	   READ (UNIT= command(6:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%th = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%th = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%th = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%th = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%th = data
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'FW=') THEN ! HFwait
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%hfw = NINT(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%hfw = NINT(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%hfw = NINT(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%hfw = NINT(data)
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%hfw = NINT(data)
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270


C-------------------------------I:   In  IT IHF------------------------------C

 90	IF (command(2:3) .EQ. 'T=') THEN ! Inc-T
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%it = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%it = ABS(data)
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%it = ABS(data)
	      END DO
	   END IF
	   GOTO 520
 	ELSEIF (command(2:4) .EQ. 'HF=') THEN ! Inc-HF
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%ihf = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%ihf = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%ihf = ABS(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%ihf = ABS(data)
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%ihf = ABS(data)
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command (3:3) .EQ. '=') THEN ! Inc-angle
	   IF (typ .GE. 2 ) GOTO 270
	   READ (UNIT= command(2:2), FMT= *, ERR= 270) j
	   IF (.NOT. motfl(j)) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' is not served on this instrument'
	      GOTO 260
	   ELSEIF (nsta .eq. 1 .AND. (j.ne.3 .and. j.ne.4) ) THEN
	      help_line(1) = ' Motor no. '// command(2:2) //
	1	   ' not moved in buffer'
	      GOTO 260		!error. stop execution of bufop command.
	   ELSE
	      READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	      DO k = l, m
		 ibuf(k)%i(j) = data
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270


C-------------------------------K:   KC  KS ---------------------------------C

 110	IF (command(2:3) .EQ. 'C=') THEN ! K-center
	   IF (typ .EQ. 1 .OR. typ .EQ. 3 .OR. typ .EQ. 4 .OR. typ .EQ. 6) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .eq. 2) THEN
	      DO k = l, m
		 qbuf(k)%kc = data
	      END DO
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = l, m
	         bbuf(k)%kc = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   ENDIF
	ELSE IF (command(2:3) .EQ. 'S=') THEN ! K-step
	   IF (typ .EQ. 1 .OR. typ .GE. 3) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   DO k = l, m
	      qbuf(k)%ks = data
	   END DO
	   GOTO 520
	END IF
	GOTO 270


C-------------------------------L:   LC  LS ---------------------------------C

 120	IF (command(2:3) .EQ. 'C=') THEN ! L-center
	   IF (typ .EQ. 1 .OR. typ .EQ. 3 .OR. typ .EQ. 4 .OR. typ .EQ. 6) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 2)THEN
	      DO k = l, m
		 qbuf(k)%lc = data
	      END DO
	      GOTO 520
	   ELSE IF (typ .EQ. 5) THEN !Bragg
	      DO k = l, m
	         bbuf(k)%lc = data
	      END DO	
	      redopos = .true.			
	      GOTO 520
	   ELSE
	      GOTO 270
	   ENDIF
	ELSE IF (command(2:3) .EQ. 'S=') THEN ! L-step
	   IF (typ .EQ. 1 .OR. typ .GE. 3) GOTO 270
	   READ (UNIT= command(4:), FMT= *, ERR= 270) data
	   DO k = l, m
	      qbuf(k)%ls = data
	   END DO
	   GOTO 520
	END IF
	GOTO 270


C----------------------------M:   MON  MPF  MT ------------------------------C

 130	IF (command (2:4) .EQ. 'ON=') THEN ! Monitor
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%mon = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%mon = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%mon = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !not valid for rbuf
	      GOTO 270
	   ELSE
	      DO k = l, m
		 tbuf(k)%mon = data
	      END DO
	   END IF
	   GOTO 520            
	ELSE IF (command (2:4) .EQ. 'PF=') THEN	! Mpf
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%mpf = ANINT(data)
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      GOTO 270
	   ELSE
	      DO k = l, m
		 tbuf(k)%mpf = ANINT(data)
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'T=') THEN ! Monitor type
	   IF (command(4:4) .EQ. 'N' .AND. nsta .NE. 0) THEN
	      str4 = 'NEUT'
	   ELSE IF (command(4:4) .EQ. 'T' .OR. nsta .EQ. 0) THEN
	      str4 = 'TIME'
	   ELSE
	      GOTO 270
	   END IF
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%mt = str4
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%mt = str4
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%mt = str4
	      END DO
	   ELSE IF (typ .EQ. 6) THEN !not for rbuffer
	      GOTO 270
	   ELSE
	      DO k = l, m
		 tbuf(k)%mt = str4
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. '3=' .AND. nsta .EQ. 4) THEN	! Move Angle 3
	   IF (command(4:4) .EQ. 'N') THEN
	      DO k = l, m
		 tbuf(k)%m3 = .FALSE.
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%m3 = .TRUE.
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270


C-------------------------------P:   PTS    ---------------------------------C

 160	IF (command(2:4) .EQ. 'TS=') THEN ! Pts
	   READ (UNIT= command(5:), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%pts = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%pts = data
	      END DO
	   ELSE IF (typ .EQ. 5 .or. typ .eq. 6) THEN
	      GOTO 270
	   ELSE 
	      DO k = l, m
		 tbuf(k)%pts = data
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270


C--------------------------------R:   RESET ---------------------------------C

 180	IF (command(2:3) .EQ. 'ES') THEN ! Reset
	   help_line(1)=' Reset not allowed in BUFOP'
	   GOTO 260		!RESET NOT ALLOWED
	END IF 
	GOTO 270


C----------------------------T:   T  TW TH TER ------------------------------C

 200	IF (command(2:2) .EQ. '=') THEN	! Tmp
	   READ (UNIT= command(3:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%tmp = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%tmp = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%tmp = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%tmp = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%tmp = data
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'W=') THEN ! T-wait
	   READ (UNIT= command(4:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%tw = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%tw = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%tw = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%tw = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%tw = data
	      END DO
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ER=') THEN ! T-err
	   READ (UNIT= command(5:50), FMT= *, ERR= 270) data
	   IF (typ .EQ. 1) THEN
	      DO k = l, m
		 ibuf(k)%ter = data
	      END DO
	   ELSE IF (typ .EQ. 2) THEN
	      DO k = l, m
		 qbuf(k)%ter = data
	      END DO
	   ELSE IF (typ .EQ. 5) THEN
	      DO k = l, m
		 bbuf(k)%ter = data
	      END DO
	   ELSE IF (typ .EQ. 6) THEN
	      DO k = l, m
		 rbuf(k)%ter = data
	      END DO
	   ELSE
	      DO k = l, m
		 tbuf(k)%ter = data
	      END DO
	   END IF
	   GOTO 520
	END IF
	GOTO 270

c       Error: return without storing any of changes in data file. None of
c       bufop command executed due to error.
 260	CALL PUTLINE(contr,help_line(1))
	GOTO 280
 270	help_line(1) = ' Operation not recognized for this buffer:'
	CALL PUTLINE(contr,help_line(1))
 280	CALL PUTLINE(contr,command)
	CALL PUTLINE(contr,' BUFOP command not executed')
	RETURN
	END

