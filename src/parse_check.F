#include "icp.h"
	SUBROUTINE Parse_CHECK (command, *)
C	***********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Checks Spelling And Syntax Of The Given Commands.               C
C     Called from: check_sequence  & check_file_sequence                      C
C                                                                             C
C     Gayle Greene, July 1992                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use instr
	use tc
	use polar
	use refl
	IMPLICIT      none
	BYTE	      ibit(0:15,0:3) /64*0/
	INTEGER*4     i, j, k, l, m,  maxit
	INTEGER*4     ios, np, type, ig
	INTEGER*4     mot, iend
	REAL*4        time, data, pos1, pos2
	REAL*4        multi(6), range, step, mon, Energy_Angle
	REAL*4        params(6),  start
	REAL*4	      toler, etrans, FZ, z1, z2
	LOGICAL       isdigit,  inlimits
	CHARACTER*4   str4
	CHARACTER*5   str5
	CHARACTER*50  command, local
	CHARACTER*80  response, tconfig(9)

	DATA tconfig /
	1  '        No:  1    Primary: DRC 82C     Secondary: none  	Motor: none',
	2  '        No:  2    Primary: DRC 93C     Secondary: none  	Motor: none',
	3  '        No:  3    Primary: Omega       Secondary: none  	Motor: none',
	4  '        No:  4    Primary: DRC 82C     Secondary: Magnet	Motor: specify',
	5  '        No:  5    Primary: DRC 93C     Secondary: Magnet	Motor: specify',
	6  '        No:  6    Primary: Omega       Secondary: Magnet	Motor: specify',
	7  '        No:  7    Primary: Magnet      Secondary: DRC 82C	Motor: specify',
	8  '        No:  8    Primary: Magnet      Secondary: DRC 93C	Motor: specify',
	9  '        No:  9    Primary: Magnet      Secondary: Omega 	Motor: specify'/



C---------------------- REMOVE ALL BLANKS FROM STRING ------------------------C

 500	j = 1
	DO i = 1, 50
	   IF (command(i:i) .NE. ' ') THEN
	      command(j:j) = command(i:i)
	      j = j + 1
	   END IF
	END DO
	command(j:50) = ' '                               ! blank is terminator
	IF (command(1:1) .NE. ';') command = ';'// command ! semicolon is for 
C							  correct loop handling
	      

C------------------ FIND NEXT COMMA OR FIND CR AND RETURN --------------------C

 520	i = 1 
    	DO WHILE (command(i:i) .NE. ';' .AND. command(i:i) .NE. ' ')
	   i = i + 1
	END DO         
	IF (command(i:i) .EQ. ' ' .OR. ( command(i:i) .EQ. ';' .AND.

	1                                command(i+1:i+1) .EQ. ' ')) THEN
	   RETURN 1
	ELSE
	   command(1:) = command (i+1:50)
	END IF

C---------------------- FIND END OF NEXT COMMAND ---------------------------C
C  This is needed when reading parameters with * format since ; is not 
C   recognized as premature end of field terminator in fortran read
	iend=1
	DO WHILE (command(iend:iend) .NE. ';' .AND. command(iend:iend) .NE. ' '
     1	   .AND. iend .LE. 50)
	  iend = iend + 1
	ENDDO
	iend = iend - 1

C----------------------- DETERMINE NATURE OF COMMAND -------------------------C

	j = ICHAR (command(1:1))                  ! determine ascii-val of char
	IF (j .EQ. 45 .OR. (j .GE. 48 .AND. j .LE. 57)) GOTO 270
	j = j - 64                                  ! 'A'...'Z'  <==>  [1...26]
	IF (j .LT. 1 .OR. j .GT. 26) GOTO 270

	GOTO (10,  20,  30,  40,  50,  60, 999,  80,  90, 999,
	1     110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
	2     210, 220, 230, 240, 999, 260) j

C---------------------------------- A: ALIGN ---------------------------------C

C---------------------------------- A: ALIGN ---------------------------------C

 10	IF (command(2:3) .EQ. 'PD') THEN              ! Align Peak at Detector
	   m = 4
	   DO WHILE (command(m:m) .EQ. '/')
	    IF (command(m+1:m+1).eq.'F')then !make file
	     m = m+2
	    ELSEIF (command(m+1:m+1).eq.'M')then !fit to minimum
	     m = m+2
	    ELSEIF (command(m+1:m+1) .EQ. 'T')THEN !monitor temp
	     m = m+2
	    ELSE
	     GOTO 270 !syntax error
	    ENDIF
	   ENDDO
	   ios = 1
	   np = 6
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .LT. 2) THEN
	     GOTO 270 !error. must specify motor & detector at least.
	   ELSE
	     ig    = IFIX (multi(1))      		  ! (1) is detector no
	     mot   = IFIX (multi(2))			  ! (2) is motor number
	   ENDIF
	   IF (mot.lt.1 .or. mot.gt.MAXMOTS)goto 270
	   IF (nsta .eq. 1) THEN
	     IF (ig .gt. scl .or. ig .lt. 1)goto 270
	   ELSEIF (.not. PSD) THEN 
	     IF (ig .gt. 4 .or. ig .lt. 1)goto 270
	   ENDIF
           IF (np .GT. 2) THEN
	     range = multi(3)				  ! (3) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 3) THEN
	     step = multi(4)				  ! (4) step width
	   ELSE
	     step = .1
	   ENDIF 
           IF (np .GT. 4) THEN
	     mon   = multi(5)				  ! (5) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
           IF (np .GT. 5) THEN
	     toler   = multi(6)				  ! (6) is toler
	   ELSE
	     toler   = .01
	   ENDIF
	   command(1:) = command (m:)
	   IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
	   IF (.NOT. motfl(mot)) THEN
	      response = 'That motor is not available on this instrument!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE !read in z. Call align-peak. Write out new z.
	     GOTO 520
	   END IF
	ELSEIF (command(2:2) .EQ. 'P' .OR. command(2:2) .EQ. 'M') THEN ! Find peak 2:1
	   i = 3
	   type = 1
	   IF (command(2:2) .EQ. 'M') type = 3
	   IF (command(2:2) .EQ. 'P' .AND. command(3:3) .EQ. 'T') THEN
	      i = 4
	      type = 2
	   END IF
	   m = i
	   DO WHILE (command(m:m) .EQ. '/')
	    IF (command(m+1:m+1).eq.'F')then !make file
	     m = m+2
	    ELSEIF (command(m+1:m+1).eq.'M')then !fit to minimum
	     m = m+2
	    ELSEIF (command(m+1:m+1) .EQ. 'T')THEN !monitor temp
	     m = m+2
	    ELSE
	     GOTO 270 !syntax error
	    ENDIF
	   ENDDO
	   ios = 1
	   np = 5
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	      READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	      IF ( ios .NE. 0 ) np = np - 1 !np has # params read in at exit
	   END DO
	   IF (np .EQ. 0) GOTO 270 !error. must specify motor at least.
	   IF (np .GT. 0) mot   = IFIX (multi(1))	  ! (1) is motor number
           IF (np .GT. 1) range = multi(2)		  ! (2) is angle range
	   IF (np .GT. 2) step  = multi(3)		  ! (3) step width
           IF (np .GT. 3) mon   = multi(4)		  ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)		  ! (5) is tolerance
	   command(1:) = command (m:)
	   IF (.NOT. motfl(mot) ) THEN
	      response = 'That motor is not available on this instrument!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE
	      GOTO 520
	   END IF
	ELSE IF (command(2:2) .EQ. 'S' .OR. command(2:2) .EQ. 'A') THEN ! Find Monitor
	   m = 3
	   DO WHILE (command(m:m) .EQ. '/')
	    IF (command(m+1:m+1).eq.'F')then !make file
	     m = m+2
	    ELSEIF (command(m+1:m+1).eq.'M')then !fit to minimum
	     m = m+2
	    ELSEIF (command(m+1:m+1) .EQ. 'T')THEN !monitor temp
	     m = m+2
	    ELSE
	     GOTO 270 !syntax error
	    ENDIF
	   ENDDO
	   ios = 1
	   np = 6
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	      READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	      IF ( ios .NE. 0 ) np = np - 1 !np has # params read in at exit
	   END DO
	   IF (np .GT. 0) mot   = IFIX (multi(1))	  ! (1) is motor number
           IF (np .GT. 1) range = multi(2)		  ! (2) is angle range
	   IF (np .GT. 2) step  = multi(3)		  ! (3) step width
           IF (np .GT. 3) mon   = multi(4)		  ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)		  ! (5) is tolerance
	   IF (np .GT. 5) maxit = IFIX (multi(6))	  ! (6) is maxit
	   IF ( command(2:2) .EQ. 'S' .AND. (mot .NE. 3 .AND. mot .NE. 4) ) THEN
	      response = 'Sample alignment must start with motor 3 or 4!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	   IF ( command(2:2) .EQ. 'A' .AND. (mot .NE. 5 .AND. mot .NE. 6) ) THEN
	      response = 'Analyzer alignment must start with motor 5 or 6!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	   command(1:) = command (m:)
	   IF (motfl(mot)) THEN
	     GOTO 520
	   ELSE
	      response = 'That motor is not available on this instrument!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	END IF
	GOTO 270


C---------------------------------- B: BYE BUFOP------------------------------C

  20	IF (command(2:5) .EQ. 'UFOP') THEN		    ! Buffer operation
	  IF (command(6:6) .EQ. 'I' )THEN		    ! Ibuffer
	    type = 1
	  ELSE IF (command(6:6) .EQ. 'Q') THEN
	    type = 2					    ! Qbuffer	
	  ELSE IF (command(6:6) .EQ. 'T') THEN
	    type = 4					    ! Tbuffer	
	  ELSE IF (command(6:6) .EQ. 'B') THEN
	    type = 5					    ! Bbuffer	
	  ELSE
	    GOTO 270					    ! Unrecognized buftype
	  ENDIF
	  command(1:)=command(5:)
	  GOTO 520
	ENDIF
	GOTO 270



C------------------------------ C:  COL CT  CO CS-----------------------------C

  30	IF (command(2:3) .EQ. 'OL') THEN                          ! Collimation
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend) , FMT= *, ERR= 270) (multi(k), k= 1, 4)
	   DO j= 1, 3
	      i = INDEX (command, ',')
	      command(1:) = command(i+1:)      
	   END DO
	   IF (nsta.eq.1 .AND.
     1	     (ifix(multi(1)) .ne. 7 .and. ifix(multi(1)) .ne. 15)) THEN
	      response=' Collim on BT1 must be 7 or 15!'
	     CALL PUTLINE (contr,response)
	     GOTO 270
	   ENDIF !invalid value on BT1
	   GOTO 520
	ELSE IF (command(2:7) .EQ. 'ONFIRM')THEN
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'O' .OR. command(2:2) .EQ. 'T') THEN
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (data .LT. 0) THEN                              ! Count with time
	      time = ABS(data)
	   ELSE                                            ! Count with monitor
	      IF (nsta .eq. 0) CALL NoMon (*270)
	   END IF !counting on monitor or time
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'S') THEN !CLOSE SHUTTER ON XRAY
	   IF (nsta.ne.0)GOTO 270
	   GOTO 520
	END IF
	GOTO 270


C--------- D: DM DA DQ Dm DmI DmT D-/+ DEA DEM DELRS DIRQ DIRB DIRI DIRT -----C

  40	IF (command(2:2) .EQ. '+') THEN                       ! Data storage ON
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN                 ! Data storage OFF
	   GOTO 520      
	ELSE IF (command(2:2) .EQ. 'M') THEN                  ! Monochr spacing
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   IF (isdigit(command(j:j))) THEN                    ! numerical value
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   ELSE					           ! alphanum reference
	      IF (command(j:j+1) .EQ. 'PG') GOTO 520
	      IF (command(j:j+1) .EQ. 'CU') GOTO 520
	      IF (command(j:j+1) .EQ. 'HS') GOTO 520
	      GOTO 270
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'A') THEN                 ! Analyzer spacing
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   IF (isdigit(command(j:j))) THEN                    ! numerical value
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   ELSE					           ! alphanum reference
	      IF (command(j:j+1) .EQ. 'PG') GOTO 520
	      IF (command(j:j+1) .EQ. 'CU') GOTO 520
	      IF (command(j:j+1) .EQ. 'HS') GOTO 520
	      GOTO 270
	   END IF
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'EFA') THEN
	  IF (nsta .ne. -5)GOTO 270
	  GOTO 520
	ELSE IF (command(2:5) .EQ. 'ELRS') THEN
	  GOTO 270
	ELSE IF (command(2:2) .EQ. 'Q') THEN                       ! Drive to Q	
	  IF (nsta .eq. -7) THEN !refl
	    ios = 1
	    np = 2
	    m = 3
	    DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
	      READ (UNIT= command(m:iend),FMT=*,IOSTAT= ios) (multi(i),i= 1,np)
	      IF ( ios .NE. 0) np = np - 1
	    ENDDO
	    IF (np .LT. 1) GOTO 270 !error. must specify qz
	    command(1:) = command (3:)
	  ELSE !not refl
	   ios = 1
	   np = 4
	   m = 3
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	      READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	      IF ( ios .NE. 0 ) np = np - 1
	   END DO
	   IF (np .LT. 3) GOTO 270 !error. must specify qx,qy,qz
	   IF (np. GE. 4) THEN
	     etrans = multi(4)
	   ELSE
	     etrans = 0
	   ENDIF
	   command(1:) = command (m:)
	 ENDIF !refl
	 GOTO 520
	ELSE IF (command(2:2) .EQ. 'R') THEN                          ! Dry Run
	   j = INDEX (command, ';')
	   IF ( j .EQ. 0 ) THEN		   ! (* only one command on this line *)
	      local(1:) = command (1:)
	   ELSE                            ! (* more than one command on line *)
	      local(1:) = command(1:j-1)
	   END IF
	   j = INDEX (local, '-')
	   IF ( j .EQ. 0 ) THEN                  ! (* execute one buffer only *)
	      READ (UNIT=local(4:iend), FMT= *, ERR= 270) l
	      m = l
	   ELSE                                 ! (* execute range of buffers *)
	      READ (UNIT=local(4:j-1), FMT= *, ERR= 270) l
	      READ (UNIT=local(j+1:iend),  FMT= *, ERR= 270) m
	   END IF
	   IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1         m .LT. 1) GOTO 270
	   str4(1:1) = local(3:3)
	      IF (str4(1:1) .EQ. 'I'.and. nsta .ne. -7) GOTO 520
	      IF (str4(1:1) .EQ. 'Q' .AND. nsta .NE. 1
     1	         .and. nsta .ne. -7) GOTO 520
	      IF (str4(1:1) .EQ. 'B' .AND. nsta .NE. 1
     1	         .and. nsta .ne. -7) GOTO 520
	      IF (str4(1:1) .EQ. 'T' .AND. nsta .EQ. 4) GOTO 520
	      IF (str4(1:1) .EQ. 'R' .AND. nsta .EQ. -7) GOTO 520
	   GOTO 270
	ELSE IF (command(2:3) .EQ. 'IR') THEN                  ! Dir of buffers
	   IF (command(4:4) .EQ. 'I') GOTO 520
	   IF (command(4:4) .EQ. 'T') THEN                           ! DirT
	      IF (nsta .NE. 4) GOTO 270
	   ELSE                                                ! DirQ & DirB
	      IF (nsta .EQ. 1) GOTO 270
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'EM') THEN            ! Drive to mono energy
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Drive is ok
	      j = 4
	      IF (command(j:j) .EQ. '=') j = 5
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, dm) !NOTE: ENERGY ANGLE USES NO HDW
	      pos2 = 2. * pos1
	      z2 = fz(2,pos2) !get linear z for motor 2 on BT4
	      pos2 = pos2+z2
	      IF (.NOT. inlimits(2,pos2,contr))THEN
		 GOTO 270
	      ELSE IF (motfl(1)) THEN
	         z1 = fz(1,pos1) !get linear z for motor 1 on BT4
		 pos1 = pos1 + z1
	         IF (.NOT. inlimits(1,pos1,contr))THEN
		    GOTO 270
	         ELSE
	            GOTO 520
		 END IF
	      ELSE
	        GOTO 520
	      END IF
	   END IF
	   GOTO 520 
	ELSE IF (command(2:3) .EQ. 'EA') THEN             ! Drive to ana energy
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Drive is ok
	      j = 4
	      IF (command(j:j) .EQ. '=') j = 5
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, da) !ENERGY_ANGLE USES NO HDW
	      pos2 = 2. * pos1 + z(6)
	      pos1 = pos1 + z(5)
	      IF (.NOT. inlimits(5, pos1, contr))THEN
	         GOTO 270
	      ELSE IF (.NOT. inlimits(6, pos2, contr))THEN
	         GOTO 270
	      ELSE
		GOTO 520
	      END IF
	   END IF
	   GOTO 520
	ELSEIF (command(2:2).eq.'S') THEN			!Drive Slit
 	   j=3
 	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j-1), FMT= *, ERR= 270) m    ! Slit number
	   IF (nsta.ne.-7) goto 270
	   IF (m.lt.1 .or. m.gt.4)goto 270
	   m = slitlist(m)
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) goto 270
	ELSE                                                    ! Drive command
	   j = 2
	   DO WHILE (isdigit(command(j:j)) .and. j .lt. iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(2:j-1), FMT= *, ERR= 270) m    ! Motor number
 	   IF (m .lt. 1 .or. m .gt. MAXMOTS) goto 270 ! WAS LINE 45
	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Drive is ok
	      L=j
	      IF (command(j:j) .NE. 'I' .AND. command(j:j) .NE. 'T'
     1	        .AND. command(j:j) .NE. 'H') THEN
	        IF (command(j:j) .NE. '=')GOTO 270
		 j = j+1
		 IF (command(j:j) .EQ. '=') j = j+1
	      ELSE 
		 j = j+1
		 IF (command(j:j) .EQ. '=') j = j+1
	      END IF
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	      IF (command(L:L) .EQ. 'I') THEN               ! Incremental drive
	        GOTO 520
	      ELSE IF (command(L:L) .EQ. 'T') THEN                  ! 1:2 drive
		 k = m - 1
		 IF (.NOT. motfl(k) .OR. MOD(m,2) .NE. 0) THEN
		    response = 'Illegal 2:1 Drive. '
		    CALL PUTLINE (contr, response)
		    GOTO 270
		 END IF
		 GOTO 520
	      ELSE IF (command(L:L) .EQ. 'H') THEN                  ! Hard drive
		 pos1 = z(m) + data                     !add zero in to check limits
		 k = m
		 pos2 = pos1
	      ELSE
		 pos1 = z(m) + data                              ! Normal drive
		 k = m
		 pos2 = pos1
	      END IF
	      IF (.NOT. inlimits(m, pos1, contr))THEN
	         GOTO 270
	      ELSE IF (.NOT. inlimits(k, pos2, contr))THEN
	         GOTO 270
	      ELSE
	         GOTO 520
	      END IF
	   END IF
	   GOTO 520
	END IF      
	GOTO 270 

C--------------------------------- E: EXIT -----------------------------------C

  50	GOTO 270


C------------- F: FCAL FAMP FIX FM FP FPT FLA FLM ----------------------------C

  60	IF (command(2:4) .EQ. 'CAL') THEN         ! Calib
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 3)
	   DO j = 1, 2
	      m = INDEX (command(1:), ',')
	      command(1:) = command (m+1:)
	   END DO
    	   mot            = IFIX (multi(1))		  ! (1) is power supply
	   IF (polfl(mot)) THEN
	     GOTO 520
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	ELSE IF (command(2:2) .EQ. 'DSA') THEN       !dsa for focus motors
	   j=5
	   IF (command(5:5) .EQ. '=')j=6
	   READ(UNIT=command(j:iend),fmt=*,err=270)data
	   IF (nsta .ne. -5)GOTO 270
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'DAD') THEN       !dad for focus motors
	   j=5
	   IF (command(5:5) .EQ. '=')j=6
	   READ(UNIT=command(j:iend),fmt=*,err=270)data
	   IF (nsta .ne. -5)GOTO 270
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN       !set flippers
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF 
	   IF (command(3:3) .EQ. 'M') THEN       !set mono flippers
	      GOTO 520
	   ELSE					  !set anal flippers
	     IF (command(3:3) .NE. 'A') goto 270
	     GOTO 520
	   ENDIF
	ELSE IF (command(2:4) .EQ. 'AMP') THEN       ! fullamp for BT2 flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot = IFIX (multi(1))		           ! (1) is power supply
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
	   IF (polfl(mot)) THEN
	     GOTO 520
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	ELSE IF (command(2:4) .EQ. 'IXM' .OR. command(2:4) .EQ. 'IXD') THEN
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0.or.nsta.eq.8) GOTO 270
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'IXT' .or. command(2:4).eq.'IXS') THEN                      ! Fix Table
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0.or.nsta.eq.8) GOTO 270
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'IXA')THEN
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0 .or.nsta.eq.8) GOTO 270
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'IX') THEN !FIX motor n
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) GOTO 270
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'PT') THEN                    ! Find peak 2:1
	   IF (command(4:5).eq.'/F')then !make file
	     m=6
	   ELSE
	     m=4
	   ENDIF
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
             READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np -1
	   END DO
	   IF (np. EQ. 0) THEN
	     GOTO 270 !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
	   IF (np .GT. 1) THEN
             range = multi(2)				  ! (2) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 2) THEN
	     step  = multi(3)				  ! (3) step width
	   ELSE
	     step  = .1					  ! (3) step width
	   ENDIF
	   IF (np .GT. 3) THEN
             mon   = multi(4)				  ! (4) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
	   command(1:) = command (m:)
	   IF (.NOT. motfl(mot) .OR. MOD(mot,2) .NE. 0) THEN
	     response = 'That motor is not available on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   ELSE
	     GOTO 520
	   ENDIF
	ELSE IF (command(2:2) .EQ. 'M') THEN                     ! Find Monitor
	   IF (command(3:4).eq.'/F')then !make file
	     m=5
	   ELSE
	     m=3
	   ENDIF
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
             READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios. NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .EQ. 0) THEN
	     GOTO 270 !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))	  		  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1) THEN
	     range = multi(2)		  		  ! (2) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 2) THEN
	     step  = multi(3)				  ! (3) step width
	   ELSE
	     step = .1
	   ENDIF
           IF (np .GT. 3) THEN
	     mon   = multi(4)				  ! (4) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
	   command(1:) = command (m:)
	   IF (motfl(mot)) THEN
	     GOTO 520
	   ELSE
	     response = 'That motor is not available on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	ELSE IF (command(2:3) .EQ. 'PD') THEN              ! Find Peak at Detector
	   IF (command(4:5).eq.'/F')then !make file
	     m=6
	   ELSE
	     m=4
	   ENDIF
	   ios = 1
	   np = 5
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .LT. 2) THEN
	     GOTO 270 !error. must specify motor & detector at least.
	   ELSE
	     ig    = IFIX (multi(1))			  ! (1) is detector no
	     mot   = IFIX (multi(2))			  ! (2) is motor number
	   ENDIF
	   IF (mot.lt.1 .or. mot.gt.MAXMOTS)goto 270
	   IF (nsta .eq. 1) THEN
	     IF (ig .gt. scl .or. ig .lt. 1)goto 270
	   ELSEIF (.not. PSD) THEN 
	     IF (ig .gt. 4 .or. ig .lt. 1)goto 270
	   ENDIF
           IF (np .GT. 2) THEN
	     range = multi(3)				  ! (3) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 3) THEN
	     step = multi(4)				  ! (4) step width
	   ELSE
	     step = .1
	   ENDIF 
           IF (np .GT. 4) THEN
	     mon   = multi(5)				  ! (5) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
c	   IF ( range .LT. .005 ) range = 1.
c	   IF ( step  .LT. .005 ) step = .1
c	   IF ( abs(mon) .LT. .5 ) mon = -1.
	   command(1:) = command (m:)
	   IF (nsta .eq. 0 .and. mon .ge. 0) GOTO 270
	   IF (motfl(mot)) THEN
	     GOTO 520
	   ELSE
	     response = 'That motor is not available on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'P') THEN                        ! Find Peak
	   IF (command(3:4).eq.'/F')then !make file
	     m=5
	   ELSEIF (command(3:4).eq.'/Q') then ! quiet
	     m=5
	   ELSEIF (command(3:4).eq.'/M') then ! fit to minimum
	     m=5
	   ELSE
	     m=3
	   ENDIF
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .EQ. 0) THEN
	     GOTO 270 !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1) THEN
	     range = multi(2)				  ! (2) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 2) THEN
	     step = multi(3)				  ! (3) step width
	   ELSE
	     step = .1
	   ENDIF 
           IF (np .GT. 3) THEN
	     mon   = multi(4)				  ! (4) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
	   command(1:) = command (m:)
	   IF (motfl(mot)) THEN
	     GOTO 520
	   ELSE
	     response = 'That motor is not available on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	END IF
	GOTO 270


C------------------------------- H: HELP  HOLD  -------------------------------C

  80	IF (command(2:2) .EQ. 'E') THEN
	  GOTO 270
	ELSE IF (command(2:4) .EQ. 'OME') THEN    !Drive motor to home 
	   j = 5
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(5:j-1), FMT= *, ERR= 270) m    ! Motor number
	   IF (nsta .ne. -7 .AND. (nsta .ne. -5 .or. mottype(m).ne.acsf))GOTO 270
	   IF (m.lt.1 .or. m.gt.MAXMOTS)goto 270
	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(5:j-1) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ENDIF
	   GOTO 520
	ELSEIF (command(2:2) .EQ. 'M') THEN                  ! Define mag mode
	   IF (command(3:3).eq.'=')THEN
	     j=4
	   ELSE
	     j=3
	   ENDIF
	   IF (command(j:j).eq.'N')THEN
	     GOTO 520
	   ELSEIF (command(j:j).eq.'P')THEN
	     GOTO 520
	   ELSE
	     goto 270
	   ENDIF
	ELSE IF (command(2:3) .EQ. 'OL') THEN              ! Hold for given time
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   GOTO 520
	ELSE IF (command(2:3) .EQ. '+') THEN
	   IF (.NOT. minit_done) THEN
	      response(1:) = '*** Magnet control not initialized. Use HDEV. ***'
	      CALL PUTLINE (contr, response)
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. '-') THEN
	   GOTO 520
        END IF
	GOTO 270


C---------------------------- I: INIM INIT IGET ISET ISCAN--------------------C

  90	IF (command(2:4) .EQ. 'NIM') THEN                  ! Reset magnet contr
	   IF (tdev%mot .NE. 0) THEN
	     GOTO 520
	   ELSE
	      response(1:) = 'Auxiliary controller must be enabled first!!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	ELSE IF (command(2:3) .EQ. 'NI') THEN ! Initialize controller to angle f
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   L=j
	   DO WHILE (isdigit(command(L:L)) .AND. L .LT. 50)
	     L=L+1
	   ENDDO
	   READ (UNIT= command(j:L-1), FMT= *, ERR=270) m
	   IF (m .lt. 1 .or. m .gt. MAXMOTS)goto 270
	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(j:l-1) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE
	      DO WHILE (.NOT. isdigit(command(L:L)) .AND. L .LT. 50)
	         L = L+1
	      END DO
	      READ (UNIT= command(L:iend), FMT= *, ERR= 270) data
	      GOTO 520
	   END IF
	ELSE IF (command(2:4) .EQ. 'GET') THEN              ! Iset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
	   IF (polfl(mot)) THEN
              GOTO 520
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	ELSE IF (command(2:4) .EQ. 'SET') THEN              ! Iset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
	   IF (polfl(mot)) THEN
              GOTO 520
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	ELSE IF (command(2:5) .EQ. 'SCAN') THEN               ! Scan flipper current
	   IF (command(6:7).eq.'/F')then !make file
	     m=8
	   ELSE
	     m=6
	   ENDIF
           READ (UNIT= command(m:iend), FMT= *, ERR= 270) (multi(i), i= 1, 5)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
           start = multi(2)				  ! (2) is start I
	   step  = multi(3)				  ! (3) step width
	   RANGE   = multi(4)                             ! (4) upper limit
           mon   = multi(5)				  ! (5) is monitor
	   DO i = 1, 4
	      m = INDEX (command(1:), ',')
	      command(1:) = command (m+1:)
	   END DO
	   GOTO 520
     	END IF
	GOTO 270


C---------------------------------- L: Lm L+ L--------------------------------C

 110	IF (command(2:2) .EQ. 'F') THEN  !Keithly Fetch command
	  IF (nsta .ne. -7)GOTO 270
	  GOTO 520
	ENDIF
        GOTO 270

C---------------------------------- L: Lm L+ L--------------------------------C

 120	IF (command(3:3) .EQ. '=' .or. command(4:4) .eq. '=') THEN  ! Change lower limits
	   L=2
	   DO WHILE (isdigit(command(L:L)) .and. L .lt. iend)
	     L=L+1
	   ENDDO
	   READ (UNIT= command(2:L-1), FMT= *, ERR= 270) j  ! determ angle no
	   IF (j .lt. 1 .or. j .gt. MAXMOTS) goto 270
	   IF (.NOT. motfl(j)) THEN
	      response = 'Motor no. '// command(2:L-1) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                ! Enable PSD
	   IF (iabs(nsta).ne.7.AND. nsta .NE. -1.AND. nsta .NE. 8) THEN
	    CALL PUTLINE(contr, 
     1	     'Position sensitive detector only implemented on NG7,BT8')
	    GOTO 270
	   ENDIF
	   IF (histo_add.eq.0) THEN
	    CALL PUTLINE(contr, 
     1	     'Position sensitive detector not specified in instr.cfg')
	    GOTO 270
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable statistics
	   GOTO 520
	END IF
	GOTO 270


C------------------------------ M: MOS MRAT MP -------------------------------C

 131	time = 5.
	GOTO 520
 130	IF (command(2:4) .EQ. 'RAT') THEN
	   j = 5
	   IF (command(j:j) .eq. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 131) data
 	   GOTO 520 ! WAS LINE 132
	ELSE IF (command(2:4) .EQ. 'ONO') THEN   !bt1: define monitor !MONO=
	   IF (nsta .ne. 1) GOTO 270
	   str5 = command(6:10)
	   IF (str5 .ne. 'CU311' .AND. str5 .ne. 'GE311' .and. 
     1		str5 .ne. 'GE733' .AND. str5 .ne. 'DUMMY')GOTO 270
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'OS') THEN                   ! define mosaic
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend) , FMT= *, ERR= 270) (multi(k), k= 1, 3)
	   DO j= 1, 2
	      i = INDEX (command, ',')
	      command(1:) = command(i+1:)      
	   END DO
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'P') THEN             ! set new motor params
	   j=3
	   DO WHILE (isdigit(command(j:j)).and. j .lt. iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j-1), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m.gt.MAXMOTS)goto 270
	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE
	      DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
		 j = j + 1
	      END DO
	      READ (UNIT= Command(j:iend), FMT= *, ERR= 270) (multi(i), i=1,5)
	      DO i = 1, 4
		 m = INDEX (command(1:), ',')
	         command(1:) = command (m+1:)
	      END DO
	      GOTO 520
	   END IF
	   GOTO 520	      
	END IF
	GOTO 270

C----- N:  NEXT ---------------------------------------------------------------C
 140	IF (command(2:4) .EQ. 'EXT') THEN                !Next sample pos
	   IF (nsta.ne.1)CALL Not_Implemented (*270)
	   IF (.NOT. motfl(15)) THEN
     	    response= 'Motor 15 not currently defined'
	    CALL PUTLINE(contr,response) 
	   ENDIF
	   IF (fixed(15)) THEN
     	    response= 'Motor 15 is fixed'
	    CALL PUTLINE(contr,response) 
	   ENDIF
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'UMX') THEN       !numx for asd
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) ig
	   IF (nsta .eq. -5 .AND. MOD(256,ig) .NE. 0)THEN
	       CALL Putcontr(' NUMX must be a divisor of 256')
	       GOTO 270
	   ENDIF
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'UMY') THEN       !numy for asd
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) ig
	   IF (nsta .eq. -5 .AND. MOD(256,ig) .NE. 0)THEN
	     CALL Putcontr(' NUMY must be a divisor of 256')
	     GOTO 270
	   ENDIF
	   GOTO 520
	ENDIF
	GOTO 270

C----- O: OS -----------------------------------------------------------------C
 150	IF (command(2:2) .EQ. 'S') THEN !close shutter on xray
	  IF (nsta.ne.0)GOTO 270
	  GOTO 520
	ENDIF
	GOTO 270
	
C----- P: PSA PHA PDA PDM PMP PRS PA PC PD PI PL PM PQ PT PB PU PV PW P-/+ ---C
C---------------------------PFAMP--- PFC ---PTP-------------------------------C

 160    IF (command(2:3) .EQ. 'FC') THEN                ! Print Flipper calibr.
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF 
	   GOTO 520
    	ELSE IF (command(1:4) .EQ. 'PFIX') THEN     ! NG7: Print fixed motors
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FDSA') THEN           ! Print fDSA FOCUS PARAM
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FDAD') THEN           ! Print fDAD FOCUS PARAM
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FAMP') THEN           ! Print famp values
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF 
	   GOTO 520
    	ELSE IF (command(2:3) .EQ. 'SA') THEN           ! Print software angles
 	   IF (isdigit(command(4:4))) THEN
	      READ (UNIT=command(4:4), FMT= '(i1)', ERR= 270) m
	   END IF
	   GOTO 520                            
	ELSE IF (command(2:3) .EQ. 'HA') THEN           ! Print hardware angles
 	   IF (isdigit(command(4:4))) THEN
	      READ (UNIT=command(4:4), FMT= '(i1)', ERR= 270) m
	   END IF
	   GOTO 520                            
	ELSE IF (command(2:3) .EQ. 'DM') THEN            ! Print mono d-spacing
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'DA') THEN             ! Print ana d-spacing
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'MP') THEN	       ! Print motor parameters
	   j=4
	   DO WHILE (isdigit(command(j:j)) .and. j .lt. iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(4:j-1), FMT= *, ERR= 270) m
	   m = ABS(m)
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) goto 270
	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(4:4) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Print is ok
	     GOTO 520
	   END IF
	ELSE IF (command(2:3) .EQ. 'TD') THEN               ! Print temp config
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'HD') THEN               ! Print mag config
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'TP') THEN               ! Print temp params
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'A') THEN	     ! Print soft & hard angles
  	   IF (isdigit(command(3:3))) THEN
	      READ (UNIT=command(3:3), FMT= '(i1)', ERR= 270) m
	   END IF
	   GOTO 520                            
	ELSE IF (command(2:2) .EQ. 'C') THEN                ! Print collimation
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'DEM') THEN  !Print pos for mono energy
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Drive is ok
	      j = 5
	      IF (command(j:j) .EQ. '=') j = 6
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	   END IF
	   GOTO 520 
	ELSE IF (command(2:4) .EQ. 'DEA') THEN    ! Print pos for ana energy
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE                                                   ! Drive is ok
	      j = 5
	      IF (command(j:j) .EQ. '=') j = 6
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	   END IF
	   GOTO 520
 	ELSE IF (command(2:2) .EQ. 'D') THEN                ! Print date & time
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'I' .OR. command(2:2) .EQ. 'Q' 
	1        .OR. (command(2:2) .EQ. 'B' .OR. command(2:2) .EQ. 'T' 
     1	     .AND. isdigit(command(3:3)))) THEN 
           j = INDEX (command, ';')                              ! Print buffer
	   IF ( j .EQ. 0 ) THEN            ! (* we have just one command here *)
	      local(1:) = command(1:)
           ELSE                         ! (* there is more to come after that *)
              local(1:j) = command(1:j-1)
	   END IF
	   j = INDEX (local, '-')
	   IF ( j .EQ. 0 ) THEN          	   ! (* print one buffer only *)
	      READ (UNIT= local(3:iend), FMT= *, ERR= 270) l        
	      m = l
	   ELSE                                   ! (* print range of buffers *)
	      READ (UNIT= local(3:j-1), FMT= *, ERR= 270) l        
	      READ (UNIT= local(j+1:iend),  FMT= *, ERR= 270) m
	   END IF
	   IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1      m .LT. 1) GOTO 270
	   str4(1:1) = local(2:2)
	   IF (str4(1:1) .EQ. 'I') THEN
	     GOTO 520
	   ELSE IF (str4(1:1) .EQ. 'Q') THEN
	      IF (nsta .EQ. 1) THEN
	         response = ' Sorry, no Q-buffers on BT-1'
	         CALL PUTLINE (contr, response)
		 GOTO 270
	      END IF
	      GOTO 520
	   ELSE IF (str4(1:1) .EQ. 'B') THEN
	      IF (nsta .EQ. 1) THEN
	         response = ' Sorry, no B-buffers on BT-1'
	         CALL PUTLINE (contr, response)
		 GOTO 270
	      END IF
	      GOTO 520
	   ELSE IF (str4(1:1) .EQ. 'T') THEN
	      IF (nsta .NE. 4) THEN
	         response = ' Sorry, no T-buffers on this instrument!'
	         CALL PUTLINE (contr, response)
		 GOTO 270
	      END IF
	      GOTO 520	
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN               ! Print lower limits
	   GOTO 520                            
	ELSE IF (command(2:2) .EQ. 'M') THEN               ! Print Mosaic
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'RSF') THEN             ! Print file sequence
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'RS') THEN                  ! Print sequence
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'T') THEN                ! Print Temperature
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'HF') THEN               ! Print Field
	   GOTO 520
 	ELSE IF (command(2:2) .EQ. 'U') THEN               ! Print upper limits
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'V') THEN	     ! Print default run number
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'W') THEN                 ! Print wavelength
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'Z') THEN                 ! Print zeropoints
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                   ! Polarized Beam 
	   IF (nsta .eq. 0) GOTO 270
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN                 ! Unpolarized beam
	   GOTO 520
	ELSE IF (command(1:3) .EQ. 'POL') THEN                ! Polfig
	   GOTO 520
	END IF
	GOTO 270


C--------------------------------- Q: QUIT -----------------------------------C
C  do not allow Q+ or Q- in command file for ng5 since need Health Physics survey
 170	GOTO 270 !error
	
C---------------------- R:  RI  RT  RQ RB  REL RM RA-ROCK-ROCKH--------------C

 180	IF (command(2:2) .EQ. 'I' .OR. command(2:2) .EQ. 'Q' .OR.
	1   command(2:2) .EQ. 'T' .OR. command(2:2) .EQ. 'B' .OR.
     1	    command(2:2) .EQ. 'R' .OR. command(2:2) .EQ. 'X') THEN
	   IF (command(3:5) .EQ. '/NF')THEN
	     command(1:) = command(1:2)//command(6:)  !remove /NF
	     iend = iend - 3
	   ENDIF
	   j = INDEX (command, ';')
	   IF ( j .EQ. 0 ) THEN		   ! (* only one command on this line *)
	      local(1:) = command (1:)
	   ELSE                            ! (* more than one command on line *)
	      local(1:) = command(1:j-1)
	   END IF
	   j = INDEX (local, '-')
	   IF ( j .EQ. 0 ) THEN                  ! (* execute one buffer only *)
	      READ (UNIT=local(3:iend), FMT= *, ERR= 270) l
	      m = l
	   ELSE                                 ! (* execute range of buffers *)
	      READ (UNIT=local(3:j-1), FMT= *, ERR= 270) l
	      READ (UNIT=local(j+1:iend),  FMT= *, ERR= 270) m
	   END IF
	   IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1      m .LT. 1) GOTO 270
	   str4(1:1) = local(2:2)
	      IF (str4(1:1) .EQ. 'I'.and. nsta .ne. -7) THEN
	         GOTO 520
	      ELSE IF (str4(1:1) .EQ. 'Q' .AND. nsta .NE. 1 .and.
     1	          nsta .ne. -7) THEN
	         GOTO 520
	      ELSE IF (str4(1:1) .EQ. 'B' .AND. nsta .NE. 1 .and. 
     1	          nsta .ne. -7) THEN
	         GOTO 520
	      ELSE IF (str4(1:1) .EQ. 'T' .AND. nsta .EQ. 4) THEN
	         GOTO 520
	      ELSE IF (str4(1:1) .EQ. 'R' .AND. nsta .EQ. -7) THEN
	         GOTO 520
	      ELSE IF (str4(1:1) .EQ. 'X' .AND. nsta .EQ. 8) THEN
	         GOTO 520
	      ELSE
		 GOTO 270
	      END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELM' .OR. command(2:4) .EQ. 'ELD') THEN
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR. nsta.eq.0) GOTO 270
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELT'.or.command(2:4).eq.'ELS') THEN   ! Release Table
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR.nsta.eq.0) GOTO 270
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELA')THEN
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR. nsta.eq.0) GOTO 270
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'EL') THEN
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) GOTO 270
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'A' .OR. command(2:2) .EQ. 'M') THEN !measure flipping ratio
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF 
	   IF (command(2:2) .EQ. 'M') THEN       !set mono flippers
	     m=8                                 ! initial state is OFF
	   ELSE					  !set anal flippers
	     IF (command(2:2) .NE. 'A') GOTO 270  
	     m=6				!initial state is OFF
	   ENDIF
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'OCKH') THEN                    ! Rock MAG
	   IF (command(6:7).eq.'/F')then !make file
	     m=8
	   ELSE
	     m=6
	   ENDIF
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .EQ. 0) THEN
	     GOTO 270 !error. must specify field at least.
	   ELSE
	     data   = multi(1)			  ! (1) center of field
	   ENDIF
           IF (np .GT. 1) THEN
	     range = multi(2)				  ! (2) is field range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 2) THEN
	     step = multi(3)				  ! (3) step width
	   ELSE
	     step = .1
	   ENDIF 
           IF (np .GT. 3) THEN
	     mon   = multi(4)				  ! (4) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
	   command(1:) = command (m:)
	   IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'OCK') THEN                        ! Rock
	   IF (command(5:6).eq.'/F')then !make file
	     m=7
	   ELSE
	     m=5
	   ENDIF
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .EQ. 0) THEN
	     GOTO 270 !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
	   IF (mot.lt.1 .or. mot.gt.MAXMOTS)goto 270
           IF (np .GT. 1) THEN
	     range = multi(2)				  ! (2) is angle range
	   ELSE
	     range = 1.
	   ENDIF
	   IF (np .GT. 2) THEN
	     step = multi(3)				  ! (3) step width
	   ELSE
	     step = .1
	   ENDIF 
           IF (np .GT. 3) THEN
	     mon   = multi(4)				  ! (4) is monitor
	   ELSE
	     mon = -1.
	   ENDIF
	   command(1:) = command (m:)
	   IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
	   IF (motfl(mot)) THEN
	     GOTO 520
	   ELSE
	      response = 'That motor is not available on this instrument!'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	END IF
	GOTO 270


C------------------ S: SETP STAT  SET  SM  ST  S-/+ SL------------------C

 190    IF (command(2:4) .EQ. 'ETP') THEN     ! Set temperature controller
	   READ (UNIT= command(5:5), FMT= '(i1)', ERR= 270) j
	   IF (j .EQ. 1 .OR. j .EQ. 2) THEN
	     GOTO 520
	   ELSE IF (j .EQ. 0) THEN
	      j = 6
	      DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
		 j = j + 1
	      END DO
	      READ (UNIT= Command(j:iend), FMT= *, ERR= 270) (multi(i), i=1,4)
	      params(1) = multi(1)
	      params(2) = multi(2)
	      params(3) = multi(3)
	      params(4) = multi(4)
	      DO i = 1, 3
		 m = INDEX (command(1:), ',')
	         command(1:) = command (m+1:)
	      END DO
	   ELSE 
	      GOTO 270
	   END IF
	   GOTO 520	      
	ELSE IF (command(2:4) .EQ. 'TAT') THEN           ! Print current status
 	   GOTO 520	
	ELSE IF (command(2:6) .EQ. 'ETDEM') THEN    !Change zeros for mono energy
	   IF (nsta.eq.8) GOTO 270 !error
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 270
	   ELSE   
	      j = 7
	      IF (command(j:j) .EQ. '=') j = 8
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, dm)
	      pos2 = 2. * pos1
	      z2 = fz(2,pos2) !get linear z for motor 2 on BT4
	      IF (.NOT. inlimits(2,pos2+z2,contr))THEN
		CALL Putcontr_NT(' Angle 2 would exceed limits.')
		GOTO 270
	      ELSE 
		IF (motfl(1)) THEN 
	         z1 = fz(1,pos1) !check for linear zero
	         IF (.NOT. inlimits(1,pos1+z1,contr))THEN
		    CALL Putcontr_NT ('Angle 1 would exceed limits.')
		    GOTO 270
	         ELSE
		    GOTO 520
		 END IF !motor 1 inlimits
	        ELSE !motor 1 not in use
		    GOTO 520
	        END IF !motor 1 in use
	      END IF !motor 2 inlimits
	   ENDIF !motfl(2)
	   GOTO 520 
	ELSE IF (command(2:6) .EQ. 'ETDEA') THEN   ! Change zeros for ana energy
	   IF (nsta.eq.8) GOTO 270
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 270
	   ELSE    
	      j = 7
	      IF (command(j:j) .EQ. '=') j = 8
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, da)
	      pos2 = 2. * pos1
	      IF (.NOT. inlimits(5,pos1+z(5),contr))THEN
	        CALL Putcontr_NT('Angle 5 would exceed limits.')
		GOTO 270
	      ELSE IF (.NOT. inlimits(6,pos2+z(6),contr))THEN
	        CALL Putcontr_NT('Angle 6 would exceed limits.')
		GOTO 270
	      ELSE
		GOTO 520
	      END IF
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'ET') THEN     ! Set motor m to angle f through zero
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   L=j
	   DO WHILE (isdigit(command(L:L)) .AND. L .LT. 50)
	     L = L+1
	   ENDDO
	   READ (UNIT= command(j:L-1), FMT= *, ERR=270) m
	   IF (m .le. 0 .or. m .gt. MAXMOTS)GOTO 270
 	   IF (.NOT. motfl(m)) THEN
	      response = 'Motor no. '// command(j:j) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   ELSE
	      j = L
	      DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	         j = j + 1
	      END DO
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'M') THEN                       ! Set magnet
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (.NOT. minit_done) THEN
	      response(1:) = '*** Magnet control not initialized. Use HDEV. ***'
	      CALL PUTLINE (contr, response)
	   ENDIF
	   IF (magtype.eq.0) THEN
	      response(1:) = '*** Mag type undefined. Use HDEV.***'
	      CALL PUTLINE (contr, response)
	   ELSEIF (magtype.eq.1 .and. tdev%mot .EQ. 0) THEN
	      response(1:) = 'There is no motor controller assigned!'
	      CALL PUTLINE (contr, response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'T') THEN                  ! Set temperature
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN               !NG7. Slits
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend) , FMT= *, ERR= 270) (multi(k), k= 1, 4)
	   IF (nsta.ne.-7)GOTO 270
	   DO k = 1,4 !do for each slit
	     m = slitlist(k)
	     IF (.NOT. motfl(m)) THEN
	       response = 'Motor no. '// command(2:j-1) //
     1		' is not served on this instrument'
	       CALL PUTLINE (contr, response)
	       GOTO 520
	     ELSE
	       data = multi(k)
	       pos1 = z(m) + data                              ! Normal drive
	       IF (.NOT. inlimits(m, pos1, contr))ig = 1 !empty action to do test
	     ENDIF !motfl
	   END DO !for each slit
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                ! Enable statistics
	   IF (PSD) THEN
	    CALL PUTLINE(contr, 
     1	     'S+ not implemented with position sensitive detector')
	    GOTO 270
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable statistics
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'F') THEN               ! NG7. Scalefactor
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (nsta .ne. -7) GOTO 270
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'AY') THEN            !Say
	   GOTO 520
	END IF
	GOTO 270


C------------------------------- T: TDEV  T-/+ -------------------------------C

 200	IF (command(2:4) .EQ. 'DEV') THEN                  ! Define temp device
	   GOTO 270
	ELSE IF (command(2:2) .EQ. '+') THEN             ! Temp control enabled
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN            ! Temp control disabled
	   GOTO 520      
	END IF
	GOTO 270


C---------------------------------- U: Ui ------------------------------------C

 210	IF (command(3:3) .EQ. '='.or. command(4:4) .EQ. '=') THEN  ! Change upper limits
	   L=2
	   DO WHILE (isdigit(command(L:L)).and.l.lt.iend)
	     L=L+1
	   ENDDO
	   READ (UNIT= command(2:L-1), FMT= *, ERR= 270) j ! determ angle no
	   IF (j .lt. 1 .or. j .gt. MAXMOTS) GOTO 270
	   IF (.NOT. motfl(j)) THEN
	      response = 'Motor no. '// command(2:2) //
     1		' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	   READ (UNIT= command(4:iend), FMT= *, ERR= 270) data
	   GOTO 520
	END IF
	GOTO 270


C----------------------------- V: VER VERF VSET ------------------------------C

 220	IF (command(2:4) .EQ. 'ERF') THEN
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'ER') THEN
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'LAT') THEN !vlat variable
	   j = 5
	   IF (command(j:j) .EQ. '=') j = j+1
	   IF (command(j:j) .EQ. 'A') THEN
	     GOTO 520
	   ELSEIF (command(j:j) .EQ. 'B') THEN
	     GOTO 520
	   ELSEIF (command(j:j) .EQ. 'C') THEN
	     GOTO 520
	   ELSE
	     GOTO 270
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'SET') THEN              ! Vset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
	   IF (polfl(mot)) THEN
	     GOTO 520
	   ELSE
	      response(1:) = 
     1	      'That power supply is not enabled on this instrument!'
	     CALL PUTLINE (contr, response)
	     GOTO 270
	   END IF
	   GOTO 520
  	ELSEIF (command(2:2) .EQ. '+' .OR. command(2:2) .eq. '-') THEN
	  GOTO 520
	END IF
	GOTO 270


C---------------------------------- W: WL ------------------------------------C

 230	IF (command(2:2) .EQ. 'L') THEN                            ! Wavelength
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   GOTO 520
	END IF
	GOTO 270


C---------------------------------- X: XDT -----------------------------------C

 240	IF (command(2:3) .EQ. 'DT') GOTO 270
	GOTO 270

C---------------------------------- Z: Zi ------------------------------------C

 260	IF (command(3:3) .EQ. '='.or. command(4:4) .eq. '=') THEN ! Change zeropoints
	   j=2
	   DO WHILE (isdigit(command(j:j)) .and. j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(2:j-1), FMT= *, ERR= 270) j ! determ angle no
	   IF (j .lt. 1 .or. j .gt. MAXMOTS) goto 270
	   IF (.NOT. motfl(j)) THEN
	      response = 'Motor no. '// command(2:j-1) //
     1	      ' is not served on this instrument'
	      CALL PUTLINE (contr, response)
	      GOTO 270
	   END IF
	   READ (UNIT= command(4:iend), FMT= *, ERR= 270) data
	   GOTO 520
	ELSE IF (command(2:2) .EQ. ',') THEN		       ! Call caculator
	   GOTO 270
	END IF
	GOTO 270


 999	CONTINUE
 270	response(1:) = ' Unrecognized command: ' // command(1:50)
	CALL PUTLINE (contr, response)
	RETURN 1
	END

