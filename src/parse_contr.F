#include "icp.h"
	SUBROUTINE Parse_Contr (command, *)
C	***********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Checks Spelling And Syntax Of The Given Commands. If Ok,        C
C     Executes  Command And Returns Control To Statement Label * In The       C
C     Calling Program                                                         C
C     Called from: Control, Sequence                                          C
C                                                                             C
C     Beat Schmid, July 1989                                                  C
C     Modified by Gayle Greene, starting April 1991                           C
C					 (adding new commands)                C
C	                        July 1992 to allow semicolon to separate      C
C				           multiple commands                  C
C     Modified by Nick Maliszewskyj - October 2011                            C
C                                     FORTRAN95/gfortran
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Note: There is a temporary command SC and temporary variables: sc_command,
C   sc_reply, lreply,lcom in this version

	use instr
	use smg_prep
	use polar
	use tc
	use histo
	use focus
	use motors
	use virtmot
	use zcoeffs
	use asd
	use lia
	use tdb
	use globals
	use refl
	use xray
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
#include "c_vme.h"
#include "c_debugval.h"
#include "c_gpiben.h"
#include "c_center.h"
#include "c_ez.h"
	   subroutine ICOIL(itop,ibot,err) bind(C,name="ICOIL")
	   use iso_c_binding
	   real(c_float), intent(in)  :: itop
	   real(c_float), intent(in)  :: ibot
	   integer(c_int), intent(out):: err
	   end subroutine ICOIL
	end interface
	BYTE	      ibit(0:15,0:3) /64*0/
	INTEGER*4     i, j, k, l, m, inc, counts(37), maxtry,ierr
	INTEGER*4     ios, np, type, ig, i1, i2,kpsd,hsum,ee,notries/10/,
     1			status,openfai/30/ !$fordef
	INTEGER*4     mot, nopoints(0:15), ibeg, iend, lastchar,lpass,which

	REAL*4        time, preset, data, rate, pos1, pos2, RdAngl
	REAL*4        multi(6), st, ct, sp, range, step, mon, Energy_Angle
	REAL*4        foc7, foc8, params(6), start
	REAL*4	      toler, etrans, bt6motor2,z2,z1,Fz,zm
	LOGICAL       isdigit, rderr, keep, lresp, move, dryrun, hldasd, montemp,
     1			inlimits, fitmin, fitinten, errflag,arch,Readef
	CHARACTER*2   str2
	CHARACTER*4   str4
	CHARACTER*5   str5
	CHARACTER*6   str6(4), curveid(0:15)
	CHARACTER*7   st7(3), PutStr
	CHARACTER*9   str9
	CHARACTER*13  str13
	CHARACTER*14  str14
	CHARACTER*20  cryo, sensorid, standard, datum
	CHARACTER*28  sys_time
	CHARACTER*50  command, local, topic
	CHARACTER*80  help_line(3), response, response_2, outpsd,passw
	CHARACTER*240 longl
	CHARACTER*320 seq
	CHARACTER*80  sc_command,sc_reply !These are only temporary for SC
	CHARACTER*20  lkcommand
	CHARACTER*35  lkreply
	INTEGER*2     lreply,lcom 
	LOGICAL       append
	LOGICAL	      Password_OK ! external function
	CHARACTER*80  GETHOME

c	RECORD /lattice/ lat

c	COMMON /drive_to_q/   lat, hkl_1, hkl_2
	TYPE(lattice) lat


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
	IF (j .EQ. 45 .OR. (j .GE. 48 .AND. j .LE. 57)) 
	1  CALL Calcul (command, *520)                                 ! number

	j = j - 64                                  ! 'A'...'Z'  <==>  [1...26]
	IF (j .LT. 1 .OR. j .GT. 26) GOTO 270

	GOTO (10,  20,  30,  40,  50,  60, 999,  80,  90, 999,
	1     999, 120, 130, 140, 150, 160, 170, 180, 190, 200,
	2     210, 220, 230, 240, 250, 260) j

C---------------------------------- A: ALIGN, A+/- ----------------------------C

 10	IF (command(2:2) .EQ. 'P' .OR. command(2:2) .EQ. 'M' .OR. !align peak
     1	    command(2:2) .EQ. 'S' .OR. command(2:2) .EQ. 'A') THEN 
	  IF (.NOT. priv) CALL NoPriv (*520)
	  lastchar = iend
	  CALL Parse_Align(command(1:iend),errflag,lastchar)
	  IF (errflag)THEN
	    GOTO 270
	  ELSE
	    command(1:) = command(lastchar:)
	    GOTO 520
	  ENDIF
	ELSE IF (command(2:2) .EQ. '+') THEN    ! Enable ASD or autolim
	   IF (.NOT. priv .and. nsta .eq. -5) CALL NoPriv (*520)
	   IF (nsta.EQ.-5)THEN
	     CALL ENET_INIT
	     statistic = .FALSE.
	     use_asd=.true.
	     keep_rawasd=.FALSE.
	   ELSEIF (nsta.EQ.-11)THEN
	     statistic = .FALSE.
	     use_asd=.true.
	   ELSEIF (nsta.EQ.1)THEN
	     autolim = .TRUE.
	   ELSE !not bt1 or ng5
	     CALL Not_Implemented (*520)
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '*') THEN    ! Enable ASD: keep raw data
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.EQ.-5)THEN
	     CALL ENET_INIT
	   ENDIF
	   use_asd=.true.
	   keep_rawasd=.TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable ASD/autolim
	   IF (.NOT. priv .and. nsta .eq. -5) CALL NoPriv (*520)
	   IF (nsta .EQ. -5) THEN
	     use_asd = .FALSE.
	     keep_rawasd=.FALSE.
	   ELSEIF (nsta.EQ.-11)THEN
	     use_asd = .FALSE.
	   ELSEIF (nsta.EQ.1)THEN
	     autolim = .FALSE.
	   ELSE !not bt1 or ng5
	     CALL Not_Implemented (*520)
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'SDSI') THEN ! Change size of ASD histogram
	   
	END IF
	GOTO 270

C---------------------------------- B: BUFOP BYE -----------------------------C

  20	IF (command(2:5) .EQ. 'UFOP') THEN		    ! Buffer operation
	  IF (command(6:6) .EQ. 'I' )THEN		    ! Ibuffer
	    type = 1
	  ELSE IF (command(6:6) .EQ. 'Q') THEN
	    type = 2					    ! Qbuffer	
	  ELSE IF (command(6:6) .EQ. 'T') THEN
	    type = 4					    ! Tbuffer	
	  ELSE IF (command(6:6) .EQ. 'B') THEN
	    type = 5					    ! Bbuffer	
	  ELSE IF (command(6:6) .EQ. 'R') THEN
	    type = 6					    ! Rbuffer	
	  ELSE
	    GOTO 270					    ! Unrecognized buftype
	  ENDIF
	  CALL Bufop (command,iend,type)
	  command(1:)=command(5:)
	  GOTO 520
	ELSE IF (command(2:3) .EQ. 'YE') THEN		    ! Exit the program
	   CALL Goodbye
	ELSE IF (command(2:5) .EQ. 'ITTE') THEN
	   priv = .TRUE.
	   GOTO 520
	END IF
	GOTO 270



C------------------------------ C:  COL CT  CO CS-----------------------------C

  30	IF (command(2:3) .EQ. 'OL') THEN                          ! Collimation
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   ios = 1
	   np = 4
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
	     READ (UNIT= command(j:iend),FMT=*,IOSTAT= ios) (multi(k),k= 1,np)
	     IF ( ios .NE. 0) np = np - 1
	   ENDDO
	   IF (np .LT. 1) GOTO 270 !error. must specify at least one param
	   IF (np .lt. 4 .and. nsta .ne. 1)goto 270 !error. 4 params for all but bt1
	   DO j= 1, 3 !shift command string for reading next command
	      i = INDEX (command, ',')
	      command(1:) = command(i+1:)      
	   END DO
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.eq.1 .AND. (ifix(multi(1)) .ne. 7 .and.
	1	ifix(multi(1)) .ne. 15 .and.
	1	ifix(multi(1)) .ne. 60)) THEN
	      response=' Collim on BT1 must be 7,15, or 60!'
	     CALL Putline_NT (contr,response)
	     GOTO 520
	   ENDIF !invalid value on BT1
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1        ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4),  !read current values
     1	      (mosaic(i), i= 1,3), wavel, dm, da, monobt1 
	   DO i= 1,np  !change collimation value(s)
	      collim(i) = IFIX (multi(i))
	   END DO
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),  !save new collim
     1	     (mosaic(i), i= 1,3), wavel, dm, da,monobt1
	   CLOSE (1)
	   IF (nsta .eq. 1)THEN
	     str5 = monobt1
	     IF (collim(1).eq.7)THEN
	         str13(1:)=str5//'_7'//'.DAT'
	     ELSEIF (collim(1).eq.15)THEN
	         str13(1:)=str5//'_15'//'.DAT'
             ELSEIF (collim(1).eq.60)THEN
                 str13(1:)=str5//'_60'//'.DAT'
	     ENDIF
	     OPEN (UNIT= 1, FILE= HOMEDIR//str13, STATUS= 'OLD', ACCESS= 
	1        'SEQUENTIAL', FORM= 'FORMATTED', ERR= 270)
	       READ (1, *) wavel, (eff(i), i= 1,scl), (zp(i), i= 1, scl) !overwrite wavel
	     CLOSE(1)
	     OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1        ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	     WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),  !memorize new wavel
     1	        (mosaic(i), i= 1,3), wavel, dm, da, monobt1 
	     CLOSE (1)
	     IF (system_type .eq. DEC) THEN
	       help_line(1) = ' Type HELP for commands '
	     ELSE
	       help_line(1) = ' See online icp manual for commands '
	     ENDIF
	     write(unit=str4(1:4),fmt='(I4)',iostat=ios)collim(1)
	     help_line(2)=' Collimation: '//str4//'  MONO='//monobt1
	     help_line(3)=' Use MONO= command to change MONO.'
	     CALL Put_Help (help_line)
	   ENDIF !BT1
	   GOTO 520
	ELSE IF (command(2:7) .EQ. 'ONFIRM') THEN !confirm
 32	   CALL Read_STRING(keyb, str9,
     1		'Type TRUE when ready to continue, FALSE to abort: ',
     1		9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     IF (ig. eq. 0)goto 32
	     READ(unit=str9(1:ig),fmt='(L1)',err=32)lresp
	     IF (.not. lresp)THEN
		CALL SETEF(contr_a)
		CALL IcpAbort(command, *520)
	     ENDIF
	  GOTO 520
	ELSE IF (command(2:2) .EQ. 'O' .OR. command(2:2) .EQ. 'T') THEN !ct
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (.NOT. priv) CALL NoPriv (*520)
	   CALL CLREF (contr_A)
	   IF (nsta .eq. -5 .and. use_asd) THEN
	     call read_asdfile
	     use_asdfile= .TRUE.
	   ENDIF
	   IF (data .LT. 0) THEN                              ! Count with time
	      IF (nsta .EQ. -5 .AND. use_asd) CALL zero_asd
	      time = ABS(data)
	      WRITE (UNIT=st7(1), FMT= '(i7)',iostat=ios) NINT(time)
	      response = 'Timer set to: '// st7(1) //' sec.   Count initiated!'
	      CALL Reset_Display       !Reset NIM count display
	      CALL RlyOff (4)                           ! Start display counter
	      IF (nsta .EQ. -5 .AND. use_asd) CALL asd_timed(1,time)
	      CALL CnTime (time)
	      CALL Putline_NT (contr, response)
	      CALL Scaler (counts)
	      IF ((nsta .eq. -5 .and. use_asd).or.(nsta.eq.-1.and.psd)
	1	   .or.(nsta.eq.-11.and.use_asd))
	1	   counts(scl+1)=NINT(fpwindow)
	      CALL RlyOn (4)			         ! Stop display counter
	      IF (READEF(contr_A))CALL IcpAbort(command, *520)
	      IF (nsta .EQ. 1) THEN
	         WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts(1)
	         response = 'Time: '// st7(1) //'      Monitor: '// str9
	         CALL Putline_NT (contr, response)
	         IF (scl .LE. 12) THEN
	           WRITE (UNIT= longl, FMT='(6x,12i6)',iostat=ios) 
	1	       (counts(i), i= 2, scl+1)
	           CALL Putline_NT (contr, longl)
		 ELSE !more than 12 scalers
		    DO ig = 1,(scl-1)/12 + 1
		      i1 = (ig-1)*12 + 2
		      i2 = i1 + 11
		      if (i2 .GT. scl+1) i2 = scl + 1
  	              WRITE(UNIT= longl, FMT='(6x,12i6)',iostat=ios) (counts(i), i= i1,i2)
	              CALL Putline_NT (contr, longl)
		    ENDDO
		 ENDIF
	      ELSE	!nsta .ne. 1
	         WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (1)
	         response = 'Time: '// st7(1) //'   Monitor: '// str9
	         WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (2)
	         response = response(1:34) //' Counter_1: '// str9
		 IF (scl .gt. 1) THEN
		    WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (3)
		    response = response(1:55) //' Counter_2: '// str9
		 ELSE
		    WRITE (UNIT= str9, FMT= '(f9.2)',iostat=ios) duration
		    response = response(1:55) // ' Seconds: '// str9
		 ENDIF
	         CALL Putline_NT (contr, response)
	      IF (PSD) THEN !write position sensitive detector data
	        outpsd(1:)=' '
	        kpsd = 2
	        DO i = 1, nocells/16 !groups of 16 cells
	          hsum=0
	          DO j = 1, 16 !sum adjacent 16 cells
	            hsum=hsum+(histodata((i-1)*16+j))
	          ENDDO
	          ee = 1
	          DO WHILE (hsum .GE. 10**ee)
	            ee = ee + 1
	          END DO
	          m = kpsd + ee-1	    	     
	          IF (m .GE. 80) THEN                        ! write string to screen
	            CALL Putline_NT (contr, outpsd(1:80))
c		    WRITE (2, '(a80)',iostat=ios) outpsd(1:80)
	            kpsd = 2
		    m = 2 + ee-1
	          END IF
	          IF (kpsd .LE. 2) outpsd(1:) = ' '
	          WRITE (UNIT=datum, FMT= '(i20)',iostat=ios) hsum
		  outpsd(kpsd:m) = datum(21-ee:20)
	          outpsd(m+1:m+1) = ','
	          kpsd = m + 2
	        END DO
c	        IF (m.gt.2)write(2,fmt='(a)',iostat=ios)outpsd(1:m)	
	        IF (m.gt.2)CALL Putline_NT (contr, outpsd(1:m))
	      ENDIF !PSD
	      	IF (nsta .eq. -5 .AND. USE_ASD) THEN !write area sensitive detector data
	          outpsd(1:)=' '
	          kpsd = 2
	          DO i = 1, noslices
	            ee = 1
		    hsum = asd_data(i)
	            DO WHILE (hsum .GE. 10**ee)
	              ee = ee + 1
	            END DO
	            m = kpsd + ee-1	    	     
	            IF (m .GE. 80) THEN                        ! write string to screen
	              CALL Putline_NT (contr, outpsd(1:80))
	              kpsd = 2
		      m = 2 + ee-1
	            END IF
	            IF (kpsd .LE. 2) outpsd(1:) = ' '
	            WRITE (UNIT= datum, FMT= '(i20)',iostat=ios) hsum
		    outpsd(kpsd:m) = datum(21-ee:20)
	            outpsd(m+1:m+1) = ','
	            kpsd = m + 2
	          END DO
	          IF (m.gt.2)CALL Putline_NT (contr, outpsd(1:m))
	      ENDIF !ASD
	      END IF   !not nsta eq 1
	   ELSE                                            ! Count with monitor
	      IF (nsta .eq. 0) CALL NoMon (*520)
	      IF (nsta .EQ. -5 .AND. use_asd) CALL zero_asd
	      preset = data
	      WRITE (UNIT= str13(1:), FMT= '(i9)',iostat=ios) NINT(preset)
	      response = 'Monitor set to: '// str13 //'   Count initiated!'
	      CALL Reset_Display       !Reset NIM count display
	      CALL RlyOff (4)                           ! Start display counter
	      IF (nsta .EQ. -5 .AND. use_asd) CALL asd_on(1)
	      CALL Count (preset)
	      CALL Putline_NT (contr, response)
	      CALL Scaler (counts)
	      IF ((nsta .eq. -5 .and. use_asd).or.(nsta.eq.-1.and.psd)
	1	   .or.(nsta.eq.-11.and.use_asd))
	1	   counts(scl+1)=NINT(fpwindow)
	      CALL RlyOn (4)			         ! Stop display counter
	      IF (READEF(contr_a))CALL IcpAbort(command, *520)
	      response = 'Monitor: '// str13
	      IF (nsta .EQ. 1) THEN
	         CALL Putline_NT (contr, response)
	         IF (scl .LE. 12) THEN
	           WRITE (UNIT= longl, FMT='(6x,12i6)',iostat=ios) 
	1	       (counts(i), i= 2, scl+1)
	           CALL Putline_NT (contr, longl)
		 ELSE !more than 12 scalers
		    DO ig = 1,(scl-1)/12 + 1
		      i1 = (ig-1)*12 + 2
		      i2 = i1 + 11
		      if (i2 .GT. scl + 1) i2 = scl + 1
  	              WRITE(UNIT= longl, FMT='(6x,12i6)',iostat=ios)
	1		   (counts(i), i= i1,i2)
	              CALL Putline_NT (contr, longl)
		    ENDDO
		 ENDIF
	      ELSE
	         WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (2)
	         response = response(1:23) // '   Counter_1: ' // str9
		 IF (scl .gt. 1) THEN
		    WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (3)
		    response = response(1:46) // '   Counter_2: ' // str9
		 ELSE
		    WRITE (UNIT= str9, FMT= '(f9.2)',iostat=ios) duration
		    response = response(1:46) // '   Seconds: '// str9
		 ENDIF
  	         CALL Putline_NT (contr, response)
	      IF (PSD) THEN !write position sensitive detector data
	        outpsd(1:)=' '
	        kpsd = 2
	        DO i = 1, nocells/16 !groups of 16 cells
	          hsum=0
	          DO j = 1, 16 !sum adjacent 16 cells
	            hsum=hsum+(histodata((i-1)*16+j))
	          ENDDO
	          ee = 1
	          DO WHILE (hsum .GE. 10**ee)
	            ee = ee + 1
	          END DO
	          m = kpsd + ee-1	    	     
	          IF (m .GE. 80) THEN                        ! write string to screen
	            CALL Putline_NT (contr, outpsd(1:80))
c		    WRITE (2, '(a80)',iostat=ios) outpsd(1:80)
	            kpsd = 2
		    m = 2 + ee-1
	          END IF
	          IF (kpsd .LE. 2) outpsd(1:) = ' '
#if defined(VMS)
	          WRITE (UNIT= outpsd(kpsd:m), FMT= '(i<ee>)',iostat=ios) hsum 
#else
	          WRITE (UNIT= datum, FMT= '(i20)',iostat=ios) hsum
		  outpsd(kpsd:m) = datum(21-ee:20)
#endif
	          outpsd(m+1:m+1) = ','
	          kpsd = m + 2
	        END DO
c	        IF (m.gt.2)write(2,fmt='(a)',iostat=ios)outpsd(1:m)	
	        IF (m.gt.2)CALL Putline_NT (contr, outpsd(1:m))
	      END IF !PSD
	      	IF (nsta .eq. -5 .AND. USE_ASD) THEN !write area sensitive detector data
	          outpsd(1:)=' '
	          kpsd = 2
	          DO i = 1, noslices
	            ee = 1
		    hsum = asd_data(i)
	            DO WHILE (hsum .GE. 10**ee)
	              ee = ee + 1
	            END DO
	            m = kpsd + ee-1	    	     
	            IF (m .GE. 80) THEN                        ! write string to screen
	              CALL Putline_NT (contr, outpsd(1:80))
	              kpsd = 2
		      m = 2 + ee-1
	            END IF
	            IF (kpsd .LE. 2) outpsd(1:) = ' '
#if defined(VMS)
	            WRITE (UNIT= outpsd(kpsd:m), FMT= '(i<ee>)',iostat=ios) hsum 
#else
	            WRITE (UNIT= datum, FMT= '(i20)',iostat=ios) hsum
		    outpsd(kpsd:m) = datum(21-ee:20)
#endif
	            outpsd(m+1:m+1) = ','
	            kpsd = m + 2
	          END DO
	          IF (m.gt.2)CALL Putline_NT (contr, outpsd(1:m))
	        ENDIF !ASD
	     ENDIF !not bt1
	   END IF !counting on monitor or time
	   GOTO 520
	ELSEIF (command(2:2) .EQ. 'S') THEN !close shutter on Xray
	  IF (nsta.ne.0) CALL Not_Implemented (*520)
	  IF (.NOT. priv) CALL NoPriv (*520)
	  ! DO NOTHING
	  GOTO 520
        ELSEIF (command(2:2) .EQ. 'E') THEN ! center
	   ! Syntax:
	   !      CENTER ZERO  (move upper translation)
	   !      CENTER -90   (move lower translation)
	   !      CENTER RESET (move sample translations to zero)
	   IF (nsta.ne.1) CALL Not_Implemented (*520)
	   ! Easter Egg
	   IF (command(2:5) .EQ. 'ENTR') THEN
	      response(1:)='This is America. The correct spelling is "CENTER"'
	      CALL Putline_NT(contr,response)
	      GOTO 520
	   ENDIF
	   WHICH = 2 ! Zero
	   ! Shocker! The ICP parser removes spaces!
	   j=7
 37	   IF (command(j:j).EQ.'Z') THEN
	      WHICH=2
	   ELSE IF (command(j:j) .EQ. '0') THEN
	      WHICH=2
	   ELSE IF (command(j:j+2).EQ.'-90') THEN
	      WHICH=1
	   ELSE IF (command(j:j) .EQ.'R') THEN
	      WHICH=0
	   ELSE IF (command(j:j) .EQ. '=') THEN
	      j=j+1
	      GOTO 37
	   ELSE
	      response(1:)='Valid options: ZERO, 0, -90, RESET'
	      CALL Putline_NT(contr,response)	      
	      GOTO 520
	   ENDIF
	   IF (.NOT. priv) CALL NoPriv (*520)
	   ! Check rotation
	   !m = 3
	   !pos1 = RdAngl(m)
	   !start = pos1 - zm(m,pos1)

	   CALL CENTER(WHICH)
	   IF (WHICH .eq. 1) THEN
	      response(1:)='Please return sample rotation to zero.'
	      CALL Putline_NT(contr,response)
	   ENDIF	   
	   GOTO 520
	END IF
	GOTO 270


C--------- D: DM DA DQ Dm DmI DmT DEA DEM DELRS DIRQ DIRI DIRT DIRB-DS---C
C-----------------DEFA(focus)-------------------------------------------------C

  40	IF (command(2:2) .EQ. 'M') THEN                  ! Monochr spacing
C CHANGE ME HERE
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. -1 .or. nsta .eq. -7) THEN
	      IF (.NOT.Password_OK())CALL Wrongpass (*520)
	   ENDIF
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   IF (isdigit(command(j:j))) THEN                    ! numerical value
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   ELSE					           ! alphanum reference
	      IF (command(j:j+1) .EQ. 'PG') THEN
	        IF (command(j+2:j+2) .eq. '2')then
		 data=1.67708 !pg2
	        else
		 data = 3.35416 !pg
	        endif
	      ELSE IF (command(j:j+1) .EQ. 'CU') THEN
		 data = 1.2780
	      ELSE IF (command(j:j+1) .EQ. 'HS') THEN
		 data = 3.445
	      ELSE IF (command(j:j+1) .EQ. 'GE') THEN     ! Ge111
		 data = 1.70576 
	      ELSE
		 GOTO 270
	      END IF
	   END IF
	   WRITE (UNIT= str13(1:10), FMT= '(f10.4)',iostat=ios) data
	   response(1:)= 'New monochromator d-spacing: '// str13(1:10)// CHAR(197)
	   CALL Putline_NT (contr, response)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1	 	(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   dm = data
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1	     (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
c	   CALL Get_hwtable
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'A') THEN                 ! Analyzer spacing
	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   IF (isdigit(command(j:j))) THEN                    ! numerical value
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   ELSE					           ! alphanum reference
	      IF (command(j:j+1) .EQ. 'PG') THEN
	        IF (command(j+2:j+2) .eq. '2')then
		 data=1.67708 !pg2
	        else
		 data = 3.35416 !pg
	        endif
	      ELSE IF (command(j:j+1) .EQ. 'CU') THEN
		 data = 1.2780
	      ELSE IF (command(j:j+1) .EQ. 'HS') THEN
		 data = 3.445
	      ELSE
		 GOTO 270
	      END IF
	   END IF
	   WRITE (UNIT= str13(1:10), FMT= '(f10.4)',iostat=ios) data
	   response(1:)= 'New analyzer d-spacing: '// str13(1:10)// CHAR(197)
	   CALL Putline_NT (contr, response)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1	     (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   da = data
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1		(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'EBUG') THEN
	   j=6
	   IF (command(j:j) .EQ. '=') THEN
	      j=j+1
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) i
#if !defined(VMS)
	      CALL DEBUGLEVEL(i)
#endif
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'ELRS') THEN                        ! del RS
	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1        FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	   seq(1:) = '# '
	   WRITE (1,REC=NBUF+1) seq
	   CLOSE (1)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'EFA') THEN    ! Drive focus motors to focus energy
	   m=5
	   ios = 1
	   np = 2
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
	     READ (UNIT= command(m:iend),FMT=*,IOSTAT= ios) (multi(k),k= 1,np)
	     IF ( ios .NE. 0) np = np - 1
	   ENDDO
	   IF (np .LT. 1) GOTO 270 !error. must specify at least one param
	   IF (np .LT. 2) multi(2) = 0. 
	   command(1:) = command (m:)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.-5) Call Not_Implemented (*520)
	   IF (.not. focus5)THEN
	     call Putline_NT(contr,' Set F+')
	     GOTO 520
	   ENDIF
	   CALL CLREF (Contr_A)
	   CALL DEFA (multi(1),multi(2)) 
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'Q') THEN                       ! Drive to Q	
	 IF (nsta.eq.8 .or. nsta .eq. 1) THEN
	   Call Not_Implemented (*520)
	 ELSEIF (nsta .eq. -7) THEN !refl
	   ios = 1
	   np = 2
	   m = 3
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
	     READ (UNIT= command(m:iend),FMT=*,IOSTAT= ios) (multi(i),i= 1,np)
	     IF ( ios .NE. 0) np = np - 1
	   ENDDO
	   IF (np .LT. 1) GOTO 270 !error. must specify qz
	   IF (np .LT. 2) multi(2)=0.0
	   command(1:) = command (3:)
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   CALL CLREF (Contr_A)
	   CALL DRIVE_QREFL(multi(1),multi(2))
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	 ELSE !not refl
	   ios = 1
	   np = 4
	   m = 3
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0)
	     READ (UNIT= command(m:iend),FMT=*,IOSTAT= ios) (multi(i),i= 1,np)
	     IF ( ios .NE. 0) np = np - 1
	   ENDDO
	   IF (np .LT. 3) GOTO 270 !error. must specify qx,qy,qz
	   IF (np .GE. 4) THEN
	     etrans = multi(4)
	   ELSE
	     etrans = 0.
	   ENDIF
	   command(1:) = command (m:)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   CALL CLREF (Contr_A)
	   CALL Drive_Q (multi,etrans) 
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	 ENDIF !refl
	 GOTO 520
	ELSE IF (command(2:2) .EQ. 'R') THEN                          ! Dry Run
	   j = INDEX (command, ';')
	   IF (command(3:3) .EQ. 'J') THEN
	      append=.false.
	      m = 4
	      DO WHILE (command(m:m) .EQ. '/')
		 IF (command(m+1:m+1).eq.'A')then !append file
		    append=.TRUE.
		    m = m+2
		 ENDIF
	      ENDDO
	      do while ((command(m:m).eq.'=').or.(command(m:m).eq.' '))
		 m = m+1
	      enddo
	      if (m .ge. iend) goto 520
	      keep=.true.
	      CALL RUN_TRJ(command(m:iend),.true.,keep)
	      GOTO 520
	   END IF

	   IF ( j .EQ. 0 ) THEN		   ! (* only one command on this line *)
	      local(1:) = command (1:)
	   ELSE                            ! (* more than one command on line *)
	      local(1:) = command(1:j-1)
	   END IF
 	   IF (command(4:5).eq.'/M')then !move motors
	     IF (.NOT. priv) CALL NoPriv (*520)
	     move=.true.
	     k=6
	   ELSE
	     move=.false.
	     k=4
	   ENDIF
	   j = INDEX (local, '-')
	   IF ( j .EQ. 0 ) THEN                  ! (* execute one buffer only *)
	      READ (UNIT=local(k:iend), FMT= *, ERR= 270) l
	      m = l
	   ELSE                                 ! (* execute range of buffers *)
	      READ (UNIT=local(k:j-1), FMT= *, ERR= 270) l
	      READ (UNIT=local(j+1:iend),  FMT= *, ERR= 270) m
	   END IF
	   IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1      m .LT. 1) GOTO 270
	   str4(1:1) = local(3:3)
	   inc = 1
	   IF (l .GT. m) inc = -1

	   DO j = l, m, inc
	      IF (str4(1:1) .EQ. 'I'.and. nsta .ne. -7 .AND.
     1		   nsta .NE. 8) THEN
	         CALL Dry_I_Buf (j)
	      ELSE IF (str4(1:1) .EQ. 'Q' .AND. nsta .NE. 1
     1	         .and. nsta .ne. -7 .AND. nsta .NE. 8) THEN
	         CALL Dry_Q_Buf (j)
	      ELSE IF (str4(1:1) .EQ. 'B' .AND. nsta .NE. 1
     1	         .and. nsta .ne. -7 .AND. nsta .NE. 8) THEN
	         CALL Dry_B_Buf (j)
	      ELSE IF (str4(1:1) .EQ. 'T' .AND. nsta .EQ. 4) THEN
	         CALL DRY_T_BUF (j)
	      ELSE IF (str4(1:1) .EQ. 'X' .AND. nsta .EQ. 8) THEN
	        IF (move)THEN
	          dryrun = .TRUE.
	          CALL CLREF (contr_A)
	          CALL Run_X_Buf (j,dryrun,keep)
	     	  IF (system_type .eq. DEC) THEN
	            help_line(1) = ' Type HELP for commands '
	     	  ELSE
	       	    help_line(1) = ' See online icp manual for commands '
	    	  ENDIF
	          help_line(2) = ' '
	          help_line(3) = ' '
	          CALL Put_Help (help_line)
	          IF (READEF (contr_A)) Call IcpAbort(command, *520)
	        ELSE
	          CALL DRY_X_BUF (j)
	        ENDIF
	      ELSE IF (str4(1:1) .EQ. 'D' .AND. nsta .EQ. 8.or.nsta.EQ.-5.or.nsta.eq.9.or.nsta.eq.-11) THEN
	        IF (move)THEN
	          dryrun = .TRUE.
	          CALL CLREF (contr_A)
	          CALL Run_D_Buf (j,dryrun,keep)
	     	  IF (system_type .eq. DEC) THEN
	            help_line(1) = ' Type HELP for commands '
	     	  ELSE
	       	    help_line(1) = ' See online icp manual for commands '
	    	  ENDIF
	          help_line(2) = ' '
	          help_line(3) = ' '
	          CALL Put_Help (help_line)
	          IF (READEF (contr_A)) Call IcpAbort(command, *520)
	        ELSE
	          CALL DRY_D_BUF (j)
	        ENDIF
	      ELSE IF (str4(1:1) .EQ. 'R' .AND. nsta .EQ. -7) THEN
	         CALL DRY_R_BUF (j)
	! ARBITRARY
	      ELSE
	         response(1:)  = ' Sorry, this is an invalid command!'
	         CALL Putline_NT (contr, response)
	         help_line(1) = ' The requested buffer does not exist on this instrument!'
	         IF (system_type .eq. DEC) THEN
	           help_line(2) = ' Type HELP for commands '
	         ELSE
	           help_line(2) = ' See online icp manual for commands '
	         ENDIF
	         help_line(3) = ' '
	         CALL Put_Help (help_line)
		 GOTO 520
	      END IF
	   END DO                                                             
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'IR') THEN                  ! Dir of buffers
	   CALL Parse_Dir(command(4:4))
	   command(1:) = command(4:)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'EM') THEN            ! Drive to mono energy
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.eq.8 .or. nsta .eq. 1) Call Not_Implemented (*520) 
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE                                                   ! Drive is ok
	      j = 4
	      IF (command(j:j) .EQ. '=') j = 5
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, dm)
	      pos2 = 2. * pos1
	      z2 = fz(2,pos2) !get linear z for motor 2 on BT4
	      pos2 = pos2 + z2
	      IF (.NOT. inlimits(2,pos2,contr))THEN
		 CALL Putcontr_NT(' Drive not executed.')
	      ELSE IF (motfl(1)) THEN
	         z1 = fz(1,pos1) !get linear z for motor 1 on BT4
		 pos1 = pos1 + z1
	         IF (.NOT. inlimits(1,pos1,contr))THEN
		    CALL Putcontr_NT ('Drive not executed.')
	         ELSE
		    CALL CLREF (Contr_A)
		    IF (nsta .eq. 4 .and. trashmode) THEN
		       CALL TrashDrive (pos2)
		    ELSE
		       CALL Drive (2, pos2)
		    ENDIF
		    CALL Drive (1, pos1)
c$$$		    IF (nsta .EQ. 4) THEN
c$$$		      IF (motfl(7)) THEN
c$$$		        start = foc7(pos2-z2) + z(7)
c$$$	      		IF (.NOT. inlimits(7,start,contr))THEN
c$$$		 	  CALL Putcontr_NT(' Motor 7 Drive not executed.')
c$$$			ELSE
c$$$			  CALL Drive (7, start)
c$$$			ENDIF
c$$$		      ENDIF
c$$$		      IF (motfl(8)) THEN
c$$$		        start = foc8(pos2-z2) + z(8)
c$$$	      		IF (.NOT. inlimits(8,start,contr))THEN
c$$$		 	  CALL Putcontr_NT(' Motor 8 Drive not executed.')
c$$$			ELSE
c$$$			  CALL Drive (8, start)
c$$$			ENDIF
c$$$		      ENDIF
c$$$		    END IF
		    IF (nsta .EQ. -5) THEN
		      IF (motfl(7)) THEN
			start = foc7(pos2-z(2)) + z(7)
	      		IF (.NOT. inlimits(7,start,contr))THEN
		 	  CALL Putcontr_NT(' Motor 7 Drive not executed.')
			ELSE
			 CALL Drive (7, start)
			ENDIF
		      ENDIF
		    ENDIF
		 END IF
	      ELSE
		 CALL CLREF (Contr_A)
	         CALL Drive (2, pos2)
	      END IF		 
	      st7(1) = PutStr (data)
	      st7(2) = PutStr (dm)
	      st7(3) = PutStr (pos2-z2)
	      response(1:) = st7(1) //' meV,     dM= '// st7(2) // 
	1                    ' ===> 2-theta MONO= '// st7(3)
	      CALL Putline_NT (contr, response)
	      IF (motfl(1)) THEN
	        st7(3) = PutStr (pos1-z1)
	        response(1:)= '     ===> theta MONO= '// st7(3)
	        CALL Putline_NT (contr, response)
	      ENDIF
	      CALL Yield
	      IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   END IF
	   GOTO 520 
	ELSE IF (command(2:3) .EQ. 'EA') THEN             ! Drive to ana energy
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.eq.8 .or. nsta.eq.1) Call Not_Implemented (*520)
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE                                                   ! Drive is ok
	      j = 4
	      IF (command(j:j) .EQ. '=') j = 5
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, da)
	      pos2 = 2. * pos1 + z(6)
	      pos1 = pos1 + z(5)
	      IF (.NOT. inlimits(5,pos1,contr))THEN
	        CALL Putcontr_NT('Drive not executed')
	      ELSE IF (.NOT. inlimits(6,pos2,contr))THEN
	        CALL Putcontr_NT('Drive not executed')
	      ELSE
		 CALL CLREF (Contr_A)
		 CALL Drive (6, pos2)
		 CALL Drive (5, pos1)
		 st7(1) = PutStr (data)
		 st7(2) = PutStr (da)
	         st7(3) = PutStr (pos2-z(6))
		 response(1:) = st7(1) //' meV,     dA= '// st7(2) // 
	1                       ' ===> 2-theta  ANA= '// st7(3)
		 CALL Putline_NT (contr, response)
	         st7(3) = PutStr (pos1-z(5))
                 response(1:) = ' ===> theta  ANA= '// st7(3)
		 CALL Putline_NT (contr, response)
		 CALL Yield
		 IF (READEF(contr_A))CALL IcpAbort(command, *520)
	      END IF
	   END IF
	   GOTO 520
	ELSEIF (command(2:2).eq.'S') THEN			!Drive Slit
 	   j=3
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j-1), FMT= *, ERR= 270) m    ! Slit number
	   IF (nsta.ne.-7) Call Not_Implemented (*520)
	   IF (m.lt.1 .or. m.gt.4)goto 270
	   m = slitlist(m)
	   IF (.NOT. priv) CALL NoPriv (*520)
	ELSE                                                    ! Drive command
 	   j=2
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(2:j-1), FMT= *, ERR= 270) m    ! Motor number
 45	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (m.lt.1 .or. m.gt.MAXMOTS)goto 270
	   IF (nsta .eq. 1 .and. .not. super .and.
     1		m .ne. 4 .and. m .ne. 3) CALL NoSuper (*520)
	   IF (nsta .eq. -11 .and. .not. super .and.
	1	.not.((m .ge. 1 .and. m .le. 8) .or.
	1	(m .ge. 13 .and. m.le.16))) CALL NoSuper (*520)
	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)')m
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   ELSE                                                   ! Drive is ok
	      L=j !remember where letter indicating which drive is
	      IF (command(j:j) .NE. 'I' .AND. command(j:j) .NE. 'T'
     1	        .AND. command(j:j) .NE. 'H' .AND. command(j:j) .NE. 'X') THEN
	           IF (command(j:j) .NE. '=')GOTO 270 ! = must be present
		   j = j+1
	      ELSE 
		 j = j+1
		 IF (command(j:j) .EQ. '=') j = j+1
	      END IF
	      READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	      IF (command(L:L) .EQ. 'I') THEN               ! Incremental drive
		 pos1 = RdAngl(m) + data
		 k = m
		 pos2 = pos1
	      ELSEIF (command(L:L) .EQ. 'X') THEN         ! Xray drive
	         IF (nsta.ne.0 .OR. .NOT. poly) Call Not_Implemented (*520)
		 pos1 = z(m) + data                              ! Normal drive
		 IF (m .eq. 3)THEN
		   k = 4
		 ELSE
		   k = 3
		 ENDIF
		 pos2 = pos1
	      ELSE IF (command(L:L) .EQ. 'T') THEN                  ! 1:2 drive
	   	IF (nsta.eq.8) Call Not_Implemented (*520)
		k = m - 1
		pos1 = data+fz(m,data)
	        st = RdAngl(m)            
		pos2 = RdAngl(k) + 0.5 * (data - (st - fz(m,st)))
		IF (.NOT. motfl(k) .OR. MOD(m,2) .NE. 0) THEN
		    response = 'Illegal 2:1 Drive. Drive not executed'
		    CALL Putline_NT (contr, response)
		    GOTO 520
		END IF
	      ELSE IF (command(L:L) .EQ. 'H') THEN                  !Drive to Hard
		 pos1 = data
		 k = m
		 pos2 = pos1
	      ELSE !normal drive
		 pos1 = data+fz(m,data)        !check for linear zero
		 k = m
		 pos2 = pos1
	      END IF
	      CALL CLREF (Contr_A)
	      IF (.NOT. inlimits(m,pos1,contr))THEN
		CALL Putcontr_NT('Drive not executed')
		IF (nsta. EQ. 8) CALL SETEF(contr_a)
	      ELSE IF (.NOT. inlimits(k,pos2,contr))THEN
	        CALL Putcontr_NT('Drive not executed')
		IF (nsta. EQ. 8) CALL SETEF(contr_a)
	      ELSE
		 IF (command(L:L) .EQ. 'H') THEN
		   CALL Drive (m, data) !drive to hardware position given
		 ELSE
		   CALL Drive (m, pos1)
		 ENDIF
		 IF (command(L:L) .EQ. 'T' .OR. command(L:L) .EQ. 'X')
     1		    CALL Drive (k, pos2)
		 CALL Yield
	      END IF
	      IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   END IF
	   GOTO 520
	END IF      
	GOTO 270 

C--------------------------------- E: EXIT--EDIT-------------------------------C

  50	IF (command(2:2) .EQ. 'X') THEN                              ! Exit ICP
	   CALL Goodbye
	ELSE IF (command(2:4) .EQ. 'DIT') THEN   !EDIT
	   command(1:)=command(1:4)//' '//command(5:) !put blank back in
	   GOTO 520
	END IF
	GOTO 270


C------------- F: FCAL FAMP FIX FM FP FPT FLA FLM FDSA FDAD FETCH------------C

  60	IF (command(2:4) .EQ. 'CAL') THEN         ! Calib      
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 3)
	   DO j = 1, 2
	      m = INDEX (command(1:), ',')
	      command(1:) = command (m+1:)
	   END DO
    	   IF (.NOT. priv) CALL NoPriv (*520)
	   mot            = IFIX (multi(1))		  ! (1) is power supply
	   IF (polfl(mot)) THEN
              f_on(mot)%cur  = multi(2)		          ! (2) is current
	      f_on(mot)%engy = multi(3)			  ! (3) is energy [meV]
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f8.3,a,f8.3,a)',iostat=ios) 
	1           'Flipper Calibration: PS no: ', mot, ' ==>  current: ',
	2	     f_on(mot)%cur, ' amps at ', f_on(mot)%engy, ' meV'
	      OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	      WRITE (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	      CLOSE(1)
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	   END IF
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSEIF (command(2:2) .eq. '+') THEN
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -5) CALL Not_Implemented (*520)
	   focus5=.true.
	   call status_line
	   goto 520
	ELSEIF (command(2:2) .eq. '-') THEN
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -5) CALL Not_Implemented (*520)
	   focus5=.false.
	   call status_line
	   goto 520
	ELSEIF (command(2:4) .EQ. 'DSA') THEN       !DSA FOR FOCUS MOTORS
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -5) CALL Not_Implemented (*520)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   FDSA = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos 
	   CLOSE (1)
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'DAD') THEN       !DAD FOR FOCUS MOTORS
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -5) CALL Not_Implemented (*520)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   FDAD = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   CLOSE (1)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN       !set flippers
	   IF (.NOT. priv) CALL NoPriv (*520)
    	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1        'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), file, statistic, tempctr, 
     1	               polarized, psd,
	1              tdev, magnctr
	   READ (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	   CLOSE (1)
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF 
	   IF (command(3:3) .EQ. 'M') THEN       !set mono flippers
	     IF (command(4:4) .EQ. '+') THEN
	       m=7
	     ELSE
	       m=8
	     ENDIF
	   ELSE					  !set anal flippers
	     IF (command(3:3) .NE. 'A') goto 270
	     IF (command(4:4) .EQ. '+') THEN
	       m=5
	     ELSE
	       m=6
	     ENDIF
	   ENDIF
	   CALL Set_Flippers(m)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'AMP') THEN       ! fullamp for BT2 flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot = IFIX (multi(1))		           ! (1) is power supply
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (polfl(mot)) THEN
	      fullamp(mot) = multi(2)
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f7.2)',iostat=ios) 
	1           'Current to voltage conversion for power supply ', mot, 
	2           ' set to: ', fullamp(mot)
	      OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
     	      WRITE (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	      CLOSE(1)
	   ELSE
	      response(1:) = 
     1		'That power supply is not enabled on this instrument!'
	   END IF
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'IXM' .OR. command(2:4) .EQ. 'IXD') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0.or.nsta.eq.8) CALL Not_Implemented (*520)
	   IF (motfl(2)) fixed(2) = .TRUE.	            ! Fix MONO/DRUM
	   IF (nsta.eq.-5 .or. nsta .eq. 4)THEN !also fix 1 & 7 on SPINS & BT4
	     IF (motfl(1)) fixed(1) = .TRUE.
	     IF (motfl(7)) fixed(7) = .TRUE.
	   ENDIF
	   IF (nsta.eq.4)THEN !also fix 8 on BT4
	     IF (motfl(8)) fixed(8) = .TRUE.
	   ENDIF
	   CALL Status_Line
	   GOTO 520 
	ELSE IF (command(2:4) .EQ. 'IXT' .or. command(2:4).eq.'IXS') THEN                      ! Fix Table
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0.or.nsta.eq.8) CALL Not_Implemented (*520)
	   IF (motfl(3)) THEN     !FIX sample/table
	      fixed(3) = .TRUE.
	   END IF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'IXA') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.0 .or.nsta.eq.8) CALL Not_Implemented (*520)
	   IF (motfl(5)) fixed(5) = .TRUE. ! Fix Analyzer
	   IF (motfl(6)) fixed(6) = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'IX') THEN !FIX motor n
	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) GOTO 270
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	   IF (motfl(m)) THEN				            ! Fix Motor
	      fixed(m) = .TRUE.
	   ELSE 
	      response = 
     1		'That motor is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   END IF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'P' .OR.            ! Find peak comands
     1		 command(2:2) .EQ. 'M') THEN
	  IF (.NOT. priv) CALL NoPriv (*520)
	  lastchar = iend
	  CALL Parse_FP(command(1:iend),errflag,lastchar)
	  IF (errflag)THEN
	    GOTO 270
	  ELSE
	    command(1:) = command(lastchar:)
	    GOTO 520
	  ENDIF
	   GOTO 520
	ELSEIF (command(2:5) .eq. 'ETCH') THEN
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. trackmoves) CALL Not_Implemented(*520)
	   call fetch_tofile(.true.)
	   goto 520
	END IF
	GOTO 270

C------------------- H: HELP  HOLD  HOWLONG   H+/- HOME HOMEF HM------------C

  80	IF (command(2:2) .EQ. 'E') THEN
#if defined(VMS)
	   j = 3    
	   IF (command(3:3) .EQ. 'L') j = 4 	 
	   IF (command(4:4) .EQ. 'P') j = 5 	 
	   IF (command(j:j) .NE. ' ') THEN 	 
	      i = INDEX (command, ' ') 		 
	      topic(1:) = command(j:i) 		 
	   ELSE 				 
	      topic(1:50) = ' ' 		 
	   END IF 				 
	   topic(1:) = 'ICP Commands '// topic(1:)   
	   status = lib$get_symbol('helplib',local,lreply)   
	   IF (status)CALL SMG$PUT_HELP_TEXT (contr, keyb, topic, local(1:lreply))  
#endif
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'OME') THEN    !Drive motor to home 
	   j = 5
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(5:j-1), FMT= *, ERR= 270) m    ! Motor number
 	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -7 .AND. nsta .ne. -5 .AND. nsta .ne. 1.AND.nsta.ne.4) CALL Not_Implemented (*520) !only refl for now
	   IF (nsta .eq. -5)THEN
	     IF (m .NE. 2 .AND. (mottype(m).NE.acsf .and. mottype(m) .NE.ez))
	1	   CALL Not_Implemented (*520) !only refl for now
	   ENDIF
	   IF (nsta .eq. 1)THEN
	     IF (m .NE. 15 .and. m .NE. 12)CALL Not_Implemented (*520) !
     	     IF (m .eq. 12 .AND. .NOT. super) CALL NoSuper (*520)
	   ENDIF
	   IF (m.lt.1 .or. m.gt.MAXMOTS)goto 270
	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)')m
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   ENDIF
	   IF (command(j:j).eq.',')j = j+1
	   str2(1:1)=command(j:j) !+ or -
	   IF (nsta .eq. 1 .and. m .eq. 12 .and. str2(1:1) .eq. '-')THEN
	     IF (RdAngl(12) - z(12) .GT. 100.) THEN
	       CALL CLREF (contr_A)
	       CALL Drive(12,100.+z(12))
	       CALL Yield
	       IF (READEF(contr_A))CALL IcpAbort(command, *520)
	     ENDIF
	   ENDIF
	   CALL CLREF (Contr_A)
	   CALL Home (m, str2(1:1))
	   CALL Yield ! NCM Addition - wait until we're done
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   IF (nsta .eq.1)THEN
	     CALL Hold(1.) !wait 1 minute for motor to get there
	     CALL CLREF (stop)
	   ENDIF
	   IF (mottype(m).eq.EZ) THEN
	      CALL IANG(m,vmot2(m))
	   ENDIF
	   IF (trackmoves)then
	     call putcontr_nt
     1	      ('Please execute FETCH from control mode when motor is at home')
	     call putcontr_nt(' to update file used by FETCH in prep mode.')
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'OL') THEN           ! Hold for given time
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   CALL CLREF (contr_A)
	   CALL Hold (data)
	   CALL CLREF (stop)
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'DEV') THEN                  ! Define mag device
	   IF (.NOT. priv) CALL NoPriv (*520)
	   Call Mconfig
	   IF (minit_done)magnctr = .TRUE.
	   Call Status_Line
           GOTO 520
	ELSEIF (command(2:2) .EQ. 'M') THEN                  ! Define mag mode
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. minit_done) CALL NoMinit (*520)
	   IF (magtype.ne.3 .AND. magtype .ne.4 .AND. magtype .ne. 5) THEN
	     CALL Putline_NT(contr,' HM only valid with superconducting magnet')
	     GOTO 520
	   ENDIF
	   IF (command(3:3).eq.'=')THEN
	     j=4
	   ELSE
	     j=3
	   ENDIF
	   IF (command(j:j).eq.'N')THEN
	     hfmode=0
	   ELSEIF (command(j:j).eq.'P')THEN
	     hfmode=1
	   ELSE
	     goto 270
	   ENDIF
	   CALL Status_Line
           GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN             ! Magn control enabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. minit_done) Call Mconfig
	   IF (minit_done)magnctr = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN            ! Magn control disabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   magnctr = .FALSE.
	   CALL Status_Line
	   GOTO 520      
	ELSE IF (command(2:3) .EQ. 'OW') THEN          ! Howlong
	   keep = .FALSE. !used to indicate whether com file to be used
	   fitmin = .FALSE. !use to indicate only part of file to be used
	   fitinten = .FALSE. !used to indicate /RS
	   m = 8
	   DO WHILE (command(m:m) .EQ. '/')
	    IF (command(m+1:m+1).eq.'F')then !use file
	     keep = .TRUE.
	     m = m+2
	    ELSEIF (command(m+1:m+1).eq.'P')then !only after * in file
	     fitmin = .TRUE.
	     m = m+2
	    ELSEIF (command(m+1:m+2).eq.'RS')then !run-sequence
	     fitinten = .TRUE.
	     m = m+1 !to get past /
	    ELSE
	     GOTO 270 !syntax error
	    ENDIF
	   ENDDO
	   IF (m .GE. iend) GOTO 270 !syntax error
	   IF (keep)THEN !using file
	     command(1:)=command(m:)
	     CALL Howl_File(command,fitmin)
	   ELSEIF (fitinten)THEN !using run sequence
	     CALL Howl_Seq
	     command(1:)=command(3:)
	   ELSE
	     lresp = .TRUE. !write time calcs to screen
	     CALL Howlong (command, iend, time, toler,lresp,lkcommand)
	     command(1:)=command(3:)
	   ENDIF
	   GOTO 520
        END IF
	GOTO 270


C---------------------------- I: INIM INIT ISET ISCAN INIF--------------------C

  90	IF (command(2:4) .EQ. 'NIM') THEN                  ! Reset magnet contr
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. minit_done) CALL NoMinit (*520)
  	   IF (magtype .eq. 1) THEN
	     CALL INIMAG
	   ELSE
	      response(1:) = 'INIM only implemented for MCU controlled magnets'
	      CALL Putline_NT (contr, response)
	   END IF
	   GOTO 520
        ELSE IF (command(2:5) .EQ. 'COIL') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   READ (UNIT= command(6:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)	   
	   IF (nsta.eq.-5) THEN
	      CALL ICOIL(multi(1),multi(2),ierr)
	   ELSE
	      response(1:) = 'ICOIL only implemented for NG5 SPINS'
	      CALL Putline_NT (contr, response)
	   ENDIF
	   GOTO 520
 	ELSE IF (command(2:3) .EQ. 'NI') THEN ! Initialize controller to angle f
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   L = j
	   DO WHILE (isdigit(command(L:L)) .AND. L .LT. 50)
	      L = L + 1
	   END DO
	   READ (UNIT= command(j:L-1), FMT= *, ERR=270) m
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (m.lt.1 .or. m.gt.MAXMOTS)goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4 .and. m .ne. 15) CALL NoSuper (*520)
	   IF (nsta .eq. -11 .AND. .not. super .and. .not.
	1	((m .ge. 1 .and. m .le. 8) .or.
	1	(m .ge. 13 .and. m .le. 16))) CALL NoSuper (*520)
	   IF (nsta .EQ. -7 .AND. (m .lt. 12 .or. m .gt. 15))THEN
     	     IF (.NOT. Password_OK())CALL WrongPass(*520)
	   ENDIF ! ng7 refl restrictions
	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)') m
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE
	      DO WHILE (.NOT. isdigit(command(L:L)) .AND. L .LT. 50)
	         L = L + 1
	      END DO
	      READ (UNIT= command(L:iend), FMT= *, ERR= 270) data
	      CALL Iang (m, data)
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'SET') THEN              ! Iset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   IF (nsta .eq. 0) CALL Not_Implemented (*520)
	   IF (poltype(mot).eq.3) then !new Kepco needs limit
             READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 3)
	   ENDIF
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (polfl(mot)) THEN
              range = multi(2)				  ! (2) is current
	      IF (poltype(mot).eq.3) then !new Kepco
	        step = multi(3) !new kepco needs limit
	      ELSE
	        step = 0.
	      ENDIF
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f7.2)',iostat=ios)
	1           'Power supply ', mot, ':  Current set to ', range
	   ELSE
	      response(1:) = 
     1	      'That power supply is not enabled on this instrument!'
	   END IF
	   CALL Putline_NT (contr, response)
           range = multi(2)				  ! (2) is current
	   CALL Iset (mot, range,step)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'GET') THEN              ! Iset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) multi(1)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   IF (nsta .eq. 0) CALL Not_Implemented (*520)
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (polfl(mot)) THEN
	      CALL Iget (mot, range)	      
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f7.2)',iostat=ios)
	1           'Power supply ', mot, ':  Current(amps) = ', range
	   ELSE
	      response(1:) = 
     1	      'That power supply is not enabled on this instrument!'
	   END IF
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'SCAN') THEN               ! Scan flipper current
	   m = 6
	   keep = .FALSE.
	   arch = .FALSE.
	   DO WHILE (command(m:m) .EQ. '/')
	     IF (command(m+1:m+1).eq.'F')then !make file
	       keep=.TRUE.
	       m = m+2
	     ELSEIF (command(m+1:m+1).eq.'A')then !archive file
	       arch = .TRUE.
	       keep=.TRUE.
	       m = m+2
	     ENDIF
	   ENDDO
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
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (poltype(mot).EQ. 3)THEN
	     response(1:) = 'That power supply is not for use with iscan.'
	     CALL Putline_NT (contr, response)
	     GOTO 520
	   END IF
	   CALL CLREF (Contr_A)
	   CALL Scan_I (mot, start,step,range, mon,keep,arch)
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   GOTO 520
  	ELSEIF (command(2:3) .EQ. 'VM') THEN               !Init virt motor
	   IF (nsta .ne. 8) CALL Not_Implemented (*520)
	   CALL ivm_array(rderr,command(4:iend))
	   IF (rderr)CALL Putcontr_NT(' Err initializing arrays')
	   GOTO 520
     	END IF
	GOTO 270


C---------------------------------- K:        --KF----------------------------C
 110	IF (command(2:2) .EQ. 'F') THEN ! Keithly Fetch command
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -7) CALL Not_Implemented (*520)
	   CALL Kfetch(command,lcom)
	   response(1:)=command(1:lcom)
	   CALL Putline_NT (contr, response)
	   GOTO 520
     	END IF
	GOTO 270

C---------------------------------- L: Lm L+ L--LF-LOGON-LOGOFF---------------C

 120	IF (command(3:3) .EQ. '=' .or. command(4:4) .EQ. '=' ) THEN ! Change lower limits
	   IF (.NOT. priv) CALL NoPriv (*520)
	   L=2
	   DO WHILE (isdigit(command(L:L)).and.L.lt.iend)
	     L=L+1
	   ENDDO
	   READ (UNIT= command(2:L-1), FMT= *, ERR= 270) j  ! determ angle no
	   IF (j.lt.1 .or. j.gt.MAXMOTS)goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. j .ne. 3 .and. 
     1		j .ne. 4) CALL NoSuper (*520)
	   IF (.NOT. motfl(j)) THEN
	      write(unit=str2,fmt='(i2)') j
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF
	   READ (UNIT= command(L+1:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   ll(j) = data
	   WRITE (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   CLOSE (1)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                ! Enable PSD
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).ne.7.AND. nsta .NE. -1.and. 
     1		nsta .NE. 8) CALL Not_Implemented (*520)
	   IF (histo_add.eq.0) THEN
     	    response= 'Position sensitive detector not specified in instr.cfg'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF
	   statistic = .false.
	   psd=.true.
	   liqwait = 0.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '*') THEN    ! Enable PSD with wait for liqs
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).ne.7.AND. nsta .NE. -1 .AND.
     1		 nsta .NE. 8) CALL Not_Implemented (*520)
	   IF (histo_add.eq.0) THEN
     	    response= 'Position sensitive detector not specified in instr.cfg'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF
	   statistic = .false.
	   psd=.true.
	   IF (nsta .eq. -7) THEN
 125	     CALL Read_STRING(keyb, str9,'Enter seconds to wait in scans: ',
	1	   9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=125)liqwait
	     CALL Putline_NT(contr,' ')
	     IF (liqwait .lt. 0) goto 125
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable PSD
	   IF (.NOT. priv) CALL NoPriv (*520)
	   psd = .FALSE.
	   liqwait=0.
	   CALL Status_Line
	   GOTO 520
        ELSE IF (command(2:2) .EQ. 'W') THEN               ! Turn on wait 
	   IF (nsta .eq. -7) THEN
 127	     CALL Read_STRING(keyb, str9,'Enter seconds to wait in scans: ',
	1	   9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=127)liqwait
	     CALL Putline_NT(contr,' ')
	     IF (liqwait .lt. 0) goto 127
	   ENDIF
	ELSE IF (command(2:5) .EQ. 'OGON') THEN               ! Enable logfile
c	   IF (nsta .EQ. 0) CALL Not_Implemented (*520)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   makelog = .TRUE.
	   CALL StampLog(.TRUE.) !write time log file enabled
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:6) .EQ. 'OGOFF') THEN              ! Disable logfile
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (makelog)THEN
	     CALL StampLog(.FALSE.) !write time log file disabled
	     makelog = .FALSE.
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(1:3) .EQ. 'LR7') THEN  ! LR7: Send to LR700
	   IF (.NOT. priv) CALL NoPriv (*520)
	   CALL forward_700
	   GOTO 520	
	ELSE IF (command(1:2) .EQ. 'LR') THEN  ! LR or LRR Send to LR400
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (command(3:3).EQ.'R')THEN !get reply
	     j = 4
	     lresp = .TRUE.
	   ELSE
	     j = 3
	     lresp = .FALSE.
	   ENDIF
	   sc_command(1:)=command(j:iend)
	   lcom=iend-2
	   call lr232(sc_command,lcom,sc_reply,lreply,lresp)
	   call Putline_NT(contr,sc_reply(1:lreply))
	   GOTO 520	
	ELSE IF (command(2:7) .EQ. 'IASTAT') THEN  ! Lia Status
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   keep = .FALSE.
	   CALL Lia_Status(keep,errflag)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'IA') THEN  ! Lia or Lia/R Send to LIA
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   IF (command(4:5).EQ.'/N')THEN !get reply
	     j = 6
	     lresp = .FALSE.
	   ELSE
	     j = 4
	     lresp = .TRUE.
	   ENDIF
	   sc_command(1:)=command(j:iend)
	   lcom=iend-j+1
	   call srlia232(sc_command,lcom,sc_reply,lreply,lresp)
	   IF (lresp)call Putline_NT(contr,sc_reply(1:lreply))
	   GOTO 520	
	ELSE IF (command(1:2) .EQ. 'LK') THEN  ! LK or LKR Send to LK340
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (command(3:3).EQ.'R')THEN !get reply
	     j = 4
	     lresp = .TRUE.
	   ELSE
	     j = 3
	     lresp = .FALSE.
	   ENDIF
	   lkcommand(1:)=command(j:iend)
	   lcom=iend-2
	   call lk232(lkcommand,lcom,lkreply,lreply,lresp)
	   call Putline_NT(contr,lkreply(1:lreply))
	   GOTO 520	
	END IF
	GOTO 270


C------------------------------ M: MOS MRAT MP MONO---------------------------C

 130	IF (command(2:4) .EQ. 'RAT') THEN
	   j = 5
	   IF (command(j:j+1) .EQ. '/S')THEN
	     keep = .TRUE.
	     j = j + 2
	   ELSE
	     keep = .FALSE.
	   ENDIF
	   IF (j .ge. iend) GOTO 131
	   IF (command(j:j) .eq. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 131) data
	   IF (data .EQ. 0.) data = 5.
	   time = ABS(data)
	   goto 132
 131	   time = 5
 132	   IF (.NOT. priv) THEN
	     IF (keep)THEN
	       CALL Read_STRING(keyb, str9,
	1	     'Simulate values for howlong calculations? '//
	1	     '[T/F(default)]: ',
	2	     9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	       CALL Putline_NT(contr,' ')
	       IF (ig .EQ. 0) GOTO 520
	       READ(unit=str9(1:ig),fmt=*,err=270)lresp
 	       IF (lresp) CALL Simulate_Mrat
	       GOTO 520
	     ELSE !not keep
	       CALL NoPriv (*520)
	     ENDIF !keep
	   ENDIF
	   IF (nsta .eq. 0) CALL NoMon (*520)
	   CALL CLREF (contr_A)
	   WRITE (UNIT= st7(1)(1:5), FMT= '(i2)',iostat=ios) NINT(time)
	   response = 'Measuring for '// st7(1)(1:2) //' seconds.  Please wait !'
	   hldasd = use_asd
	   use_asd = .FALSE. !disable area sensitive detector when doing mrat
	   CALL Reset_Display	! Reset NIM count display
	   CALL RlyOff (4)	! Start display counter
	   CALL CnTime (time)
	   t_count = .FALSE.
	   CALL Putline_NT (contr, response)
	   CALL Scaler (counts)
	   CALL RlyOn (4)       ! Stop display counter
	   use_asd = hldasd
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   WRITE (UNIT= st7(2), FMT= '(i5)',iostat=ios) NINT(time)
	   WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) counts(1)
	   response = st7(1)(1:7) //' counts in '// st7(2) //' seconds. '
	   rate = counts(1)/time
	   WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT (rate)
	   response = response(1:32) //'===> RATE =  '// st7(1) //' cps '
	   WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(rate*60.)
	   response = response(1:57) //' or '// st7(1) //' cpm.'
	   CALL Putline_NT (contr, response)
	   IF (rate .LT. 5) THEN
	      response = '*** WARNING: Rate is very low!'//
     1		'        Beam-tube may be closed'
	      CALL Putline_NT (contr, response)
	   END IF
	   IF (keep)CALL Save_MRAT(rate)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ONO') THEN   !bt1: define monitor !MONO=
	   IF (nsta .ne. 1) CALL Not_Implemented (*520)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.not. super)CALL NoSuper (*520)
	   str5 = command(6:10)
	   IF (str5 .ne. 'CU311' .AND. str5 .ne. 'GE311' .and. 
     1		str5 .ne. 'GE733' .AND. str5 .ne. 'DUMMY')GOTO 270
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',  !read current vals
	1        ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS)
	   READ (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1	      (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE(1)
	   monobt1 = str5
	   IF (str5 .EQ. 'CU311')THEN
		 ll(4) = -1.
		 ul(4) = 13.1
		 CALL Putcontr_NT(' Motor 4 limits set to -1. and 13.1')
	   ELSEIF (str5 .EQ. 'GE311')THEN
		 ll(4) = -1.
		 ul(4) = 11.4
		 CALL Putcontr_NT(' Motor 4 limits set to -1. and 11.4')
	   ELSEIF (str5 .EQ. 'GE733')THEN
		 ll(4) = -0.7
		 ul(4) = 13.1
		 CALL Putcontr_NT(' Motor 4 limits set to -0.7 and 13.1')
	   ENDIF
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1        ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   WRITE (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	   WRITE (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),  !memorize new limits & mono
     1	     (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   IF (collim(1).eq.7)THEN
	     str13(1:)=str5//'_7'//'.DAT'
	   ELSEIF (collim(1).eq.15)THEN
	     str13(1:)=str5//'_15'//'.DAT'
           ELSEIF (collim(1).eq.60)THEN
             str13(1:)=str5//'_60'//'.DAT'
	   ELSE
     	     response=' Warning: collim must be reset to 7, 15, or 60.'
	     CALL Putline_NT(contr,response)
	     GOTO 520
	   ENDIF
	   OPEN (UNIT= 1, FILE= HOMEDIR//str13, STATUS= 'OLD', ACCESS= 
	1        'SEQUENTIAL', FORM= 'FORMATTED', ERR= 270)
	   READ (1, *) wavel, (eff(i), i= 1,scl), (zp(i), i= 1, scl) !overwrite wavel
	   CLOSE(1)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1        ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),  !memorize new wavel
     1	     (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   IF (system_type .eq. DEC) THEN
	       help_line(1) = ' Type HELP for commands '
	   ELSE
	       help_line(1) = ' See online icp manual for commands '
	   ENDIF
	   write(unit=str4(1:4),fmt='(I4)',iostat=ios)collim(1)
	   help_line(2)=' Collimation: '//str4//'  MONO='//monobt1
	   help_line(3)=' Use COL= command to change collimation'
	   CALL Put_Help (help_line)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'OS') THEN                   ! define mosaic
	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend) , FMT= *, ERR= 270) (multi(k), k= 1, 3)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1		(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   DO i= 1, 3
	      mosaic(i) = IFIX (multi(i))
	   END DO
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1		(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   DO j= 1, 2
	      i = INDEX (command, ',')
	      command(1:) = command(i+1:)      
	   END DO
c	   IF (i .EQ. 0) i = INDEX (command, ' ')
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'P') THEN             ! set new motor params
	   j=3
	   DO WHILE (isdigit(command(j:j)).and. j .lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j-1), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m.gt.MAXMOTS) goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	   IF (nsta .eq. 0) CALL Not_Implemented (*520)
	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)')j
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE
	      DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
		 j = j + 1
	      END DO
	      READ (UNIT= Command(j:iend), FMT= *, ERR= 270) (multi(i), i=1,5)
	      DO i = 1, 4
		 m = INDEX (command(1:), ',')
	         command(1:) = command (m+1:)
	      END DO
	      IF (.NOT. priv) CALL NoPriv (*520)
	      base(m) = multi(1)
	      ramp(m) = multi(2)
	      top(m)  = multi(3)
	      back(m) = multi(4)
	      merr(m) = multi(5)
	      response(1:6) = 'Motor '
	      WRITE (UNIT= response(7:), FMT=
	1	    '(i1,a14,i4,a9,i3,a7,i5,a9,i3,a12,i3)',iostat=ios)
	2            m, ':  Base Rate= ', base(m), '   Ramp= ', ramp(m), 
	3	    '   Top=', top(m), '   Back= ', back(m), '   Err Tol= ', 
	4	    merr(m)
	      CALL Putline_NT (contr, response)
  	      CALL MotPar (m)
	   END IF
	   GOTO 520	      
	ELSE IF (command(2:2).EQ.'E') THEN   !motor enable
 	   j=3
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j), FMT= *, ERR= 270) m
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. 1 .AND. .not. super) CALL NoSuper (*520)
	   ! Password protect for BT1
	   IF (mottype(m) .EQ. VME)THEN
	      CALL VMEEnable(m)
	      write(unit=response,fmt='(a,i2,a)') 'Axis ',m,' has been enabled'
	      call putline_nt(contr,response)
	   ELSE
	      CALL Putcontr(' This feature is not implemented for this axis')
	   ENDIF	   
	   GOTO 520
	ELSE IF (command(2:2).EQ.'D') THEN   !motor disable
 	   j=3
	   DO WHILE (isdigit(command(j:j)).and.j.lt.iend)
	     j=j+1
	   ENDDO
	   READ (UNIT= command(3:j), FMT= *, ERR= 270) m
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. 1 .AND. .not. super) CALL NoSuper (*520)
	   ! Password protect for BT1
	   IF (mottype(m) .EQ. VME)THEN
	      CALL VMEDisable(m)
	      write(unit=response,fmt='(a,i2,a)') 'Axis ',m,' has been disabled'
	      call putline_nt(contr,response)
	   ELSE
	      CALL Putcontr(' This feature is not implemented for this axis')
	   ENDIF	   
	   GOTO 520
	END IF
	GOTO 270

C----- N:  NEXT NOSUPER NEWMONO------------------------------------------
 140	IF (command(2:4) .EQ. 'EXT') THEN                !Next sample pos
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.1)CALL Not_Implemented (*520)
	   IF (.NOT. motfl(15)) THEN
     	    response= 'Motor 15 not currently defined'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF
	   IF (fixed(15)) THEN
     	    response= 'Motor 15 is fixed'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF

           IF (mottype(15).eq.VME) THEN
	      CALL VMEEnable(15)
	   ENDIF
	   m = NINT(Rdangl(15))
	   CALL CLREF (contr_A)

	   CALL Drive(15,m+.8)
	   CALL Yield
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   CALL Home (15,'+')
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   CALL Yield_Home(15,lresp)
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   IF (.not. lresp)THEN
	     CALL Putcontr_NT(' Could not verify motor at next position')
	   ELSE
	     m = 1 + MOD(NINT(Rdangl(15))-1,6)
	     CALL Iang(15,float(m))
	   ENDIF

           IF (mottype(15).eq.VME) THEN
	      CALL VMEDisable(15)
	   ENDIF

	   GOTO 520
	ELSE IF (command(2:7) .EQ. 'EWMONO') THEN   !bt1: NEWMONO=
	   IF (nsta .ne. 1) CALL Not_Implemented (*520)
	   IF (.NOT. priv) CALL NoPriv (*520)
C	   IF (.not. super)CALL NoSuper (*520)
C	   IF (.NOT. Password_OK())CALL WrongPass(*520)	
           CALL Putline_NCR(contr,' Enter Password: ')
	   lpass=20
	   passw(1:)=' '
	   CALL ReadPass(contr,keyb,passw,lpass)
C	   print *,' Read password: ',passw(1:)
	   IF (passw(1:4) .NE. 'JUDY') CALL WrongPass(*520)
	   
	   IF (.NOT. motfl(3)  .or. .not. motfl(4) .or.
     1	       .NOT. motfl(12) .or. .not. motfl(14) .or.
     1	       .NOT. motfl(7) .or. .not. motfl(1) .or.
     1	       .NOT. motfl(10) )THEN
     	    response= 'Some necessary motors are not currently defined'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF
	   str5 = command(9:13)
	   IF (str5 .ne. 'CU311' .AND. str5 .ne. 'GE311' .and. 
     1		str5 .ne. 'GE733' .AND. str5 .ne. 'DUMMY')GOTO 270
	   CALL NewMono(str5,errflag)
	   super = .FALSE.
	   CALL status_line
C	   IF (trackmoves)then
C	     call putcontr_nt
C     1	      ('Please execute FETCH from control mode when motor is at home')
C	     call putcontr_nt(' to update file used by FETCH in prep mode.')
C	   ENDIF
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'UMX') THEN       !numx for asd
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) ig
	   IF (nsta .eq. -5 .AND. MOD(256,ig) .NE. 0)THEN
	       CALL Putcontr_NT(' NUMX must be a divisor of 256')
	   ELSE
	     OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	     READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	     asd_numx = ig
	     WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos 
	     CLOSE (1)
	   ENDIF
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'UMY') THEN       !numy for asd
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) ig
	   IF (nsta .eq. -5 .AND. MOD(256,ig) .NE. 0)THEN
	     CALL Putcontr_NT(' NUMY must be a divisor of 256')
	   ELSE
	     OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	     READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	     asd_numy = ig
	     WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	     CLOSE (1)
	   ENDIF
	   GOTO 520
 	ELSEIF (command(2:7) .EQ. 'OSUPER') THEN
   	  IF (.NOT. priv) CALL NoPriv (*520)
	  super = .FALSE.
	  CALL status_line
	  GOTO 520
	ENDIF
	GOTO 270


C----- O:  OS ----------------------------------------------------------------C

 150	IF (command(2:2) .EQ. 'S') THEN !Open shutter on XRAY
	  IF (nsta .ne. 0) CALL Not_Implemented (*520)
	  IF (.NOT. priv) CALL NoPriv (*520)
	  ! DO NOTHING
	  GOTO 520
	ENDIF
	GOTO 270
	
C----- P: PSA PHA PDA PDM PMP PRS PA PC PD PI PL PM PQ PB PT PU PV PW P-/+ ---C
C-------PZC--PHF--PFA--PFAMP--PFC--PTP-PVMC-PFS-PASD--PGAA+/- phdev-PASSWORD---C
C-------PWIN PLAT PFLAGS

 160    IF (command(2:3) .EQ. 'FC') THEN                ! Print Flipper calibr.
    	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1        'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), 
     1			file, statistic, tempctr, 
     1	               polarized, psd,
	1              tdev, magnctr
	   READ (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	   CLOSE (1)
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF 
           response(1:) = '                      Power supply setup for flipper control'	
	   CALL Putline_NT (contr, response)
	   CALL Putline_NT (contr, ' ')
	   DO j = pol_chn, pol_chn + pols - 1
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f8.3,a,f8.3,a)',iostat=ios)
	1           'Power supply: ', j, ' flipper current:', f_on(j)%cur, 
	2           ' at energy', f_on(j)%engy, ' meV'
	      CALL Putline_NT (contr, response)
	   END DO 
	   CALL Putline_NT (contr, ' ')
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'GAA+') THEN                ! Enable PGAA commun
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.-7) CALL Not_Implemented (*520)
	   pgaa=.true.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'GAA-') THEN               ! Disable PGAA commun
	   IF (.NOT. priv) CALL NoPriv (*520)
	   pgaa = .FALSE.
	   CALL Status_Line
	   GOTO 520
    	ELSE IF (command(1:4) .EQ. 'PFIX') THEN     ! Print fixed motors
    	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1        'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), 
     1			file, statistic, tempctr, 
     1	               polarized, psd,
	1              tdev, magnctr
	   CLOSE (1)
	   IF (nsta .eq. 1 .and. .not. super)THEN
	     l = 3
	     m = 4
	   ELSE
	     l = 1
	     m = MAXMOTS
	   ENDIF
           response(1:) = ' '	
	   k = 0
	   DO j = l,m
	     IF (fixed(j).and.motfl(j)) THEN
	        WRITE (UNIT= str2(1:2), FMT= '(i2)',iostat=ios) j
	        IF (k .EQ. 0) THEN
		  response(1:2) = str2(1:2)
	          k = k + 2
	        ELSE
		  response(1:) = response(1:k) // ' ' // str2(1:2)
	          k = k + 3
	        END IF
	     END if !fixed
	   END DO !each motor
	   if (k.eq. 0) response(1:)=' None'
	   response(1:) = 'Fixed: '// response(1:) 
	   CALL Putline_NT (contr, response)
	   CALL Putline_NT (contr, ' ')
	   GOTO 520
    	ELSE IF (command(2:4) .EQ. 'WIN') THEN           ! Print WINdow intensity range
	   RESPONSE(1:)='XMIN: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_xmin
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='XMAX: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_xmax
	   CALL Putline_NT (contr, RESPONSE)
    	ELSE IF (command(2:4) .EQ. 'ASD') THEN           ! Print ASD PARAMS
	   RESPONSE(1:)='XMIN: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_xmin
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='YMIN: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_ymin
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='XMAX: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_xmax
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='YMAX: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_ymax
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='NUMX: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_numx
	   CALL Putline_NT (contr, RESPONSE)
	   RESPONSE(1:)='NUMY: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)asd_numy
	   CALL Putline_NT (contr, RESPONSE)
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FDSA') THEN           ! Print fDSA FOCUS PARAM
	   RESPONSE(1:)='FDSA: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)FDSA
	   CALL Putline_NT (contr, RESPONSE)
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FDAD') THEN           ! Print fDAD FOCUS PARAM
	   RESPONSE(1:)='FDAD: '
	   WRITE(UNIT=RESPONSE(7:),FMT=*,iostat=ios)FDAD
	   CALL Putline_NT (contr, RESPONSE)
	   GOTO 520
    	ELSE IF (command(2:5) .EQ. 'FAMP') THEN           ! Print famp values
    	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1        'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), 
     1			file, statistic, tempctr, 
     1	               polarized, psd,
	1              tdev, magnctr
	   READ (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	   CLOSE (1)
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF 
           response(1:) = '                      Power supply FAMP settings'	
	   CALL Putline_NT (contr, response)
	   CALL Putline_NT (contr, ' ')
	   DO j = pol_chn, pol_chn + pols - 1
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f8.3)',iostat=ios)
	1           'Power supply: ', j, ' :',fullamp(j) 
	      CALL Putline_NT (contr, response)
	   END DO 
	   CALL Putline_NT (contr, ' ')
	   GOTO 520
    	ELSE IF (command(2:3) .EQ. 'SA') THEN           ! Print software angles
	   IF (.NOT. priv) CALL NoPriv (*520)
 	   IF (isdigit(command(4:4))) THEN
	      IF (isdigit(command(5:5))) THEN
	        READ (UNIT=command(4:5), FMT= '(i2)', ERR= 270) m
	      ELSE
	        READ (UNIT=command(4:4), FMT= '(i1)', ERR= 270) m
	      ENDIF
	      IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	      IF (m .lt. 1 .or. m .gt. MAXMOTS)then
		 m = 1
	         l = MAXMOTS
	      ENDIF
	      k = m
	      l = k
	   ELSE
	     IF (nsta .eq. 1 .and. .not. super)THEN
 	       k = 3
   	       l = 4
	     ELSE
 	       k = 1
   	       l = MAXMOTS
	     ENDIF
	   END IF
   	   j = 0       ! (* character pointer *)
	   i = 0       ! (* number of motors written *) 
	   DO m = k, l
 	      IF (motfl(m)) THEN
	         pos1 = RdAngl(m)
		 zm = fz(m,pos1)
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'A', m,'=', pos1-zm, '  '
		 IF (j .EQ. 0) THEN
		    response = str14
		 ELSE
		    response = response(1:j) // str14
		 END IF
		 j = j + 14
	         i = i + 1
	      END IF
	      IF (i .GT. 5) THEN
		 CALL Putline_NT (contr, response)
		 j = 0
		 i = 0
	      END IF
	   END DO                    
	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   GOTO 520                            
	ELSE IF (command(2:3) .EQ. 'HA') THEN           ! Print hardware angles
	   IF (.NOT. priv) CALL NoPriv (*520)
 	   IF (isdigit(command(4:4))) THEN
	     IF (isdigit(command(5:5))) THEN
	      READ (UNIT=command(4:5), FMT= '(i2)', ERR= 270) m
	     ELSE
	      READ (UNIT=command(4:4), FMT= '(i1)', ERR= 270) m
	     ENDIF
	      IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	      IF (m .lt. 1 .or. m .gt.MAXMOTS) then
		k=1
	        l=MAXMOTS
	      ELSE
	        k = m
	        l = k
	      ENDIF
	   ELSE
	     IF (nsta .eq. 1 .and. .not. super)THEN
 	       k = 3
   	       l = 4
	     ELSE
 	       k = 1
   	       l = MAXMOTS
	     ENDIF
	   END IF
  	   j = 0       ! (* character pointer *)
	   i = 0       ! (* number of motors written *) 
	   DO m = k, l
	      IF (motfl(m)) THEN
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'A', m, '=', RdAngl(m), '  '
		 IF (j .EQ. 0) THEN
		    response = str14
		 ELSE
		    response = response(1:j) // str14
		 END IF
		 j = j + 14
	         i = i + 1
	      END IF
	      IF (i .GT. 5) THEN
		 CALL Putline_NT (contr, response)
		 j = 0
		 i = 0
	      END IF
	   END DO                    
	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   GOTO 520                            
	ELSE IF (command(2:3) .EQ. 'DM') THEN            ! Print mono d-spacing
	   WRITE (UNIT=response(1:), FMT= '(a,1x,f8.5,1x,a1)',iostat=ios) 
	1         'Monochromator d-spacing= ', dm, CHAR(197)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'DA') THEN             ! Print ana d-spacing
	   WRITE (UNIT=response(1:), FMT= '(a,1x,f8.5,1x,a1)',iostat=ios) 
	1         'Analyzer d-spacing= ', da, CHAR(197)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'MP') THEN	       ! Print motor parameters
	   READ (UNIT= command(4:iend), FMT= *, ERR= 270) m
	   m = ABS(m) !? why was this here?
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) GOTO 270
	   IF (nsta .eq. 0) CALL Not_Implemented (*520)
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)') m	      
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE                                                   ! Print is ok
	      response(1:6) = 'Motor '
	      WRITE (UNIT= response(7:), FMT=
	1	    '(i2.2,a14,i4,a9,i3,a7,i5,a9,i4,a12,i3)',iostat=ios)
	2            m, ':  Base Rate= ', base(m), '   Ramp= ', 
	3            ramp(m), '   Top=', top(m), '   Back= ', back(m), 
	4            '   Err Tol= ', merr(m)
	      CALL Putline_NT (contr, response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'TD') THEN               ! Print temp config
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   CALL Ptconfig
	   IF (tcareful)CALL Putcontr_NT(' Using CAREFUL APPROACH !!!')
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'HD') THEN               ! Print magnet config
	   IF (.NOT. minit_done) CALL NoMinit (*520)
	   CALL PMconfig
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'TP') THEN               ! Print temp params
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   CALL Rdpar (params)
	   IF (temptype.EQ.3 .OR. temptype .EQ. 10)THEN !lk330 or lk340
             WRITE (UNIT= response(1:), FMT= 
     1		'(a8, f8.3, a8, f8.3, a8, f8.3,a8,f8.3)',iostat=ios)
	1         ' Gain: ', params(1), ' Rate: ', params(2), 
	2         ' Reset: ', params(3),' Range: ',params(4)
	     CALL Putline_NT (contr, response)
	     IF (temptype .EQ. 3)THEN
	       IF (params(5).EQ.0.)GOTO 520
	       response(1:)='Autotuning on '
	       IF (params(5).EQ.3)THEN
	         response(15:)='Gain, Reset, Rate'
	       ELSEIF (params(5).EQ. 2)THEN
	         response(15:)='Gain, Reset'
	       ELSEIF (params(5).EQ. 1)THEN
	         response(15:)='Gain'
	       ENDIF
	       CALL Putline_NT (contr, response)
	     ENDIF
	   ELSEIF (temptype.EQ.16)THEN !ltc21
             WRITE (UNIT= response(1:), FMT= 
     1	      '(a8, f8.3, a8, f8.3, a8, f8.3,a8,f8.3)',iostat=ios)
	1         ' Gain: ', params(1), ' Rate: ', params(2), 
	2         ' Reset: ', params(3),' Power%: ',params(4)
	     CALL Putline_NT (contr, response)
	   ELSEIF (temptype.EQ.18)THEN !omega 6070
             WRITE (UNIT= response(1:), 
     1		FMT= '(a13, f8.0, a21, f8.0)',iostat=ios)
	1         'Gain: ', params(1), '    Rate/Reset: ', params(2)
	     CALL Putline_NT (contr, response)
	   ELSE
             WRITE (UNIT= response(1:), 
     1		FMT= '(a13, f8.3, a21, f8.3, a15, f8.3)',iostat=ios)
	1         'Gain: ', params(1), '    Rate: ', params(2), 
	2         '    Reset: ', params(3)
	     CALL Putline_NT (contr, response)
	   ENDIF
	   GOTO 520
        ELSEIF (command(2:8) .EQ. 'ASSWORD') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT.Password_OK())CALL Wrongpass (*520)
	   CALL Putcontr_NT(' Enter New Password')
	   CALL Putcontr_NT(' ')
	   i1 = 20
	   CALL Readpass(contr,keyb,local(1:20),i1)
	   CALL Putcontr_NT(' Verify New Password')
	   CALL Putcontr_NT(' ')
	   i2 = 20
	   CALL Readpass(contr,keyb,local(21:40),i2)
	   IF (local(1:i1) .EQ. local(21:21+i2-1))THEN
	       CALL Putcontr_NT(' Changing Password')
	       CALL UPDATE_MOTCLE(local(1:i2),i2)
	   ELSE
	       CALL Putcontr_NT(' Incorrect verification. Password unchanged.')
	   ENDIF
	  GOTO 520
	ELSE IF (command(2:2) .EQ. 'A') THEN	     ! Print soft & hard angles
	   IF (.NOT. priv) CALL NoPriv (*520)
 	   IF (isdigit(command(3:3))) THEN
	      IF (isdigit(command(4:4))) THEN
	        READ (UNIT=command(3:4), FMT= '(i2)', ERR= 270) m
	      ELSE
	        READ (UNIT=command(3:3), FMT= '(i1)', ERR= 270) m
	      ENDIF
	      IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4 .and. m .ne. 12) CALL NoSuper (*520)
	      IF (m .lt. 1 .or. m .gt. MAXMOTS) THEN
	        k=1
	        l=MAXMOTS
	      ELSE
	        k = m
	        l = k
	      ENDIF
	   ELSE
	     IF (nsta .eq. 1 .and. .not. super)THEN
 	       k = 3
   	       l = 4
	     ELSE
 	       k = 1
   	       l = MAXMOTS
	     ENDIF
	   END IF
 	   response(1:)   = 'Soft: '
	   response_2(1:) = 'Hard: '
	   j = 6
	   i = 0
	   bt6motor2 = -999.99 !flag for not read
	   DO m = k, l 
	      IF (motfl(m)) THEN
	         IF (nsta .eq. 0) CALL libwait(.1,status)
		 data = RdAngl(m)
		 IF (nsta .eq. 6 .and. m .eq. 2)bt6motor2 = data
		 zm = fz(m,data)
	        IF (nsta.eq.1 .and. super)THEN
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f9.2, a1)',iostat=ios) 
	1              'A', m, '=', data-zm, ' '
		ELSE
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'A', m, '=', data-zm, '  '
	        ENDIF
		 response   = response(1:j) // str14
	        IF (nsta.eq.1 .and. super)THEN
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f9.2, a1)',iostat=ios) 
	1              'A', m, '=', data, ' '
		ELSE
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'A', m, '=', data, '  '
		ENDIF
		 response_2 = response_2(1:j) // str14
		 j = j + 14
		 i = i + 1
	      END IF
	      IF (i .GT. 4) THEN
		 CALL Putline_NT (contr, response)
		 CALL Putline_NT (contr, response_2)
		 j = 6
		 i = 0
	         response(1:6)   = 'Soft: '
	         response_2(1:6) = 'Hard: '
	      END IF
	   END DO                    
	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   IF (i .GT. 0) CALL Putline_NT (contr, response_2)
	   IF (nsta. eq. 6 .and. bt6motor2 .ne. -999.99) THEN
	     bt6motor2 = bt6motor2 * 4.8
	     write(str14(1:8),fmt='(f8.3)',err=520)bt6motor2
	     response(1:)='Motor 2 Calibration Angle: '//str14(1:8)
	     CALL Putline_NT (contr, response)
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'FL') THEN 		!print flags
	   CALL Status_Line
	   GOTO 520                            
	ELSE IF (command(2:2) .EQ. 'C') THEN                ! Print collimation
	   WRITE (UNIT= response(1:), FMT= '(a12,2x,4i3)',iostat=ios) 'Collimation:',
	1        (collim(i), i= 1, 4)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'MONO') THEN                ! Print mono/collimation
	   IF (nsta.ne.1) CALL Not_Implemented (*520)
	   WRITE (UNIT= response(1:), FMT= '(a12,2x,4i3,2x,2a5)',iostat=ios) 'Collimation:',
	1        (collim(i), i= 1, 4),'MONO=',monobt1
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'DEM') THEN    ! Print pos for mono energy
	   IF (nsta.eq.8 .or. nsta .eq. 1) Call Not_Implemented (*520)
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE                                                   ! Drive is ok
	      j = 5
	      IF (command(j:j) .EQ. '=') j = 6
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, dm)
	      pos2 = 2. * pos1
	      z2 = fz(2,pos2) !check for linear zero
	      pos2 = pos2+z2
	      IF (.NOT. inlimits(2,pos2,contr))THEN
		 CALL Putcontr_NT(' Drive would not be executed.')
	      ELSE 
	        st7(1) = PutStr (data)
	        st7(2) = PutStr (dm)
	        st7(3) = PutStr (pos2-z2)
		IF (motfl(1)) THEN 
	         z1 = fz(1,pos1) !get linear z for motor 1 on BT4
		 pos1 = pos1 + z1
	         IF (.NOT. inlimits(1,pos1,contr))THEN
		    CALL Putcontr_NT ('Drive would not be executed.')
	         ELSE
	           response(1:) = 'For '//st7(1) //' meV,     dM= '// st7(2) // 
	1                    '     2-theta MONO would be '// st7(3)
	           CALL Putline_NT (contr, response)
	           st7(3) = PutStr (pos1-z1)
	           response(1:)= '    theta MONO would be '// st7(3)
	           CALL Putline_NT (contr, response)
c$$$		   IF (nsta .EQ. 4) THEN
c$$$		       IF (motfl(7)) THEN
c$$$			 start = foc7(pos2-z2)
c$$$		         st7(3) = Putstr(start)
c$$$	                 IF (.NOT. inlimits(7,start+z(7),contr))THEN
c$$$	                   response(1:)= '    motor 7 would exceed limits'
c$$$			 ELSE
c$$$	                   response(1:)= '    motor 7 would be '// st7(3)
c$$$			 ENDIF
c$$$	                 CALL Putline_NT (contr, response)
c$$$		       ENDIF 
c$$$		       IF (motfl(8)) THEN
c$$$			 start = foc8(pos2-z2)
c$$$		         st7(3) = Putstr(start)
c$$$	                 IF (.NOT. inlimits(8,start+z(8),contr))THEN
c$$$	                   response(1:)= '    motor 8 would exceed limits'
c$$$			 ELSE
c$$$	                   response(1:)= '    motor 8 would be '// st7(3)
c$$$			 ENDIF
c$$$	                 CALL Putline_NT (contr, response)
c$$$		       ENDIF 
c$$$		   END IF
		   IF (nsta .EQ. -5) THEN
		       IF (motfl(7)) THEN
			 start = foc7(pos2-z2)
		         st7(3) = Putstr(start)
	                 IF (.NOT. inlimits(7,start+z(7),contr))THEN
	                   response(1:)= '    motor 7 would exceed limits'
			 ELSE
	                   response(1:)= '    motor 7 would be '// st7(3)
			 ENDIF
	                 CALL Putline_NT (contr, response)
		       ENDIF 
		   ENDIF
		 END IF !motor 1 inlimits
	        ELSE !motor 1 not in use
	          response(1:) = 'For '//st7(1) //' meV,     dM= '// st7(2) // 
	1                    '     2-theta MONO would be '// st7(3)
	          CALL Putline_NT (contr, response)
	        END IF !motor 1 in use
	      END IF !motor 2 inlimits
	   ENDIF !motfl(2)
	   GOTO 520 
	ELSE IF (command(2:4) .EQ. 'DEA') THEN   ! Print pos for ana energy
	   IF (nsta.eq.8 .or. nsta .eq. 1) Call Not_Implemented (*520)
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE                                                   ! Drive is ok
	      j = 5
	      IF (command(j:j) .EQ. '=') j = 6
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, da)
	      pos2 = 2. * pos1 + z(6)
	      pos1 = pos1 + z(5)
	      IF (.NOT. inlimits(5,pos1,contr))THEN
	        CALL Putcontr_NT('Drive would not executed')
	      ELSE IF (.NOT. inlimits(6,pos2,contr))THEN
	        CALL Putcontr_NT('Drive would not executed')
	      ELSE
		 st7(1) = PutStr (data)
		 st7(2) = PutStr (da)
	         st7(3) = PutStr (pos2-z(6))
		 response(1:) = 'For '//st7(1) //' meV,     dA= '// st7(2) // 
	1                       '     2-theta  ANA would be '// st7(3)
		 CALL Putline_NT (contr, response)
	         st7(3) = PutStr (pos1-z(5))
                 response(1:) = ' theta  ANA would be '// st7(3)
		 CALL Putline_NT (contr, response)
	      END IF
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'D') THEN                ! Print date & time
	   CALL DATE_TIME (sys_time)
	   CALL Putline_NT (contr, sys_time(1:17))
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'I' .OR. command(2:2) .EQ. 'Q' 
	1        .OR. (command(2:2) .EQ. 'B' .OR. command(2:2) .EQ. 'T' 
     1	         .AND. isdigit(command(3:3)))) THEN
	  lastchar = iend 
	  CALL Parse_Pbuf(command(1:iend),errflag,lastchar)
	  IF (errflag)THEN
	    GOTO 270
	  ELSE
	    command(1:) = command(lastchar:)
	    GOTO 520
	  ENDIF
	ELSE IF (command(2:4) .EQ. 'LAT') THEN     ! Print lasta,b,c,ef, & wavel
	   WRITE (UNIT= response(1:), FMT= '(1x,5(a5,2x,F8.5))',iostat=ios)
	1	'  A: ',lasta,'  B: ',lastb,'  C: ',lastc, ' FE: ',lastef,
	1	' WL: ',wavel
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN               ! Print lower limits
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   CLOSE (1)
	   IF (nsta .eq. 1 .and. .not. super)THEN
	     k = 3
	     l = 4
	   ELSE
	     k = 1
	     l = MAXMOTS
	   ENDIF
	   j = 0
 	   i = 0
	   DO m = k, l
	      IF (motfl(m)) THEN
	         IF (nsta.eq.1 .and. super)THEN
		    WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f9.2, a1)',iostat=ios) 
	1              'L', m, '=', ll(m), ' '
		 ELSE
		    WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'L', m, '=', ll(m), '  '
		 ENDIF
		 IF (j .EQ. 0) THEN
		    response = str14
		 ELSE
		    response = response(1:j) // str14
		 END IF
		 j = j + 14
		 i = i + 1
	      END IF
	      IF (i .GT. 4) THEN
		 CALL Putline_NT (contr, response)
		 j = 0
		 i = 0
	      END IF
	   END DO                    
	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   GOTO 520                            
	ELSE IF (command(2:2) .EQ. 'M') THEN                     ! Print mosaic
	   WRITE (UNIT= response(1:), FMT= '(a7,2x,3i5)',iostat=ios) 'Mosaic:',
	1        (mosaic(i), i= 1, 3)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'RSF') THEN            !type out file sequence
	   command(1:)=command(5:)
	   CALL Type_File (command)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'RS') THEN                  ! Print sequence
	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1        FORM= 'UNFORMATTED',RECL=IBUF_RECL)
	   READ (1,REC=NBUF+1) seq
	   CLOSE (1)
	   i = INDEX (seq, '#') - 1
	   IF (i .GT. 1) THEN
	      CALL Putline_WRAPWORD (contr, seq(1:i))
	   ELSE
	      response='No sequence defined '
	      CALL Putline_NT (contr,response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'T') THEN                ! Print Temperature
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   CALL RdTemp (st, ct, sp)
	   IF ( (temptype .eq. 4 .or. temptype .eq. 7 .or. temptype.eq.19) .and.
     1		temptype .NE. tdev%cfg)THEN !using resistance
	     CALL Putmax(st,str13,13,12)
	     response(1:) = 'Sample Temp: '//str13
	     CALL Putmax(sp,str13,13,12)
	     response(1:) = response(1:26)//' Set Point: '//str13
	     IF (temptype .ne. 4)THEN
	       CALL Putmax(ct,str13,13,12)
	       response(1:) = response(1:51)//' Contr Temp: '//str13
	     ENDIF
	   ELSE
	     IF (temptype.eq.13)THEN !SR830
	       CALL Putcontr(' X Y FREQ SLVL <cr> AUXV1 AUXV2: ')
	       keep = .FALSE.
	       call lia_wrdata(keep,errflag,np)
	       IF (scanlia .eq. 0)THEN
	         response(1:)='f(XY): '
	       ELSEIF (scanlia .eq. 1)THEN
	         response(1:)='SLVL: '
	       ELSEIF (scanlia .EQ. 2)THEN
	         response(1:)='FREQ: '
	       ELSEIF (scanlia .EQ. 3)THEN
	         response(1:)='AUXV: '
	       ENDIF
	       write(unit=response(8:),fmt=*,iostat=ios)st
	     ELSEIF (temptype.eq.12)THEN !tdboard
	       response(1:)='MSV: '
	       write(unit=response(8:),fmt=*,iostat=ios)st
	     ELSEIF (temptype.eq.17)THEN !shear cell
	       if (sp .eq. 0) then
	         response(1:) = ' FAULT received from shear cell'
               elseif (sp .eq. -1) then
	         response(1:) = ' shear cell not moving'
	       elseif (sp .eq. -999.) then
		 response(1:) = ' err communicating with shear cell'
	       else
	         response(1:) = ' Shear cell speed: '
	         write(unit=response(20:24),fmt='(I5)',iostat=ios)NINT(st)
	       endif
	     ELSEIF (temptype.eq.11)THEN !Eurotherm furnace
               WRITE (UNIT= response(1:), 
     1		FMT= '(a13, f7.2, a21, f7.2, a15, f7.2)',iostat=ios)
	1         'Sample Temp: ', st, '    Controller Temp: ', ct, 
	2         '    Set point: ', sp
	     ELSEIF (temptype.ne.4)THEN !all but lr400
               WRITE (UNIT= response(1:), 
     1		FMT= '(a13, f7.3, a21, f7.3, a15, f7.3)',iostat=ios)
	1         'Sample Temp: ', st, '    Controller Temp: ', ct, 
	2         '    Set point: ', sp
	     ELSE !lr400
               WRITE (UNIT= response(1:), 
     1		FMT= '(a13, f12.3, a15, f12.3)',iostat=ios)
	1         'Sample Temp: ', st, 
	2         '    Set point: ', sp
	     ENDIF
	   ENDIF !temptype .eq. tdev%cfg
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'HF') THEN                ! Print Field
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. minit_done) CALL NoMinit (*520)
	   IF (magtype .EQ. 5) THEN ! LAKESHORE GAUSSMETER
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Field: ', range, '  tesla'
	   ELSEIF (magtype .EQ. 4)THEN
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Field: ', range, '  tesla'
	   ELSEIF (magtype .EQ. 3)THEN
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Field: ', range, '  tesla'
	   ELSEIF (magtype .EQ. 2)THEN !hp power supply
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Voltage: ', range
	   ELSEIF (magtype .EQ. 6)THEN
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Field: ', range, '  tesla'
	   ELSEIF (magtype .EQ. 7)THEN
	     CALL RdField (range)
	     write(unit=str14,fmt='(f14.4)',err=270)range
             WRITE (UNIT= response(1:), FMT= '(a,f13.4,a)',iostat=ios)
	1         'Units: ', range, '  '
	   ELSE
	     response(1:)='Field read is not yet implemented for this controller'
	   ENDIF
	   CALL Putline_NT (contr, response)
	   GOTO 520
 	ELSE IF (command(2:2) .EQ. 'U') THEN               ! Print upper limits
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS)
	   CLOSE (1)
	   IF (nsta .eq. 1 .and. .not. super)THEN
	     k = 3
	     l = 4
	   ELSE
	     k = 1
	     l = MAXMOTS
	   ENDIF
	   j = 0
	   i = 0
	   DO m= k, l
	      IF (motfl(m)) THEN
	         IF (nsta.eq.1 .and. super)THEN
		    WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f9.2, a1)',iostat=ios) 
	1              'U', m, '=', ul(m), ' '
		 ELSE
		    WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1              'U', m, '=', ul(m), '  '
		 ENDIF
	         IF (j .EQ. 0) THEN
		    response = str14
	         ELSE		
		    response = response(1:j) // str14
	         END IF
	         j = j + 14
		 i = i + 1
	      END IF
	      IF (i .GT. 4) THEN
		 CALL Putline_NT (contr, response)
		 j = 0
		 i = 0
	      END IF
	   END DO                    
	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'VMC') THEN  ! Print virt mot coeffs
	   IF (nsta .ne. 8) CALL Not_Implemented (*520)
	     CALL Putcontr_NT(' Matrix A: ')
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a(1,i),i=1,3)
	     CALL Putline_NT (contr,response)
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a(2,i),i=1,3)
	     CALL Putline_NT (contr,response)
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a(3,i),i=1,3)
	     CALL Putline_NT (contr,response)
	     CALL Putcontr_NT(' Matrix A_Inv: ')
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a_inv(1,i),i=1,3)
	     CALL Putline_NT (contr,response)
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a_inv(2,i),i=1,3)
	     CALL Putline_NT (contr,response)
	     write(unit=response(1:),fmt=*,iostat=ios)(vm_a_inv(3,i),i=1,3)
	     CALL Putline_NT (contr,response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'VER') THEN	     ! Print default run number
	   WRITE (UNIT= str4, FMT= '(i4)',iostat=ios) def_ver
	   response = 'Current default run number: '// str4
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'V') THEN	     ! Print global variables
	   write(unit=response(1:),fmt=
     1	   '(''VLAT: '',F14.6,''  VZ: '',F14.6,''  V0: '',F14.6)',
     1		iostat=ios)vlat,vz,vg(0)
	   CALL Putline_NT (contr, response)
	   write(unit=response(1:),fmt=
     1	   '(''V1:   '',F14.6,''  V2: '',F14.6,''  V3: '',F14.6)',
     1		iostat=ios)vg(1),vg(2),vg(3)
	   CALL Putline_NT (contr, response)
	   write(unit=response(1:),fmt=
     1	   '(''V4:   '',F14.6,''  V5: '',F14.6,''  V6: '',F14.6)',
     1	    iostat=ios)vg(4),vg(5),vg(6)
	   CALL Putline_NT (contr, response)
	   write(unit=response(1:),fmt=
     1	    '(''V7:   '',F14.6,''  V8: '',F14.6,''  V9: '',F14.6)',
     1		iostat=ios)vg(7),vg(8),vg(9)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'W') THEN                 ! Print wavelength
	   WRITE (UNIT=response(1:), FMT= '(a4,2x,f5.3)',iostat=ios) 'Wl= ', wavel
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'ZC') THEN                 ! Print zero coeffs
	   IF (nsta .ne. 4) CALL Not_Implemented (*520)  
	     write(unit=str14,fmt='(F14.6)',iostat=ios)z1coeff1
	     write(unit=str13,fmt='(F13.6)',iostat=ios)z1coeff2
	     response(1:)='Z1 = '//str14//' * A1 +'//str13
	     CALL Putline_NT (contr,response)
	     IF (z(1) .NE. -99.)
     1	       CALL Putline_NT (contr,' WARNING: Special flag for motor 1 NOT set')
	     write(unit=str14,fmt='(F14.6)',iostat=ios)z2coeff1
	     write(unit=str13,fmt='(F13.6)',iostat=ios)z2coeff2
	     response(1:)='Z2 = '//str14//' * A2 +'//str13
	     CALL Putline_NT (contr,response)
	     IF (z(2) .NE. -99.)
     1	       CALL Putline_NT (contr,' WARNING: Special flag for motor 2 NOT set')
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'Z') THEN                 ! Print zeropoints
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	   CLOSE (1)
	   IF (nsta .eq. 1 .and. .not. super)THEN
	     k = 3
	     l = 4
	   ELSE
	     k = 1
	     l = MAXMOTS
	   ENDIF
	   j = 0
	   i = 0
	   DO m= k, l
	      IF (motfl(m)) THEN
		 WRITE (UNIT= str14, FMT= '(a1, i2.2, a1, f8.3, a2)',iostat=ios) 
	1               'Z', m, '=', z(m), '  '
		 IF (j .EQ. 0) THEN
		    response = str14
		 ELSE
		    response = response(1:j) // str14
		 END IF
		 j = j + 14
		 i = i + 1
	      END IF
	      IF (i .GT. 4) THEN
		 CALL Putline_NT (contr, response)
		 j = 0
		 i = 0
	      END IF
	   END DO                    
    	   IF (i .GT. 0) CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                   ! Polarized Beam 
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. 0 .OR. nsta .eq. 1 .OR. nsta .EQ. 8 .OR.
     1		nsta .eq. 4 .or. nsta .eq. 6 .or. nsta .eq. 9)
     1		CALL Not_Implemented (*520)
	   polarized = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN                 ! Unpolarized beam
	   IF (.NOT. priv) CALL NoPriv (*520)
	   polarized = .FALSE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'SF') THEN                ! Print scale factor
	   IF (nsta .ne. -7) CALL Not_Implemented (*520)
	   WRITE (UNIT= response(1:), FMT= '(a15,2x,F5.3)',iostat=ios)' Scale factor: ',
	1        eff(1)
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(1:3) .EQ. 'POL') THEN                ! Polfig
	   IF (.NOT. priv) CALL NoPriv (*520)			   ! Run buffer
	   j = INDEX (command, ';')
	   IF ( j .EQ. 0 ) THEN		   ! (* only one command on this line *)
	      j=iend
	   ELSE                            ! (* more than one command on line *)
	      j = j-1
	   END IF
	   CALL CLREF (contr_A)
	   CALL RUN_p_BUF(command(4:j),j-3)
	      IF (system_type .eq. DEC) THEN
	        help_line(1) = ' Type HELP for commands '
	      ELSE
	        help_line(1) = ' See online icp manual for commands '
	      ENDIF
	      help_line(2) = ' '
	      help_line(3) = ' '
	      CALL Put_Help (help_line)
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   GOTO 520
	END IF
	GOTO 270


C--------------------------------- Q: QUIT -----------------------------------C

 170    IF (command(2:4) .EQ. 'UIT') THEN                    ! Exit the program
	   CALL Goodbye
	ELSE IF (command(2:2) .EQ. '+') THEN    ! Enable wait for liqs
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .EQ. -7)THEN
	     IF (psd)THEN
     	       response= 'Position sensitive detector in use. Use L* instead.'
	       CALL Putline_NT(contr,response) 
	       GOTO 520
	     ENDIF
 175	     CALL Read_STRING(keyb, str9,'Enter seconds to wait in scans: ',
     1		9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=175)liqwait
	     CALL Putline_NT(contr,' ')
	     IF (liqwait .lt. 0) goto 175
	     CALL Status_Line
	   ELSEIF (nsta .eq. -5)THEN
	     IF (negqpos) THEN
	       CALL Putcontr_NT(' Defocused condition already selected.')
	       GOTO 520
	     ENDIF
	     IF (.not. super) CALL NoSuper (*520)
	     CALL Read_STRING (keyb, str4, 
     1		'  Has Health Physics surveyed and approved this [NO]? ', 
     1	       4,i1,i1,i1,ig,i1, contr) !i1 is dummy param for absoft
	     IF (str4(1:1) .NE. 'Y') GOTO 520
	     CALL Putcontr_NT(' Remember to change limits of motors 3 and 4.')
	     CALL Putcontr_NF(' ')
	     negqpos = .TRUE.
	     CALL Status_Line
	   ELSE
	     CALL Not_Implemented (*520)
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable PSD
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. -7) THEN
	     liqwait=0.
	     CALL Status_Line
	   ELSEIF (nsta .eq. -5) THEN
	     IF (.not. negqpos) THEN
	       CALL Putcontr_NT(' Defocused condition not currently selected.')
	       GOTO 520
	     ENDIF
	     IF (.not. super) CALL NoSuper (*520)
	     CALL Read_STRING (keyb, str4, 
     1		'  Has Health Physics surveyed and approved this [NO]? ', 
     1	       4,i1,i1,i1,ig,i1, contr) !i1 is dummy param for absoft
	     IF (str4(1:1) .NE. 'Y') GOTO 520
	     CALL Putcontr_NT(' Remember to change limits of motors 3 and 4.')
	     CALL Putcontr_NF(' ')
	     negqpos = .FALSE.
	     CALL Status_Line
	   ELSE
	     CALL Not_Implemented (*520)
	   ENDIF
	   GOTO 520
	END IF
	GOTO 270
	
C---------------------- R:  RI  RT  RQ RB  REL RM RA RF -ROCK-ROCKM-R+/-----------C

 180	IF (command(2:2) .EQ. 'I' .OR. command(2:2) .EQ. 'Q' .OR.
	1   command(2:2) .EQ. 'T' .OR. command(2:2) .EQ. 'B' .OR.
     1	    command(2:2) .EQ. 'R' .OR. command(2:2) .EQ. 'X' .OR.
     1	    command(2:2) .EQ. 'D') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)			   ! Run buffer
	   IF (command(3:5) .EQ. '/NF')THEN
	     keep = .FALSE.
	     command(1:) = command(1:2)//command(6:)  !remove /NF
	     iend = iend - 3
	   ELSE
	     keep = .TRUE.
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
	   inc = 1
	   IF (l .GT. m) inc = -1
	   ! DO NOTHING FOR XRAY
	   DO j = l, m, inc
	      CALL CLREF (contr_A)
	      IF (str4(1:1) .EQ. 'I'.and. nsta .ne. -7 .AND.
     1		  nsta .NE. 8) THEN
	         CALL Run_I_Buf (j,keep)
	      ELSE IF (str4(1:1) .EQ. 'Q' .AND. nsta .NE. 1 .and.
     1	          nsta .ne. -7 .AND. nsta .NE. 8) THEN
	         CALL Run_Q_Buf (j,keep)
	      ELSE IF (str4(1:1) .EQ. 'B' .AND. nsta .NE. 1 .and. 
     1	          nsta .ne. -7 .AND. nsta .NE. 8) THEN
	         CALL Run_B_Buf (j,keep)
	      ELSE IF (str4(1:1) .EQ. 'T' .AND. nsta .EQ. 4) THEN
	         CALL Run_T_Buf (j,keep)
	      ELSE IF (str4(1:1) .EQ. 'X' .AND. nsta .EQ. 8) THEN
		  dryrun = .FALSE.
	         CALL Run_X_Buf (j,dryrun,keep)
	      ELSE IF (str4(1:1) .EQ. 'D' .AND. nsta .EQ. 8.OR. nsta.EQ.-5.or.nsta.eq.9.or.nsta.eq.-11) THEN
		  dryrun = .FALSE.
	         CALL Run_D_Buf (j,dryrun,keep)
	      ELSE IF (str4(1:1) .EQ. 'R' .AND. nsta .EQ. -7) THEN
	         IF (wavel .eq. 0. .OR. dm .eq. 0.) THEN
	            response(1:) = ' Wavelength, dm, qz cannot be 0.'
		    CALL Putline_NT (contr, response)
		    GOTO 520
	         ELSE
		    noback = .TRUE. !set flag to ignore backlash correction
	            CALL Run_R_Buf (j,keep)
		    noback = .FALSE.
	         ENDIF
	      ELSE
	         response(1:)  = ' Sorry, this is an invalid command!'
		 CALL Putline_NT (contr, response)
	      	 help_line(1) = ' The requested buffer does not exist on this instrument!'
	         IF (system_type .eq. DEC) THEN
	           help_line(2) = ' Type HELP for commands '
	         ELSE
	           help_line(2) = ' See online icp manual for commands '
	         ENDIF
		 help_line(3) = ' '
		 CALL Put_Help (help_line)
		 ! DO NOTHING FOR XRAY
		 GOTO 520
	      END IF

           maxtry = 0
	   OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
     1	    ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	     CALL libwait(.2,status)
	     OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
     1	      ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
	     maxtry = maxtry + 1
	   ENDDO
	   IF (ios .EQ. 0)THEN
	     CALL DATE_TIME (sys_time)
	     write(unit=2,fmt='(1x,a17,1x,a)',err=1209)sys_time(1:17),
     1	       'End of Run'
 1209	     close(unit=2,err=1210)
	   ENDIF

 1210	      IF (system_type .eq. DEC) THEN
	        help_line(1) = ' Type HELP for commands '
	      ELSE
	        help_line(1) = ' See online icp manual for commands '
	      ENDIF
	      help_line(2) = ' '
	      help_line(3) = ' '
	      IF (READEF (contr_A))THEN
	         CALL IcpAbort(command, *520)
	      ENDIF !contr_a
	   END DO
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELM' .OR. command(2:4) .EQ. 'ELD') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR. nsta.eq.0) CALL Not_Implemented (*520)
	   IF (motfl(2)) THEN                                    ! Release Drum
	      IF (fixed(2)) THEN
		 fixed(2) = .FALSE.
	      ELSE
	         response='Motor 2 was not fixed'
		 CALL Putline_NT (contr,response)
	      END IF
	   ELSE
	      response='Motor 2 not declared on this instrument'
	      CALL Putline_NT (contr,response)
	   END IF
	   IF (nsta.eq.-5)THEN !SPINS
	     IF (motfl(1)) THEN                                    ! Release Drum
	      IF (fixed(1)) THEN
		 fixed(1) = .FALSE.
	      ELSE
		 response='Motor 1 was not fixed'
		 CALL Putline_NT (contr,response)
	      END IF !fixed
	     ELSE !not motfl
	       response='  Motor 1 not declared on this instrument'
	       CALL Putline_NT (contr,response)
	     END IF !motfl 1
	     IF (motfl(7)) THEN                                ! Release Drum
	      IF (fixed(7)) THEN
		 fixed(7) = .FALSE.
	      ELSE
		 response='  Motor 7 was not fixed'
		 CALL Putline_NT (contr,response)
	      END IF !fixed
	     ELSE !not motfl
	       response='Motor 7 not declared on this instrument'
	       CALL Putline_NT (contr,response)
	     END IF !motfl 1
	   ENDIF !SPINS
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELT'.or.command(2:4).eq.'ELS') THEN   ! Release Table
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR.nsta.eq.0) CALL Not_Implemented (*520)
	   IF (motfl(3)) THEN
	      IF (fixed(3)) THEN
		 fixed(3) = .FALSE.
		 CALL Status_Line
	      ELSE
		 response = 'Angle 3 was not fixed'
		 CALL Putline_NT (contr, response)
	      END IF
	   ELSE
	      response = 'Motor no. '// command(4:4) //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ELA') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (iabs(nsta).eq.7.or.iabs(nsta).eq.1.or.
     1		nsta.eq.8 .OR. nsta.eq.0) CALL Not_Implemented (*520)
	   IF (motfl(6)) THEN                                ! Release Analyzer
	      IF (fixed(6)) THEN
		 fixed(6) = .FALSE.
	      ELSE
		 response = 'Motor 6 was not fixed'
		 CALL Putline_NT (contr, response)
	      END IF
	   ELSE
	      response = 'Motor 6 is not declared on this instrument'
	      CALL Putline_NT (contr, response)
	   END IF
	   IF (motfl(5)) THEN                                ! Release Analyzer
	      IF (fixed(5)) THEN
		 fixed(5) = .FALSE.
	      ELSE
		 response = 'Motor 5 was not fixed'
		 CALL Putline_NT (contr, response)
	      END IF
	   ELSE
	      response = 'Motor 5 is not declared on this instrument'
	      CALL Putline_NT (contr, response)
	   END IF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'EL') THEN
	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) m
	   IF (m .lt. 1 .or. m .gt. MAXMOTS) GOTO 270
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	   IF (motfl(m)) THEN    
	      IF (fixed(m)) THEN
		 fixed(m) = .FALSE.
		 CALL Status_Line
	      ELSE
		 response = 'That motor was not fixed'
		 CALL Putline_NT (contr, response)
	      END IF
	   ELSE
	      response = 
     1		'That motor is not served on this instrument'
	      CALL Putline_NT (contr, response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'A' .OR. command(2:2) .EQ. 'M' .OR.
     1		 command(2:2) .EQ. 'F') THEN !measure flipping ratio
    	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1        'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), 
     1			file, statistic, tempctr, 
     1	               polarized, psd,
	1              tdev, magnctr
	   READ (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2   	       (f_on(i)%engy, i=1, pols)
	   CLOSE (1)
	   IF (pols .EQ. 0) THEN
	      response(1:) = 'No power supplies enabled on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF 
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. -5 .and. use_asd)use_asdfile = .false.
	   iend = 2 !default is two counting configs
	   IF (command(2:2) .EQ. 'F')iend = 3 !3 configs for "both" command
	   DO i = 1, iend
C  set flippers to configuration corresponding to number in i 
	     IF (i .EQ. 1)THEN  !set flippers according to which config #
	       IF (command(2:2) .EQ. 'M' ) THEN       !set mono flippers
	         CALL Set_Flippers(8)            
	         response = 'Flipper OFF'
	       ELSEIF (command(2:2) .EQ. 'A') THEN   !set anal flippers 
	         CALL Set_Flippers(6) 
	         response = 'Flipper OFF'
	       ELSE   !both flippers
	         CALL Set_Flippers(8)                !set mono flippers
	         CALL Set_Flippers(6)                !set anal flippers
	         response = 'Both Flippers OFF'
c		 data = NINT(data/10.)
c		 IF (data .EQ. 0.) data = 1.
	       ENDIF
	     ELSEIF (i .EQ. 2)THEN !second config
	       IF (command(2:2) .EQ. 'M') THEN       !set mono flippers
	         CALL Set_Flippers(7)
	         response = 'Flipper ON'
	       ELSEIF (command(2:2) .EQ. 'A') THEN   !set anal flippers 
	         CALL Set_Flippers(5)
	         response = 'Flipper ON'
	       ELSE   !working with both flippers
		 data = data * 10.
	         CALL Set_Flippers(7)                 !set mono flippers
	         response = 'Mono Flipper ON'
	       ENDIF
	     ELSEIF (i .EQ. 3)THEN  !third config, Must be both flippers.
	       CALL Set_Flippers(8)                 !set mono flipper OFF
	       CALL Set_flippers(5) 		    !set anal flippers ON
	       response = 'Analyzer Flipper ON'
	     ENDIF !i: config number
	     CALL Putline_NT (contr, response)
	     CALL CLREF (contr_A)
C  Now go count
	     IF (data .LT. 0) THEN                              ! Count with time
	       time = ABS(data)
	       WRITE (UNIT=st7(1), FMT= '(i7)',iostat=ios) NINT(time)
	       response = 'Timer set to: '// st7(1) //' sec.   Count initiated!'
	      CALL Reset_Display       !Reset NIM count display
	       CALL RlyOff (4)                           ! Start display counter
	       IF (nsta .EQ. -5 .AND. use_asd) THEN
		 CALL zero_asd
		 CALL asd_timed(1,time)
	       ENDIF
	       CALL CnTime (time)
	       CALL Putline_NT (contr, response)
	       CALL Scaler (counts)
	       IF (nsta .eq. -5 .and. use_asd)counts(2)=NINT(asd_data(1))
	       IF (nsta.eq.-11.and.use_asd)	counts(scl+1)=NINT(fpwindow)

	       CALL RlyOn (4)			         ! Stop display counter
	       IF (READEF(contr_A))CALL IcpAbort(command, *520)
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (1)
	       response = 'Time: '// st7(1) //'   Monitor: '// str9
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (2)
	       response = response(1:34) //' Counter_1: '// str9
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (3)
	       response = response(1:55) //' Counter_2: '// str9
	       CALL Putline_NT (contr, response)
	     ELSE                                            ! Count with monitor
	       preset = data
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) NINT(preset)
	       response = 'Monitor set to: '// str9//'   Count initiated!'
	       CALL Reset_Display       !Reset NIM count display
	       CALL RlyOff (4)                           ! Start display counter
	       IF (nsta .EQ. -5 .AND. use_asd) THEN
		 CALL zero_asd
		 CALL asd_on(1)
	       ENDIF
	       CALL Count (preset)
	       CALL Putline_NT (contr, response)
	       CALL Scaler (counts)
	       IF (nsta .eq. -5 .and. use_asd)counts(2)=NINT(asd_data(1))
	       IF (nsta.eq.-11.and.use_asd)	counts(scl+1)=NINT(fpwindow)
	       CALL RlyOn (4)			         ! Stop display counter
	       IF (READEF(contr_A))CALL IcpAbort(command, *520)
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (1)
	       response = 'Monitor: '// str9
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (2)
	       response = response(1:19) // '   Counter_1: ' // str9
	       WRITE (UNIT= str9, FMT= '(i9)',iostat=ios) counts (3)
	       response = response(1:42) // '   Counter_2: ' // str9
  	       CALL Putline_NT (contr, response)
	     END IF      
	     IF (i .eq. 1) THEN
	       pos1 = counts(2) !remember counts from first config
	     ELSEIF (i .eq. 2) THEN
	       pos2 = counts(2) !remember counts from 2nd config
	     ENDIF
	   END DO ! i = 1 , iend

C  Now compute flipping ratios from pos1, pos2, and counts(2)
	   IF (command(2:2) .EQ. 'F')THEN
	     IF (pos2.ne.0.)THEN !avoid division by zero
	       WRITE (UNIT= str13(1:),FMT= '(f7.2)',iostat=ios)(pos1*10.)/pos2
	       response = 'Flipping Ratio RM: '//str13
	     ELSE
	       response = 'RM undefined - division by 0'
	     ENDIF
	     IF (counts(2).ne.0.)THEN !avoid division by zero
  	       CALL Putline_NT (contr, response)
	       WRITE (UNIT= str13(1:),FMT= '(f7.2)',iostat=ios)(pos1*10.)/counts(2)
	       response = 'Flipping Ratio RA: '//str13
	     ELSE
	       response = 'RA undefined - division by 0'
	     ENDIF
	   ELSE !command is ra or rm
	     IF (counts(2).ne.0)THEN
	       WRITE (UNIT= str13(1:),FMT= '(f7.2)',iostat=ios)(pos1/pos2)
	       response = 'Flipping Ratio: '//str13
	     ELSE
	       response = 'Zero counts first flipper state!'
	     ENDIF
	   ENDIF !which command
  	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:5) .EQ. 'OCKH') THEN                    ! Rock MAG
	   IF (.NOT. minit_done) CALL NoMinit (*520)
	   m = 6
	   keep = .TRUE. ! Turn on file creation and archiving, no matter what
	   arch = .TRUE.
	   DO WHILE (command(m:m) .EQ. '/')
	     IF (command(m+1:m+1).eq.'F')then !make file
	       keep=.TRUE.
	       m = m+2
	     ELSEIF (command(m+1:m+1).eq.'A')then !archive file
	       arch = .TRUE.
	       keep=.TRUE.
	       m = m+2
	     ENDIF
	   ENDDO
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
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
	   CALL CLREF (Contr_A)
	   CALL ROCK_H (data, range, step, mon, keep,arch)
	   IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'OCK') THEN                        ! Rock
	   keep = .TRUE.  ! Turn on file creation and archiving, no matter what
	   move = .FALSE.
	   arch = .TRUE.
	   m = 5
	   np = 4
	   DO WHILE (command(m:m) .EQ. '/')
	     IF (command(m+1:m+1).eq.'F')then !make file
	        keep=.TRUE.
	        m = m+2
	     ELSEIF (command(m+1:m+1).eq.'A')then !archive file
	       arch = .TRUE.
	       keep=.TRUE.
	       m = m+2
	     ELSEIF (command(m+1:m+1).eq.'X')then !drive to max inten
	        move = .TRUE.
	        m = m+2
		np = 5
	     ELSE
	        GOTO 270 !syntax error
	     ENDIF
	   ENDDO
	   ios = 1
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
	   IF (nsta .eq. 1 .AND. .not. super .and. mot .ne. 3 .and. 
     1		mot .ne. 4) CALL NoSuper (*520)
	   IF (nsta .eq. -11 .AND. .not. super .and. .not.
	1	((mot .ge. 1 .and. mot .le. 8) .or.
	1	(mot .ge. 13 .and. mot .le. 16))) CALL NoSuper (*520)
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
           IF (np .GT. 4) THEN
	     toler   = multi(5)				  ! (5) is toler
	   ELSE
	     toler = 0.01
	   ENDIF
c	   IF ( range .LT. .005 ) range = 1.
c	   IF ( step  .LT. .005 ) step = .1
c	   IF ( abs(mon) .LT. .5 ) mon = -1.
	   command(1:) = command (m:)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
	   IF (motfl(mot)) THEN
	      CALL CLREF (Contr_A)
	      CALL ROCK (mot, range, step, mon, keep, arch, move, toler)
	      IF (READEF(contr_A))CALL IcpAbort(command, *520)
	   ELSE
	      response = 'That motor is not available on this instrument!'
	      CALL Putline_NT (contr, response)
	   END IF
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN    ! Enable Rock during count
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.1) CALL Not_Implemented (*520)
	   rockfl = .FALSE. !default
 2025	   CALL Read_STRING(keyb, str9,'Enter number of motor to rock: ',
     1		9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=2025)rkmot
	     IF (rkmot .NE. 3 .OR. .not.motfl(rkmot))THEN
	        response(1:) = 'Invalid motor number'
	        CALL Putline_NT (contr, response)
	        CALL Putline_NT(contr,' ')
	        CALL Status_Line
	        GOTO 520
	     ENDIF
	     IF (fixed(rkmot))THEN 
		response(1:) = 
     1		 'WARNING: that motor is currently FIXED and will not move.'
	        CALL Putline_NT (contr, response)
	     ENDIF
 2035	   CALL Read_STRING(keyb, str9,'Enter lower rocking boundary: ',
     1		9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=2035)rkpos1
	     pos1 = rkpos1 + z(rkmot)
	     IF (.NOT. inlimits(rkmot,pos1,contr))THEN
	         CALL Putline_NT(contr,' ')
	         CALL Status_Line
		 GOTO 520
	     ENDIF
 2045	   CALL Read_STRING(keyb, str9,'Enter upper rocking boundary: ',
     1		9,i1,i1,i1, ig,i1, contr) !i1 is dummy param for absoft
	     READ(unit=str9(1:ig),fmt=*,err=2045)rkpos2
	     pos2 = rkpos2 + z(rkmot)
	     IF (.NOT. inlimits(rkmot,pos2,contr))THEN
	         CALL Putline_NT(contr,' ')
	         CALL Status_Line
		 GOTO 520
	     ENDIF
	   CALL Putline_NT(contr,' ')
	       
	   rockfl = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN    ! Disable Rock during count
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.1) CALL Not_Implemented (*520)
	   rockfl = .FALSE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'J') THEN ! Run traJectory
	   append=.false.
	   m = 3
	   DO WHILE (command(m:m) .EQ. '/')
	     IF (command(m+1:m+1).eq.'A')then !append file
	        append=.TRUE.
	        m = m+2
             ENDIF
           ENDDO
	   do while ((command(m:m).eq.'=').or.(command(m:m).eq.' '))
	      m = m+1
	   enddo
	   if (m .ge. iend) goto 520
	   keep=.true.
	   CALL RUN_TRJ(command(m:iend),.false.,keep)
	   goto 520
	ELSE IF (command(2:7) .EQ. 'ESTORE') THEN ! RESTORE
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. trackmoves) CALL Not_Implemented(*520)
	   call restore_fromfile(0,.true.)
	   GOTO 520
	END IF
	GOTO 270


C------------------ S: SETP STAT  SET  SM  ST  S-/+ SF SL--SC-SUPER ---------C
C----------------------SETDEM--SETDEA----------------------------------------C

 190    IF (command(2:4) .EQ. 'ETP') THEN     ! Set temperature controller
	   READ (UNIT= command(5:5), FMT= '(i1)', ERR= 270) j
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   IF (j .EQ. 1 .OR. j .EQ. 2) THEN
	      IF (temptype.eq.4 .OR. temptype .EQ. 3 .OR. 
     1		  temptype .eq. 10 .OR. temptype .eq. 18) GOTO 270 !no default: lr400, lk330, omega 6090
	      CALL WrPar (j, params)	
	      IF (j .EQ. 1) THEN
	         response = 'Default parameters for high temperature set!'
	      ELSE
		 response = 'Default parameters for low temperature set!'
	      END IF
	      CALL Putline_NT (contr, response)
	   ELSE IF (j .EQ. 0) THEN
	      j = 6
	      DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
		 j = j + 1
	      END DO
	      IF (temptype .eq. 4 .OR. temptype .EQ. 7 .OR. temptype .eq. 18) THEN !lr400, lr700, omega6070
		  np=2
	      ELSEIF (temptype .eq. 16) THEN !ltc21
		  np=5
	      ELSE
		  np=4
	      ENDIF
	      READ (UNIT= Command(j:iend), FMT= *, ERR= 270) (multi(i), i=1,np)
	      params(1) = multi(1)
	      params(2) = multi(2)
	      params(3) = multi(3)
	      params(4) = multi(4)
	      params(5) = multi(5)
	      response(1:23) = 'Controller Parameters: '
	      IF (temptype.eq.4 .OR. temptype.eq.7) THEN !lr400 or lr700
	        WRITE (UNIT= response(24:), FMT= '(a,f10.2,a,f10.2)',iostat=ios)
	2           '  Range= ', params(1), '  Excitation= ', params(2)
	      ELSEIF (temptype.eq.18) THEN !omega 6070
	        WRITE (UNIT= response(24:), FMT= '(a,f10.2,a,f10.2)',iostat=ios)
	2           '   Gain= ', params(1), '  Rate/Reset= ', params(2)
		IF (params(1) .lt. 1 .or. params(1) .gt. 400)THEN
		  CALL Putcontr(' Allowed values for Gain: 1-400')
		  GOTO 270
		ENDIF
		IF (params(2) .lt. 0 .or. params(1) .gt. 255)THEN
		  CALL Putcontr(' Allowed values for Rate/Reset: 0-250')
		  GOTO 270
		ENDIF
	      ELSEIF (temptype .EQ. 3) THEN !lk330
		IF (params(4).LT. 0. .OR. params(4) .GT. 3)GOTO 270
		IF (params(1) .LT. 0.) THEN !autotune
		  IF (params(3) .GT. 0. .AND. params(2) .GT. 0)THEN
		    params(5)=1. !P: gain only
	            WRITE (UNIT= response(24:), FMT= 
     1			'(a8,f5.1,a9,f5.1,a10,f3.0)',iostat=ios)
	2           '  Rate= ', params(2), 
	3	    '  Reset= ', params(3), '  Range = ', params(4)
		  ELSEIF (params(3) .LT. 0. .AND. params(2) .GT. 0)THEN
		    params(5)=2. !PI: gain,reset
	            WRITE (UNIT= response(24:), FMT= 
     1			'(a8,f5.1,a10,f3.0)',iostat=ios)
	2           '  Rate= ', params(2), 
	3	    '  Range = ', params(4)
		  ELSEIF (params(3) .LT. 0. .AND. params(2) .LT. 0)THEN
		    params(5)=3. !PID: gain, reset, rate
	            WRITE (UNIT= response(24:), FMT= '(a10,f3.0)',iostat=ios)
	3	    '  Range = ', params(4)
		  ELSE !invalid autotune request
		    CALL Putcontr_NT(' Can only autotune on  P,PI, or PID')
		    GOTO 270
		  ENDIF
		ELSE !do not autotune
		  params(5) = 0. !params(5) used as autotune command
	          WRITE (UNIT= response(24:), FMT= 
     1		   '(a8,f5.1,a8,f5.1,a9,f5.1,a10,f3.0)',iostat=ios)
	2           '  Gain= ', params(1), '  Rate= ', params(2), 
	3	    '  Reset= ', params(3), '  Range = ', params(4)
		  ENDIF
	      ELSEIF (temptype .EQ. 10) THEN !lk340
	        WRITE (UNIT= response(24:), FMT= 
     1		  '(a8,f5.1,a8,f5.1,a9,f5.1,a10,f3.0)',iostat=ios)
	2           '  Gain= ', params(1), '  Rate= ', params(2), 
	3	    '  Reset= ', params(3), '  Range = ', params(4)
	      ELSEIF (temptype .EQ. 16) THEN !ltc21
	        WRITE (UNIT= response(24:), FMT= 
     1		  '(a6,I5,a6,I5,a7,I4,a7,I3,a8,I5)',iostat=ios)
	2           ' Gain=', NINT(params(1)), ' Rate=', NINT(params(2)), 
	3	    ' Reset=', NINT(params(3)), ' Range=',NINT(params(4)),
	4	     ' Limit%=',NINT(params(5))
	      ELSE !not lr400,lr700,or lk330, or lk340
	        WRITE (UNIT= response(24:), FMT= 
     1		  '(a8,f5.1,a8,f5.1,a9,f5.1,a10,f3.0)',iostat=ios)
	2           '  Gain= ', params(1), '  Rate= ', params(2), 
	3	    '  Reset= ', params(3), '  Power = ', params(4)
	      ENDIF !lr400
	      CALL Putline_NT (contr, response)
	      IF (temptype.EQ.3)CALL Putcontr_NT(' neg value indicates to autotune')
  	      CALL WrPar (j, params)
	      DO i = 1, 3
		 m = INDEX (command(1:), ',')
	         command(1:) = command (m+1:)
	      END DO
	   ELSE 
	      GOTO 270
	   END IF
	   GOTO 520	      
        ELSE IF (command(2:4) .EQ. 'ETR') THEN           ! Set ramp
	   !!! NCM ADDITION
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   j = 5
	   IF (command(j:j) .EQ. '=') j = 6
	   READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	   CALL WRRAMP(data)   
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'TAT') THEN           ! Print current status
	   OPEN (UNIT= 1, FILE= STATUS_ICP, STATUS= 'OLD', 
	1        ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= STAT_RECL, 
	2        ERR= 191)
	   DO j = 1, 3
	      READ (1,REC=j) response
	      CALL Putline_NT (contr, response)
	   END DO
	   CLOSE (1)
#if !defined(VMS)
	   CALL STATUS_LINE
#endif
 191	   GOTO 520	
	ELSE IF (command(1:2) .EQ. 'SC') THEN  ! SC or SCR Send to magnet
	   IF (command(3:3).EQ.'R')THEN !get reply
	     j = 4
	     lresp = .TRUE.
	   ELSE
	     j = 3
	     lresp = .FALSE.
	   ENDIF
	   sc_command(1:)=command(j:iend)
	   lcom=iend-2
	   call sc232(sc_command,lcom,sc_reply,lreply,lresp)
	   call Putline_NT(contr,sc_reply(1:lreply))
	   GOTO 520	
	ELSE IF (command(2:6) .EQ. 'ETDEM') THEN    !Change zeros for mono energy
	   IF (nsta.eq.8 .or. nsta .eq. 1) Call Not_Implemented (*520)
	   IF (.NOT. motfl(2)) THEN
	      response = 'The drum cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   ENDIF
	   IF (nsta .EQ. 4 .AND. (z(2) .EQ. -99. .OR. z(1) .EQ. -99.) )THEN
	      CALL Read_STRING (keyb, str4, 
     1		'  Overwrite linear zero flag [NO]? ', 4,
     1		i1,i1,i1,ig,i1, contr) !i1 is dummy param for absoft
	      IF (str4(1:1) .NE. 'Y') GOTO 520
	   ENDIF
	    j = 7
	    IF (command(j:j) .EQ. '=') j = 8
	    READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	    pos1 = Energy_Angle (data, dm)
	    pos2 = 2. * pos1
	    z2 = fz(2,pos2) !get linear z for motor 2 on BT4
	    IF (.NOT. inlimits(2,pos2+z2,contr))THEN
		CALL Putcontr_NT(' Angle 2 would exceed limits.')
	    ELSE 
	      OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	      READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	      IF (motfl(1)) THEN 
	         z1 = fz(1,pos1) !get linear z for motor 1 on BT4
	         IF (.NOT. inlimits(1,pos1+z1,contr))THEN
		    CALL Putcontr_NT ('Angle 1 would exceed limits.')
	         ELSE
	           z(2) = RdAngl(2) - pos2
	           z(1) = RdAngl(1) - pos1
		   if (trackmoves)call memcurpos(1,0,z(1))
		   if (trackmoves)call memcurpos(2,0,z(2))
	           st7(3) = PutStr (pos2)
	           response(1:) = '     2-theta MONO set to '// st7(3)
	           CALL Putline_NT (contr, response)
	           st7(3) = PutStr (pos1)
	           response(1:)= '    theta MONO set to '// st7(3)
	           CALL Putline_NT (contr, response)
		 END IF !motor 1 inlimits
	      ELSE !motor 1 not in use
	          z(2) = RdAngl(2) - pos2
		  if (trackmoves)call memcurpos(2,0,z(2))
	          st7(3) = PutStr (pos2)
	          response(1:) = '     2-theta MONO set to '// st7(3)
	          CALL Putline_NT (contr, response)
	      END IF !motor 1 in use
	      WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	      CLOSE (1)
	    END IF !motor 2 inlimits
	   GOTO 520 
	ELSE IF (command(2:6) .EQ. 'ETDEA') THEN   ! Change zeros for ana energy
	   IF (nsta.eq.8 .or. nsta.eq.1) Call Not_Implemented (*520)
	   IF (.NOT. (motfl(5) .AND. motfl(6))) THEN
	      response = 'The analyzer cannot be changed on this instrument'
	      CALL Putline_NT (contr, response)
	   ELSE    
	      j = 7
	      IF (command(j:j) .EQ. '=') j = 8
	      READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	      pos1 = Energy_Angle (data, da)
	      pos2 = 2. * pos1
	      IF (.NOT. inlimits(5,pos1+z(5),contr))THEN
	        CALL Putcontr_NT('Angle 5 would exceed limits.')
	      ELSE IF (.NOT. inlimits(6,pos2+z(6),contr))THEN
	        CALL Putcontr_NT('Angle 6 would exceed limits.')
	      ELSE
	        OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	        READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	        z(6) = RdAngl(6) - pos2
	        z(5) = RdAngl(5) - pos1
	        WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	        CLOSE (1)
		if (trackmoves)call memcurpos(6,0,z(6))
		if (trackmoves)call memcurpos(5,0,z(5))
	        st7(3) = PutStr (pos2)
		response(1:) = '     2-theta  ANA set to '// st7(3)
		CALL Putline_NT (contr, response)
	        st7(3) = PutStr (pos1)
                response(1:) = ' theta  ANA set to '// st7(3)
		CALL Putline_NT (contr, response)
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
	      L = L + 1
	   END DO
	   READ (UNIT= command(j:L-1), FMT= *, ERR=270) m
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (m .le. 0 .or. m .gt. MAXMOTS)goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. m .ne. 3 .and. 
     1		m .ne. 4) CALL NoSuper (*520)
	   IF (nsta .eq. -11 .AND. .not. super .and. .not.
	1	((m .ge. 1 .and. m .le. 8) .or.
	1	(m .ge. 13 .and. m .le. 16))) CALL NoSuper (*520)
 	   IF (.NOT. motfl(m)) THEN
	      write(unit=str2,fmt='(i2)')m
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   ENDIF
	   IF (nsta .EQ. 4 .AND. m .LE. 2 .AND. z(m) .EQ. -99.)THEN
	      CALL Read_STRING (keyb, str4, 
     1		'  Overwrite linear zero flag [NO]? ', 4,
     1		i1,i1,i1,ig,i1, contr) !i1 is dummy param for absoft
	      IF (str4(1:1) .NE. 'Y') GOTO 520
	   ENDIF
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	   j = L
	   IF (command(j:j) .EQ. '=')j=j+1
	   IF (command(j:j) .EQ. 'V')THEN !have a variable
	     CALL Getval(command(j:),data,*270)
	   ELSE
	     DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	         j = j + 1
	     END DO
	     READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   ENDIF
	   z(m) = RdAngl(m) - data
	   WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	   CLOSE (1)
	   if (trackmoves) call memcurpos(m,0,z(m))
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'M') THEN                       ! Set magnet
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   ios = 1
	   np = 2
	   DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(j:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	   ENDDO
	   IF (np .LT. 2) multi(2) = 0.  !let default be Ross wait
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. minit_done) CALL NoMinit (*520)
	   IF (magtype.eq.0) THEN
	      response(1:) = '*** Mag type undefined. Use HDEV.***'
	   ELSEIF (magtype.eq.1 .and. tdev%mot .EQ. 0) THEN
	      response(1:) = 'There is no motor controller assigned!'
	   ELSE 
	      write(unit=str13,fmt='(f13.4)',err=270)multi(1)
	      range = multi(1) / hfconv
	      write(unit=str14,fmt='(f14.4)',err=270)range
	      IF (abs(range) .GT. fieldmax)THEN
		response(1: ) = 'ERROR!! '//str14//' Amps  out of range'
	      ELSE
	        response(1:) = 'Magnet being set to Field: '// str13 //
     1		  '   = '//str14//' Amps: '
	        CALL CLREF (Contr_A)
	        CALL Setmag(range,multi(2),lresp)
	        IF (READEF(contr_A))CALL IcpAbort(command, *520)
	      ENDIF
	   END IF
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'T') THEN                  ! Set temperature
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   IF (.NOT. tcareful)THEN
	     READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	     CALL WrTemp (data)
	   ELSE !tcareful flag set
	     ios = 1
	     np = 3
	     DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	       READ (UNIT= command(j:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	       IF ( ios .NE. 0 ) np = np - 1
	     ENDDO
	     IF (np .LT. 1) THEN
	       GOTO 270 !error. must specify temp at least.
	     ELSE
	       data = multi(1) !first param is temp
	     ENDIF
	     IF (np .GT. 2)THEN
	       time = multi(3)  !third param is wait time
	     ELSE
	       time = 30. !make 30 minutes the default
	     ENDIF
	     IF (np .GT. 1)THEN
	       toler = multi(2)  !second param is error
	     ELSE
	       toler = ABS(.10 * data) !make 10% the default
	     ENDIF
	     montemp = tempctr !memorize current value of t flag
	     lresp = magnctr !memorize current value of h flag
	     tempctr = .TRUE.
	     magnctr = .FALSE.
	     CALL New_Temp_Field(data,data,0.,time,toler,0.,0.,0.,0,
     1		0.,0.,0.)
	     tempctr = montemp !restore memorized value of mag controller
	     magnctr = lresp !restore memorized value
	     CALL CLREF (stop) !in case wait was aborted
	   ENDIF !tcareful flag set
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN                ! Enable statistics
	   IF (PSD .OR. USE_ASD) THEN
     	    response= 'S+ not implemented with position sensitive detector'
	    CALL Putline_NT(contr,response) 
	    GOTO 520
	   ENDIF
	   IF (.NOT. priv) CALL NoPriv (*520)
	   statistic = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN               ! Disable statistics
	   IF (.NOT. priv) CALL NoPriv (*520)
	   statistic = .FALSE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'L') THEN               !NG7. Slits
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend) , FMT= *, ERR= 270) (multi(k), k= 1, 4)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.ne.-7)CALL Not_Implemented(*520)
	   DO k = 1,4 !do for each slit
	     m = slitlist(k)
	     IF (.NOT. motfl(m)) THEN
		write(unit=str2,fmt='(i2)')m
	       response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
C	       CALL Putline_NT (contr, response)
	       GOTO 520
	     ELSE
	       data = multi(k)
	       pos1 = z(m) + data                              ! Normal drive
	       IF (.NOT. inlimits(m,pos1,contr))THEN
	         CALL Putcontr_NT('Drive not executed.')
	       ELSE
		 CALL CLREF (Contr_A)
		 CALL Drive (m, pos1)
		 CALL Yield
	         IF (READEF(contr_A))CALL IcpAbort(command, *520)
	       END IF
	     ENDIF !motfl
	   END DO !for each slit
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'F') THEN               ! NG7. Scalefactor
	   j = 3
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .ne. -7) CALL Not_Implemented (*520)
	   IF (eff(1) .NE. data) THEN
	     eff(1) = data
	     OPEN (UNIT=1,file=SCLFAC_DAT,status='old',
#if defined(VMS)
     1	      err=195,dispose='delete')
#else
     1		err=195)
#endif
	     CLOSE(UNIT=1,err=195)
 195	     OPEN (UNIT=1, FILE= SCLFAC_DAT,STATUS='NEW',ACCESS=
     1	         'SEQUENTIAL',FORM='FORMATTED')
	     WRITE(1,*)EFF(1)
	     CLOSE(UNIT=1)
	   ENDIF
	   GOTO 520
        ELSEIF (command(2:5) .EQ. 'UPER') THEN     ! Become super user
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT.Password_OK())CALL WrongPass (*520)
	   super = .TRUE.
	   response(1: ) = ' '
	   CALL Putline_NT (contr, response)
	   CALL Status_Line
	   GOTO 520	      
	ELSE IF (command(2:3) .EQ. 'AY') THEN            !Say
	   command(1:)=command(4:)
	   CALL Putcontr_NT(command)
	   GOTO 520
	END IF
	GOTO 270


C------------------------------- T: TDEV  T-/+ TC-/+--TR-/+-----------------C

 200	IF (command(2:4) .EQ. 'DEV') THEN                  ! Define temp device
	   IF (.NOT. priv) CALL NoPriv (*520)
	   Call Tconfig
	   IF (tinit_done)tempctr = .TRUE.
	   Call Status_Line
           GOTO 520
	ELSE IF (command(2:3) .EQ. 'C+') THEN             ! Temp careful enabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   IF (tdev%cfg .LE. 3 .OR. tdev%cfg .EQ. 10)THEN
	     tcareful = .TRUE.
	   ELSE
	     CALL Putcontr_NT(' Careful approach only possible with Lakeshore')
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'C-') THEN            ! Temp careful disabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   tcareful = .FALSE.
	   CALL Status_Line
	   GOTO 520      
	ELSE IF (command(2:2) .EQ. '+') THEN             ! Temp control enabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) Call Tconfig
	   IF (tinit_done)tempctr = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN            ! Temp control disabled
	   IF (.NOT. priv) CALL NoPriv (*520)
	   tempctr = .FALSE.
	   tcareful = .FALSE.
	   CALL Status_Line
	   GOTO 520      
	ELSE IF (command(2:3) .EQ. 'R+') THEN    ! Enable Trashmode
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.NE.4)CALL Not_Implemented (*520)
	   trashmode = .TRUE.
           IF (priv) THEN
	      CALL putcontr('TR+ flag set. '//
	1	   'Remember to move motor 4 to 46.0 deg')
C	       CALL Drive(4,50.0) ! Move the analyzer out of the way
C	       CALL Yield
           END IF

	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'R-') THEN     ! Disable Trashmode
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.NE.4)CALL Not_Implemented (*520)
	   trashmode = .FALSE.
	   CALL Status_Line
	   GOTO 520
	END IF
	GOTO 270


C---------------------------------- U: Ui UF----------------------------------C

 210 	IF (command(3:3) .EQ. '=' .or. command(4:4) .EQ. '=') THEN ! Change upper limits
	   IF (.NOT. priv) CALL NoPriv (*520)
	   L=2
	   DO WHILE (isdigit(command(L:L)).and.L.lt.iend)
	     L=L+1
	   ENDDO
	   READ (UNIT= command(2:L-1), FMT= *, ERR= 270) j ! determ angle no
	   IF (j .lt. 1 .or. j.gt. MAXMOTS)goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. j .ne. 3 .and. 
     1		j .ne. 4) CALL NoSuper (*520)
	   IF (.NOT. motfl(j)) THEN
	      write(unit=str2, fmt='(i2)')j
	      response = 'Motor no. '// str2 //
     1		' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF
	   READ (UNIT= command(L+1:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	   ul(j) = data
	   WRITE (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	   CLOSE (1)
	   GOTO 520
	END IF
	GOTO 270


C----------------------------- V: VER VERF VSET V-/+ VME-/+ ----------------C

 220	IF (command(2:4) .EQ. 'ERF') THEN
	   response = 'Collimation: '
	   WRITE (UNIT=response(14:29), FMT= '(4i4)',iostat=ios) (collim(i), i= 1, 4)
	   response = response(1:29) //'   Mosaic: '
	   WRITE (UNIT= response(41:55), FMT= '(3i5)',iostat=ios) (mosaic(i), i= 1, 3)
	   response = response(1:55) //'  Wavelength: '
	   WRITE (UNIT= response(70:75), FMT= '(f6.3)',iostat=ios) wavel
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:3) .EQ. 'ER') THEN
	   j = 4
	   DO WHILE (.NOT. isdigit(command(j:j)) .AND. j .LT. 50)
	      j = j + 1
	   END DO
	   READ (UNIT=command(j:iend), FMT= *, ERR= 270) data
	   def_ver = INT(data)
	   WRITE (UNIT= str4, FMT= '(i4)',iostat=ios) def_ver
	   response = ' Default run number: '// str4
	   CALL Putline_NT (contr, response)
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'LAT') THEN !vlat variable
	   j = 5
	   IF (command(j:j) .EQ. '=') j = j+1
	   IF (command(j:j) .EQ. 'A') THEN
	     vlat= lasta
	   ELSEIF (command(j:j) .EQ. 'B') THEN
	     vlat= lastb
	   ELSEIF (command(j:j) .EQ. 'C') THEN
	     vlat= lastc
	   ELSE
	     GOTO 270
	   ENDIF
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'SET') THEN              ! Vset for flippers
           READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 2)
	   mot   = IFIX (multi(1))			  ! (1) is power supply
	   IF (poltype(mot).eq.3) then !new Kepco needs limit
             READ (UNIT= command(5:iend), FMT= *, ERR= 270) (multi(i), i= 1, 3)
	   ENDIF
	   m = INDEX (command(1:), ',')
	   command(1:) = command (m+1:)
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (polfl(mot)) THEN
              range = multi(2)				  ! (2) is voltage
	      IF (poltype(mot).eq.3) then !new Kepco
	        step = multi(3) !limit
	      ELSE
	        step = 0.
	      ENDIF
	      step = multi(3)
	      WRITE (UNIT=response(1:), FMT= '(a,i1,a,f7.2)',iostat=ios)
	1           'Power supply ', mot, ':  Voltage set to ', range
	   ELSE
	      response(1:) = 
     1	      'That power supply is not enabled on this instrument!'
	   END IF
	   CALL Putline_NT (contr, response)
	   CALL Vset (mot, range, step)
	   GOTO 520
	ELSEIF (command(2:2) .eq. '+') THEN   !verbose_run
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   verbose_run = .true.
	   call status_line
	   goto 520
	ELSEIF (command(2:2) .eq. '-') THEN  !not verbose_run
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   verbose_run = .false.
	   call status_line
	   goto 520
	ELSE IF (command(2:4) .EQ. 'ME+') THEN    ! Enable VMEDATA
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.NE.4)CALL Not_Implemented (*520)
	   vmedata = .TRUE.
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:4) .EQ. 'ME-') THEN     ! Disable VMEDATA
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta.NE.4)CALL Not_Implemented (*520)
	   vmedata = .FALSE.
	   CALL Status_Line
	   GOTO 520
	END IF
	GOTO 270


C---------------------------------- W: WL W+/- WT------------------------------C

 230	IF (command(2:2) .EQ. 'L') THEN                            ! Wavelength
	   j = 3
	   IF (command(3:3) .EQ. '=') j = 4
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (nsta .eq. -1 .or. nsta .eq. -7) THEN
	      IF (.NOT.Password_OK())CALL Wrongpass (*520)
	   ENDIF
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1		(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   wavel = data
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
     1		(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '+') THEN            !W+  Enable logfile
c	   IF (nsta .EQ. 0) CALL Not_Implemented (*520)
	   IF (.NOT. priv) CALL NoPriv (*520)
	   makelog = .TRUE.
	   CALL StampLog(.TRUE.)
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. '-') THEN         !W- Disable logfile
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (makelog)THEN
	     CALL StampLog(.FALSE.) !write time log file disabled
	     makelog = .FALSE.
	   ENDIF
	   CALL Status_Line
	   GOTO 520
	ELSE IF (command(2:2) .EQ. 'T') THEN         !WT: Wait temp
	   IF (.NOT. priv) CALL NoPriv (*520)
	   ! Need to somehow remember the setpoint
	   CALL WAIT_TEMP(5, 60.0)
	   GOTO 520
	END IF
	GOTO 270


C---------------------------------- X: XCAL XDT -------------------------------C

 240	IF (command(2:3) .EQ. 'DT') THEN                                  ! XDT
	   IF (.NOT. priv) CALL NoPriv (*520)
	   IF (.NOT. tinit_done) CALL NoTinit (*520)
	   response(1:) = ' Be patient, it takes a while to read all those curves!' 
	   CALL Putline_NT (contr, response)
	   CALL Putline_NT (contr, ' ')
	   !CALL XdtSub (nopoints, curveid, rderr)
	   IF (.NOT. rderr) THEN
	      response(1:) = ' Temperature Controller Database Contents: '
	      CALL Putline_NT (contr, response)
	      CALL Putline_NT (contr, ' ')
	      response(1:) = '  Curve number     points     Sensor Label'//
     1		'      Settings for Switches 5 6 7 8'
	      CALL Putline_NT (contr, response)
	      i = 0
	      DO WHILE (i .LE. 15) 
	         IF (nopoints(i) .NE. 0) THEN
	            k = i
	            DO m = 3, 0, -1      ! define switch values to binary of curve no.
	               IF (k-2**m .GE. 0) THEN
	                  k = k-2**m
	                  ibit(i,m) = 1
	               END IF
	            END DO
	            WRITE (UNIT= str6(1)(1:2), FMT= '(i2.2)',iostat=ios) i
	            WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) nopoints(i)
	            WRITE (UNIT= str13(1:8), FMT= '(4(1x,i1))',iostat=ios) 
	1		  (ibit(i,m), m= 3, 0, -1)
		    response(1:6) = '      '
		    response(7:20) = str6(1)(1:2) 
		    response(21:34)= str6(2)(1:3) 
		    response(35:69)= curveid(i)(1:6)
		    response(70:77)= str13(1:8)
		    CALL Putline_NT (contr, response)              
	         END IF
	         i = i + 1
	      END DO
	   ELSE   
	      response(1:) = ' Error reading temperature controller '
	      CALL Putline_NT (contr, response)
	   END IF  !(* rderr *)
	   CALL Putline_NT (contr, ' ')
	   response(1:) = ' Sensor Assignements:  CRYOSTAT             SENSOR ID            STANDARD'
	   CALL Putline_NT (contr, response)	   	
	   OPEN (UNIT=1, FILE=trim(gethome())//
	1	'lakeshore/sensors/SENSORS.TABLE',
	1        STATUS='OLD')
	   READ (1, FMT='(a1)') response                          ! skip header
 241	   READ (1, FMT= *, END= 242) cryo, sensorid, standard
	   response(1:25)  = ' '
	   response(26:80) = cryo(1:20) // sensorid(1:20) // standard(1:20)
	   CALL Putline_NT (contr, response)
	   GOTO 241
 242	   CLOSE(1)
	   CALL Putline_NT (contr, ' ')
           GOTO 520
	ELSEIF (command(2:4) .EQ. 'MIN') THEN       !xmin for asd & psd
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1	ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
	2	asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   asd_xmin = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
	1	asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   CLOSE (1)
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'MAX') THEN       !xmax for asd & psd
   	   IF (.NOT. priv) CALL NoPriv (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
	2	asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   asd_xmax = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   CLOSE (1)
	   GOTO 520
	END IF
	GOTO 270

C---------------------------------- Y: Ymin, Ymax-----------------------------C

 250 	IF (command(2:4) .EQ. 'MIN') THEN       !ymin for asd
   	   IF (.NOT. priv) CALL NoPriv (*520)
C	   IF (nsta .ne. -5.or.nsta.ne.-11) CALL Not_Implemented (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   asd_ymin = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   CLOSE (1)
	   GOTO 520
	ELSEIF (command(2:4) .EQ. 'MAX') THEN       !ymax for asd
   	   IF (.NOT. priv) CALL NoPriv (*520)
C	   IF (nsta .ne. -5) CALL Not_Implemented (*520)
	   j = 5
	   IF (command(5:5) .EQ. '=') j = 6
	   READ (UNIT= command(j:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   asd_ymax = data
	   WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
     1		asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	   CLOSE (1)
	   GOTO 520
	END IF
	GOTO 270

C---------------------------------- Z: Zi ------------------------------------C

 260 	IF (command(3:3) .EQ. '='.or. command(4:4) .eq. '=') THEN ! Change zeropoints
	   IF (.NOT. priv) CALL NoPriv (*520)
	   L=2
	   IF (command(4:4) .eq. '=')L=3
	   READ (UNIT= command(2:L), FMT= *, ERR= 270) j ! determ angle no
	   IF (j .lt. 1 .or. j.gt.MAXMOTS) goto 270
	   IF (nsta .eq. 1 .AND. .not. super .and. j .ne. 3 .and. 
     1		j .ne. 4) CALL NoSuper (*520)
	   IF (nsta .eq. -11 .AND. .not. super .and. .not.
	1	((j .ge. 1 .and. j .le. 8) .or.
	1	(j .ge. 13 .and. j .le. 16))) CALL NoSuper (*520)
	   IF (.NOT. motfl(j)) THEN
	      write(unit=str2,fmt='(i2)')j
	      response = 'Motor no. '// str2 //
     1	      ' is not served on this instrument'
	      CALL Putline_NT (contr, response)
	      GOTO 520
	   END IF
	   READ (UNIT= command(L+2:iend), FMT= *, ERR= 270) data
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	   z(j) = data
	   WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	   CLOSE (1)
	   if (trackmoves) call memcurpos(j,0,z(j))
	   GOTO 520
	ELSE IF (command(2:2) .EQ. ',') THEN		       ! Call caculator
	   command(1:1) = '0'
	   CALL Calcul (command, *520)
	END IF
	GOTO 270


 999	CONTINUE
 270	response(1:) = ' Unrecognized command: ' // command(1:50)
	CALL Putline_NT (contr, response)
	IF (system_type .eq. DEC) THEN
	       help_line(1) = ' Type HELP for commands '
	ELSE
	       help_line(1) = ' See online icp manual for commands '
	ENDIF
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	RETURN 1
	END

