#include "icp.h"
	SUBROUTINE Run_D_Buf (buf_no,dryrun,keep)
C	*****************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes Dartz D Buffer No. buf_no                                    C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, Jan 2001    					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use histo
	use tc
	use polar
	use tdb
	use ad201
	use dartz
	IMPLICIT      none
	LOGICAL	      keep !true to keep data file
	LOGICAL	      dryrun, inlimits
	INTEGER*4     i, j, k, status, st, typ, ver, kpsd,ee,m, ip
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37)
	INTEGER*4     context,ig,i1,i2,ios, idum
	INTEGER*4     min, sec, polindex, right, left, hsum,iwhere
	REAL*4        af(10), posit(10), oldpos(10), RdAngl, dummy, secdiff
	REAL*4        elap_time, dsec, foc7, foc8, temp /0./,drytime/3./
	LOGICAL       limit, wraP, monmag, comerr, shutdone, alarm, Readef
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr, hstr
	CHARACTER*8   out, new_line /'        '/
	CHARACTER*9   str9
	CHARACTER*10   str10
	CHARACTER*12  fname, File_Name,str12(4)
	CHARACTER*20  datum
	CHARACTER*28  sys_time
	CHARACTER*80  response, info(4), error, help_line(3), subdir,
     1		       outpsd
	CHARACTER*80 longl
	REAL*4 fenergy, field, hlevel

	INTEGER*4	jr, curpoint, curstep(maxloops), steps_below(maxloops), 
     1			numsteps(maxloops), lastloop, numprf
	CHARACTER*1	str1
	LOGICAL		getnew, newpos(maxloops)
	LOGICAL		posout(MAXMOTS), tdbflag, realpsd

	TYPE(d_buf) dbuf
	TYPE(polarize) polcfg(5)

	finishup = .FALSE.
	OPEN (UNIT= 1, FILE= 'DBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', RECL= DBUF_RECL)
	READ (1,REC=buf_no) dbuf
	CLOSE (1)
	realpsd = psd !memorize if global psd flag set at start
	IF (tempctr .AND. (temptype .EQ. 12 .or. temptype .eq. 14) )THEN
	  tdbflag = .TRUE. !using transducer board
	ELSE
	  tdbflag = .FALSE. !not using transducer board
	ENDIF
         IF (PSD .and. dbuf%mpf.ne.1 .AND. .NOT. dryrun .AND. .NOT. tdbflag) THEN
	  dbuf%mon = dbuf%mon * dbuf%mpf !no pref  with position sensitive det.
	  dbuf%mpf=1
	 ENDIF
	CALL DATE_TIME (sys_time)
	info(2)(1:)=dbuf%com
C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	  IF (dbuf%ihf .NE. 0)monmag=.TRUE.
	  call putmax(dbuf%hf,str10,10,4)
	  info(2)(51:)=' H-field: '//str10
	ENDIF
C--------------------------- DETERMINE DATA FILE -----------------------------C
	IF (keep .AND. .NOT. dryrun) THEN                                     ! Data storage is on
	   fname = File_Name (dbuf%com(1:5))
	   Call getdir(subdir,right)			   ! Get name of subdir
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
C------------------------------- PRINT HEADER --------------------------------C
	CALL PUTLINE_NF (contr, new_line)
	CALL PUTLINE_NF (contr,sys_time(1:17))
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'dbuffer '// bufno // ':  '// dbuf%com
	CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, new_line)
	DO j = 1,MAXMOTS
	  posout(j) = .FALSE. !position has not been written
	ENDDO
	DO j= 1, numdmots 
	  jr = dbuf%mn(j)
	  IF (jr .NE. 0)THEN !a motor number has been supplied
	   IF (motfl(jr) .AND. .NOT. fixed(jr)) THEN
	      af(j) = dbuf%a(j) + (dbuf%lpts(j) - 1) * dbuf%i(j)
	      str7(1) = PutStr (dbuf%a(j))
	      str7(2) = PutStr (dbuf%i(j))
	      str7(3) = PutStr (af(j))
	      write(unit=str1,fmt='(I1)',IOSTAT=ios)dbuf%lpn(j)
	      WRITE (UNIT= out(1:2), FMT= '(i2)') jr
	      response = 'A'// out(1:2) //'-beg= '// str7(1) //'  Inc-'// 
	1		  out(1:2) //'= '// str7(2) //'  A'// out(1:2) //
	1		  '-end= '// str7(3)//' Loop #'//str1
 	      CALL PUTLINE_NT (contr, response)
	      posout(jr) = .TRUE.
	   END IF
	  ENDIF !jr ne 0
	END DO
	CALL Putmax(dbuf%tmp,str6(1),6,3)
	CALL Putmax(dbuf%it,str6(2),6,3)
	CALL putmax(dbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(dbuf%tw)
	call putmax(dbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') dbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN !magnetic field 
	  call putmax(dbuf%hf,str12(1),12,4)
	  call putmax(dbuf%ihf,str12(2),12,4)
	  write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(dbuf%hfw)
c	  call putmax(dbuf%hfw,str12(3),12,4)
	  call putmax(hfconv,str12(4),12,4)
	  response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1    ' Hwait= '//
	2    str12(3)//' Hconv= '//str12(4)
	  CALL PUTLINE_NT (contr, response)
	ENDIF !magnetic field
	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(dbuf%mon)
	WRITE (UNIT= out(1:4), FMT= '(i4)') NINT(dbuf%mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1        //'    Type= '// dbuf%mt 
	IF (keep .AND. .NOT. polarized .AND. .NOT. dryrun)
	2    response = response(1:46) //'           Datafile: '// fname
	CALL PUTLINE_NT (contr, response)
	polindex = 0
	polcfg(1)%fname = fname
	ip = 1 !multiply duration of buffer by this in check_channel
	IF (polarized) THEN
	   CALL PUTLINE_NT (contr, 'Polarization analysis enabled.')
	   CALL PUTLINE_NT (contr, 'The following cross section(s) will be taken:')
	   IF (keep .AND. .NOT. dryrun) READ (UNIT=fname(6:8), FMT= '(i3)') ver
	   ip = 0
	   DO j = 1, 4
	      IF (dbuf%flip_state(j)) THEN
	         ip = ip + 1
	         response(1:) = 'Flipper 1: '// flp(j,1) // '  Flipper 2: ' 
	1                       // flp(j,2) 
	         polindex = polindex + 1 
   		 polcfg(polindex)%csec  = j 
	         IF (keep .AND. .NOT. dryrun) THEN
	            polcfg(polindex)%fname = fname
	            WRITE (UNIT=polcfg(polindex)%fname(6:8), FMT= '(i3.3)') ver
	            IF (flp(j,1).eq.'OFF'.and.flp(j,2).eq.'OFF')THEN
			polcfg(polindex)%fname(11:11)='A'
		    ELSE IF (flp(j,1).eq.'ON '.and.flp(j,2).eq.'OFF')THEN
			polcfg(polindex)%fname(11:11)='B'
		    ELSE IF(flp(j,1).eq.'OFF'.and.flp(j,2).eq.'ON ')THEN
			polcfg(polindex)%fname(11:11)='C'
		    ELSE !flp(j,1).eq.'ON '.and.flp(j,2).eq.'ON '
			polcfg(polindex)%fname(11:11)='D'
		    ENDIF
                    response(1:) = response(1:30) //'    Datafile: '
	1                                         // polcfg(polindex)%fname
	         END IF
                 CALL PUTLINE_NT (contr, response)
	      END IF
	   END DO
	   IF (ip .le. 0) ip = 1 !multiply duration by this in check_channel
	END IF      
	DO WHILE (polindex .LT. 5)
	   polindex = polindex + 1
	   polcfg(polindex)%csec = 0                              ! signals end
	END DO
	fname=polcfg(1)%fname                             ! this always true
C---------------------------- UPDATE HELP SCREEN -----------------------------C
	IF (.NOT. dryrun)THEN
	  help_line(1)(1:) = ' Executing:  D-Buffer '// bufno(1:2) //
     1              '                Datafile: '
	ELSE
	  help_line(1)(1:) = ' Testing:  D-Buffer '// bufno(1:2) 
	ENDIF
	IF (keep .AND. .NOT. dryrun) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:50) // 'none        '
	END IF
	help_line(2)(1:) = ' '// dbuf%com(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
	info(1) = help_line(1)(2:)
C------------------ PRINT CURRENT POSITION OF MOTORS NOT MOVING-------C
	response(1:) = 'Other motors: '
	k = 14
	DO j= 1, MAXMOTS
	   IF (motfl(j) .AND. .not. posout(j)) THEN
	      WRITE (UNIT= out(1:2), FMT='(i2)') j
	      response(1:) = response(1:k) //' A'// out(1:2) //'='
	      k = k + 5
 	      WRITE (UNIT= out(1:8), FMT= '(f8.3)') RdAngl(j) - z(j)
	      response(1:) = response(1:k) // out(1:8)
	      k = k + 8
	   END IF
	   IF (k .GT. 65)THEN
	     CALL PUTLINE_NT (contr,response)
	     response(1:) = ' '
	     k = 14
	   ENDIF
	END DO
	IF (k .GT. 14) CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, new_line)
C-------- Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE!!')
	ENDIF
C---------Print warning if buffer has temp info but not T+
	IF (dbuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF
C---------Print warning if buffer has field info but not H+
	IF ( (dbuf%ihf .NE. 0.0 .or. dbuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF
C--------- DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	     if (wavel.ne.0)then
	       fenergy=81.804/(wavel**2)
	     else
	       fenergy=0.
	     endif
	IF (.NOT. dryrun)CALL Check_Channel (1, dbuf%mt, dbuf%mon, 
     1	    dbuf%mpf, dbuf%pts,fname,fenergy,ip,hstr)
	IF (READEF(contr_A))THEN
	   CALL PUTLINE (contr, ' **** ABORT during monitor rating ****')
	   RETURN
	END IF
C--------- Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5) .AND. 
     1		.NOT. dryrun) THEN !SC magnet
	  response(1:) = 'Helium level: '//hstr
	  CALL PUTLINE_NT (contr, response)
	  CALL PUTLINE_NF(contr, new_line)
	ENDIF
C---------------- PRINT DATA HEADER ACCORDING TO INSTRUMENT ------------------C
	i = 0  	! (* number of motors written *)
	wrap = .FALSE.
	response(1:) = 'Time     Pt# '
	k = 13
	DO j= 1,  numdmots
	  jr = dbuf%mn(j)
	  IF (jr .NE. 0)THEN
	   IF (motfl(jr) .AND. .NOT. fixed(jr)) THEN
	      WRITE (UNIT= out(1:2), FMT='(i2)') jr
	      response(1:k+8) = response(1:k) //'   A'// out(1:2) //'  '
	      k = k + 8
	      i = i + 1
	   END IF
	  ENDIF
	   IF (i .GE. 6 .or. (i .GE. 5 .AND. monmag)) THEN
	      CALL PUTLINE_NF (contr, response)
	      i = 0
	      k = 0
	      wrap = .TRUE.
	   ELSE !2nd line of point output will not be needed
	      info(4)=' '
	   END IF
	END DO
	IF (k .EQ. 0) THEN
	  IF (polarized) THEN
	    response(1:) = ' '
	    response(41:52) = '     TIME   '
	    k = 40
	  ELSE
	    response(1:) = '     TIME   '
	  ENDIF
	ELSE
	   response(1:k+12) = response(1:k) //'     TIME   '
	END IF
	k = k + 12
	IF (nsta .NE. 1) THEN
	   response(1:k+10) = response(1:k) //'INTEN     '
	   k = k + 10
	   IF (tempctr) THEN
	     response(1:k+6) = response(1:k) //'TEMP    '
	     k = k+8
	   ENDIF
	   IF (monmag)THEN !monitor field
	     response(1:k+5) = response(1:k)//' HF  '
	     k = k+5
	   ENDIF
	ELSE
	   IF (tempctr) THEN
	      response(1:k+5) = response(1:k) //' TEMP    '
	      k = k + 9
	   END IF
	   IF (monmag)THEN !monitor field
	     response(1:k+5) = response(1:k)//' HF  '
	     k = k+5
	   ENDIF
	   response (1:k+17) = response(1:k) //'   (Intensities) '
	END IF       
	CALL PUTLINE_NF (contr, response)

	CALL PUTLINE_NF (contr, new_line)
C-------------Initialize loop variables --------------------------------------C
	lastloop = 1
	DO k = 1,maxloops
	   numsteps(k) = 0 ! initialize to zero
        ENDDO
	DO k = 1,maxloops
	   jr = dbuf%lpn(k)
	   if (jr.le.0) continue
	   if (jr .gt. maxloops) jr = maxloops
	   if (dbuf%lpts(k).gt.0) then
	      numsteps(dbuf%lpn(k)) = dbuf%lpts(k) !memorize no steps for each loop
	      IF (dbuf%lpn(k) .GT. lastloop)lastloop = dbuf%lpn(k) !find lowest loop
	   endif
	   curstep(k) = 0	!start at first step within each loop
	   steps_below(k) = 0
	ENDDO

	steps_below(lastloop) = 1
	DO k = lastloop-1, 1,-1
	  steps_below(k) = steps_below(k+1) * numsteps(k+1)
	ENDDO

C-----------------------------------------------------------------------------C
C--------------------- COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C
	curpoint = 0
	DO WHILE (curpoint .LE. NINT(dbuf%pts) - 1 .AND. .NOT. finishup)
!       C--------------------- CHECK PAUSE FLAG ------------------------------C
	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event)
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 
!          C------- IF NECESSARY SWITCH POLARIZING FLIPPER --------------------C
	   polindex = 1
	   IF (polcfg(polindex)%csec .NE. 0 .AND. .NOT. dryrun) CALL Set_Flippers (polcfg(polindex)%csec)
!	   C-------------- Calculate new motor positions as necessary
	   do i = 1, maxloops
	     newpos(i) = .false. !default is motors  in this loop are not moved
	   enddo
	   do i = 1, lastloop !check each loop to see if its motors to move
	     IF (curpoint .EQ. 0)THEN !all motors are moved at point # 0
	       newpos(lastloop) = .TRUE.
	     ENDIF
	     IF (i .EQ. lastloop)THEN !last loop motors are always moved
	       newpos(i) = .TRUE.
	     ELSEIF ( MOD(curpoint+1,steps_below(i)) .EQ. 0)THEN
	       newpos(i) = .TRUE.
	     ENDIF
	   enddo
!          C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C
	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) curpoint+1, '/', NINT(dbuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7)
	   i = 0		! (* number of motors written *)
	   k = 13
	   limit = .FALSE.
	   DO j = 1,  numdmots 		             ! (* print new position first *)
	     jr = dbuf%mn(j)
	     IF (jr .NE. 0)THEN
	      IF (motfl(jr) .AND. .NOT. fixed(jr)) THEN
	         posit(j) = dbuf%a(j) + dbuf%i(j) * curstep(dbuf%lpn(j))
	         WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
	         response(1:)= response(1:k) // out(1:8) 
	         k = k + 8
	         i = i + 1
		 IF (.NOT. inlimits(jr,posit(j)+z(jr),contr))THEN
	            limit = .TRUE.
	            error = 'Buffer aborted !!'
	         END IF
c	         coords(j) = dbuf%a_xyz(j) + dbuf%i_xyz(j) * curstep(dbuf%lpn(j))
	      END IF
	     ENDIF !jr ne 0
	   END DO
	   response = response(1:k) //' *'
	   k = k + 2
 	   CALL Putline_NCR (contr, response(1:k))
	   IF (.not.wrap) THEN
	     CALL CURSOR_ABS (contr, 19, k+1)
	     iwhere = k+1
	   ELSE
	     iwhere = 1
	   ENDIF
	   info(3)(1:) = response(1:k)
	   IF (limit) THEN
	      CALL PUTLINE_NF (contr, new_line)
	      CALL PUTLINE (contr, error)
	      CALL PUTLINE_NF (contr, new_line)
	      IF (keep .AND. curpoint .GT. 0 .AND. .NOT. dryrun) CALL 
	1        Save_File (fname, curpoint)
	      info(3)  = 'Buffer aborted, angle outside software limits!'
	      RETURN
	   END IF
	   IF (polarized .AND. polcfg(polindex)%csec .NE. 0 .AND. .NOT. dryrun) THEN
	      i = polcfg(polindex)%csec
	      CALL DATE_TIME (sys_time)           ! say which cross section
	      response(1:) = sys_time(13:17) //'   #####  F1: '// flp(i,1) // 
	1                    '   F2: '// flp(i,2) // '       *'
	      CALL SCROLL_DISPLAY (contr)
	      CALL PUTCHARS (contr, response(1:40), 19, 1)
	      iwhere = 41
           END IF
	   DO j = 1, numdmots
	    IF (READEF(contr_A))GOTO 50
	     jr = dbuf%mn(j)
	     IF (jr .NE. 0)THEN !motor number specified
	      IF (curpoint .EQ. 0) THEN
  	         IF (motfl(jr) .AND. .NOT. fixed(jr))
	1	      CALL Drive (jr, posit(j) + z(jr))
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(jr)
	1            .AND. .NOT. fixed(jr) )
	1	      CALL Drive (jr, posit(j) + z(jr))
	      END IF
	      oldpos(j) = posit(j)
	      CALL Yield
	     ENDIF !jr ne 0
	   END DO
 50	   IF (READEF(contr_A))THEN         
	      IF (keep .AND. .NOT. dryrun) CALL Save_File(fname, curpoint)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
   	   END IF
!          C------------ SET NEW TEMPERATURE AND WAIT ------------------------C
	   IF (tdbflag) THEN
	     if (temptype .eq. 12)THEN
		tdbpoint = 0 !init data pointer to zero for each point
	     else !ad201
	       ad2point = 0 !init data pointer to zero for each point
	     endif
	   ENDIF
	   IF (tempctr .OR. magnctr .AND. .NOT. dryrun) THEN
	      CALL New_Temp_Field (temp,dbuf%tmp,dbuf%it,dbuf%tw,dbuf%ter,
     1	       dbuf%th0,dbuf%th,dbuf%hf, curpoint,dbuf%ihf,dbuf%hfh,dbuf%hfw)
	      IF (READEF(contr_A))THEN
	         IF (keep .AND. .NOT. dryrun) CALL Save_File(fname, curpoint)
		 CALL PUTLINE (contr, ' **** ABORT during field change ****')
	         RETURN
	      END IF
	   END IF
!          C------------------- START COUNTING -------------------------------C
 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.
!          C---------------- LOOP OVER PREFACTOR -----------------------------C
	   CALL Reset_Display       !Reset NIM count display
	   IF (.NOT. dryrun)THEN
	     numprf = NINT(dbuf%mpf)
	     IF (numprf .GT. 256) numprf = 256
	   ELSE
	     numprf = 1
	   ENDIF
 200	   DO prf= 1, numprf
 201	      CALL RlyOff (4)			        ! Start display counter
	      IF (dryrun)THEN
  	         CALL CnTime (drytime)
              ELSEIF (dbuf%mt .EQ. 'NEUT') THEN
  	         CALL Count (dbuf%mon)
	      ELSE
  	         CALL CnTime (dbuf%mon)
	      END IF
	      IF (tdbflag .AND. realpsd)THEN
	        IF (numprf .eq. prf)THEN !last prefactor so final data read
	          psd = realpsd 
	        ELSEIF (PSD) THEN !ignore psd after starting first count until last read
	          psd = .FALSE.
	        ENDIF
	      ENDIF !tdbflag
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
	      IF (READEF(contr_A))THEN
		 psd = realpsd
	         IF (keep .AND. .NOT. dryrun) CALL Save_File (fname, curpoint)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
!       C--------------------- CHECK PAUSE FLAG ------------------------------C
	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event)
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      IF (iwhere .eq. 1) THEN
	        CALL SCROLL_DISPLAY (contr)
	        CALL CURSOR_ABS (contr, 19, 1)
	      ELSE
	        CALL CURSOR_ABS (contr, 19, iwhere)
	      ENDIF
	   END IF 
!             C-------- CHECK STATISTICS IF ENABLED -------------------------C
	      IF (statistic .AND. .NOT. dryrun) THEN
		  CALL Test_Ststcs (dbuf%mon, prf, counts, *100, *201)
	      ENDIF
              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (dryrun)THEN
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(drytime)
	      ELSEIF (dbuf%mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(dbuf%mon)
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT ( MOD (elap_time, 60.))
	      IF (tdbflag .AND. .NOT. dryrun) CALL RdTemp (temp, dummy, dummy)
           END DO		! Prefactor loop
	   IF (tempctr) then
	      IF (dryrun)THEN
		 temp = dbuf%tmp  + curpoint * dbuf%it !calculate new temp
	      ELSEIF (.not. tdbflag) THEN
		 IF (which_temp.eq.2)THEN !control
		    CALL RdTemp (dummy, temp, dummy)
		 ELSEIF (which_temp.eq.3) THEN !setpoint
		    CALL RdTemp (dummy, dummy, temp)
		 ELSE		!sample
		    CALL RdTemp (temp, dummy, dummy)
		 ENDIF
	      ENDIF		!dryrun
	   ENDIF
	   IF (monmag)THEN	!monitor field
	      IF (dryrun)THEN
		 field = dbuf%hf + curpoint * dbuf%ihf !calculate new field
	      ELSE		!not dryrun
		 IF (magtype .eq.4)THEN !read white superconducting only for now
		    Call RdField(field)
		    IF (field .EQ. -999.9)THEN !err reading back
		       field = fieldlast * hfconv
		    ELSE	!convert tesla to amps & then to user units !HOW???
		       field = field !just put out in tesla for now
		    ENDIF
		 ELSEIF (magtype.eq.3 .or. magtype.eq.5)THEN !blue superconducting 
		    field = fieldlast * hfconv
		 ELSE
		    field = dbuf%hf + curpoint*dbuf%ihf
		 ENDIF
		 IF (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)THEN !either SC magnet
		    hlevel = -999
		    CALL check_helium(alarm,hlevel,comerr,shutdone) !update done earlier
		    IF (shutdone)THEN
		       CALL Contr_Ring_Bell(3)
		       hstr(1:)='Shutoff'
		    ELSEIF (comerr)THEN !communication problem
		       hstr(1:)='???????'
		    ELSE
		       CALL Putmax(hlevel,hstr,7,7)
		    ENDIF
		 ENDIF
	      ENDIF		!dryrun
	   ENDIF
!       C-------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C
	   IF (nsta .NE. 1) THEN ! NON BT-1
	      IF (tempctr) THEN
		 CALL putmax(temp,str9,9,3)
		 IF (monmag)THEN
		    CALL putmax(field,str12(1),12,4)
		    WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a9,a12)') 
	1		 min, ':', sec, accucnts(2), str9,str12(1)
		 ELSE
		    WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a9)') 
	1		 min, ':', sec, accucnts(2), str9
		 ENDIF
	      ELSE
		 IF (monmag)THEN
		    CALL putmax(field,str12(1),12,4)
		    WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a12)') 
	1		 min, ':', sec, accucnts(2),str12(1)
		 ELSE
		    WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9)') 
	1		 min, ':', sec, accucnts(2)
		 ENDIF
	      END IF
	      IF (wrap) response = '  '// response
	      CALL PUTLINE_NF (contr, response)
	      IF (PSD) THEN	!write position sensitive detector data
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
		    IF (m .GE. 80) THEN	! write string to screen
		       CALL PUTLINE_NF (contr, outpsd(1:80))
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    IF (kpsd .LE. 2) outpsd(1:) = ' '
		    WRITE (UNIT= datum         , FMT= '(i20)') hsum
		    outpsd(kpsd:m) = datum(21-ee:20)			  
		    outpsd(m+1:m+1) = ','
		    kpsd = m + 2
		 END DO
		 IF (m.gt.2)CALL PUTLINE_NF (contr, outpsd(1:m))
	      ENDIF
	   ELSE			! BT-1
	      IF (tempctr) THEN
		 CALL putmax(temp,str10,10,3)
		 IF (scl .LE. 5) THEN
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a12,a2,5i6)') 
	1		    min, ':', sec, str10, field,'  ', 
	2		    (accucnts(i), i= 2, scl+1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a2,5i6)') 
	1		    min, ':', sec, str10, '  ', (accucnts(i), i= 2, scl+1)
		    ENDIF
		    CALL PUTLINE_NF (contr, response)
		 ELSE		!more than 5 scalers
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a12)') 
	1		    min, ':', sec, str10, str12(1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10)') 
	1		    min, ':', sec, str10
		    ENDIF
	            CALL PUTLINE_NF (contr, response)
		    DO ig = 1,(scl-1)/12 + 1
		       i1 = (ig-1)*12 + 2
		       i2 = i1 + 11
		       if (i2 .GT. scl + 1) i2 = scl + 1
		       WRITE(UNIT= longl, FMT='(6x,12i6)') (accucnts(i), i= i1,i2)
		       CALL PUTLINE_NF (contr, longl)
		    ENDDO
	         END IF
	      ELSE
		 IF (scl .LE. 5) THEN
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a12,a4,5i6)') 
	1		    min, ':', sec, str12(1),'    ', (accucnts(i), i= 2, scl+1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a4,5i6)') 
	1		    min, ':', sec, '    ', (accucnts(i), i= 2, scl+1)
		    ENDIF
		    CALL PUTLINE_NF (contr, response)
		 ELSE		!more than 5 scalers
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a12)') 
	1		    min, ':', sec, str12(1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2)') 
	1		    min, ':', sec
		    ENDIF
		    CALL PUTLINE_NF (contr, response)
		    DO ig = 1,(scl-1)/12 + 1
		       i1 = (ig-1)*12 + 2
		       i2 = i1 + 11
		       if (i2 .GT. scl + 1) i2 = scl + 1
		       WRITE(UNIT= longl, FMT='(6x,12i6)') (accucnts(i), i= i1,i2)
		       CALL PUTLINE_NF (contr, longl)
		    ENDDO
		 END IF
	      END IF
           END IF
	   IF (wrap)THEN
	      info(4) = response
	   ELSE	   
	      info(3)(k+1:) = response
	   ENDIF
	   IF (keep .AND. .NOT. dryrun) 
	1	CALL Dmp_dbuf (fname, curpoint, accucnts, elap_time, 1, dbuf,
	2	temp, polcfg(polindex)%csec,field,monmag)
	   polindex = polindex + 1
	   DO WHILE (polcfg(polindex)%csec .NE. 0 .AND. .NOT. dryrun)
	      IF (tempctr .and. (dbuf%tmp .ne. 0.) .and. (dbuf%tw .ne. 0.))
	1	   Call Check_Heater
	      CALL Run_DPolar (dbuf,idum, curpoint,keep)

	      IF (READEF(contr_A))THEN
	         IF (keep .AND. .NOT. dryrun) CALL Save_File (fname, curpoint)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
	      polindex = polindex + 1
	   END DO
!       C------------------ UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5) .AND.
     1		.NOT. dryrun) info(4)(60:) = 'Helium Lvl:'//hstr
	   IF (.NOT. dryrun)CALL Status_File (info)
	   DO i = 1, maxloops	!increment step number in each loop for which motors moved
	      IF (newpos(i))THEN !all motors in this loop went to new positions
		 curstep(i) = curstep(i) + 1
		 IF ( curstep(i) .GE. numsteps(i) )curstep(i) = 0
	      ENDIF
	   ENDDO
	   curpoint = curpoint + 1
	END DO			! (* Number of points loop *)
	IF (curpoint .LE. NINT(dbuf%pts) - 1 .AND. finishup)THEN
	   finishup = .FALSE.
	   IF (keep .AND. curpoint .GT. 0) CALL Save_File (fname, curpoint) !adjust #pts & final positions
	   CALL Putcontr(' Ctrl F detected')
	   CALL Putcontr_NF(' ')
	   CALL Status_Line	!erase CtrlF message
	ENDIF
	IF (keep .AND. .NOT. dryrun) THEN
	   context = 0
	   CALL Make_Backup (polcfg(1)%fname)
	   polindex = 2
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Make_Backup (polcfg(polindex)%fname)
	      polindex = polindex + 1
	   END DO
	END IF
	END
