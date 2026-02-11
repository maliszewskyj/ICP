#include "icp.h"
	SUBROUTINE Run_B_Buf (buf_no, keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes BRAGG Buffer No. buf_no                                      C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, August 1992                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use smg_prep
	use instr
	use polar
	use histo
	use tc
	use asd
	use globals
	IMPLICIT      none
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, j, k, status, st,  typ, ver, kpsd,ee,m,jstart
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37),iwhere
	INTEGER*4     context,ig,i1,i2,lowest, idum, iduma(10)
	INTEGER*4     min, sec, polindex, right, left, hsum, ip,ios
	REAL*4        af(10), posit(10), oldpos(10), RdAngl, dummy, secdiff
	REAL*4        elap_time, dsec, foc7, foc8, temp /0./, RdAm, zj, Fz
	LOGICAL       limit, wrap, monmag, comerr, shutdone, alarm, inlimits,
     1				Readef
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr, hstr
	CHARACTER*8   out, new_line /'        '/
	CHARACTER*9   str9
	CHARACTER*10  str10
	CHARACTER*12  fname, File_Name,str12(4)
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response, info(4), error, help_line(3), subdir,
     1		       outpsd
	CHARACTER*80 longl
	REAL*4 fenergy, field, hlevel, bpos(10)

	TYPE(b_buf) bbuf
	TYPE(polarize) polcfg(5)

	finishup = .FALSE.

	OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', RECL= BBUF_RECL)
	READ (1,REC=buf_no) bbuf
C	Recalculate motor angles since they can change with rdangl93)
        CALL Bragg_Pos(bbuf,bpos,iduma,iduma,comerr,.false.)
	IF (comerr)THEN
	  CALL Putcontr_NT('Q-vector not in scattering plane. Buffer aborted')
	  CLOSE(1)
	  RETURN
	ENDIF
        DO i=1,2
	    bbuf%a(i) = bpos(i)
        ENDDO
        DO i=5,6
	    bbuf%a(i) = bpos(i)
        ENDDO
	bbuf%a(3) = bpos(3) - int(bbuf%pts/2.)*bbuf%i(3)
	bbuf%a(4) = bpos(4) - int(bbuf%pts/2.)*bbuf%i(4)
	WRITE (1,REC=buf_no) bbuf
	CLOSE(1)
	lasta = bbuf%lat_a !memorize last lattice constants
	lastb = bbuf%b
	lastc = bbuf%c
	lastef = bbuf%ef

	IF (nsta .eq. -5 .AND. use_asd) THEN
	  call read_asdfile
	  use_asdfile = .TRUE.
	ENDIF

	CALL DATE_TIME (sys_time)
	info(2) (1:)=bbuf%com

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	  IF (bbuf%ihf .NE. 0 .or. magtype .ge. 2)monmag=.TRUE.
C	  IF (bbuf%ihf .NE. 0 )monmag=.TRUE.
	  call putmax(bbuf%hf,str10,10,4)
	  info(2)(51:)=' H-field: '//str10
	ENDIF

C-------------------------- DETERMINE WHETHER MOTOR ONE IS MOVED--------------C
	lowest = 2 !1 is not moved on most instruments
	if (nsta.eq.4 .or. nsta.eq.-5 .or. nsta.eq.2 .or.
     1		nsta .eq. 9)lowest=1 !1 is moved on BT2,BT4,BT9 and NG5

C--------------------------- DETERMINE DATA FILE -----------------------------C

	IF (keep) THEN                                     ! Data storage is on
	   fname = File_Name (bbuf%com(1:5))
	   Call getdir(subdir,right)			   ! Get name of subdir
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
      

C------------------------------- PRINT HEADER --------------------------------C

	CALL PUTLINE_NF (contr, ' ')
	CALL PUTLINE_NF (contr, ' ')
	CALL PUTLINE_NF (contr, sys_time(1:17))
	WRITE (UNIT= bufno(1:2), FMT= '(i2)') buf_no
	response = 'Bbuffer '// bufno // ':  '// bbuf%com
	CALL PUTLINE_NT (contr, response)
	response = ' '
	CALL PUTLINE_NF (contr, response)
	str7(1) = PutStr (bbuf%ec)
	str7(2) = PutStr (bbuf%es)
	str7(3) = PutStr (bbuf%ef)
	response = 'E-center = '// str7(1) //'  E-step = '// str7(2)
	IF (bbuf%eft .EQ. 'ANA') THEN
	   typ = 2
	   response = response(1:36) //'     Analyzer fixed at '// str7(3) //
     1                                 ' meV'
	ELSE
	   typ = 3
	   response = response(1:36) //'     Monochromator fixed at '// str7(3)
     1		 		     //' meV'	
	END IF
	CALL PUTLINE_NT (contr, response)	
	WRITE (UNIT= str6(1), FMT= '(f6.3)') bbuf%hc
	WRITE (UNIT= str6(2), FMT= '(f6.3)') bbuf%kc
	WRITE (UNIT= str6(3), FMT= '(f6.3)') bbuf%lc   
	response = 'Q-center = ('// str6(1) //','// str6(2) //','//str6(3)//')'
	WRITE (UNIT= str6(1), FMT= '(f6.4)') bbuf%hs
	WRITE (UNIT= str6(2), FMT= '(f6.4)') bbuf%ks
	WRITE (UNIT= str6(3), FMT= '(f6.4)') bbuf%ls   
	response = response(1:33) //'      Q-step = ('// str6(1) //','// str6(2)
     1                            //','// str6(3) //')'
	CALL PUTLINE_NT (contr, response) 
	CALL PUTLINE_NF (contr, new_line)
	DO j= lowest,6
	   IF (motfl(j)) THEN
	      af(j) = bbuf%a(j) + (bbuf%pts - 1) * bbuf%i(j)
	      str7(1) = PutStr (bbuf%a(j))
	      str7(2) = PutStr (bbuf%i(j))
	      str7(3) = PutStr (af(j))
	      WRITE (UNIT= out(1:1), FMT= '(i1)') j
	      response = 'A'// out(1:1) //'-beg= '// str7(1) //'  Inc-'// 
	1		  out(1:1) //'= '// str7(2) //'  A'// out(1:1) //
	1		  '-end= '// str7(3)
	      CALL PUTLINE_NT (contr, response)
	   END IF
	END DO

	CALL Putmax(bbuf%tmp,str6(1),6,5)
	CALL Putmax(bbuf%it,str6(2),6,5)
	CALL putmax(bbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(bbuf%tw)
	call putmax(bbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') bbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
     	1          '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2        //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN !magnetic field 
	  call putmax(bbuf%hf,str12(1),12,4)
	  call putmax(bbuf%ihf,str12(2),12,4)
	  write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(bbuf%hfw)
c	  call putmax(bbuf%hfw,str12(3),12,4)
	  call putmax(hfconv,str12(4),12,4)
	  response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
     1	    ' Hwait= '//
     1	     str12(3)//' Hconv= '//str12(4)
	  CALL Putline_NT (contr, response)
	ENDIF !magnetic field
	WRITE (UNIT= out(1:7), FMT= '(i7)') NINT(bbuf%mon)
	WRITE (UNIT= str6(1)(1:3), FMT= '(i3)') NINT(bbuf%mpf)
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(bbuf%pts)
	response = 'Monitor= '// out(1:7) //'  Prefac= '// str6(1)(1:3)
     1         //'  Type= '// bbuf%mt //'  #pts = '// str6(2)(1:3)
	IF (keep .AND. .NOT. polarized) response = response(1:53) //'    Datafile: '// fname
	CALL PUTLINE_NT (contr, response)

	polindex = 0
	polcfg(1)%fname = fname
	DO j = 1,5
	   polcfg(j)%csec = 0                              ! signals end
	END DO
	ip = 1 !multiply duration of buffer by this in check_channel
	IF (polarized) THEN
	   CALL PUTLINE_NT (contr, 'Polarization analysis enabled.')
	   CALL PUTLINE_NT (contr, 'The following cross section(s) will be taken:')
	   IF (keep) READ (UNIT=fname(6:8), FMT= '(i3)') ver
	   ip = 0
	   DO j = 1, 4
	      IF (bbuf%flip_state(j)) THEN
		 ip = ip + 1
	         response(1:) = 'Flipper 1: '// flp(j,1) // '  Flipper 2: ' 
	1                       // flp(j,2) 
	         polindex = polindex + 1 
   		 polcfg(polindex)%csec  = j 
	         IF (keep) THEN
	            polcfg(polindex)%fname = fname
	            WRITE (UNIT=polcfg(polindex)%fname(6:8), FMT= '(i3.3)') ver
	            IF (flp(j,1).eq.'OFF'.and.flp(j,2).eq.'OFF')THEN
			polcfg(polindex)%fname(11:11)='a'
		    ELSE IF (flp(j,1).eq.'ON '.and.flp(j,2).eq.'OFF')THEN
			polcfg(polindex)%fname(11:11)='b'
		    ELSE IF(flp(j,1).eq.'OFF'.and.flp(j,2).eq.'ON ')THEN
			polcfg(polindex)%fname(11:11)='c'
		    ELSE !flp(j,1).eq.'ON '.and.flp(j,2).eq.'ON '
			polcfg(polindex)%fname(11:11)='d'
		    ENDIF
                    response(1:) = response(1:30) //'    Datafile: '
	1                                         // polcfg(polindex)%fname
	         END IF
                 CALL PUTLINE_NT (contr, response)
	      END IF
	   END DO
	   IF (ip .le. 0) ip = 1 !multiply duration by this in check_channel
	END IF      
	fname=polcfg(1)%fname                             ! this always true


C---------------------------- UPDATE HELP SCREEN -----------------------------C

	help_line(1)(1:) = ' Executing:  B-Buffer '// bufno(1:2) //
	1              '                Datafile: '
	IF (keep) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:67) // 'none        '
	END IF
	help_line(2)(1:) = ' '// bbuf%com(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
	info(1) = help_line(1)(2:)
      

C------------------ PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	k = 14
	DO j= lowest, 6
	   IF (motfl(j) .AND. fixed(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:) = response(1:k) //'  A'// out(1:1) //'='
	      k = k + 5
	      RdAm = RdAngl(j)
	      IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
		zj = fz(j,RdAm)
 	      ELSE 
		zj = z(j)
	      ENDIF
 	      WRITE (UNIT= out(1:8), FMT= '(f8.3)') RdAm - zj
	      response(1:) = response(1:k) // out(1:8)
	      k = k + 8
	   END IF
	END DO
	IF (k .GT. 14) CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, new_line)
	
C-------- Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE!!')
	ENDIF

C---------Print warning if buffer has temp info but not T+
	IF (bbuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF

C---------Print warning if buffer has field info but not H+
	IF ( (bbuf%ihf .NE. 0.0 .or. bbuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF

C--------- DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	     if (wavel.ne.0)then
	       fenergy=81.804/(wavel**2)
	     else
	       fenergy=0.
	     endif
  	CALL Check_Channel (1, bbuf%mt, bbuf%mon, bbuf%mpf, bbuf%pts,fname,
     1	    fenergy,ip,hstr)
	IF (READEF(contr_A))THEN
	   CALL PUTLINE (contr, ' **** ABORT during monitor rating ****')
	   RETURN
	END IF

C--------- Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	  response(1:) = 'Helium level: '//hstr
	  CALL PUTLINE_NT (contr, response)
	  CALL PUTLINE_NF (contr, new_line)
	ENDIF

C---------------- PRINT DATA HEADER ACCORDING TO INSTRUMENT ------------------C

	i = 0  	! (* number of motors written *)
	wrap = .FALSE.
	response(1:) = 'Time     Pt# '
	k = 13
	DO j= lowest, 6
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:k+8) = response(1:k) //'    A'// out(1:1) //'  '
	      k = k + 8
	      i = i + 1
	   END IF
	   IF (i .GE. 6 .OR. (i .GE. 5 .AND. monmag)) THEN
	      CALL Putline_NF (contr, response)
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
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, new_line)


C-----------------------------------------------------------------------------C
C--------------------- COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	loop = 0

	DO WHILE (loop .LE. NINT(bbuf%pts) - 1 .AND. .NOT. finishup)

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
	   IF (polcfg(polindex)%csec .NE. 0) CALL Set_Flippers (polcfg(polindex)%csec)


!          C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) loop+1, '/', NINT(bbuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7)
	   i = 0		! (* number of motors written *)
	   k = 13
	   limit = .FALSE.
	   DO j = lowest, 6 		             ! (* print new position first *)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	         posit(j) = bbuf%a(j) + bbuf%i(j) * loop 
	         WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
	         response(1:)= response(1:k) // out(1:8) 
	         k = k + 8
	         i = i + 1
	         IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
	           zj = fz(j,posit(j))
	         ELSE 
	           zj = z(j)
	         ENDIF
		 IF (.NOT. inlimits(j,posit(j)+zj,contr))THEN
	           limit = .TRUE.
	           error = 'Buffer aborted !!'
	         END IF
	      END IF
	   END DO
	   IF (nsta .EQ. 4.and..not.fixed(2)) THEN 			       !(* focussing *)
              posit(7) = foc7(posit(2))
	      posit(8) = foc8(posit(2))
	      DO j = 7, 8
		 IF (READEF(contr_A)) GOTO 50
	         IF (loop .EQ. 0) THEN
  	            IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	         ELSE
	            IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1              .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	         END IF
	         oldpos(j) = posit(j)
	      END DO
	   END IF
	   IF (nsta .EQ. -5.and..not.fixed(2)) THEN 			       !(* focussing *)
              posit(7) = foc7(posit(2))
	      DO j = 7, 7
		 IF (READEF(contr_A))GOTO 50
	         IF (loop .EQ. 0) THEN
  	            IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	         ELSE
	            IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1              .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	         END IF
	         oldpos(j) = posit(j)
	      END DO
	   END IF
	   response = response(1:k) //' *'
	   k = k + 2
#if defined(VMS)
	   CALL Putline_NF (contr, response(1:k))
#else
 	   CALL Putline_NCR (contr, response(1:k))
#endif
	   IF (.not.wrap) THEN
	     CALL CURSOR_ABS (contr, 19, k+1)
	     iwhere = k + 1
	   ELSE
	     iwhere = 1
	   ENDIF
	   info(3)(1:) = response(1:k)
	   IF (limit) THEN
	      CALL Putline (contr, new_line)
	      CALL Putline (contr, error)
	      CALL PUTLINE (contr, new_line)
	      IF (keep .AND. loop .GT. 0) CALL 
	1        Save_File (fname, loop)
	      info(3)  = 'Buffer aborted, angle outside software limits!'
	      RETURN
	   END IF

	   IF (polarized .AND. polcfg(polindex)%csec .NE. 0) THEN
	      i = polcfg(polindex)%csec
	      CALL DATE_TIME (sys_time)           ! say which cross section
	      response(1:) = sys_time(13:17) //'   #####  F1: '// flp(i,1) // 
	1                    '   F2: '// flp(i,2) // '       *'
	      CALL SCROLL_DISPLAY (contr)
	      CALL PUTCHARS (contr, response(1:40), 19, 1)
	      iwhere = 41
           END IF
	   DO j = 6, lowest, -1                 ! (* drive motors in reverse order *)
	      IF (READEF(contr_A))GOTO 50
	      IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
	        zj = fz(j,posit(j))
	      ELSE 
	        zj = z(j)
	      ENDIF
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + zj)
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1            .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + zj)
	      END IF
	      oldpos(j) = posit(j)
	   END DO
 50	   CALL Yield               
	   IF (READEF(contr_A))THEN
	      IF (keep) CALL Save_File (fname, loop)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
   	   END IF
        

!          C------------ SET NEW TEMPERATURE AND WAIT ------------------------C

	   IF (tempctr .OR. magnctr) THEN
	      CALL New_Temp_Field (temp,bbuf%tmp,bbuf%it,bbuf%tw,bbuf%ter,
     1	             bbuf%th0,bbuf%th,bbuf%hf, loop,bbuf%ihf,bbuf%hfh,bbuf%hfw)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
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
	   IF (nsta .EQ. -5 .AND. use_asd) CALL Zero_ASD !zero data array
 200	   DO prf= 1, NINT(bbuf%mpf)
 201	      CALL RlyOff (4)			        ! Start display counter
              IF (bbuf%mt .EQ. 'NEUT') THEN
	         IF (nsta .eq. -5 .AND. use_asd)call asd_on(1)
  	         CALL Count (bbuf%mon)
	      ELSE
	         IF (nsta .eq. -5 .AND. use_asd)call asd_timed(1,bbuf%mon)
  	         CALL CnTime (bbuf%mon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF

	     IF (nsta .eq. -5 .and. use_asd)counts(2) = fpwindow

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
	      IF (iwhere .eq. 1 ) THEN
	        CALL SCROLL_DISPLAY (contr)
	        CALL CURSOR_ABS (contr, 19, 1)
	      ELSE
	        CALL CURSOR_ABS (contr, 19, iwhere)
	      ENDIF
	   END IF 

!             C-------- CHECK STATISTICS IF ENABLED -------------------------C

	      IF (statistic) CALL Test_Ststcs (bbuf%mon, prf, counts, *100, *201)


              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (bbuf%mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(bbuf%mon)
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT ( MOD (elap_time, 60.))
	      IF (nsta .eq. -5 .and. use_asd .and. .not. keep_rawasd)call asd_image
           END DO                                              ! Prefactor loop
	   IF (tempctr) then
	     IF (which_temp.eq.2)THEN !control
	       CALL RdTemp (dummy, temp, dummy)
	     ELSEIF (which_temp.eq.3) THEN !setpoint
	       CALL RdTemp (dummy, dummy, temp)
	     ELSE !sample
	       CALL RdTemp (temp, dummy, dummy)
	     ENDIF
	   ENDIF


	   IF (monmag)THEN !monitor field
	     IF (magtype .eq.4)THEN !read white superconducting only for now 
     		Call RdField(field)
		IF (field .EQ. -999.9)THEN !err reading back
		  field = fieldlast * hfconv
		ELSE !convert tesla to amps & then to user units !HOW???
		  field = field !just put out in tesla for now
		ENDIF
	     ELSEIF (magtype.eq.3 .or. magtype.eq.5)THEN !blue superconducting 
C		field = fieldlast * hfconv
		Call Rdfield(field)		
	     ELSEIF (magtype.eq.2)THEN !read back voltage from HP
		Call Rdfield(field)
	     ELSE
     		field = bbuf%hf + loop*bbuf%ihf
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
	   ENDIF
!          C-------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

	   IF (nsta .NE. 1) THEN                                    ! NON BT-1
	      IF (tempctr) THEN
	        str9(1:)=' '
	     	IF (temptype .EQ. tdev%cfg) THEN !using degrees
		  CALL putmax(temp,str9(2:9),8,5)
	        ELSE !using resistance
		  CALL putmax(temp,str9(2:9),8,7)
	        ENDIF
		IF (monmag)THEN
		  CALL putmax(field,str12(1),12,4)
	          WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a9,a12)') 
	1               min, ':', sec, accucnts(2), str9,str12(1)
		ELSE
	          WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a9)') 
	1               min, ':', sec, accucnts(2), str9
	        ENDIF
	      ELSE
		IF (monmag)THEN
		  CALL putmax(field,str12(1),12,4)
	          WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a12)') 
	1               min, ':', sec, accucnts(2),str12(1)
		ELSE
	          WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9)') 
	1               min, ':', sec, accucnts(2)
		ENDIF
	      END IF
	      IF (wrap) response = '  '// response
	      CALL Putline_NF (contr, response)
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
	            CALL Putline_NF (contr, outpsd(1:80))
c		    WRITE (2, '(a80)') outpsd(1:80)
	            kpsd = 2
		    m = 2 + ee-1
	          END IF
	          IF (kpsd .LE. 2) outpsd(1:) = ' '
	          WRITE (UNIT= datum, FMT= '(i20)') hsum
		  outpsd(kpsd:m) = datum(21-ee:20)
	          outpsd(m+1:m+1) = ','
	          kpsd = m + 2
	        END DO
c	        IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	        IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	      ENDIF
	      IF (nsta .EQ. -5 .AND. USE_ASD) THEN !write area sensitive detector data
	        outpsd(1:)=' '
	        kpsd = 2
	        DO i = 1, noslices
	          hsum=asd_data(i)
	          ee = 1
	          DO WHILE (hsum .GE. 10**ee)
	            ee = ee + 1
	          END DO
	          m = kpsd + ee-1	    	     
	          IF (m .GE. 80) THEN                        ! write string to screen
	            CALL Putline_NF (contr, outpsd(1:80))
c		    WRITE (2, '(a80)') outpsd(1:80)
	            kpsd = 2
		    m = 2 + ee-1
	          END IF
	          IF (kpsd .LE. 2) outpsd(1:) = ' '
		  WRITE (UNIT= datum, FMT= '(i20)') hsum
		  outpsd(kpsd:m) = datum(21-ee:20)
	          outpsd(m+1:m+1) = ','
	          kpsd = m + 2
	        END DO
c	        IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	        IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	      ENDIF
	   ELSE							        ! BT-1
	      IF (tempctr) THEN
		str10(1:)=' '
	     	IF (temptype .EQ. tdev%cfg) THEN !using degrees
		  CALL putmax(temp,str10(2:10),9,5)
	        ELSE !using resistance
		  CALL putmax(temp,str10(2:10),9,8)
	        ENDIF
		 IF (scl .LE. 5) THEN
		   IF (monmag)THEN
		     CALL putmax(field,str12(1),12,4)
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a12,a2,5i6)') 
	1                  min, ':', sec, str10, field,'  ', 
     1			   (accucnts(i), i= 2, scl+1)
		   ELSE
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a2,5i6)') 
	1                  min, ':', sec, str10, '  ', (accucnts(i), i= 2, scl+1)
		   ENDIF
  	      	   CALL Putline_NF (contr, response)
		 ELSE !more than 5 scalers
		   IF (monmag)THEN
		     CALL putmax(field,str12(1),12,4)
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a12)') 
	1                  min, ':', sec, str10, str12(1)
		   ELSE
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10)') 
	1                  min, ':', sec, str10
		   ENDIF
	            CALL Putline_NF (contr, response)
		    DO ig = 1,(scl-1)/12 + 1
		      i1 = (ig-1)*12 + 2
		      i2 = i1 + 11
		      if (i2 .GT. scl + 1) i2 = scl + 1
  	              WRITE(UNIT= longl, FMT='(6x,12i6)') (accucnts(i), i= i1,i2)
	              CALL Putline_NF (contr, longl)
		    ENDDO
	         END IF
	      ELSE
		 IF (scl .LE. 5) THEN
		   IF (monmag)THEN
		     CALL putmax(field,str12(1),12,4)
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a12,a4,5i6)') 
	1                  min, ':', sec, str12(1),'    ', (accucnts(i), i= 2, scl+1)
		   ELSE
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a4,5i6)') 
	1                  min, ':', sec, '    ', (accucnts(i), i= 2, scl+1)
		   ENDIF
		   CALL Putline_NF (contr, response)
		 ELSE !more than 5 scalers
		   IF (monmag)THEN
		     CALL putmax(field,str12(1),12,4)
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2,a12)') 
	1                  min, ':', sec, str12(1)
		   ELSE
	             WRITE (UNIT= response, FMT= '(i4,a,i2.2)') 
	1                  min, ':', sec
		   ENDIF
		    CALL Putline_NF (contr, response)
		    DO ig = 1,(scl-1)/12 + 1
		      i1 = (ig-1)*12 + 2
		      i2 = i1 + 11
		      if (i2 .GT. scl + 1) i2 = scl + 1
  	              WRITE(UNIT= longl, FMT='(6x,12i6)') (accucnts(i), i= i1,i2)
	              CALL Putline_NF (contr, longl)
		    ENDDO
		 END IF
	      END IF
           END IF
	   IF (wrap) THEN
	     info(4) = response
	   ELSE
	     info(3)(k+1:) = response
	   ENDIF
	   IF (keep) THEN
		CALL Dmp_Data (fname, loop, accucnts, elap_time, 5,
	1	   idum,idum,idum,bbuf, temp, polcfg(polindex)%csec,
	1	   field,monmag) 

	     IF (nsta .EQ. -5 .AND. USE_ASD .AND. Keep_RawASD)CALL Dmp_ASD(fname,loop)
	   ENDIF !keep
	   polindex = polindex + 1
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      IF (tempctr .and. (bbuf%tmp .ne. 0.) .and. (bbuf%tw .ne. 0.))
     1	        Call Check_Heater
	      CALL Run_Polar (5,idum,idum,bbuf, polcfg(polindex), loop,keep)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
	      polindex = polindex + 1
	   END DO

!          C------------------ UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5))info(4)(60:)=
     1		'Helium Lvl:'//hstr

	   CALL Status_File (info)

	  loop = loop + 1

	END DO  ! (* Number of points loop *)

	IF (loop .LE. NINT(bbuf%pts) - 1 .AND. finishup)THEN
	  finishup = .FALSE.
 	  IF (keep .AND. loop .GT. 0) CALL Save_File (fname, loop) !adjust #pts & final positions
	  CALL Putcontr(' Ctrl F detected')
	  CALL Putcontr_NF(' ')
	  CALL Status_Line !erase CtrlF message
	ENDIF

	IF (keep) THEN
	   context = 0
	   CALL Make_Backup (polcfg(1)%fname)
	   polindex = 2
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Make_Backup (polcfg(polindex)%fname)
	      polindex = polindex + 1
	   END DO
	END IF
	END

