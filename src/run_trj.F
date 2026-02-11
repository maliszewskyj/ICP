#include "icp.h"
	SUBROUTINE Run_Trj (runfile,dryrun,keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes "Arbitrary" trajectory from a trajectory specification       C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Nick Maliszewskyj, March 2007                                         C
C					                                      C
C       Remove traces of polarized beam operation                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use smg_prep
	use instr
	use polar
	use histo
	use tc
	use tdb
	use ad201
	use trj
	IMPLICIT      none
	LOGICAL	      keep	!true to keep data file
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
	CHARACTER*12  fname, File_Name,str12(4),runfile
	CHARACTER*20 datum
	CHARACTER*28  sys_time
	CHARACTER*80  response, info(4), error, help_line(3), subdir,
	1    outpsd
	CHARACTER*80 longl
	REAL*4 fenergy, field, hlevel
	REAL*4 xbuf_tmp, xbuf_it, xbuf_ter, xbuf_tw,xbuf_hf, xbuf_ihf


	INTEGER*4	jr, curpoint, numprf
	CHARACTER*1	str1
	LOGICAL		getnew

	LOGICAL		posout(MAXMOTS), tdbflag, realpsd

	TYPE(polarize) polcfg(5)

	finishup = .FALSE.


	xbuf_tmp=300
	xbuf_it=0 
	xbuf_ter=0 
	xbuf_tw=0
	xbuf_hf=0
	xbuf_ihf=0

CCCCC   READ trajectory from file
	CALL RD_TRJ(runfile, status)
	if (status .ne. 1) then
! Error
	   return
	endif
	
	realpsd = psd		!memorize if global psd flag set at start
	IF (tempctr .AND. (temptype .EQ. 12 .or. temptype .eq. 14) )THEN
	   tdbflag = .TRUE.	!using transducer board
	ELSE
	   tdbflag = .FALSE.	!not using transducer board
	ENDIF
	IF (PSD .and. mpf.ne.1 .AND. .NOT. dryrun .AND. .NOT. tdbflag) THEN
	   mon = mon * mpf	!no pref  with position sensitive det.
	   mpf=1
	ENDIF

	CALL DATE_TIME (sys_time)
	info(2)(1:)=comments

C---------------------------DETERMINE DATA FILE -----------------------------C

	IF (keep .AND. .NOT. dryrun) THEN ! Data storage is on
	   fname = File_Name (comments(1:5))
	   Call getdir(subdir,right) ! Get name of subdir
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
	

C-------------------------------PRINT HEADER --------------------------------C

	CALL PUTLINE_NF (contr, new_line)
	CALL PUTLINE_NF (contr,sys_time(1:17))
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'Trajectory '// runfile // ':  '// comments
	CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, new_line)
	DO j = 1,MAXMOTS
	   posout(j) = .FALSE.	!position has not been written
	ENDDO
	
	DO j= 1, nmots 
	   jr = axes(j)
	   IF (jr .NE. 0)THEN	!a motor number has been supplied
	      IF (motfl(jr) .AND. .NOT. fixed(jr)) THEN
		 str7(1) = PutStr (angles(j,1))
		 str7(3) = PutStr (angles(j,npts))
		 WRITE (UNIT= out(1:2), FMT= '(i2)') jr
		 response = 'A'// out(1:2) //'-beg= '// str7(1) //
	1	      '  A'// out(1:2) //
	1	      '-end= '// str7(3)
		 CALL PUTLINE_NT (contr, response)
		 posout(jr) = .TRUE.
	      END IF
	   ENDIF		!jr ne 0
	END DO
	CALL Putmax(xbuf_tmp,str6(1),6,3)
	CALL Putmax(xbuf_it,str6(2),6,3)
	CALL putmax(xbuf_ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(xbuf_tw)
	call putmax(xbuf_hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') xbuf_hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '//
	1    str6(4)(1:3) //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN	!magnetic field 
	   call putmax(xbuf_hf,str12(1),12,4)
	   call putmax(xbuf_ihf,str12(2),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hconv= '//str12(4)
	   CALL PUTLINE_NT (contr, response)
	ENDIF			!magnetic field
	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(mon)
	WRITE (UNIT= out(1:4), FMT= '(i4)') NINT(mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1    //'    Type= '// mt 
	IF (keep .AND. .NOT. polarized .AND. .NOT. dryrun)
	2    response = response(1:46) //'           Datafile: '// fname
	CALL PUTLINE_NT (contr, response)

	polcfg(1)%fname = fname
	ip = 1			!multiply duration of buffer by this in check_channel

C----------------------------UPDATE HELP SCREEN -----------------------------C
	IF (.NOT. dryrun)THEN
	   help_line(1)(1:) = ' Executing:  Trajectory '// runfile //
	1	'                Datafile: '
	ELSE
	   help_line(1)(1:) = ' Testing:  Trajectory '// runfile 
	ENDIF
	IF (keep .AND. .NOT. dryrun) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:50) // 'none        '
	END IF
	help_line(2)(1:) = ' '// comments(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
	info(1) = help_line(1)(2:)
	

C------------------PRINT CURRENT POSITION OF MOTORS NOT MOVING-------C

	IF (.NOT. dryrun) THEN
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
	ENDIF 

C--------Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE !!')
	ENDIF

C---------DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	if (wavel.ne.0)then
	   fenergy=81.804/(wavel**2)
	else
	   fenergy=0.
	endif
	IF (.NOT. dryrun)CALL Check_Channel (1, mt, mon,
	1    mpf, npts,fname,fenergy,ip,hstr)
	IF (READEF(contr_A))THEN
	   CALL PUTLINE (contr, ' **** ABORT during monitor rating ****')
	   CALL CLREF(contr_A)
	   RETURN
	END IF

C----------------PRINT DATA HEADER ACCORDING TO INSTRUMENT ------------------C

	i = 0			! (* number of motors written *)
	wrap = .FALSE.
	response(1:) = 'Time     Pt# '
	k = 13
	DO j= 1,  nmots
	   jr = axes(j)
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
	   ELSE			!2nd line of point output will not be needed
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
	   IF (monmag)THEN	!monitor field
	      response(1:k+5) = response(1:k)//' HF  '
	      k = k+5
	   ENDIF
	ELSE
	   IF (tempctr) THEN
	      response(1:k+5) = response(1:k) //' TEMP    '
	      k = k + 9
	   END IF
	   IF (monmag)THEN	!monitor field
	      response(1:k+5) = response(1:k)//' HF  '
	      k = k+5
	   ENDIF
	   response (1:k+17) = response(1:k) //'   (Intensities) '
	END IF       
	CALL PUTLINE_NF (contr, response)
c	IF (tdbflag .and. temptype .eq. 14)CALL AD201_Head(keep) !may need to test on temptype later
	CALL PUTLINE_NF (contr, new_line)

C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	curpoint = 0

	DO WHILE (curpoint .LT. npts .AND. .NOT. finishup)
!       C--------------------- CHECK PAUSE FLAG ------------------------------C

	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()	! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event)
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
C       CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 


!       C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) curpoint+1, '/', npts
	   response(1:) = sys_time(13:17) //' '// out(1:7)
	   i = 0		! (* number of motors written *)
	   k = 13
	   limit = .FALSE.
	   DO j = 1,  nmots	! (* print new position first *)
	      jr = axes(j)
	      IF (jr .NE. 0)THEN
		 IF (motfl(jr) .AND. .NOT. fixed(jr)) THEN
		    posit(j) = angles(j,(curpoint+1))
		    WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
		    response(1:)= response(1:k) // out(1:8) 
		    k = k + 8
		    i = i + 1
		    IF (.NOT. inlimits(jr,posit(j)+z(jr),contr))THEN
		       limit = .TRUE.
		       error = 'Buffer aborted !!'
		    END IF
		 END IF
	      ENDIF		!jr ne 0
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
	1	   Save_File (fname, curpoint)
	      info(3)  = 'Buffer aborted, angle outside software limits!'
	      RETURN
	   END IF

	   IF (.not.dryrun) THEN
	      DO j = 1, nmots
		 IF (READEF(contr_A))GOTO 50
		 jr = axes(j)
		 IF (jr .NE. 0)THEN !motor number specified
		    IF (curpoint .EQ. 0) THEN
		       IF (motfl(jr) .AND. .NOT. fixed(jr)) CALL Drive (jr, 
	1		    posit(j) + z(jr))
		    ELSE
		       IF ( posit(j) .NE. oldpos(j) .AND. motfl(jr)
	1		    .AND. .NOT. fixed(jr) ) CALL Drive (jr, 
	2		    posit(j) + z(jr))
		    END IF
		    oldpos(j) = posit(j)
		    CALL Yield
		 ENDIF		!jr ne 0
	      END DO
	   END IF
 50	   IF (READEF(contr_A))THEN         
	      IF (keep .AND. .NOT. dryrun) CALL Save_File(fname, curpoint)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      CALL CLREF(contr_A)
	      RETURN
   	   END IF
	   
!       C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.

!       C---------------- LOOP OVER PREFACTOR -----------------------------C

	   IF (.NOT. dryrun)THEN
	      CALL Reset_Display !Reset NIM count display
	      numprf = mpf
	      IF (numprf .GT. 256) numprf = 256
	   ELSE
	      numprf = 1
	   ENDIF
 200	   DO prf= 1, numprf
	      IF (.not. dryrun) THEN
 201		 CALL RlyOff (4) ! Start display counter
		 IF (mt .EQ. 'NEUT') THEN
		    CALL Count (mon)
		 ELSE
		    CALL CnTime (mon)
		 END IF
		 IF (tdbflag .AND. realpsd)THEN
		    IF (numprf .eq. prf)THEN !last prefactor so final data read
		       psd = realpsd 
		    ELSEIF (PSD) THEN !ignore psd after starting first count until last read
		       psd = .FALSE.
		    ENDIF
		 ENDIF		!tdbflag
		 CALL Scaler (counts)
		 CALL RlyOn (4)	! Stop display counter
	      END IF 
	      IF (READEF(contr_A))THEN
		 psd = realpsd
	         IF (keep .AND. .NOT. dryrun) CALL Save_File (fname, curpoint)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
		 CALL CLREF(contr_A)
	         RETURN
	      END IF

!       C--------------------- CHECK PAUSE FLAG ------------------------------C

	      IF (READEF(pause))THEN
		 CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
		 CALL CURSOR_OFF (paste)
		 CALL HIBER()	! Hibernate 
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

!       C-------- CHECK STATISTICS IF ENABLED -------------------------C

C       IF (statistic .AND. .NOT. dryrun) THEN
C       CALL Test_Ststcs (mon, prf, counts, *100, *201)
C       ENDIF


              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (dryrun)THEN
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(drytime)
	      ELSEIF (mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + mon
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT ( MOD (elap_time, 60.))
	      IF (tdbflag .AND. .NOT. dryrun) CALL RdTemp (temp, dummy, dummy)
           END DO		! Prefactor loop
	   
	   IF (tempctr) then
	      IF (dryrun)THEN
		 temp = 300	! Short circuit temperature flag
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
c       WRITE (2, '(a80)') outpsd(1:80)
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    IF (kpsd .LE. 2) outpsd(1:) = ' '

		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) hsum
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
c       IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
		 IF (m.gt.2)CALL PUTLINE_NF (contr, outpsd(1:m-1))
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
	1	CALL Dmp_trj (fname, curpoint, accucnts, elap_time, 1, 
	2	temp, 0 ,field,monmag) 

	   IF (tempctr .and. (xbuf_tmp .ne. 0.) .and. (xbuf_tw .ne. 0.))
     1	        Call Check_Heater
	   
	   IF (READEF(contr_A))THEN
	      IF (keep .AND. .NOT. dryrun) CALL Save_File (fname, curpoint)
	      CALL PUTLINE (contr, ' **** ABORT during count ****')
	      CALL CLREF(contr_A)
	      RETURN
	   END IF

!       C------------------ UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5) .AND.
     1		.NOT. dryrun) info(4)(60:) = 'Helium Lvl:'//hstr

	   IF (.NOT. dryrun)CALL Status_File (info)

	   curpoint = curpoint + 1

	END DO			! (* Number of points loop *)

	IF (curpoint .LE. npts - 1 .AND. finishup)THEN
	   finishup = .FALSE.
	   IF (keep .AND. curpoint .GT. 0) CALL Save_File (fname, curpoint) !adjust #pts & final positions
	   CALL Putcontr(' Ctrl F detected')
	   CALL Putcontr_NF(' ')
	   CALL Status_Line	!erase CtrlF message
	ENDIF

	IF (keep .AND. .NOT. dryrun) THEN
	   context = 0
	   CALL Make_Backup (polcfg(1)%fname)
	END IF
	END

