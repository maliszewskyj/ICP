#include "icp.h"
	SUBROUTINE Run_I_Buf (buf_no, keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes Increment Buffer No. buf_no                                  C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Beat Schmid, August 1989                                              C
C	Modified by Gayle Greene                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use histo
	use polar
	use tc
	use asd
	use lia
	use xray
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
#include "c_vme.h"
	end interface
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, j, k, status, st, typ, ver, kpsd,ee,m, ip
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37),lowest
	INTEGER*4     context,ig,i1,i2,ios, idum
	INTEGER*4     min, sec, polindex, right, left, hsum,iwhere
	INTEGER*4	errno, highest !highest is diff for bt1
	REAL*4        af(10), posit(10), oldpos(10), RdAngl, dummy, secdiff
	REAL*4        elap_time, dsec, foc7, foc8, temp /0./, RdAm,zj,Fz
	LOGICAL       limit, wraP, monmag, comerr, shutdone, alarm, inlimits,
     1			fasttemp, prftemp, Readef
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr, hstr
	CHARACTER*8   out, new_line /'        '/
	CHARACTER*9   str9
	CHARACTER*10   str10
	CHARACTER*5   str5
	CHARACTER*13   str13
	CHARACTER*12  fname, File_Name,str12(4)
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response, info(4), error, help_line(3), subdir,
     1		       outpsd
	CHARACTER*80 longl
	REAL*4 fenergy, field, hlevel

	logical verifycol,colfound,colwait
	integer whichcollim,collim7, collim15,collim60
	parameter (collim7 = 1)
	parameter (collim15 = 2)
	parameter (collim60 = 3)
	integer*4 colwaits
	parameter (colwaits = 20) !number of waits to recheck shutter for bt1
	real*4 holdtime
	CHARACTER*80 gethome

	TYPE(i_buf) ibuf
	TYPE(polarize) polcfg(5)

	finishup = .FALSE.

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL= IBUF_RECL)
	READ (1,REC=buf_no) ibuf
	CLOSE (1)

	IF (tempctr .AND. temptype .EQ. 13)THEN
	   prftemp = .TRUE.	!read temp each prefactor
	ELSE
	   prftemp = .FALSE.	!no need to read temp each prefactor
	ENDIF

	IF (nsta .eq. -5 .AND. use_asd) THEN
	   call read_asdfile
	   use_asdfile = .TRUE.
	ENDIF

        IF ((PSD .and. (nsta .eq.7 .or. nsta .eq. -1.or.nsta.eq.8))
	1    .or.(use_asd .and. nsta .eq. -11)
	2  .and. ibuf%mpf.ne.1) THEN
	   ibuf%mon = ibuf%mon * ibuf%mpf !no pref  with position sensitive det.
	   ibuf%mpf=1
	ENDIF

	IF (nsta .eq. 1) THEN
	   whichcollim = 0	! No assumptions about collimation
	   colfound=.false.
	   verifycol=.false.
	   loop = 0
	   do while (loop .lt. colwaits .and. .not. colfound)
	      holdtime=0.0	!default is no wait
	      CALL READ_RELAY(STATUS)
	      IF (BTEST(STATUS,(2-1)))THEN !collim is 7
		 IF (verifycol) THEN
		    IF (whichcollim.eq.collim7) THEN
		       colfound=.true.
		    ELSE
		       verifycol=.false. !no match, start over
		    ENDIF
		 ELSE
		    verifycol = .true. !need to confirm reading
		    whichcollim=collim7
		    holdtime=5. !wait only 5 secs before confirming
		 ENDIF
	      ELSEIF (BTEST(STATUS,(3-1)))THEN !collim is 15
		 IF (verifycol) THEN
		    IF (whichcollim.eq.collim15) THEN
		       colfound=.true.
		    ELSE
		       verifycol=.false. !no match, start over
		    ENDIF
		 ELSE
		    verifycol = .true. !need to confirm reading
		    whichcollim=collim15
		    holdtime=5. !wait only 5 secs before confirming
		 ENDIF
	      ELSEIF (BTEST(STATUS,(4-1)))THEN !collim is 60
		 IF (verifycol) THEN
		    IF (whichcollim.eq.collim60) THEN
		       colfound=.true.
		    ELSE
		       verifycol=.false. !no match, start over
		    ENDIF
		 ELSE
		    verifycol = .true. !need to confirm reading
		    whichcollim=collim60
		    holdtime=5. !wait only 5 secs before confirming
		 ENDIF
	      ELSE
		 CALL Putcontr('WARNING	!!! Shutter seems to be closed. Waiting')
		 holdtime=15.	!wait 15 secs before rechecking
	      ENDIF
	      IF (holdtime .ne. 0.0) THEN !wait
		 CALL Holds (holdtime) !at return: stop is clear if ctrl_a detected
		 CALL clref (stop) ! Normal completion
	      ENDIF
	      loop = loop + 1
	   ENDDO

	   IF (.not. colfound) THEN
	      CALL Putcontr('WARNING !! Assuming collim = 15')
	      whichcollim=collim15
	   ENDIF

	   IF ( (whichcollim .eq. collim7  .and. collim(1) .ne. 7) .or.
	1	(whichcollim .eq. collim15 .and. collim(1) .ne. 15).or.
	1	(whichcollim .eq. collim60 .and. collim(1) .ne. 60))THEN
	      OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	   ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	      READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), !read current monobt1
	1	   (mosaic(i), i= 1,3), wavel, dm, da, monobt1 
	      CLOSE (1)
	      IF (whichcollim .eq. collim7)THEN
		 collim(1) = 7
	      ELSEIF (whichcollim.eq.collim60)THEN
		 collim(1) = 60 
	      ELSE
		 collim(1) = 15
	      ENDIF
	      str5 = monobt1
	      IF (collim(1).eq.7)THEN
	         str13(1:)=str5//'_7'//'.DAT'
	      ELSEIF (collim(1).eq.15)THEN
	         str13(1:)=str5//'_15'//'.DAT'
	      ELSEIF (collim(1).eq.60)THEN
	         str13(1:)=str5//'_60'//'.DAT'
	      ENDIF
	      OPEN (UNIT= 1, FILE= HOMEDIR//str13, STATUS= 'OLD', ACCESS= 
	1	   'SEQUENTIAL', FORM= 'FORMATTED', IOSTAT=IOS)
	      IF (ios .ne. 0)THEN
		 call putcontr(' Error updating collim,wavel,eff for data file')
	      ELSE
		 READ (1, *) wavel, (eff(i), i= 1,scl), (zp(i), i= 1, scl) !overwrite wavel
		 CLOSE(1)
		 OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	      ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
		 WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), !memorize new wavel
	1	      (mosaic(i), i= 1,3), wavel, dm, da, monobt1 ! & collim
		 CLOSE (1)
	      ENDIF
	   ENDIF		!collim to be changed
	ENDIF			!bt1

	IF (tempctr .AND. nsta.EQ.1 .AND. ibuf%mt.EQ.'NEUT' .AND.
	1  .NOT. prftemp .AND.
	2  ( NINT(ibuf%mpf).NE.1 .OR. ibuf%it.EQ.0.) )THEN 
	   fasttemp = .TRUE.	!read temp while counting last prefactor
	ELSE
	   fasttemp = .FALSE.
	ENDIF

	CALL DATE_TIME (sys_time)
	info(2)(1:)=ibuf%com

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	   IF (ibuf%ihf .NE. 0 .or. magtype .ge. 2)monmag=.TRUE.
C       IF (ibuf%ihf .NE. 0)monmag=.TRUE.
	   call putmax(ibuf%hf,str10,10,4)
	   info(2)(51:)=' H-field: '//str10
	ENDIF

C--------------------------DETERMINE WHETHER MOTOR ONE IS MOVED--------------C
	if (nsta.eq.4 .or. nsta.eq.-5 .or. nsta.eq.2 .or.
	1  nsta.eq.9 .or. nsta.eq.-1 .OR.nsta.eq.-11.or.
	2  (nsta .EQ. 0 .AND. poly) ) THEN
	   lowest=1		!1 is moved on these instruments
	   highest = 6
	elseif (nsta .eq. 1) THEN
	   lowest = 3
	   highest = 4
	else
	   lowest = 2		!1 is not moved on most instruments
	   highest = 6
	endif

C---------------------------DETERMINE DATA FILE -----------------------------C

	IF (keep) THEN		! Data storage is on
	   fname = File_Name (ibuf%com(1:5))
	   Call getdir(subdir,right) ! Get name of subdir
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
	

C-------------------------------PRINT HEADER --------------------------------C

	CALL PUTLINE_NF (contr, new_line)
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	CALL PUTLINE_NF (contr, sys_time(1:17))
	response = 'IBuffer '// bufno // ':  '// ibuf%com
	CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, new_line)
	DO j= lowest, highest	!lowest will be 2 except on bt4,bt2.bt9,ng5 where it is 1
	   IF (motfl(j)) THEN
	      af(j) = ibuf%a(j) + (ibuf%pts - 1) * ibuf%i(j)
	      str7(1) = PutStr (ibuf%a(j))
	      str7(2) = PutStr (ibuf%i(j))
	      str7(3) = PutStr (af(j))
	      WRITE (UNIT= out(1:1), FMT= '(i1)') j
	      response = 'A'// out(1:1) //'-beg= '// str7(1) //'  Inc-'// 
	1	   out(1:1) //'= '// str7(2) //'  A'// out(1:1) //
	1	   '-end= '// str7(3)
 	      CALL PUTLINE_NT (contr, response)
	   END IF
	END DO
	IF (rockfl .AND. .not. fixed(rkmot))THEN
	   str7(1) = PutStr (rkpos1)
	   str7(2) = PutStr (rkpos2)
	   WRITE(unit=out(1:2), FMT = '(i2)')rkmot
	   response(1:) = 'A'//out(1:2)//' rocking between '//str7(1)//
     1		' and '//str7(2)//' while counting.'
	   CALL Putline_NT (contr, response)
	ENDIF
	CALL Putmax(ibuf%tmp,str6(1),6,5)
	CALL Putmax(ibuf%it,str6(2),6,5)
	CALL putmax(ibuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(ibuf%tw)
	call putmax(ibuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') ibuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN	!magnetic field 
	   call putmax(ibuf%hf,str12(1),12,4)
	   call putmax(ibuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(ibuf%hfw)
c       call putmax(ibuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NT (contr, response)
	ENDIF			!magnetic field
	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(ibuf%mon)
	WRITE (UNIT= out(1:4), FMT= '(i4)') NINT(ibuf%mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1    //'    Type= '// ibuf%mt 
	IF (keep .AND. .NOT. polarized) response = response(1:46) //'           Datafile: '// fname
	CALL Putline_NT (contr, response)
	IF (nsta.eq.1)THEN
	   write(unit=str5,fmt='(I5)',iostat=ios)collim(1)
	   IF (colfound)THEN
	      CALL Putline_NT(contr,monobt1//' mono  '//str5//' collim')
	   ELSE
	      CALL Putline_NT(contr,monobt1//' mono  '//str5//' ASSUMED collim')
	   ENDIF
	ENDIF

	polindex = 0
	polcfg(1)%fname = fname
	ip = 1			!multiply duration of buffer by this in check_channel
	IF (polarized) THEN
	   CALL PUTLINE_NT (contr, 'Polarization analysis enabled.')
	   CALL PUTLINE_NT (contr, 'The following cross section(s) will be taken:')
	   IF (keep) READ (UNIT=fname(6:8), FMT= '(i3)') ver
	   ip = 0
	   DO j = 1, 4
	      IF (ibuf%flip_state(j)) THEN
	         ip = ip + 1
	         response(1:) = 'Flipper 1: '// flp(j,1) // '  Flipper 2: ' 
	1	      // flp(j,2) 
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
		    ELSE	!flp(j,1).eq.'ON '.and.flp(j,2).eq.'ON '
		       polcfg(polindex)%fname(11:11)='d'
		    ENDIF
                    response(1:) = response(1:30) //'    Datafile: '
	1		 // polcfg(polindex)%fname
	         END IF
                 CALL PUTLINE_NT (contr, response)
	      END IF
	   END DO
	   IF (ip .le. 0) ip = 1 !multiply duration by this in check_channel
	END IF      
	DO WHILE (polindex .LT. 5)
	   polindex = polindex + 1
	   polcfg(polindex)%csec = 0 ! signals end
	END DO
	fname=polcfg(1)%fname	! this always true


C----------------------------UPDATE HELP SCREEN -----------------------------C

	help_line(1)(1:) = ' Executing:  I-Buffer '// bufno(1:2) //
	1    '                Datafile: '
	IF (keep) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:50) // 'none        '
	END IF
	help_line(2)(1:) = ' '// ibuf%com(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
	info(1) = help_line(1)(2:)
	

C------------------PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	k = 14
	DO j= lowest, highest
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

C--------Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE !!')
	ENDIF

C---------Print warning if buffer has temp info but not T+
	IF (ibuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF

C---------Print warning if buffer has field info but not H+
	IF ( (ibuf%ihf .NE. 0.0 .or. ibuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF

C---------DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	if (wavel.ne.0)then
	   fenergy=81.804/(wavel**2)
	else
	   fenergy=0.
	endif
  	CALL Check_Channel (1, ibuf%mt, ibuf%mon, ibuf%mpf, ibuf%pts,fname,
	1  fenergy,ip,hstr)
	IF (READEF(contr_A))THEN
	   CALL PUTLINE (contr, ' **** ABORT during monitor rating ****')
	   RETURN
	END IF

C---------Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	   response(1:) = 'Helium level: '//hstr
	   CALL PUTLINE_NT (contr, response)
	   CALL PUTLINE_NF (contr, new_line)
	ENDIF

C----------------PRINT DATA HEADER ACCORDING TO INSTRUMENT ------------------C

	i = 0			! (* number of motors written *)
	wrap = .FALSE.
	response(1:) = 'Time     Pt# '
	k = 13
	DO j= lowest, highest
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:k+8) = response(1:k) //'    A'// out(1:1) //'  '
	      k = k + 8
	      i = i + 1
	   END IF
	   IF (i .GE. 6 .or. (i .GE. 5 .AND. monmag)) THEN
	      CALL Putline_NF (contr, response)
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
	CALL Putline_NF (contr, response)
	IF (prftemp)CALL Lia_Head(keep) !may need to test on temptype later
	CALL Putline_NF (contr, new_line)

C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	loop = 0

	DO WHILE (loop .LE. NINT(ibuf%pts) - 1 .AND. .NOT. finishup)

!       C--------------------- CHECK PAUSE FLAG ------------------------------C

	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()	! Hibernate 
              CALL clref (contr_A)
              CALL clref (hdw_event)
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 


!       C------- IF NECESSARY SWITCH POLARIZING FLIPPER --------------------C

	   polindex = 1
	   IF (polcfg(polindex)%csec .NE. 0) CALL Set_Flippers (polcfg(polindex)%csec)


!       C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) loop+1, '/', NINT(ibuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7)
	   i = 0		! (* number of motors written *)
	   k = 13
	   limit = .FALSE.
	   DO j = lowest, highest ! (* print new position first *)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	         posit(j) = ibuf%a(j) + ibuf%i(j) * loop 
	         IF (rockfl .AND. rkmot .EQ. j)THEN
		    WRITE (UNIT= out(1:8), FMT='(f8.3)') rkpos1
		 ELSE
		    WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
		 ENDIF
	         response(1:)= response(1:k) // out(1:8) 
	         k = k + 8
	         i = i + 1
	         IF (rockfl .AND. rkmot .EQ. j)response(k:k) = 'R'
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
	   IF (nsta .EQ. 4.and..not.fixed(2)) THEN !(* focussing *)
              posit(7) = foc7(posit(2))
	      posit(8) = foc8(posit(2))
	      DO j = 7, 8
		 IF (READEF(contr_A))GOTO 50
	         IF (loop .EQ. 0) THEN
  	            IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	         ELSE
	            IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1		 .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	         END IF
	         oldpos(j) = posit(j)
	      END DO
	   END IF
	   IF (nsta .EQ. -5.and..not.fixed(2)) THEN !(* focussing *)
              posit(7) = foc7(posit(2))
	      DO j = 7, 7
		 IF (READEF(contr_A))GOTO 50
	         IF (loop .EQ. 0) THEN
  	            IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	         ELSE
	            IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1		 .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	         END IF
	         oldpos(j) = posit(j)
	      END DO
	   END IF
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
	      CALL Putline (contr, new_line)
	      CALL PUTLINE (contr, error)
	      CALL Putline (contr, new_line)
	      IF (keep .AND. loop .GT. 0) CALL 
	1	   Save_File (fname, loop)
	      info(3)  = 'Buffer aborted, angle outside software limits!'
	      RETURN
	   END IF

	   IF (polarized .AND. polcfg(polindex)%csec .NE. 0) THEN
	      i = polcfg(polindex)%csec
	      CALL DATE_TIME (sys_time)	! say which cross section
	      response(1:) = sys_time(13:17) //'   #####  F1: '// flp(i,1) // 
	1	   '   F2: '// flp(i,2) // '       *'
	      CALL SCROLL_DISPLAY (contr)
	      CALL PUTCHARS (contr, response(1:40), 19, 1)
	      iwhere = 41
           END IF
	   DO j = highest, lowest, -1 ! (* drive motors in reverse order *)
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
	1	      .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + zj)
	      END IF
	      oldpos(j) = posit(j)
	   END DO
 50	   CALL Yield    
	   IF (READEF(contr_A))THEN
	      IF (keep) CALL Save_File (fname, loop)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
   	   END IF
	   

!       C------------ SET NEW TEMPERATURE AND WAIT ------------------------C

	   IF (prftemp) prfpoint = 0
	   IF (tempctr .OR. magnctr) THEN
	      CALL New_Temp_Field (temp,ibuf%tmp,ibuf%it,ibuf%tw,ibuf%ter,
	1	   ibuf%th0,ibuf%th,ibuf%hf, loop,ibuf%ihf,ibuf%hfh,ibuf%hfw)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
		 CALL PUTLINE (contr, ' **** ABORT during field change ****')
	         RETURN
	      END IF
	   END IF


!       C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO

C       Temporary code for debugging noise problem on bt1
	   IF (nsta .EQ. 1)THEN
	      DO i = scl + 2, 37
		 accucnts(i) = 0
	      ENDDO
	   ENDIF
C       End of temporary code

	   elap_time = 0.


!       C---------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL Reset_Display	!Reset NIM count display
	   IF (nsta .EQ. -5 .AND. use_asd) CALL Zero_ASD !zero data array
 200	   DO prf= 1, NINT(ibuf%mpf)
 201	      CALL RlyOff (4)	! Start display counter
              IF (ibuf%mt .EQ. 'NEUT') THEN
	         IF (nsta .EQ. -5 .AND. use_asd) CALL asd_on(1)
  	         CALL Count (ibuf%mon)
	      ELSE
	         IF (nsta .EQ. -5 .AND. use_asd) CALL asd_timed(1,ibuf%mon)
  	         CALL CnTime (ibuf%mon)
	      END IF
	      IF (rockfl .AND. .NOT. fixed(rkmot)) CALL Rock_Count(errno)
	      IF (prf .EQ. NINT(ibuf%mpf) .AND. fasttemp) then !last prefactor
		 IF (which_temp.eq.2)THEN !control
	            CALL RdTemp (dummy, temp, dummy)
		 ELSEIF (which_temp.eq.3) THEN !setpoint
	            CALL RdTemp (dummy, dummy, temp)
		 ELSE		!sample
	            CALL RdTemp (temp, dummy, dummy)
		 ENDIF
	      ENDIF		!last prefactor & fasttemp
  	      CALL Scaler (counts)
	      CALL RlyOn (4)	! Stop display counter
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF

	      IF ((nsta .eq. -5 .and. use_asd).or.(nsta.eq.-1.and.psd)
	1	   .or.(nsta.eq.-11.and.use_asd))
	1	   counts(scl+1)=NINT(fpwindow)

!       C--------------------- CHECK PAUSE FLAG ------------------------------C

	      IF (READEF(pause))THEN
		 CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
		 CALL CURSOR_OFF (paste)
		 CALL HIBER()	! Hibernate 
		 CALL clref (contr_A)
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

	      IF (statistic) THEN
		 IF (nsta .eq. 1) THEN !Bt1 so need filename
		    CALL Test_Ststcs_BT1 (fname,ibuf%mon, prf, counts, *100, *201)
		 ELSE
		    CALL Test_Ststcs (ibuf%mon, prf, counts, *100, *201)
		 ENDIF
	      ENDIF


              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      if (vmedata) then
		 call VSCALERADD
	      endif
C       Temporary code for debugging noise problem on bt1
	      IF (nsta .EQ. 1)THEN
		 DO i = scl + 2, 37
		    accucnts(i) = accucnts(i) + counts(i)
		 ENDDO
	      ENDIF
C       End of temporary code
	      IF (ibuf%mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
! elap_time = elap_time + NINT(ibuf%mon)
		 dsec = SecDiff()
		 elap_time = elap_time + dsec
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT ( MOD (elap_time, 60.))
	      IF (prftemp) CALL RdTemp (temp, dummy, dummy)
	      IF (nsta .eq. -5 .and. use_asd .and. .not. keep_rawasd)call asd_image
           END DO		! Prefactor loop
	   IF (rockfl .AND. .NOT. fixed(rkmot))CALL Yield !in case rocking was not finished
           IF (tempctr .AND. .NOT. fasttemp) THEN !temp not read while counting
	      IF (.not. prftemp) THEN
		 IF (which_temp.eq.2)THEN !control
		    CALL RdTemp (dummy, temp, dummy)
		 ELSEIF (which_temp.eq.3) THEN !setpoint
		    CALL RdTemp (dummy, dummy, temp)
		 ELSE		!sample
		    CALL RdTemp (temp, dummy, dummy)
		 ENDIF
	      ENDIF
	   ENDIF

	   IF (monmag)THEN	!monitor field
	      IF (magtype .eq.4)THEN !read white superconducting only for now 
		 Call RdField(field)
		 IF (field .EQ. -999.9)THEN !err reading back
		    field = fieldlast * hfconv
		 ELSE		!convert tesla to amps & then to user units !HOW???
		    field = field !just put out in tesla for now
		 ENDIF
	      ELSEIF (magtype.eq.3 .or. magtype.eq.5)THEN !blue superconducting 
		 field = fieldlast * hfconv
C       
		 Call Rdfield(field)
c       ELSEIF (magtype.eq.2)THEN !read back voltage for HP
c       CALL Rdfield(field) 
	      ELSE
		 field = ibuf%hf + loop*ibuf%ihf
	      ENDIF
	      IF (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)THEN !either SC magnet
		 hlevel = -999
		 CALL check_helium(alarm,hlevel,comerr,shutdone) !update done earlier
		 IF (shutdone)THEN
		    Call Contr_ring_bell(3)
		    hstr(1:)='Shutoff'
		 ELSEIF (comerr)THEN !communication problem
		    hstr(1:)='???????'
		 ELSE
		    CALL Putmax(hlevel,hstr,7,7)
		 ENDIF
	      ENDIF
	   ENDIF
!       C-------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

	   IF (nsta .NE. 1) THEN ! NON BT-1
	      IF (tempctr) THEN
		 str9(1:)=' '
		 IF (temptype .EQ. tdev%cfg) THEN !using degrees
		    CALL putmax(temp,str9(2:9),8,5)
		 ELSE		!using resistance
		    CALL putmax(temp,str9(2:9),8,7)
		 ENDIF
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
	      CALL Putline_NF (contr, response)
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
		       CALL Putline_NF (contr, outpsd(1:80))
c       WRITE (2, '(a80)') outpsd(1:80)
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    IF (kpsd .LE. 2) outpsd(1:) = ' '
		    WRITE (UNIT= datum, FMT= '(i20)') hsum
		    outpsd(kpsd:m) = datum(21-ee:20)
		    outpsd(m+1:m+1) = ','
		    kpsd = m + 2
		 END DO
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
		    IF (m .GE. 80) THEN	! write string to screen
		       CALL Putline_NF (contr, outpsd(1:80))
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    IF (kpsd .LE. 2) outpsd(1:) = ' '
		    WRITE (UNIT= datum, FMT= '(i20)') hsum
		    outpsd(kpsd:m) = datum(21-ee:20)
		    outpsd(m+1:m+1) = ','
		    kpsd = m + 2
		 END DO
		 IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	      ENDIF
	   ELSE			! BT-1
	      IF (tempctr) THEN
		 str10(1:)=' '
	     	 IF (temptype .EQ. tdev%cfg) THEN !using degrees
		    CALL putmax(temp,str10(2:10),9,5)
	         ELSE		!using resistance
		    CALL putmax(temp,str10(2:10),9,8)
	         ENDIF
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
		    CALL Putline_NF (contr, response)
		 ELSE		!more than 5 scalers
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10,a12)') 
	1		    min, ':', sec, str10, str12(1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a10)') 
	1		    min, ':', sec, str10
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
	1		    min, ':', sec, str12(1),'    ', (accucnts(i), i= 2, scl+1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a4,5i6)') 
	1		    min, ':', sec, '    ', (accucnts(i), i= 2, scl+1)
		    ENDIF
		    CALL Putline_NF (contr, response)
		 ELSE		!more than 5 scalers
		    IF (monmag)THEN
		       CALL putmax(field,str12(1),12,4)
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2,a12)') 
	1		    min, ':', sec, str12(1)
		    ELSE
		       WRITE (UNIT= response, FMT= '(i4,a,i2.2)') 
	1		    min, ':', sec
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
	   IF (wrap)THEN
	      info(4) = response
	   ELSE	   
	      info(3)(k+1:) = response
	   ENDIF
	   IF (keep) THEN
	      CALL Dmp_Data (fname, loop, accucnts, elap_time, 1, ibuf,
	1	   idum ,idum,idum, temp, polcfg(polindex)%csec,field,monmag) 
	      IF (nsta .EQ. -5 .AND. USE_ASD .AND. Keep_RawASD)
	2	   CALL Dmp_ASD(fname,loop)
	   ENDIF		!keep
	   polindex = polindex + 1
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      IF (tempctr .and. (ibuf%tmp .ne. 0.) .and. (ibuf%tw .ne. 0.))
	1	   Call Check_Heater
              CALL Run_Polar (1, ibuf,idum,idum, polcfg(polindex), loop,keep)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
	      polindex = polindex + 1
	   END DO

!       C------------------ UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5))
	1	info(4)(60:)='Helium Lvl:'//hstr

	   CALL Status_File (info)

	   loop = loop + 1

	END DO			! (* Number of points loop *)

	IF (loop .LE. NINT(ibuf%pts) - 1 .AND. finishup)THEN
	   finishup = .FALSE.
	   IF (keep .AND. loop .GT. 0) CALL Save_File (fname, loop)
	   CALL Putcontr(' Ctrl F detected')
	   CALL Putcontr_NF(' ')
	   CALL Status_Line	!erase CtrlF message
	ENDIF

	IF (keep) THEN
	   IF (nsta .eq. 1) CALL Stampfile (fname)
	   context = 0
	   CALL Make_Backup (polcfg(1)%fname)
	   polindex = 2
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Make_Backup (polcfg(polindex)%fname)
	      polindex = polindex + 1
	   END DO
	END IF
	END

