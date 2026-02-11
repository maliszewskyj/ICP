#include "icp.h"
	SUBROUTINE Run_R_Buf (buf_no, keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes Refl Buffer No. buf_no                                      C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, Feb   1993                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use refl
	use polar
	use histo
	use tc
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, j, m, status, st,  typ, ver, kj,kpsd,ip
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37)
	INTEGER*4     K
	INTEGER*4     min, sec, polindex, right, ios
	REAL*4         RdAngl, qxnow,qznow,alphai,alphaf
	REAL*4	      tmat(3,3), q0(3), q(3), v1(3), v2(3)
	REAL*4        elap_time, dsec, temp /0./, dummy, t, af, secdiff
	LOGICAL       limit,varimon, monmag, comerr, shutdone, alert, alarm,
     1			inlimits, Readef
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(3), PutStr, hstr
	CHARACTER*8   out, new_line/'        '/
	CHARACTER*9   str9
	CHARACTER*10  str10(3)
	CHARACTER*12  fname, File_Name, str12(4)
	character*20   str20(4)
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response, info(4), error, help_line(3), subdir,outpsd
	CHARACTER*240 longl
	REAL*4        fenergy, field, hlevel
	INTEGER*4     tomove,motlist(10),hsum,ee,IWAIT/1/,NOWAIT/0/
	REAL	      runmon
	CHARACTER*33 PGSTART/'TYPE PGAA"CPGA "::"TASK=STARTPGA"'/
	CHARACTER*32 PGSTOP/'TYPE PGAA"CPGA "::"TASK=STOPPGA"'/

	REAL*4   posit(MAXMOTS), oldpos(MAXMOTS),slitpos(4)
	INTEGER*4  slitname(4)/1,2,3,4/ 
   
	TYPE(r_buf) rbuf
	TYPE(polarize) polcfg(5)

	finishup = .FALSE.

	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL=RBUF_RECL)
	READ (1,REC=buf_no) rbuf
	CLOSE (1)

	IF (PSD .AND. RBUF%MPF .NE. 1) THEN
	   rbuf%mon0 = rbuf%mon0 * rbuf%mpf !no pref with position sensitive det.
	   rbuf%mpf = 1
	   CALL PUTLINE (contr, ' Prefactors not allowed with PSD !!!')
	ENDIF

	CALL DATE_TIME (sys_time)
	info(2)(1:)=rbuf%com
	info(4)=' '

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	   IF (rbuf%ihf .NE. 0)monmag=.TRUE.
	   call putmax(rbuf%hf,str10(1),10,4)
	   info(2)(51:)=' H-field: '//str10(1)
	ENDIF


c       - For 2.35 Angstroms, use PG filter.  Otherwise use Be filter
	if (wavel.lt.3) then
	   tomove = PGmove
	   DO i = 1,tomove
	      motlist(i) = PGlist(i)
	   ENDDO
	   st7(3) = 'PG'
	else   
	   tomove = Bemove
	   DO i = 1,tomove
	      motlist(i) = Belist(i)
	   ENDDO
	   st7(3) = 'Be'
	endif

C---------------------------DETERMINE DATA FILE -----------------------------C

	IF (keep) THEN		! Data storage is on
	   fname = File_Name (rbuf%com(1:5))
	   CALL Getdir(subdir,right)
	   CALL PUTLINE_NF (contr, ' ')
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
	

C-------------------------------PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
	CALL Putline_NF (contr, ' ')
	CALL Putline_NF (contr, sys_time(1:17))
	WRITE (UNIT= bufno(1:2), FMT= '(i2)') buf_no
	response = 'Refl_buf '// bufno // ':  '// rbuf%com
	CALL Putline_NT (contr, response)
	response = ' '
	CALL Putline_NF (contr, response)

	write(unit=st7(1),fmt='(f7.4)',iostat=ios)dm
	write(unit=st7(2),fmt='(f7.3)',iostat=ios)wavel
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(rbuf%pts)
	response(1:) = 'Filter: '//st7(3)(1:2)//'  Wavel: '//st7(2)//
	1  '  Dm: '//st7(1)//'  #pts= '// str6(2)(1:3)
	IF (keep .AND. .NOT. polarized) response = response(1:50) //
	1  ' Datafile: '// fname
	CALL Putline_NT (contr, response)

	af = rbuf%qz + (rbuf%pts - 1) * rbuf%qzi
	write (UNIT=str10(1),fmt='(F10.6)',iostat=ios)rbuf%qz
	write (UNIT=str10(2),fmt='(F10.6)',iostat=ios)rbuf%qzi
	write (UNIT=str10(3),fmt='(F10.6)',iostat=ios)af
	response = 'QZ-beg= '// str10(1) //'  Inc-QZ= '// 
	1    str10(2) //'  QZ-end= '// str10(3)
	CALL Putline_NT (contr, response)

	af = rbuf%qx + (rbuf%pts - 1) * rbuf%qxi
	write (UNIT=str10(1),fmt='(F10.6)',iostat=ios)rbuf%qx
	write (UNIT=str10(2),fmt='(F10.6)',iostat=ios)rbuf%qxi
	write (UNIT=str10(3),fmt='(F10.6)',iostat=ios)af
	response = 'QX-beg= '// str10(1) //'  Inc-QX= '// 
	1    str10(2) //'  QX-end= '// str10(3)
	CALL Putline_NT (contr, response)

	DO kj = 1, 4		!slit motors
	   j=slitlist(kj)
	   IF (motfl(j)) THEN
	      af = rbuf%s(kj) + (rbuf%pts - 1) * rbuf%i(kj)
	      st7(1) = PutStr (rbuf%s(kj))
	      st7(2) = PutStr (rbuf%i(kj))
	      st7(3) = PutStr (af)
	      WRITE (UNIT= out(1:1), FMT= '(i1)') kj
	      response = 'S'// out(1:1) //'-beg=  '// st7(1) //'    Inc-'// 
	1	   out(1:1) //'=   '// st7(2) //'    S'// out(1:1) //
	1	   '-end=  '// st7(3)
	      CALL Putline_NT (contr, response)
	   END IF
	ENDDO

	IF (rbuf%numx .NE. 0)THEN !Xtra motor
	   j=rbuf%numx
	   IF (motfl(j)) THEN
	      af = rbuf%sx + (rbuf%pts - 1) * rbuf%ix
	      st7(1) = PutStr (rbuf%sx)
	      st7(2) = PutStr (rbuf%ix)
	      st7(3) = PutStr (af)
	      WRITE (UNIT= out(1:2), FMT= '(i2)',iostat=ios) j
	      response = out(1:2) //'-beg=  '// st7(1) //'    Inc-'// 
	1	   out(1:2) //'=  '// st7(2) //'    '// out(1:2) //
	1	   '-end=  '// st7(3)
	      CALL Putline_NT (contr, response)
	   END IF
	ENDIF


	CALL Putmax(rbuf%tmp,str6(1),6,3)
	CALL Putmax(rbuf%it,str6(2),6,3)
	CALL Putmax(rbuf%ter,str6(3)(1:3),3,2)	
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',iostat=ios) NINT(rbuf%tw)
	call putmax(rbuf%hf,out(1:8),8,4)

	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min   HField= '// out(1:8)
	CALL Putline_NT (contr, response)
	IF (magnctr) THEN	!magnetic field
	   call Putmax(rbuf%hf,str12(1),12,4)
	   Call Putmax(rbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(rbuf%hfw)
	   Call Putmax(hfconv,str12(4),12,4)
	   response(1:)='Hfield= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   Call Putline_NT (contr,response)
	ENDIF			!magnetic field
	WRITE (UNIT= out, FMT= '(i8)',iostat=ios) NINT(rbuf%mon0)
	WRITE (UNIT= str6(1)(1:3), FMT= '(i3)',iostat=ios) NINT(rbuf%mpf)
	response(1:) = 'Prefac= '//str6(1)(1:3)//'  Mon0='//out
	j = 27
	IF (rbuf%mon1 .ne. 0) THEN
	   write(unit= out, fmt= '(i8)',iostat=ios)NINT(rbuf%mon1)
	   write(unit= str6(1)(1:6),fmt='(f6.2)',iostat=ios)rbuf%exp
	   response(1:)=response(1:j)//'  Mon1= '//out//'  Exp= '//str6(1)
	   j = j + 29
	   varimon = .true.	!variable count time or neutrons
	ELSE
	   varimon = .false.
	ENDIF
	response(1:)=response(1:j)//'  Type= '//rbuf%mt
	j = j + 13
	WRITE (UNIT= str6(1)(1:5), FMT= '(f5.3)',iostat=ios) eff(1)
	response(1:)=response(1:j)//' SF= '//str6(1)(1:5)
	CALL Putline_NT (contr, response)
	polindex = 0
	polcfg(1)%fname = fname
	ip = 1			!multiply duration of buffer by this in check_channel
	IF (polarized) THEN
	   CALL Putline_NT (contr, 'Polarization analysis enabled.')
	   CALL Putline_NT (contr, 'The following cross section(s) will be taken:')
	   IF (keep) READ (UNIT=fname(6:8), FMT= '(i3)') ver
	   ip = 0
	   DO j = 1, 4
	      IF (rbuf%flip_state(j)) THEN
	         ip = ip+1
	         response(1:) = 'Flipper 1: '// flp(j,1) // '  Flipper 2: ' 
	1	      // flp(j,2) 
	         polindex = polindex + 1 
   		 polcfg(polindex)%csec  = j 
	         IF (keep) THEN
	            polcfg(polindex)%fname = fname
	            WRITE (UNIT=polcfg(polindex)%fname(6:8), FMT= '(i3.3)',
	1		 iostat=ios) ver
	            IF (flp(j,1).eq.'OFF'.and.flp(j,2).eq.'OFF')THEN
		       polcfg(polindex)%fname(11:11)='A'
		    ELSE IF (flp(j,1).eq.'ON '.and.flp(j,2).eq.'OFF')THEN
		       polcfg(polindex)%fname(11:11)='B'
		    ELSE IF(flp(j,1).eq.'OFF'.and.flp(j,2).eq.'ON ')THEN
		       polcfg(polindex)%fname(11:11)='C'
		    ELSE	!flp(j,1).eq.'ON '.and.flp(j,2).eq.'ON '
		       polcfg(polindex)%fname(11:11)='D'
		    ENDIF
                    response(1:) = response(1:30) //'    Datafile: '
	1		 // polcfg(polindex)%fname
	         END IF
                 CALL Putline_NT (contr, response)
	      END IF
	   END DO
	END IF      
	DO WHILE (polindex .LT. 5)
	   polindex = polindex + 1
	   polcfg(polindex)%csec = 0 ! signals end
	END DO
	fname=polcfg(1)%fname



C----------------------------UPDATE HELP SCREEN -----------------------------C

	help_line(1)(1:) = ' Executing:  R-Buffer '// bufno(1:2) //
	1    '           Datafile: '
	IF (keep) THEN 
	   help_line(1) = help_line(1)(1:45) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:67) // 'none        '
	END IF
	help_line(2)(1:) = ' '// rbuf%com(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
        info(1) = help_line(1)(2:)

C------------------PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	m = 14
	DO j= 1, MAXMOTS
	   IF (motfl(j) .AND. fixed(j)) THEN
	      WRITE (UNIT= out(1:2), FMT='(i2)') j
	      response(1:) = response(1:m) //'  A'// out(1:2) //'='
	      m = m + 6
  	      WRITE (UNIT= out(1:8), FMT= '(f8.3)',iostat=ios) RdAngl(j) - z(j)
	      response = response(1:m) // out(1:8)
	      m = m + 8
	   END IF
	END DO
	IF (m .GT. 15) CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')
	
C--------Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE !!')
	ENDIF

C---------Print warning if buffer has temp info but not T+
	IF (rbuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF

C---------Print warning if buffer has field info but not H+
	IF ( (rbuf%ihf .NE. 0.0 .or. rbuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF

C---------DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	if (wavel.ne.0) then
	   fenergy=81.804/(wavel**2) !??????????????????????????
	else
	   fenergy=0.
	endif
  	CALL Check_Channel (2, rbuf%mt, rbuf%mon0, rbuf%mpf, rbuf%pts,fname,
	1    fenergy,ip,hstr)
	IF (READEF(contr_A))THEN
	   CALL Putline (contr, ' **** ABORT during monitor rating ****')
	   RETURN
	END IF

C---------Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	   response(1:) = 'Helium level: '//hstr
	   CALL PUTLINE_NT (contr, response)
	   CALL PUTLINE_NF (contr, new_line)
	ENDIF

C----------------------------PRINT DATA HEADER ------------------------------C

	response(1:) = 'Time    Pt#    '
	m = 15
	DO kj = 1, 4
	   j = slitlist(kj)
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      WRITE (UNIT= response(m+1:m+9), FMT= '(a5,i1,a3)',iostat=ios) 
	1	   '    S',slitname(kj),'   '  
	      m = m + 9
	   END IF
	END DO
	IF (rbuf%numx .NE. 0) THEN
	   write(unit=response(m+1:m+9),fmt='(1x,2x,a,I2.2)',iostat=ios)'X',rbuf%numx
	   m = m+9
	ENDIF
	IF (tempctr .AND. rbuf%tmp .NE. 0. .AND. rbuf%it .NE. 0.) THEN
	   WRITE (UNIT= response(m+1:m+9), FMT= '(a9)') '   T-set '
	END IF
	CALL Putline_NF (contr, response)
	response = '              QZ         QX                TIME        MON    COUNTS     COUNTS'
	IF (tempctr) THEN
	   response = response(1:69) //'TEMP      INTEN'
	   k = 84
	ELSE
	   k = 79
	ENDIF
	IF (monmag)THEN		!monitor field
	   response(1:k+5) = response(1:k)//'   HF'
	   k = k+5
	ENDIF
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	

	


C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	loop = 0

	DO WHILE (loop .LE. NINT(rbuf%pts) - 1 .AND. .NOT. finishup)

!       C------------------ CHECKPAUSE FLAG ------------------------------C

	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()	! Hibernate 
              CALL clref (contr_A)
	      CALL clref (pause)
              CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
              CALL CURSOR_ABS (contr, 19, 1)
           END IF 



C       C--CALCULATE ANGLES -----------------------------------------------C

           qznow = rbuf%qz + rbuf%qzi * loop
           qxnow = rbuf%qx + rbuf%qxi * loop 
	   Call refl_pos(qznow,qxnow,alphai,alphaf,posit) !Puts positions in array posit
	   IF (varimon .and. qznow .eq. 0 .and. rbuf%exp .le. 0) THEN
	      write(unit= str6(1)(1:6),fmt='(f6.2)',iostat=ios)rbuf%exp
	      response(1:)=' *** Warning: Point skipped because qz = 0. and exp = '
	1	   //str6(1)//' ***'
	      CALL Putline(contr,response)
	      GOTO 999
	   ENDIF

	   DO kj = 1, 4		!slit motors
	      j=slitlist(kj)
	      posit(j) = rbuf%s(kj) + rbuf%i(kj) * loop
	   ENDDO

	   IF (rbuf%numx .NE. 0) posit(rbuf%numx) = rbuf%sx + 
     1		rbuf%ix * loop

!       C-------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
!       C---------------- IF NOT GIVE WARNING -----------------------------C
	   
	   limit = .FALSE.
	   DO kj = 1, tomove	!check all motors to be moved
	      j = motlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,contr))THEN
		    limit = .TRUE.
		    CALL Putline (contr, ' Buffer aborted')
	         ENDIF
	      END IF
	   END DO
	   DO kj = 1, 4		!check slit openings
	      j = slitlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,contr))THEN
		    limit = .TRUE.
		    CALL Putline (contr, ' Buffer aborted')
	         ENDIF
	      END IF
	   END DO
	   IF (rbuf%numx .NE. 0) THEN !Xtra motor
	      j = rbuf%numx
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,contr))THEN
		    limit = .TRUE.
		    CALL Putline (contr, ' Buffer aborted')
	         ENDIF
	      END IF
	   END IF		!Xtra motor
	   IF (limit) GOTO 999

	   
!       C------- IF NECESSARY SWITCH POLARIZING FLIPPER --------------------C

	   polindex = 1
	   IF (polcfg(polindex)%csec .NE. 0) 
	1	CALL Set_Flippers (polcfg(polindex)%csec)


!       C------------ SET NEW TEMPERATURE AND WAIT ------------------------C

	   IF (tempctr .OR. magnctr) THEN
	      CALL New_Temp_Field (temp,rbuf%tmp,rbuf%it,rbuf%tw,rbuf%ter,
	1	   rbuf%th0,rbuf%th,rbuf%hf, loop,rbuf%ihf,rbuf%hfh,rbuf%hfw)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL save_file (fname, loop)
		 CALL Putline (contr, ' **** ABORT during field change ****')
	         RETURN
	      END IF
	   END IF

!       C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION OF SLITs -----C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',iostat=ios) loop+1, '/', NINT(rbuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7) // ' '
	   info(3)(1:) = response(1:14)
	   m = 14
	   DO kj = 1, 4
! (* print new position first *)
	      j = slitlist(kj)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)',iostat=ios) posit(j)
		 m = m + 9
	      END IF
  	   END DO	   
	   IF (rbuf%numx .NE. 0) THEN !xtra motor
! (* print new position first *)
	      j = rbuf%numx
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)',iostat=ios) posit(j)
		 m = m + 9
	      END IF
  	   END IF		!xtra motor
	   IF (tempctr .AND. rbuf%tmp .NE. 0. .AND. rbuf%it .NE. 0.)
	1	CALL Putmax(temp,response(m+1:m+9),9,3)
	   CALL Putline_NF (contr, response)
	   DO kj = 1, tomove
	      IF (READEF(contr_A))GOTO 50
	      j = motlist(kj)
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1	      .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	      END IF
	      oldpos(j) = posit(j)
	   END DO
	   DO kj = 1, 4		!move slit openings
	      IF (READEF(contr_A))GOTO 50
	      j = slitlist(kj)
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1	      .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	      END IF
	      oldpos(j) = posit(j)
	   END DO
	   IF (rbuf%numx .ne. 0) THEN
	      IF (READEF(contr_A))GOTO 50
	      j = rbuf%numx
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1	      .AND. .NOT. fixed(j) ) CALL Drive (j, posit(j) + z(j))
	      END IF
	      oldpos(j) = posit(j)
	   END IF		!Xtra motor
	   WRITE (UNIT= response, FMT= '(11x,2f11.6,a)',iostat=ios)
	1	qznow, qxnow, '   *'
	   m = 40
 	   CALL Putline_NCR (contr, response(1:m))
	   CALL CURSOR_ABS (contr, 19, m+1)
	   info(3)(15:) = response(1:m)
	   k = m
	   IF (polarized .AND. polcfg(polindex)%csec .NE. 0) THEN
	      i = polcfg(polindex)%csec
	      CALL DATE_TIME (sys_time)	! say which cross section
	      response(1:) = sys_time(13:17) //'   #####  F1: '// flp(i,1) // 
	1	   '   F2: '// flp(i,2) // '       *'
	      CALL SCROLL_DISPLAY (contr)
	      CALL PUTCHARS (contr, response(1:40), 19, 1)
           END IF
 50 	   CALL Yield   
	   IF (READEF(contr_A))THEN
	      IF (keep) CALL save_file (fname, loop)
	      CALL Putline (contr, ' **** ABORT during drive ****')
	      RETURN
	   END IF


!       C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.


!       C---------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL Reset_Display	!Reset NIM count display
	   CALL RlyOn (3)	! Reset little display counter 
	   CALL RlyOff (3)
 200       DO prf = 1, NINT(rbuf%mpf)
 201          continue
 	      CALL RlyOff (4)	! Start display counter
	      IF (varimon)THEN
c       write(unit=str20(1),fmt=*)qznow
c       write(unit=str20(2),fmt=*)rbuf%exp
c       CALL Putcontr(' qznow: '//str20(1)//' .exp: '//str20(2))
		 runmon = rbuf%mon0 + (rbuf%mon1 * (abs(qznow)**rbuf%exp))
c       CALL Putcontr(' back from varimon')
	      ELSE
		 runmon = rbuf%mon0
	      ENDIF
	      IF (liqwait.ne.0.)CALL libwait(liqwait,status) !wait for liq to stabilize
	      IF (rbuf%mt .EQ. 'NEUT') THEN
  	         CALL Count (runmon)
	      ELSE	
  		 CALL CnTime (runmon)
	         CALL DATE_TIME (start_time) !calc elapsed time for NG7
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)	! Stop display counter
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL save_file (fname, loop)
		 CALL Putline (contr, ' **** ABORT during count ****')
		 RETURN
	      END IF


!       C--------- CHECK STATISTICS IF ENABLED -------------------------C

	      IF (statistic) THEN              
		 CALL Test_Ststcs (rbuf%mon0, prf, counts, *100, *201)
	      END IF
	      

!       C--------------- ACCUMULATE COUNTS ------------------------------C

	      DO i = 1, scl + 1
		 accucnts(i) = accucnts(i) + counts(i)
	      END DO

	      IF (rbuf%mt .EQ. 'NEUT' .or. varimon) THEN
		 dsec = SecDiff() !function to get stop_time - start_time
		 elap_time = elap_time + dsec
	      ELSE
		 elap_time = elap_time + NINT(rbuf%mon0)
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT( MOD (elap_time, 60.))
 	   END DO		! Prefactor loop
	   IF (tempctr) then
	      IF (which_temp.eq.2)THEN !control
		 CALL RdTemp (dummy, temp, dummy)
	      ELSEIF (which_temp.eq.3) THEN !setpoint
		 CALL RdTemp (dummy, dummy, temp)
	      ELSE		!sample
		 CALL RdTemp (temp, dummy, dummy)
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
	      ELSE
		 field = rbuf%hf + loop*rbuf%ihf
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

!       C--------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

	   IF (tempctr) THEN
	      Call Putmax(temp,str10(1),10,3)
	      IF (monmag)THEN
		 CALL putmax(field,str12(1),12,4)
		 WRITE (UNIT= response, FMT= 
	1	      '(i4,a,i2.2,1x,i11,1x,i9,1x,a10,1x,i9,a12)',iostat=ios) 
	2	      min, ':', sec, accucnts(1),accucnts(2),str10(1),
	3	      accucnts(3), str12(1)
	      ELSE
		 WRITE (UNIT= response, FMT= 
	1	      '(i4,a,i2.2,1x,i11,1x,i9,1x,a10,1x,i9,1x,a10)',iostat=ios) 
	2	      min, ':', sec, accucnts(1),accucnts(2),str10(1),
	3	      accucnts(3)
	      ENDIF
	   ELSE
	      IF (monmag)THEN
		 CALL putmax(field,str12(1),12,4)
		 WRITE (UNIT= response, FMT= '(i4,a,i2.2,i11,i9,i9,a12)',
	1	      iostat=ios) 
	2	      min, ':', sec, accucnts(1), accucnts(2),accucnts(3),
	3	      str12(1)
	      ELSE
		 WRITE (UNIT= response, FMT= '(i4,a,i2.2,i11,i9,i9)',
	1	      iostat=ios) 
	2	      min, ':', sec, accucnts(1), accucnts(2),accucnts(3)
	      ENDIF
	   END IF
	   CALL Putline_NF (contr, response)
	   info(3)(k+1:) = response
	   IF (PSD) THEN	!write position sensitive detector data
	      outpsd(1:)=' '
	      kpsd = 2
	      DO i = 1, nocells/16 !groups of 16 cells
		 hsum=0
		 DO j = 1, 16	!sum adjacent 16 cells
	            hsum=hsum+(histodata((i-1)*16+j))
		 ENDDO
		 ee = 1
		 DO WHILE (hsum .GE. 10**ee)
	            ee = ee + 1
		 END DO
		 m = kpsd + ee-1	    	     
		 IF (m .GE. 80) THEN ! write string to screen
	            CALL Putline_NF (contr, outpsd(1:80))
c       WRITE (2, '(a80)') outpsd(1:80)
	            kpsd = 2
		    m = 2 + ee-1
		 END IF
		 IF (kpsd .LE. 2) outpsd(1:) = ' '
		 WRITE (UNIT= datum, FMT= '(i20)',iostat=ios) hsum
		 outpsd(kpsd:m) = datum(21-ee:20)
		 outpsd(m+1:m+1) = ','
		 kpsd = m + 2
	      END DO
c       IF (m.gt.2)write(2,fmt='(a)',iostat=ios)outpsd(1:m)	
	      IF (m.gt.2)THEN
		 CALL Putline_NF (contr, outpsd(1:m))
		 CALL Putline_NF (contr, ' ')
	      ENDIF
	   ENDIF
	   IF (keep) CALL Dmp_Refl (fname, loop, accucnts, elap_time, rbuf,
	1	temp, 0 ,field,monmag)
	   polindex = polindex + 1
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Run_R_Polar (rbuf, polcfg(polindex), loop, keep)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL save_file (fname, loop)
	         CALL Putline (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
	      polindex = polindex + 1
	   END DO
           GOTO 300

C---------OUTSIDE LIMITS: SET COUNTS TO ZERO THEN GO TO NEXT POINT ----------C

 999  	   accucnts(2) = 0
 	   accucnts(1) = 0
	   accucnts(3) = 0
	   temp = 0.
	   elap_time = 0
	   IF (keep) CALL Dmp_Refl (fname, loop, accucnts, elap_time,rbuf,
	1	temp, 0 ,field,monmag)


!       C--------------------- UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5))info(4)(60:)=
     1		'Helium Lvl:'//hstr


 300	   CALL Status_File (info)

	   loop = loop + 1

	END DO			! (* Number of points loop *)

	IF (loop .LE. NINT(rbuf%pts) - 1 .AND. finishup)THEN
	   finishup = .FALSE.
	   IF (keep .AND. loop .GT. 0) CALL Save_File (fname, loop) !adjust #pts & final positions
	   CALL Putcontr(' Ctrl F detected')
	   CALL Putcontr_NF(' ')
	   CALL Status_Line	!erase CtrlF message
	ENDIF

        IF (keep) THEN
c       CALL Make_Backup (polcfg(1)%fname)
	   polindex = 2
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Make_Backup (polcfg(polindex)%fname)
	      polindex = polindex + 1
	   END DO
	END IF
	END

