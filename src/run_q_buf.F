#include "icp.h"
	SUBROUTINE Run_Q_Buf (buf_no, keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes Const-Q Buffer No. buf_no                                      C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, August 1989                                                C
C     Modified by Gayle Greene, April 1991 (change in polarized filenames     C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use polar
	use focus
	use tc
	use globals
	use asd
	use lia
	IMPLICIT      none

	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, j, m, status, st, typ, ver, ip, ios, idum
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37)
	INTEGER*4     ilast, kpsd,ee
	INTEGER*4     imin, sec, polindex, right, iwhere, hsum
	REAL*4        posit(MAXMOTS), oldpos(MAXMOTS), RdAngl, e0, h0, k0, l0, energy
	REAL*4	      tmat(3,3), q0(3), q(3), v1(3), v2(3)
	REAL*4	      Energy_Angle, Phi_Angle, Psi_Angle, RdAm, secdiff
	REAL*4        elap_time, dsec, foc7, foc8, temp /0./, dummy, t, zj, Fz
	LOGICAL       limit, monmag, comerr, shutdone, alarm, inlimits, 
     1			prftemp,Readef,savepos
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(3), PutStr, hstr
	CHARACTER*8   out, new_line/'        '/
	CHARACTER*9   str9
	CHARACTER*10  str10
	CHARACTER*12  fname, File_Name,str12(4)
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response, info(4), error, help_line(3), subdir, outpsd
	CHARACTER*240 longl
	REAL*4        fenergy,field, hlevel

	TYPE(q_buf) qbuf
C	TYPE(q_buf) man_q ! in smg_prep
	TYPE(lattice) cr
	TYPE(polarize) polcfg(5)

C	COMMON /drive_to_q/ man_q            ! Common with drive_q for defaults

	finishup = .FALSE.

	OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL=QBUF_RECL)
	READ (1,REC=buf_no) qbuf
	CLOSE (1)
	man_q = qbuf		! Determines defaults for drive_q
	lasta = qbuf%a		!memorize last lattice constants
	lastb = qbuf%b
	lastc = qbuf%c
	lastef = qbuf%ef

c       do we save angle positions to a separate data file?
	if (verbose_run .and. (qbuf%es .ne. 0.0 .or. qbuf%hs .ne. 0.0 .or.
	1  qbuf%ks .ne. 0.0 .or. qbuf%ls .ne. 0.0) )THEN
	   savepos = .true.
	else
	   savepos = .false.
	endif	

	IF (tempctr .AND. temptype .EQ. 13)THEN
	   prftemp = .TRUE.	!read temp each prefactor
	ELSE
	   prftemp = .FALSE.	!no need to read temp each prefactor
	ENDIF

	IF (nsta .eq. -5 .AND. use_asd) THEN
	   call read_asdfile
	   use_asdfile = .TRUE.
	ENDIF

	CALL DATE_TIME (sys_time)
	info(2)(1:)=qbuf%com

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	   IF (qbuf%ihf .NE. 0 .or. magtype .ge. 2)monmag=.TRUE.
C       IF (qbuf%ihf .NE. 0)monmag=.TRUE.
	   call putmax(qbuf%hf,str10,10,4)
	   info(2)(51:)=' H-field: '//str10
	ENDIF

C---------------------------DETERMINE DATA FILE -----------------------------C

	IF (keep) THEN		! Data storage is on
	   fname = File_Name (qbuf%com(1:5))
	   CALL Getdir(subdir,right)
	   CALL PUTLINE_NF (contr, ' ')
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
	
C-------------------------------PRINT HEADER --------------------------------C

	CALL PUTLINE_NF (contr, ' ')
	CALL PUTLINE_NF (contr, ' ')
	CALL PUTLINE_NF (contr, sys_time(1:17))
	WRITE (UNIT= bufno(1:2), FMT= '(i2)') buf_no
	response = 'QBuffer '// bufno // ':  '// qbuf%com
	CALL PUTLINE_NT (contr, response)
	response = ' '
	CALL PUTLINE_NF (contr, response)
	st7(1) = PutStr (qbuf%ec)
	st7(2) = PutStr (qbuf%es)
	st7(3) = PutStr (qbuf%ef)
	response = 'E-center = '// st7(1) //'  E-step = '// st7(2)
	IF (qbuf%eft .EQ. 'ANA') THEN
	   typ = 2
	   response = response(1:36) //'     Analyzer fixed at '// st7(3) //
	1	' meV'
	ELSE
	   typ = 3
	   response = response(1:36) //'     Monochromator fixed at '// st7(3)
	1	//' meV'	
	END IF
	CALL PUTLINE_NT (contr, response)	
	WRITE (UNIT= str6(1), FMT= '(f6.3)') qbuf%hc
	WRITE (UNIT= str6(2), FMT= '(f6.3)') qbuf%kc
	WRITE (UNIT= str6(3), FMT= '(f6.3)') qbuf%lc   
	response = 'Q-center = ('// str6(1) //','// str6(2) //','//str6(3)//')'
	WRITE (UNIT= str6(1), FMT= '(f6.4)') qbuf%hs
	WRITE (UNIT= str6(2), FMT= '(f6.4)') qbuf%ks
	WRITE (UNIT= str6(3), FMT= '(f6.4)') qbuf%ls   
	response = response(1:33) //'      Q-step = ('// str6(1) //','// str6(2)
	1  //','// str6(3) //')'
	CALL PUTLINE_NT (contr, response) 


	CALL Putmax(qbuf%tmp,str6(1),6,5)
	CALL Putmax(qbuf%it,str6(2),6,5)
	CALL putmax(qbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(qbuf%tw)
	call putmax(qbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') qbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN	!magnetic field 
	   call putmax(qbuf%hf,str12(1),12,4)
	   call putmax(qbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(qbuf%hfw)
c       call putmax(qbuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NT (contr, response)
	ENDIF			!magnetic field
	WRITE (UNIT= out(1:7), FMT= '(i7)') NINT(qbuf%mon)
	WRITE (UNIT= str6(1)(1:3), FMT= '(i3)') NINT(qbuf%mpf)
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(qbuf%pts)
	response = 'Monitor= '// out(1:7) //'  Prefac= '// str6(1)(1:3)
	1  //'  Type= '// qbuf%mt //'  #pts = '// str6(2)(1:3)
	IF (keep .AND. .NOT. polarized) response = response(1:53) //'    Datafile: '// fname
	CALL PUTLINE_NT (contr, response)
	polindex = 0
	polcfg(1)%fname = fname
	DO j = 1,5
	   polcfg(j)%csec = 0	! signals end
	END DO
	ip = 1			!multiply duration of buffer by this in check_channel
	IF (polarized) THEN
	   CALL PUTLINE_NT (contr, 'Polarization analysis enabled.')
	   CALL PUTLINE_NT (contr, 'The following cross section(s) will be taken:')
	   IF (keep) READ (UNIT=fname(6:8), FMT= '(i3)') ver
	   ip = 0
	   DO j = 1, 4
	      IF (qbuf%flip_state(j)) THEN
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
	fname=polcfg(1)%fname


C----------------------------UPDATE HELP SCREEN -----------------------------C

	help_line(1)(1:) = ' Executing:  Q-Buffer '// bufno(1:2) //
	1    '                Datafile: '
	IF (keep) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:67) // 'none        '
	END IF
	help_line(2)(1:) = ' '// qbuf%com(1:50)
	help_line(3)     = ' '
	CALL Put_Help (help_line)
        info(1) = help_line(1)(2:)

C------------------PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	m = 14
	DO j= 1, MAXMOTS
	   IF (motfl(j) .AND. fixed(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:) = response(1:m) //'  A'// out(1:1) //'='
	      m = m + 5
	      RdAm = RdAngl(j)
	      IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
		 zj = fz(j,RdAm)
  	      ELSE 
		 zj = z(j)
	      ENDIF
	      WRITE (UNIT= out(1:8), FMT= '(f8.3)') RdAm - zj
	      response = response(1:m) // out(1:8)
	      m = m + 8
	   END IF
	END DO
	IF (m .GT. 14) CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, ' ')
	

C---------Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	   response(1:) = 'Helium level: '//hstr
	   CALL PUTLINE_NT (contr, response)
	   CALL PUTLINE_NF (contr, new_line)
	ENDIF

C--------Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE !!')
	ENDIF

C---------Print warning if buffer has temp info but not T+
	IF (qbuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF

C---------Print warning if buffer has field info but not H+
	IF ( (qbuf%ihf .NE. 0.0 .or. qbuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	   Call Contr_ring_bell(3)
	   CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF

C---------DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	fenergy=qbuf%ef
  	CALL Check_Channel (2, qbuf%mt, qbuf%mon, qbuf%mpf, qbuf%pts,fname,
	1    fenergy,ip,hstr)
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

C---------------------------Verify something is varying --------------------C
	IF (qbuf%es .NE. 0)THEN
	   IF ( (qbuf%eft .EQ. 'ANA' .AND. fixed(2)) .OR.
	1	(qbuf%eft .EQ. 'MON' .AND. fixed(6)) )THEN
	      response = '**** WARNING ****  Incident & Final energies both fixed!!!!!!'
	      Call Contr_ring_bell(3)
	      CALL PUTCONTR (response)      
	      CALL PUTCONTR_NF (' ')
	   ENDIF
	ENDIF			!qbuf%es ne 0

C----------------------------PRINT DATA HEADER ------------------------------C


	response(1:) = 'Time    Pt#    '
	m = 15
	DO j = 1, 6
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      WRITE (UNIT= response(m+1:m+9), FMT= '(a5,i1,a3)') 
	1	   '    A',j,'   '  
	      m = m + 9
	   END IF
	END DO
	info(4)=' '
	
	IF (tempctr .AND. qbuf%tmp .NE. 0. .AND. qbuf%it .NE. 0.) THEN
	   WRITE (UNIT= response(m+1:m+9), FMT= '(a9)') '   T-set '
	END IF
	CALL Putline_NF (contr, response)
	response = '       H       K       L         E         TIME    INTEN  '
	m = 58
	IF (tempctr) THEN
	   response = response(1:m) //'    TEMP'
	   m = m+8
	ENDIF
	IF (monmag)THEN
	   response = response(1:m) //'   H-FIELD'
	   m = m+10
	ENDIF
	CALL Putline_NF (contr, response)
	IF (prftemp)CALL Lia_Head(keep) !may need to test on temptype later
	CALL Putline_NF (contr, ' ')
	


C--------CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

	CALL Rec_Lattice (qbuf, cr, tmat)
	CALL Mat_Vec (tmat, qbuf%hkl1, v1)
	CALL Mat_Vec (tmat, qbuf%hkl2, v2)
	e0 = qbuf%ec - ((qbuf%pts -1.) / 2.) * qbuf%es
	h0 = qbuf%hc - ((qbuf%pts -1.) / 2.) * qbuf%hs
	k0 = qbuf%kc - ((qbuf%pts -1.) / 2.) * qbuf%ks
	l0 = qbuf%lc - ((qbuf%pts -1.) / 2.) * qbuf%ls
	


C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	loop = 0

	DO WHILE (loop .LE. NINT(qbuf%pts) - 1 .AND. .NOT. finishup)

!       C------------------ CHECKPAUSE FLAG ------------------------------C

	   IF (READEF(pause))THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()	! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event)
	      CALL clref (pause)
              CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
              CALL CURSOR_ABS (contr, 19, 1)
           END IF 


!       C------------ CALCULATE ENERGY-TRANSFER AND ------------------------C
!       C--------- CORRESPONDING MONO AND ANA ANGLES -----------------------C

	   energy = e0 + loop * qbuf%es		
	   IF (qbuf%eft .EQ. 'ANA') THEN
	      posit(1) = Energy_Angle (qbuf%ef+energy, dm)
	      posit(2) = 2. * posit(1)
	      IF (nsta.eq.-5 .and. focus5)THEN
		 CALL focA_energy(qbuf%ef,0.,posit)
	      ELSE
		 posit(5) = Energy_Angle (qbuf%ef, da)
		 posit(6) = 2. * posit(5)
	      ENDIF
	   ELSE
	      posit(1) = Energy_Angle (qbuf%ef, dm)
	      posit(2) = 2. * posit(1)
	      posit(5) = Energy_Angle (qbuf%ef-energy, da)
	      posit(6) = 2. * posit(5)
	   END IF
	   IF (nsta .EQ. 4) THEN ! (* focussing *)
	      posit(7) = foc7(posit(2))
	      posit(8) = foc8(posit(2))
	   END IF
	   IF (nsta .EQ. -5) THEN ! (* focussing *)
	      posit(7) = foc7(posit(2))
	   END IF


!       C------- CALCULATE THE MOMENTUM-TRANSFER AND THE -------------------C
!       C---------- CORRESPONDING PHI AND PSI ANGLES -----------------------C

	   q0(1) = h0 + loop * qbuf%hs
	   q0(2) = k0 + loop * qbuf%ks
	   q0(3) = l0 + loop * qbuf%ls
	   CALL Mat_Vec (tmat, q0, q)
	   IF (nsta.eq.-5 .and. focus5)THEN
	      posit(3) = Psi_Angle (q, posit(1), posit(6)/2.,  
	1	   v1, qbuf%a1, v2)
	   ELSE
	      posit(3) = Psi_Angle (q, posit(1), posit(5),  
	1	   v1, qbuf%a1, v2)
	   ENDIF
 	   IF (q(1) .EQ. 0. .AND. q(2) .EQ. 0. .AND. q(3) .EQ. 0.) THEN
	      IF (nsta .eq. -5 .and. negqpos)THEN
		 posit (4) = min(0.,ul(4))
	      ELSE
		 posit (4) = max(0.,ll(4))
	      ENDIF
	   ELSE
	      IF (nsta.eq.-5 .and. focus5)THEN
		 posit(4) = Phi_Angle (q, posit(1), dm, posit(6)/2., da)
	      ELSE
		 posit(4) = Phi_Angle (q, posit(1), dm, posit(5), da)
	      ENDIF
	   END IF
	   IF (nsta .eq. -5 .and. negqpos)THEN
	      posit(3) = -posit(3)
	      posit(4) = -posit(4)
	   ENDIF


!       C-------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
!       C---------------- IF NOT GIVE WARNING -----------------------------C
	   
	   limit = .FALSE.
	   IF (nsta .eq. -5 .AND. focus5)THEN
	      ilast = MAXMOTS
	   ELSE
	      ilast = 6
	   ENDIF
	   DO j = 1, ilast
	      IF (j .LE. 6 .OR. j .GE. first_focus)THEN !include focus motors
		 IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
		    zj = fz(j,posit(j))
		 ELSE 
		    zj = z(j)
		 ENDIF
		 t = posit(j) + zj
		 IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		    IF (.NOT. inlimits(j,t,contr))limit = .TRUE.
		 END IF
	      ENDIF
	   END DO
	   IF (limit) THEN
	      WRITE (UNIT= error, FMT= '(6f8.3)') (posit(j), j= 1, 6)
	      CALL PUTLINE (contr, ' Configuration outside limits: '//
	1	   error)	
	      IF (nsta .eq. -5 .AND. focus5)THEN
		 WRITE (UNIT= error, FMT= '(6f8.3)') (posit(j), 
	1	      j= first_focus,first_focus+5)
		 CALL PUTLINE (contr, '    '//
	1	      error)	
		 WRITE (UNIT= error, FMT= '(6f8.3)') (posit(j), 
	1	      j= first_focus,first_focus+num_focus-1)
		 CALL PUTLINE (contr, '    '//
	1	      error)
	      ENDIF	
	      GOTO 999
	   END IF

	   
!       C------- IF NECESSARY SWITCH POLARIZING FLIPPER --------------------C

	   polindex = 1
	   IF (polcfg(polindex)%csec .NE. 0) 
	1	CALL Set_Flippers (polcfg(polindex)%csec)


!       C------------ SET NEW TEMPERATURE AND WAIT ------------------------C
	   IF (prftemp) prfpoint = 0
	   IF (tempctr .OR. magnctr) THEN
	      CALL New_Temp_Field (temp,qbuf%tmp,qbuf%it,qbuf%tw,qbuf%ter,
	1	   qbuf%th0,qbuf%th,qbuf%hf, loop, qbuf%ihf,qbuf%hfh,qbuf%hfw)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
		 CALL PUTLINE (contr, ' **** ABORT during field change ****')
	         RETURN
	      END IF
	   END IF

!       C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) loop+1, '/', NINT(qbuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7) // ' '
	   info(3)(1:) = response(1:14)
	   m = 14
	   DO j = 1, 6		! (* print new position first *)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)') posit(j)
		 m = m + 9
	      END IF
  	   END DO	   
	   IF (tempctr .AND. qbuf%tmp .NE. 0. .AND. qbuf%it .NE. 0.)
	1	Call Putmax(temp,response(m+1:m+9),9,5)
	   CALL Putline_NF (contr, response)
	   DO j = 6, 1, -1	! (* drive motors in reverse order *)
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
	   IF (nsta .EQ. 4.and..not.fixed(2)) THEN ! (* focussing *)
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
	   IF (nsta .EQ. -5.and..not.fixed(2)) THEN ! (* focussing *)
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
	   IF (nsta.eq.-5 .and. focus5) THEN
	      DO j = first_focus, first_focus+num_focus-1
		 IF (READEF(contr_A))GOTO 50
	         IF (loop .EQ. 0) THEN
  	            IF (motfl(j) .AND. .NOT. fixed(j)) 
	1		 CALL Drive (j, posit(j) + z(j))
	         ELSE
	            IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1		 .AND. .NOT. fixed(j) ) 
	2		 CALL Drive (j, posit(j) + z(j))
	         END IF
	         oldpos(j) = posit(j)
	      ENDDO
	   ENDIF		!using focus
	   WRITE (UNIT= response, FMT= '(f11.4,2f8.4,f9.3,a4)')
	1	q0(1), q0(2), q0(3), energy, '   *'
	   m = 40
 	   CALL Putline_NCR (contr, response(1:m))
	   CALL CURSOR_ABS (contr, 19, m+1)
	   iwhere = m+1
	   info(3)(15:) = response
	   IF (polarized .AND. polcfg(polindex)%csec .NE. 0) THEN
	      i = polcfg(polindex)%csec
	      CALL DATE_TIME (sys_time)	! say which cross section
	      response(1:) = sys_time(13:17) //'   #####  F1: '// flp(i,1) // 
	1	   '   F2: '// flp(i,2) // '       *'
	      CALL SCROLL_DISPLAY (contr)
	      CALL PUTCHARS (contr, response(1:40), 19, 1)
	      iwhere = 41
           END IF
 50 	   CALL Yield               
	   IF (READEF(contr_A))THEN
	      IF (keep) CALL Save_File (fname, loop)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
	   END IF


!       C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.


!       C---------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL Reset_Display	!Reset NIM count display
	   IF (nsta .EQ. -5 .AND. use_asd) CALL Zero_ASD !zero data array
 200       DO prf = 1, NINT(qbuf%mpf)
 201          continue
 	      CALL RlyOff (4)	! Start display counter
	      IF (qbuf%mt .EQ. 'NEUT') THEN
c       IF (nsta .EQ. -5 .AND. asd) CALL asd_on(prf)
		 IF (nsta .EQ. -5 .AND. use_asd) CALL asd_on(1)
  	         CALL Count (qbuf%mon)
	      ELSE	
c       IF (nsta .EQ. -5 .AND. asd) CALL asd_timed(prf,qbuf%mon)
		 IF (nsta .EQ. -5 .AND. use_asd) CALL asd_timed(1,qbuf%mon)
		 CALL CnTime (qbuf%mon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)	! Stop display counter
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
		 CALL PUTLINE (contr, ' **** ABORT during count ****')
		 RETURN
	      END IF

	      IF (nsta .eq. -5 .and. use_asd)counts(2) = fpwindow
	      IF (nsta .eq. -11 .and. use_asd) counts(scl) = fpwindow

!       C------------------ CHECKPAUSE FLAG ------------------------------C

	      IF (READEF(pause))THEN
		 CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
		 CALL CURSOR_OFF (paste)
		 CALL HIBER()	! Hibernate 
		 CALL clref (contr_a)
		 CALL clref (hdw_event)
		 CALL clref (pause)
		 CALL PUTCHARS_REV (stat, '             ', 1, 15)
		 CALL CURSOR_ON (paste)
		 CALL CURSOR_ABS (contr, 19, iwhere)
              END IF 


!       C--------- CHECK STATISTICS IF ENABLED -------------------------C

	      IF (statistic) THEN              
		 CALL Test_Ststcs (qbuf%mon, prf, counts, *100, *201)
	      END IF
	      

!       C--------------- ACCUMULATE COUNTS ------------------------------C

	      DO i = 2, scl + 1
		 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (qbuf%mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
		 elap_time = elap_time + dsec
	      ELSE
		 accucnts(1) = accucnts(1) + counts(1)
!elap_time = elap_time + NINT(qbuf%mon)
		 dsec = SecDiff() !function to get stop_time - start_time
		 elap_time = elap_time + dsec
	      END IF
	      imin = INT (elap_time / 60.)
	      sec = NINT( MOD (elap_time, 60.))
	      IF (prftemp) CALL RdTemp (temp, dummy, dummy)
	      IF (nsta .eq. -5 .and. use_asd .and. .not. keep_rawasd)call asd_image
	   END DO		! Prefactor loop

	   IF (tempctr) then
	      IF (.not. prftemp) THEN
		 IF (which_temp.eq.2)THEN !control
		    CALL RdTemp (dummy, temp, dummy)
		 ELSEIF (which_temp.eq.3) THEN !setpoint
		    CALL RdTemp (dummy, dummy, temp)
		 ELSE		!sample
		    CALL RdTemp (temp, dummy, dummy)
		 ENDIF
	      ENDIF		!not prftemp
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
C       field = fieldlast * hfconv
		 CALL Rdfield(field)
	      ELSEIF (magtype.eq.2)THEN !read back voltage for HP
		 CALL Rdfield(field)
	      ELSE
		 field = qbuf%hf + loop*qbuf%ihf
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
	      str9(1:)=' '
	      IF (temptype .EQ. tdev%cfg) THEN !using degrees
		 CALL putmax(temp,str9(2:9),8,5)
	      ELSE		!using resistance
		 CALL putmax(temp,str9(2:9),8,7)
	      ENDIF
	      IF (monmag)THEN
		 CALL putmax(field,str12(1),12,4)
		 WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9,a12)') 
	1	      imin, ':', sec, accucnts(2), str9,str12(1)
	      ELSE
		 WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9)') 
	1	      imin, ':', sec, accucnts(2), str9
	      ENDIF
	   ELSE
	      IF (monmag)THEN
		 CALL putmax(field,str12(1),12,4)
		 WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a12)') 
	1	      imin, ':', sec, accucnts(2),str12(1)
	      ELSE
		 WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9)') 
	1	      imin, ':', sec, accucnts(2)
	      ENDIF
	   END IF
	   info(4) = response	!since never wrap with q-buffer, can do this
	   CALL Putline_NF (contr, response)
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
		 IF (m .GE. 80) THEN ! write string to screen
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
c       IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	      IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	   ENDIF
	   IF (keep) THEN
	      CALL Dmp_Data (fname, loop, accucnts, elap_time, typ,
	1	   idum,qbuf,idum,idum, temp, polcfg(polindex)%csec,field,monmag)
	      IF (nsta .EQ. -5 .AND. USE_ASD .AND. Keep_RawASD)CALL Dmp_ASD(fname,loop)
	      IF (savepos) CALL Dmp_Qpos(fname,loop,accucnts,qbuf,posit,
	1	   polcfg(polindex)%csec)
	   ENDIF
	   polindex = polindex + 1
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      IF (tempctr .and. (qbuf%tmp .ne. 0.) .and. (qbuf%tw .ne. 0.))
	1	   Call Check_Heater
	      CALL Run_Polar (typ,idum, qbuf,idum, polcfg(polindex),
	1	   loop, keep)
	      IF (READEF(contr_A))THEN
	         IF (keep) CALL Save_File (fname, loop)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF
	      polindex = polindex + 1
	   END DO
           GOTO 300

C---------OUTSIDE LIMITS: SET COUNTS TO ZERO THEN GO TO NEXT POINT ----------C

 999  	   accucnts(2) = 0
	   temp = 0.
	   elap_time = 0
	   IF (polindex .eq. 0) polindex = 1

	   IF (keep) THEN
	      CALL Dmp_Data (fname, loop, accucnts, elap_time, typ,

	1	   idum,qbuf,idum,idum, temp, polcfg(polindex)%csec,field,monmag)
	      ! ^^^^ polcfg(polindex) needs to be a scalar, not sure if I have it right
	      IF (nsta .EQ. -5 .AND. USE_ASD .AND. Keep_RawASD)CALL Dmp_ASD(fname,loop)
	      IF (savepos) CALL Dmp_Qpos(fname,loop,accucnts,qbuf,posit,
	1	   polcfg(polindex)%csec)
	   ENDIF

!       C--------------------- UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5))info(4)(60:)=
     1		'Helium Lvl:'//hstr

 300	   CALL Status_File (info)
	   
	   loop = loop + 1

	END DO			! (* Number of points loop *)

	IF (loop .LE. NINT(qbuf%pts) - 1 .AND. finishup)THEN
	   finishup = .FALSE.
	   IF (keep .AND. loop .GT. 0) CALL Save_File (fname, loop) !adjust #pts & final positions
	   CALL Putcontr(' Ctrl F detected')
	   CALL Putcontr_NF(' ')
	   CALL Status_Line	!erase CtrlF message
	ENDIF

        IF (keep) THEN
	   CALL Make_Backup (polcfg(1)%fname)
	   polindex = 2
	   DO WHILE (polcfg(polindex)%csec .NE. 0)
	      CALL Make_Backup (polcfg(polindex)%fname)
	      polindex = polindex + 1
	   END DO
	END IF
	END
