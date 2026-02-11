#include "icp.h"
	SUBROUTINE Run_T_Buf (buf_no, keep)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes Trash Buffer No. buf_no, i.e. only motor 1 & 2 are sigificant  C
C     and move; Intensities are taken from detector no 2!	              C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, January 1990                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use iso_c_binding
	IMPLICIT      none
	interface
	   subroutine FANSVEC(energy0,npts,defrac,vector)
	1	bind(C,name="FANSVEC")
	   use iso_c_binding
	   real(c_float),  intent(in)::energy0
	   integer(c_int), intent(in)::npts
	   real(c_float),  intent(in)::defrac
	   real(c_float),  intent(out)::vector(*)
	   end subroutine
#include "c_libwait.h"
#include "c_vme.h"
	end interface
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, j, m, status, st, typ, ip, ios, idum
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37)
	INTEGER*4     min, sec, right, iwhere
	REAL*4        posit(10), oldpos(10), RdAngl,RdAm,zj,Fz
	REAL*4	      Energy_Angle, e0, energy, t, temp /0./, dummy,secdiff
	REAL*4        dsec, elap_time, foc7, foc8
	REAL*4        ATAND,SIND
	REAL*4        ef
	REAL*4	      energyvec(1024)
	LOGICAL       limit, old_3, wrap, monmag, comerr, shutdone, alarm,
     1			inlimits, Readef
	logical      stopped, plus, minus, home
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(3), PutStr, hstr
	CHARACTER*8   out, new_line/'        '/
	CHARACTER*9   str9
	CHARACTER*10  str10
	CHARACTER*12  fname, File_Name,str12(4)
	CHARACTER*28  sys_time
	CHARACTER*80  response, info(4),error, help_line(3), subdir
	CHARACTER*240 longl
	REAL*4	      fenergy, field, hlevel

	TYPE(t_buf) tbuf

	finishup = .FALSE.
	trashmode = .true.
		
	OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED',RECL=TBUF_RECL)
	READ (1,REC=buf_no) tbuf
	CLOSE (1)

	CALL DATE_TIME (sys_time)
	info(2)(1:)=tbuf%com

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr)THEN
	  IF (tbuf%ihf .NE. 0 .or. magtype .ge. 2)monmag=.TRUE.
	  call putmax(tbuf%hf,str10,10,4)
	  info(2)(51:)=' H-field: '//str10
	ENDIF


C--------------------------- DETERMINE DATA FILE -----------------------------C

	IF (keep) THEN                                     ! Data storage is on
	   fname = File_Name (tbuf%com(1:5))
	END IF
      

C---------------------------- WHAT ABOUT ANGLE 3 -----------------------------C

	old_3 = fixed(3)                                      ! save old status
C	IF (tbuf%m3) THEN
C	   fixed(3) = .FALSE.
C	ELSE 
	   fixed(3) = .TRUE.
C	END IF
	CALL Status_Line

C------------------------------- PRINT HEADER --------------------------------C

	IF (keep) THEN                                     ! Data storage is on
	   CALL Getdir(subdir,right)
	   CALL PUTLINE_NF (contr, ' ')
	   CALL PUTLINE (contr, subdir(1:right)//fname//'  '//sys_time(1:17))
	END IF
	CALL PUTLINE_NF (contr, ' ')
	CALL PUTLINE_NF (contr, sys_time(1:17))
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'TBuffer '// bufno // ':  '// tbuf%com
	CALL PUTLINE_NT (contr, response)
	response = ' '
	CALL PUTLINE_NF (contr, response)
	st7(1) = PutStr (tbuf%ec)
	st7(2) = PutStr (tbuf%es)
	st7(3) = PutStr (tbuf%ef)
	response = 'E-center = '// st7(1) //'  E-step = '// st7(2) //
	1          '  Fix-E = '// st7(3)
	typ = 4                                                   ! Filetype
	response = response(1:53) //'     Scan in Trash Mode'
	CALL PUTLINE_NT (contr, response)	


	CALL Putmax(tbuf%tmp,str6(1),6,5)
	CALL Putmax(tbuf%it,str6(2),6,5)
	CALL putmax(tbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(tbuf%tw)
	call putmax(tbuf%hf,str10,10,4)
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
     	1          '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2        //' min  HField= '// str10
	CALL PUTLINE_NT (contr, response)
	IF (magnctr) THEN !magnetic field 
	  call putmax(tbuf%hf,str12(1),12,4)
	  call putmax(tbuf%ihf,str12(2),12,4)
	  write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(tbuf%hfw)
	  call putmax(hfconv,str12(4),12,4)
	  response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
     1	    ' Hwait= '//
     1	     str12(3)//' Hconv= '//str12(4)
	  CALL Putline_NT (contr, response)
	ENDIF !magnetic field

	WRITE (UNIT= str6(1), FMT= '(i6)') NINT(tbuf%mon)
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)') NINT(tbuf%mpf)
	WRITE (UNIT= str6(3)(1:3), FMT= '(i3)',iostat=ios) NINT(tbuf%pts)
	response = 'Monitor= '// str6(1) //'  Prefac= '// str6(2)(1:3)
     1         //'  Type= '// tbuf%mt //'  #pts = '// str6(3)(1:3)
	IF (keep) response = response(1:52) //'     Datafile: '// fname
	CALL PUTLINE_NT (contr, response)


C---------------------------- UPDATE HELP SCREEN -----------------------------C

	help_line(1)(1:) = ' Executing:  T-Buffer '// bufno(1:2) //
	1              '                Datafile: '
	IF (keep) THEN 
	   help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	ELSE
	   help_line(1) = help_line(1)(1:67) // 'none        '
	END IF
	help_line(2) = ' '// tbuf%com(1:50)
	help_line(3) = ' '
	CALL Put_Help (help_line)
	info(1) = help_line(1)(2:)

C------------------ PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	m = 14
	i = 4
	IF (fixed(3)) i = 3
	DO j = i, 6
	   IF (motfl(j)) THEN
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
	CALL PUTLINE_NT (contr, response)
	CALL PUTLINE_NF (contr, ' ')
	

	IF (FIXED(1))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 1 is fixed  ****')
	IF (FIXED(2))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 2 is fixed  ****')


C--------- Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	  response(1:) = 'Helium level: '//hstr
	  CALL PUTLINE_NT (contr, response)
	  CALL PUTLINE_NF (contr, new_line)
	ENDIF

C-------- Print warning if no data file to be kept
	IF (.NOT. keep)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: NO DATA FILE IS BEING MADE!!')
	ENDIF

C---------Print warning if buffer has temp info but not T+
	IF (tbuf%tmp .NE. 0.0 .and. .not. tempctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has temp info but T flag NOT set')
	ENDIF

C---------Print warning if buffer has field info but not H+
	IF ( (tbuf%ihf .NE. 0.0 .or. tbuf%hf .NE. 0.0) .and. .not. magnctr)THEN
	  Call Contr_ring_bell(3)
	  CALL Putcontr_NT(' WARNING: Buffer has field info but H flag NOT set')
	ENDIF

C--------- DETERMINE MONITOR RATE AND ESTIMATE LENGTH OF MEASUREMENT ---------C
	     fenergy=ef
	     ip = 1
  	CALL Check_Channel (2, tbuf%mt, tbuf%mon, tbuf%mpf, tbuf%pts,fname,
     1	    fenergy,ip,hstr)
	IF (READEF(contr_A)) THEN
	   CALL PUTLINE (contr, ' **** ABORT during monitor rating ****')
	   RETURN
	END IF


C--------- Add helium level to screen header if it can be determined ---------C
	IF (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)) THEN !SC magnet
	  response(1:) = 'Helium level: '//hstr
	  CALL PUTLINE_NT (contr, response)
	  CALL PUTLINE_NF (contr, new_line)
	ENDIF

C---------------------------- PRINT DATA HEADER ------------------------------C

	response = 'Time    Pt#    '
	m = 15
	DO j = 1, 3
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      WRITE (UNIT= response(m+1:m+9), FMT= '(a5,i1,a3)') 
     1        '    A',j,'   '  
	      m = m + 9
	   END IF
	END DO
	response(1:) = response(1:m) //'     E         TIME    INTEN  '
	m = m + 30
	IF (tempctr) THEN
	  response = response(1:m) //'    TEMP'
	  m = m+8
	ENDIF
	IF (monmag)THEN
	  IF (m .lt. 72)THEN
	    wrap = .FALSE.
	    response(1:) = response(1:m)//'    HF'
	    info(4)(1:)=' ' !this line will not be needed
	  ELSE
	    CALL Putline_NF (contr, response)
	    wrap = .TRUE.
	    response(1:)='    HF'
	  ENDIF
	ENDIF
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	


C-------- CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

	ef = 1.2
	i = NINT(tbuf%pts)
	call FANSVEC(tbuf%ec,i,tbuf%es,energyvec(1))
	tbuf%pts = i


C-----------------------------------------------------------------------------C
C--------------------- COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	loop = 0

	DO WHILE (loop .LE. NINT(tbuf%pts) - 1 .AND. .NOT. finishup)

!          C------------------ CHECK PAUSE FLAG ------------------------------C

	   IF (READEF(pause)) THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL clref (31)
              CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
              CALL CURSOR_ABS (contr, 19, 1)
           END IF 


!          C------------ CALCULATE ENERGY-TRANSFER AND ------------------------C
!          C--------- CORRESPONDING MONO AND ANA ANGLES -----------------------C
!
	   energy = energyvec(loop + 1)
	   posit(1) = Energy_Angle (ef+energy, dm)
	   posit(2) = 2. * posit(1)
	   posit(5) = Energy_Angle (ef, da)
	   posit(3) = ATAND ( dm * SIND(posit(1)) / (da * SIND(posit(5))) )  
	   posit(7) = foc7(posit(2))
	   posit(8) = foc8(posit(2))

!          C-------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
!          C---------------- IF NOT GIVE WARNING -----------------------------C
                                     
	   limit = .FALSE.
	   DO j = 1, 3
	      IF (nsta.eq.4 .and. z(j).eq.-99.) THEN !special flag
		t = posit(j) + fz(j,posit(j))
	      ELSE
	        t = posit(j) + z(j)
	      ENDIF
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	      END IF
	   END DO
	   IF (limit) THEN
	      WRITE (UNIT= error, FMT= '(6f8.3)') (posit(j), j= 1, 6)
	      CALL PUTLINE (contr, ' Configuration outside limits: '//
     1		                          error)	
	      GOTO 999
	   END IF

          
!          C-- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	   CALL DATE_TIME (sys_time)
	   WRITE (UNIT= out(1:7), FMT= '(i3,a1,i3)',IOSTAT=ios) loop+1, '/', NINT(tbuf%pts)
	   response(1:) = sys_time(13:17) //' '// out(1:7) // ' '
	   m = 14
	   DO j = 1, 3                          ! (* print new position first *)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)') posit(j)
		 m = m + 9
	      END IF
  	   END DO	   
	   WRITE (UNIT= response(m+1:), FMT= '(f9.3,a4)') energy, '   *'
	   m = m + 13
 	   CALL Putline_NCR (contr, response(1:m))
	   IF (.not.wrap) THEN
	     CALL CURSOR_ABS (contr, 19, m+1)
	     iwhere = m+1
	   ELSE
	     iwhere = 1
	   ENDIF
	   info(3)(1:) = response(1:m)
	   DO j = 3, 1, -1                 ! (* drive motors in reverse order *)
	      IF (READEF(contr_A)) GOTO 50
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	           IF (nsta.eq.4 .and. z(j).eq.-99.)THEN
                     IF (j .eq. 2) THEN
			CALL TrashDrive (posit(j) + fz(j,posit(j)))
		     ELSE
			CALL Drive (j, posit(j) + fz(j,posit(j)))
		     ENDIF
	           ELSE
                     IF (j .eq. 2) THEN
			CALL TrashDrive (posit(j) + z(j))
		     ELSE
			CALL Drive (j, posit(j) + z(j))
		     ENDIF
		   ENDIF
	         ENDIF
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1            .AND. .NOT. fixed(j) ) THEN
		   IF (nsta.eq.4 .and. z(j).eq.-99.)THEN
		      IF (j .eq. 2 .and. trashmode) THEN
			 CALL TrashDrive (posit(j) + fz(j,posit(j)))
		      ELSE
			 CALL Drive (j, posit(j) + fz(j,posit(j)))
		      ENDIF
		   ELSE
		      IF (j .eq. 2 .and. trashmode) THEN
			 CALL TrashDrive (posit(j) + z(j))
		      ELSE
			 CALL Drive (j, posit(j) + z(j))
		      ENDIF
		   ENDIF
	         ENDIF
	      END IF
	      oldpos(j) = posit(j)
	      IF (j .EQ. 2) CALL YIELD
	   END DO
	   DO j = 8, 7, -1                 ! (* drive motors in reverse order *)
	      IF (READEF(contr_A)) GOTO 50
	      IF (loop .EQ. 0) THEN
  	         IF (motfl(j) .AND. .NOT. fixed(j) .and. .not. fixed(2)) 
     1	          CALL Drive (j, posit(j) + z(j))
	      ELSE
	         IF ( posit(j) .NE. oldpos(j) .AND. motfl(j)
	1            .AND. .NOT. fixed(j).and. .not. fixed(2)) 
     1		  CALL Drive (j, posit(j) + z(j))
	      END IF
	      oldpos(j) = posit(j)
	   END DO
 50  	   CALL Yield
C          C-----------Check to make sure we're docked -----------------------C
	   j = TRASHANAL
	   call check_limits(j, stopped, plus, minus, home, comerr)
	   if (.not. minus) then
	      call putline(contr,'*** Redocking analyzer ***')
	      call go_limit(j, .true., comerr)
	      call yield
	   endif


	   IF (READEF(contr_A)) THEN
	      IF (keep) CALL Save_File (fname, loop)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
	   END IF


!          C------------ SET NEW TEMPERATURE AND WAIT ------------------------C

	   IF (tempctr .OR. magnctr) THEN
	       CALL New_Temp_Field (temp,tbuf%tmp,tbuf%it,tbuf%tw,tbuf%ter,
     1	             tbuf%th0,tbuf%th,tbuf%hf, loop,tbuf%ihf,tbuf%hfh,tbuf%hfw)
	   END IF


!          C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.


!          C---------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL Reset_Display       !Reset NIM count display
 200       DO prf = 1, NINT(tbuf%mpf)
 201          continue
 	      CALL RlyOff (4)                           ! Start display counter
	      IF (tbuf%mt .EQ. 'NEUT') THEN
  	         CALL Count (tbuf%mon)
	      ELSE	
  		 CALL CnTime (tbuf%mon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
	      IF (READEF(contr_A)) THEN
	         IF (keep) CALL Save_File (fname, loop)
		 CALL PUTLINE (contr, ' **** ABORT during count ****')
		 RETURN
	      END IF

!          C------------------ CHECK PAUSE FLAG ------------------------------C

	   IF (READEF(pause)) THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL clref (31)
              CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
              CALL CURSOR_ABS (contr, 19, iwhere)
           END IF 

!             C--------- CHECK STATISTICS IF ENABLED -------------------------C

	      IF (statistic) THEN              
		 CALL Test_Ststcs_T (tbuf%mon, prf, counts, *100, *201)
	      END IF
	

!             C--------------- ACCUMULATE COUNTS ------------------------------C

	      accucnts(2) = accucnts(2) + counts(3)
	      if (vmedata) then
		 call VSCALERADD
	      endif
	      IF (tbuf%mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
		 elap_time = elap_time + dsec
	      ELSE
		 accucnts(1) = accucnts(1) + counts(1)
		 elap_time = elap_time + NINT(tbuf%mon)
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT( MOD (elap_time, 60.))
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
		field = fieldlast * hfconv
	     ELSE
     		field = tbuf%hf + loop*tbuf%ihf
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
!          C--------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

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
	   CALL Putline_NF (contr, response)
	   IF (wrap)THEN
	     info(4) = response
	   ELSE
	     info(3)(m+1:) = response
	   ENDIF
	   IF (keep) CALL Dmp_Data (fname, loop, accucnts, elap_time, typ,
     1                idum ,idum,tbuf,idum, temp, 0,field,monmag)
 999  	   CONTINUE  ! (* after error message *)

!       C--------------------- UPDATE STATUS FILE -----------------------------C
	   IF (magnctr .AND. (magtype .EQ. 3 .or. magtype .EQ. 4 .or. magtype .EQ. 5))info(4)(60:)=
     1		'Helium Lvl:'//hstr

	   CALL Status_File (info)

	  loop = loop + 1

	END DO  ! (* Number of points loop *)

	IF (loop .LE. NINT(tbuf%pts) - 1 .AND. finishup)THEN
	  finishup = .FALSE.
 	  IF (keep .AND. loop .GT. 0) CALL Save_File (fname, loop) !adjust #pts & final positions
	  CALL Putcontr(' Ctrl F detected')
	  CALL Putcontr_NF(' ')
	  CALL Status_Line !erase CtrlF message
	ENDIF

	CALL SCROLL_DISPLAY (contr)
	CALL CURSOR_ABS (contr, 19, 1)
	fixed(3) = old_3
	CALL Status_Line

	IF (keep) CALL Make_Backup (fname)
	END
 
