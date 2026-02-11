#include "icp.h"
	SUBROUTINE Dmp_dbuf (fname,pts,counts,tim,typ,dbuf,
     1		temp,which,field,monmag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to dump dbuf Data. The Intensities Are Written To A            C
C     Character String. As Soon As The Length Of That String Exceeds 80       C
C     Characters, The Whole String Is Dumped In To The Data File.             C
C     Called from: Run_D_Buf                                                  C
C                                                                             C
C	Gayle Greene, Mar, 1996                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use histo
	use dartz
	use tc
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface
	INTEGER*4    pts, typ, ee, raw /1/, dummy(3) /0,0,0/,right,ios, jr,
     1			status,notries/50/,openfai/30/ !from $fordef
	INTEGER*4    i, j, m, maxtry, counts(37), which, ik, kpsd, lstr90
	REAL*4       npoints, e, qh, qk, ql, temp, min, tim,
     1	             zeropos/0./,field,posnow, Rdangl
	CHARACTER*5  ch_raw /'''RAW'''/
	CHARACTER*6  m_typ
	CHARACTER*10 str10(4)
	CHARACTER*12 fname,outbuf,str12
	CHARACTER*14 f_nam
	CHARACTER*28 date
	CHARACTER*3  fcap
	CHARACTER*20 datum
	CHARACTER*80 line, outpsd
	CHARACTER*90 str90,str90b
	CHARACTER*1  tchar
	CHARACTER*6  tstring
	CHARACTER*8  str8
	CHARACTER*9  str9
	CHARACTER*7  str7(2)
	CHARACTER*13 str13
	CHARACTER*40 vstr !set up description of what is varying
	LOGICAL      written, monmag, errflag
	CHARACTER*2800  bstr !more than 256*10 + 80
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)
	CHARACTER*2 str2
	INTEGER*4	lb, lv
	LOGICAL		posout(MAXMOTS)
	CHARACTER*1     coord(6)
	character*80 gethome
c	REAL*4 		xyzpos(numdmots)
c	INTEGER*4	ig, xyzmots(3)
	TYPE(d_buf) dbuf     
C--------------------------- INITIALIZE DATA FILE ----------------------------C
	IF (pts .EQ. 0) THEN	! first call
	   CALL DATE_TIME (date)
c       add filename to monitor.rec
	   Call Getdir(str90,right)
           maxtry = 0
	   OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	ACCESS= 'APPEND', STATUS='UNKNOWN',
	1	IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	      CALL libwait(.2,status)
	      OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	   ACCESS= 'APPEND', STATUS='UNKNOWN',
	1	   IOSTAT=ios)
	      maxtry = maxtry+1
	   ENDDO
	   IF (ios .EQ. 0)THEN
	      write(unit=2,fmt='(1x,a,1x,a,1x,a)',err=2209)
	1	   date(1:17),' Created: ',str90(1:right)//fname
 2209	      close(unit=2,err=2210)
	   ENDIF
c       write header
 2210	   OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED',
	1	ACCESS= 'SEQUENTIAL',STATUS= 'NEW')
	   f_nam = CHAR(39) // fname // CHAR(39) 
	   datum = CHAR(39) // date(1:17) // CHAR(39) 
	   tchar='D'
	   tstring = CHAR(39) // tchar // CHAR(39)//'   '
	   m_typ = CHAR(39) // dbuf%mt // CHAR(39)
	   WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	f_nam, datum,tstring, 
	2	dbuf%mon, NINT(dbuf%mpf), m_typ, NINT(dbuf%pts), ch_raw
	   WRITE (2, FMT='(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)')
	1	'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	'#pts','Type'
	   IF (polarized .AND. which .NE. 0) THEN
	      WRITE (2, '(a50,a18)') dbuf%com(1:50), '  F1: '//flp(which,1)//
	1	   '  F2: '//flp(which,2)
	   ELSE
	      WRITE (2, '(a50)') dbuf%com(1:50)
	   END IF
	   vstr(1:)='VARY='
	   lv = 5
	   DO j = 1,6
	      jr = dbuf%mn(j)	!get real motor number
	      IF (jr .NE. 0) THEN !number was specified
		 IF (motfl(jr) .and. dbuf%i(j).ne.0. .and. .not. fixed(jr)) THEN !non-zero increment
		    write(unit=str2,fmt='(i2.2)',iostat=ios)jr !write motno
		    vstr(1:)=vstr(1:lv)//'A'//str2//' '
		    lv = lv + 4
		 ENDIF		!motfl
	      ENDIF		!jr ne 0
	   ENDDO		!j = 1,6
	   IF (tempctr .AND. dbuf%it .NE. 0 .AND. dbuf%tmp .NE. 0)THEN
	      vstr(1:)=vstr(1:lv)//'T '
	      lv = lv + 2
	   ENDIF
	   IF (magnctr .AND. dbuf%ihf .NE. 0) THEN
	      vstr(1:)=vstr(1:lv)//'HF '
	      lv = lv + 3
	   ENDIF
	   write(unit=str9,fmt='(I9)',iostat=ios)NINT(dbuf%pts)
	   call upcase(fcap,fname(10:12))
	   bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//tab//
	1	'FILE='//str90(1:right)//fname//tab//vstr(1:lv)//
	1	tab//dbuf%com(1:50)
	   lb = 78+right+lv	!length of filled part of bstr
	   CALL Gobroad(lb,bstr)
	   call putmax(dbuf%hf,str13(1:9),9,4)
	   call putmax(dbuf%tmp,str8,8,3)
	   call putmax(dbuf%it,str7(1),7,3)
	   WRITE (unit=str90(1:71),FMT=
	1	'(4(i3,1x),3i3,4x,f7.4,4x,a8,1x,a7,1x,a9,1x,i4)',IOSTAT=ios)
	1	(collim(i), i= 1,4), (mosaic(i), i=1,3), wavel,
	2	str8, str7(1), str13(1:9), scl
	   lstr90 = 71
	   WRITE (unit=str90b(1:71), FMT=
	1	'(1x,a11,6x,a6,4x,a10,3x,a7,3x,a5,3x,a7,1x,a4)') 
	1	'Collimation', 'Mosaic', 'Wavelength',
	2	'T-Start', 'Incr.', 'H-field','#Det'
	   IF (magnctr)THEN
	      call putmax(hfconv,str7(1)(1:7),7,4)
	      call putmax(dbuf%ihf,str7(2)(1:7),7,4)
	      WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
	      str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
	      lstr90=lstr90+16
	   ENDIF
	   WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	   WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)
	   DO j = 1, MAXMOTS	!flag to remember which positions written
	      posout(j) = .FALSE.
	   ENDDO
	   DO j= 1, 6		!treat each motor line in buffer
	      IF (dbuf%xyz(j) .eq. xcoord) THEN
		 coord(j)='X'
	      ELSEIF (dbuf%xyz(j) .eq. ycoord) THEN
		 coord(j)='Y'
	      ELSEIF (dbuf%xyz(j) .eq. zcoord) THEN
		 coord(j)='Z'
	      ELSE
		 coord(j)=' '
	      ENDIF
	      jr = dbuf%mn(j)	!get motor number specified in line
	      IF (jr .NE. 0)THEN !has a motor been declared in line?
		 IF (motfl(jr).AND. .NOT. fixed(jr))THEN !will it be moving?
		    posout(jr) = .TRUE. !have written this one to file
		    if (coord(j) .eq. ' ') THEN
		       WRITE (2, '(i3,3x,3f11.5,2x,I1)',IOSTAT=ios)  jr, 
	1		    dbuf%a(j), dbuf%i(j), 
	2		    dbuf%a(j)+ dbuf%i(j)*(dbuf%lpts(j)-1),
	3		    dbuf%lpn(j)
		    else
		       WRITE (2, '(i3,3x,3f11.5,2x,I1,1x,a1,3f11.5)',
	1		    IOSTAT=ios)jr,dbuf%a(j), dbuf%i(j), 
	2		    dbuf%a(j)+ dbuf%i(j)*(dbuf%lpts(j)-1),
	3		    dbuf%lpn(j),coord(j),dbuf%a_xyz(j),dbuf%i_xyz(j),
	4		    dbuf%a_xyz(j)+dbuf%i_xyz(j)*(dbuf%lpts(j)-1)
		    endif
		 ELSE		!motor isn't in Use or isn't moving
		    IF (coord(j) .ne. ' ') THEN
		       WRITE (2, '(i3,38x,I1,1x,a1,3f11.5)',
	1		    IOSTAT=ios)jr,
	2		    dbuf%lpn(j),coord(j),dbuf%a_xyz(j),dbuf%i_xyz(j),
	3		    dbuf%a_xyz(j)+dbuf%i_xyz(j)*(dbuf%lpts(j)-1)
		    ENDIF
		 ENDIF		!motfl or not
	      ELSE		!jr eq 0: no motor
		 IF (coord(j) .ne. ' ') THEN
		    WRITE (2, '(41x,I1,1x,a1,3f11.5)',
	1		 IOSTAT=ios)
	2		 dbuf%lpn(j),coord(j),dbuf%a_xyz(j),dbuf%i_xyz(j),
	3		 dbuf%a_xyz(j)+dbuf%i_xyz(j)*(dbuf%lpts(j)-1)
		 ENDIF
	      ENDIF		!jr ne 0
	   END DO		!for each motor to be scanned
	   WRITE (2, FMT='(1x,a4,4x,a5,7x,a4,6x,a3,2x,a6,1x,a3,1x,a9,2x,a8,4x,a7)')
	1	'Mot:', 'Start', 'Step', 'End', 'Loop #','xyz',
	1	'xyz_Start','xyz_Step','xyz_End'
	   DO jr= 1, MAXMOTS	!write out positions of other motors
	      IF (motfl(jr) .AND. .NOT. posout(jr))THEN
		 posnow = rdangl(jr) - z(jr) !go read current position 
		 WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  jr,posnow
		 posout(jr) = .TRUE.
	      ENDIF		!jr ne 0
	   END DO		!for each motor
	   WRITE (2, FMT='(1x,a4,4x,a8)') 'Mot:', 'Position'
	   WRITE (2, FMT='(1x,a4,2x,f9.2,6x,a4,2x,f9.2)')
	1	'Phi:',dbuf%phi,'Psi:',dbuf%psi
	   i=0
	   line(1:)=' '
	   ik=1
	   DO j=1,6		!check if each of first 6 motors moving
	      IF (i.lt.6) THEN	!6 motors max will fit on line
		 jr = dbuf%mn(j) !get real motor number
		 IF (jr .NE. 0) THEN !number was specified
		    IF (motfl(jr) .and. .not. fixed(jr)) THEN
	               write (unit=outbuf(1:2),fmt='(i2)',IOSTAT=ios)jr
	               if (i.eq.0)then
			  line(1:9)='    A'//outbuf(1:2)//'   '
			  ik=9
		       else
			  line(1:ik+9)=line(1:ik)//'    A'//outbuf(1:2)//'   '
			  ik=ik+9
		       endif
		       i=i+1
		    ENDIF	!motfl
		 ENDIF		!jr ne 0
	      ENDIF		! .lt. 6
	   ENDDO
	   IF (tempctr) THEN
	      line(ik+1:ik+8)=' TEMP  '
	      ik=ik+8
	   ENDIF
	   ik = ik+1
	   IF (monmag) THEN
	      line(ik+1:ik+9)=' H-Field '
	      ik=ik+9
	   ENDIF
	   line(ik+1:ik+7)='  MIN  '
	   ik=ik+7
	   line(ik+1:ik+12)='     COUNTS '
	   ik=ik+12
	   WRITE (UNIT=2,fmt='(a)')line(1:ik)
C       new line with coords and phi, psi
c       line(1:)=' '
c       ik=5
c       do ig = 1,3
c       if (xyzmots(ig) .ne. 0)then
c       line(ik+1:ik+6)=' '
c       line(ik+7:ik+12)=coord(xyzmots(ig))
c       ik=ik+12
c       endif
c       enddo
c       line(ik+1:ik+9)= '   PHI  '
c       ik=ik+9
c       line(ik+1:ik+9)= '   PSI  '
c       ik=ik+9
c       WRITE (UNIT=2,fmt='(a)')line(1:ik)
	ELSE			!not first point
 1210      maxtry = 0
 	   OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	STATUS= 'OLD', IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	      CALL libwait(.2,status)
	      OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	   STATUS= 'OLD', IOSTAT=ios)
	      maxtry = maxtry + 1
	   ENDDO
	   IF (ios .NE. 0)THEN	!error opening file
	      CALL DATE_TIME (date)
	      Call Getdir(str90,right)
	      OPEN (UNIT=2, FILE= ERRORS_ICP, FORM= 'FORMATTED', 
	1	   ACCESS= 'APPEND',
	1	   STATUS= 'UNKNOWN', IOSTAT=ios)
	      write(unit=2,fmt='(1x,a,1x,a,1x,a,1x,I6)',iostat=ios)
	1	   date(1:17),' File: ',str90(1:right)//fname//' Point # ',pts+1
	   ENDIF		!error opening data file
	END IF
C---------------------------DUMP INTENSITY DATA -----------------------------C
	write(unit=str9,fmt='(I9)',iostat=ios)pts+1
	call upcase(fcap,fname(10:12))
	bstr(1:)=fcap(1:3)//':'//tab//'PT='//str9 !BT8:tab PT= and clear rest
	lb = 17			!length of filled part of bstr
	i=0
	line(1:)=' '
	ik=1
	DO j=1,6		!check if each of first 6 motors moving
	   IF (i.lt.6) THEN	!6 motors max will fit on line
	      jr = dbuf%mn(j)	!get real motor number
	      IF (jr .NE. 0) THEN !number was specified
		 IF (motfl(jr) .and. .not. fixed(jr)) THEN
		    posnow = rdangl(jr) - z(jr)	!go read current position 
		    write (unit=outbuf(1:9),fmt='(f9.3)',err=300)
     1	                 posnow
		    if (i.eq.0)then !first column
		       line(1:9)=outbuf(1:9)
		       ik=9
		    else
		       line(1:ik+9)=line(1:ik)//outbuf(1:9)
		       ik=ik+9
		    endif
		    i=i+1
		    write(unit=str2,fmt='(i2.2)',iostat=ios)jr !write motno
		    bstr(1:)=bstr(1:lb)//tab//'A'//str2//'='//outbuf(1:9)
		    lb = lb + 14
		 ENDIF		!motfl
	      ENDIF		!jr ne 0
	   ENDIF		! .lt. 6
	ENDDO
	ik = ik+1
	IF (tempctr) THEN
	   CALL Putmax(temp,line(ik+1:ik+8),8,3)
	   bstr(1:)=bstr(1:lb)//tab//'T='//line(ik+1:ik+8)
	   lb = lb + 11
	   ik=ik+8
	ENDIF
	IF (monmag)THEN
	   ik=ik+1
	   call putmax(field,line(ik+1:ik+7),7,4)
	   bstr(1:)=bstr(1:lb)//tab//'HF='//line(ik+1:ik+7)
	   lb = lb + 11
	   ik=ik+7
	ENDIF
	write(unit=line(ik+1:ik+7),fmt='(f7.2)',err=300) tim/60.
	bstr(1:)=bstr(1:lb)//tab//'M='//line(ik+1:ik+7)
	lb = lb + 10
	ik=ik+7
	write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(2)
	bstr(1:)=bstr(1:lb)//tab//'DATA='//line(ik+1:ik+12)
	lb = lb + 18
	ik=ik+12
	WRITE (UNIT=2,fmt='(a)',err=300)line(1:ik)

	IF (PSD) THEN		!write position sensitive detector data
	   bstr(lb+1:)=tab//'PSD='
	   lb = lb + 5
	   outpsd(1:)=' '
	   kpsd = 2
	   m = 0
	   DO j = 1, nocells
	      ee = 1
	      DO WHILE (histodata(j) .GE. 10**ee)
		 ee = ee + 1
	      END DO
	      m = kpsd + ee-1	    	     
	      IF (m .GE. 80) THEN ! write string to file
		 WRITE (2, '(a80)') outpsd(1:80)
		 bstr(1:)=bstr(1:lb)//outpsd(1:80)
		 lb = lb + 80
		 outpsd(1:) = ' '
		 kpsd = 2
		 m = 2 + ee-1
	      END IF
	      WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) histodata(j)
	      outpsd(kpsd:m) = datum(21-ee:20)	
	      m = m + 1
	      outpsd(m:m) = ','
	      kpsd = m + 1
	   END DO
	   IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m-1)	
	   bstr(1:)=bstr(1:lb)//outpsd(1:m-1)
	   lb = lb + m-1
	ENDIF
	GOTO 400
 300	CALL Putcontr(' Error writing to data file')
	Write(Unit=2,fmt='(1x,a,I10,a,I12)',err=400)
	1  ' Point: ',pts,' Counts: ',counts(2)
 400	CLOSE (2)
	IF (tempctr .AND. temptype .eq. 12)THEN
	   Call tdb_wrdata(.TRUE.,errflag,pts+1)
	ELSEIF (tempctr .AND. temptype .eq. 14)THEN
	   Call ad201_wrdata(.TRUE.,errflag,pts+1)
	ENDIF
	CALL Gobroad(lb,bstr)
	END
