#include "icp.h"
	SUBROUTINE Dmp_Refl (fname,pts,counts,tim,rbuf,temp,which,
     1		field,monmag)
C	************************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Dump Intensity Data for Reflectometer data                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use histo
	use refl
	use tc
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface
	
	INTEGER*4    pts, typ, ee, raw /1/, dummy(3) /0,0,0/,right,kj
	INTEGER*4    i, j, m, maxtry, counts(37), which, ik, kpsd,
     1			status,ios,notries/50/,openfai/30/ !from $fordef
	INTEGER*4    lstr90
	REAL*4       npoints, e, qh, qk, ql, temp, tset, min, tim,
     1	             zeropos/0./,field, rval
	CHARACTER*5  ch_raw /'''RAW'''/
	CHARACTER*6  m_typ
	CHARACTER*12 fname,outbuf
	CHARACTER*8  str8
	CHARACTER*9  str9
	CHARACTER*7  str7(2)
	CHARACTER*13 str13
	CHARACTER*14 f_nam
	CHARACTER*3  fcap
	CHARACTER*28 date
	CHARACTER*20 datum
	CHARACTER*80 line, outpsd
	CHARACTER*90 str90, str90b
	CHARACTER*1  tchar
	CHARACTER*6  tstring
	CHARACTER*40 vstr !set up description of what is varying
	LOGICAL      written, monmag
	CHARACTER*2800  bstr !more than 256*10 + 80
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)
	CHARACTER*2 str2
	INTEGER*4  lb, lv
	CHARACTER*80 gethome

	TYPE(r_buf) rbuf      

	


C--------------------------- INITIALIZE DATA FILE ----------------------------C

	IF (pts .EQ. 0) THEN                                       ! first call
	   CALL DATE_TIME (date)
c	  add filename to monitor.rec
	   Call Getdir(str90,right)
           maxtry = 0
	   OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	ACCESS= 'APPEND', STATUS='UNKNOWN',
	1	IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	     CALL libwait(.2,status)
	     OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	  ACCESS= 'APPEND', STATUS='UNKNOWN',
	2	  IOSTAT=ios)
	     maxtry = maxtry + 1
	   ENDDO
	   IF (ios .EQ. 0)THEN
	     write(unit=2,fmt='(1x,a,1x,a,1x,a)',err=2209)
     1	     date(1:17),' Created: ',str90(1:right)//fname
 2209	     close(unit=2,err=2210)
	   ENDIF

c         write header
 2210	   OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED',
	1	ACCESS= 'SEQUENTIAL',STATUS= 'NEW')
	   f_nam = CHAR(39) // fname // CHAR(39) 
	   datum = CHAR(39) // date(1:17) // CHAR(39) 
	   tchar='R'
	   tstring = CHAR(39) // tchar // CHAR(39)//'   '

	   m_typ = CHAR(39) // rbuf%mt // CHAR(39)
	   WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	f_nam, datum,tstring, 
	2	rbuf%mon0, NINT(rbuf%mpf), m_typ, NINT(rbuf%pts), ch_raw
	   WRITE (2, '(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)')
	1	'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	'#pts','Type'
	   IF (polarized .AND. which .NE. 0) THEN
	      WRITE (2, '(a50,a18)') rbuf%com(1:50), '  F1: '//flp(which,1)//
	1	   '  F2: '//flp(which,2)
	   ELSE
	      WRITE (2, '(a50)') rbuf%com(1:50)
	   END IF
	   vstr(1:)='VARY='
	   lv = 5

	   IF (rbuf%qzi.ne.0.)THEN !nonzero qz
	      vstr(1:)=vstr(1:lv)//'QZ '
	      lv = lv + 3
	   ENDIF
	   IF (rbuf%qxi.ne.0.)THEN !nonzero qx
	      vstr(1:)=vstr(1:lv)//'QX '
	      lv = lv + 3
	   ENDIF
	   IF (rbuf%numx .NE. 0) THEN
	      j = rbuf%numx
	      IF (motfl(j) .and. rbuf%ix.ne.0.) THEN !non-zero increment
		 write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		 vstr(1:)=vstr(1:lv)//'A'//str2//' '
		 lv = lv + 4
	      ENDIF		!non-zero
	   ENDIF		!numx ne 0
	   DO kj=1,4		!check if each of 4 slit motors moving
	      j = slitlist(kj)
	      IF (motfl(j) .and. rbuf%i(kj).ne.0.) THEN !non-zero increment
		 write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		 vstr(1:)=vstr(1:lv)//'A'//str2//' '
		 lv = lv + 4
	      ENDIF
	   ENDDO
	   IF (tempctr .AND. rbuf%it .NE. 0 .AND. rbuf%tmp .NE. 0)THEN
	      vstr(1:)=vstr(1:lv)//'T '
	      lv = lv + 2
	   ENDIF
	   IF (magnctr .AND. rbuf%ihf .NE. 0) THEN
	      vstr(1:)=vstr(1:lv)//'HF '
	      lv = lv + 3
	   ENDIF
	   write(unit=str9,fmt='(I9)',iostat=ios)NINT(rbuf%pts)
	   call upcase(fcap,fname(10:12))
	   bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//
     1		tab//'FILE='//str90(1:right)//
     1		fname//tab//vstr(1:lv)//tab//rbuf%com(1:50)
	   lb = 78+right+lv	!length of filled part of bstr
	   CALL Gobroad(lb,bstr)
C	IF (rbuf%mon1 .ne. 0.) THEN
C	  write(unit= out, fmt= '(i8)')NINT(rbuf%mon1)
c	  write(unit= str6(1)(1:6),fmt='(f6.2)')rbuf%exp
c	  response(1:)=response(1:j)//'  Mon1= '//out//'  Exp= '//str6(1)
c       j = j + 29
	   call putmax(rbuf%hf,str13(1:9),9,4)
	   call putmax(rbuf%tmp,str8,8,3)
	   call putmax(rbuf%it,str7(1),7,3)
	   WRITE (unit=str90(1:71),FMT=
	1	'(1x,i8,1x,F6.2,1x,F7.4,1x,f7.3,1x,a8,1x,a7,1x,a9,i5,2x,f5.3)',
	1	IOSTAT=ios) NINT(rbuf%mon1), rbuf%exp, dm, wavel,str8,
	2	str7(1), str13(1:9), scl,eff(1)
	      lstr90 = 71
	      WRITE (unit=str90b,FMT=
	1	   '(5x,a4,4x,a3,3x,a2,6x,a5,2x,a7,2x,a5,1x,a9,1x,a4,1x,a6)') 
	2	   'Mon1','Exp','Dm', 'Wavel',
	3	   'T-Start', 'Incr.', 'Hf(Tesla)','#Det','SclFac'
	      IF (magnctr)THEN
		 call putmax(hfconv,str7(1)(1:7),7,4)
		 call putmax(rbuf%ihf,str7(2)(1:7),7,4)
		 WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
		 str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
		 lstr90=lstr90+16
	      ENDIF
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)
c	      WRITE (2, '(4i3,3x,3i3,5x,f7.3,4x,2f8.3,f13.3,i8)')
c	1           (collim(i), i= 1,4), (mosaic(i), i=1,3), wavel,
c	2	    rbuf%tmp, rbuf%it, rbuf%hf, scl
c	      WRITE (2, '(1x,a11,6x,a6,4x,a10,3x,a7,3x,a5,3x,a12,3x,a5)') 
c	1	    'Collimation', 'Mosaic', 'Wavelength',
c	2	    'T-Start', 'Incr.', 'H-fld(Tesla)','# Det'
	      WRITE (2, '(a3,3x,3f11.6)',IOSTAT=ios)  ' Qz', 
	1	   rbuf%qz, rbuf%qzi, rbuf%qz + rbuf%qzi*(rbuf%pts-1.)
	      WRITE (2, '(a3,3x,3f11.6)',IOSTAT=ios)  ' Qx', 
	1	   rbuf%qx, rbuf%qxi, rbuf%qx + rbuf%qxi*(rbuf%pts-1.)
	      DO kj= 1, 4
		 j = slitlist(kj)
		 IF (motfl(j))THEN	
	            WRITE (2, '(1x,a1,i1,3f11.3)',IOSTAT=ios)'S',  kj, 
	1		 rbuf%s(kj), rbuf%i(kj),
	1		 rbuf%s(kj)+ rbuf%i(kj)*(rbuf%pts-1.)
	          ELSE
		     WRITE (2, '(1x,a1,i1,3f11.3)',IOSTAT=ios)'S',kj,
	1		  zeropos,zeropos,zeropos
	          ENDIF !motfl or not
	      END DO !for each motor
	      IF (rbuf%numx .NE. 0) THEN
		 j = rbuf%numx
		 IF (motfl(j))THEN	
	            WRITE (2, '(1x,i2,3f11.3)',IOSTAT=ios) j, 
	1		 rbuf%sx, rbuf%ix, rbuf%sx+ rbuf%ix*(rbuf%pts-1.)
		 ELSE
	            WRITE (2, '(1x,i2,3f11.3)',IOSTAT=ios)j, zeropos,zeropos,zeropos
		 ENDIF		!motfl or not
	      END IF		!Xtra motor
	      WRITE (2, '(1x,a4,4x,a5,7x,a4,6x,a3)') 'Mot:', 'Start', 'Step', 'End'
	      i=0
	      line(1:)=' '
	      ik=1
	      IF (rbuf%qzi.ne.0.)THEN !nonzero qz
		 if (i.eq.0)then
		    line(1:11)='     QZ   '
		    ik=11
		 else
		    line(1:ik+11)=line(1:ik)//'     QZ   '
		    ik=ik+11
		 endif
		 i=i+1
	      ENDIF
	      IF (rbuf%qxi.ne.0.)THEN !nonzero qx
		 if (i.eq.0)then
		    line(1:10)='    QX   '
		    ik=10
		 else
		    line(1:ik+10)=line(1:ik)//'    QX   '
		    ik=ik+10
		 endif
		 i=i+1
	      ENDIF

	      IF (rbuf%numx .NE. 0) THEN
		 j = rbuf%numx
		 IF (i.lt.4) THEN !4 motors max will fit on line
		    IF (motfl(j) .and. rbuf%ix.ne.0.) THEN !non-zero increment
	               write (unit=outbuf(1:2),fmt='(i2)',IOSTAT=ios)j
	               if (i.eq.0)then
			  line(1:6)=' '//outbuf(1:2)//'  '
			  ik=6
		       else
			  line(1:ik+6)=line(1:ik)//' '//outbuf(1:2)//'  '
			  ik=ik+6
		       endif
		       i=i+1
		    ENDIF
		 ENDIF
	      ENDIF		!numx ne 0
	      DO kj=1,4		!check if each of 4 slit motors moving
		 j = slitlist(kj)
		 IF (i.lt.4) THEN !4 motors max will fit on line
		    IF (motfl(j) .and. rbuf%i(kj).ne.0.) THEN !non-zero increment
	               write (unit=outbuf(1:1),fmt='(i1)',IOSTAT=ios)kj
	               if (i.eq.0)then
			  line(1:6)=' S'//outbuf(1:1)//'  '
			  ik=6
		       else
			  line(1:ik+6)=line(1:ik)//' S'//outbuf(1:1)//'  '
			  ik=ik+6
		       endif
		       i=i+1
		    ENDIF
		 ENDIF
	      ENDDO
	      IF (tempctr) THEN
		 line(ik+1:ik+8)='  TEMP  '
		 ik=ik+8
	      ENDIF
	      IF (MONMAG) THEN
		 line(ik+1:ik+7)='H-FLD'
		 ik=ik+7
	      ENDIF
	      line(ik+1:ik+12)='       MON  '
	      ik=ik+12
	      line(ik+1:ik+12)='  #1 COUNTS '
	      ik=ik+12
	      line(ik+1:ik+12)='  #2 COUNTS '
	      ik=ik+12
	      WRITE (UNIT=2,fmt='(a)')line(1:ik)
	   ELSE
 1210	      maxtry = 0
	      OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	   STATUS= 'OLD', IOSTAT=ios)
	      DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
		 CALL libwait(.2,status)
		 OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	      STATUS= 'OLD', IOSTAT=ios)
		 maxtry = maxtry + 1
	      ENDDO
	      IF (ios .NE. 0)THEN !error opening file
		 CALL DATE_TIME (date)
		 Call Getdir(str90,right)
		 OPEN (UNIT=2, FILE= ERRORS_ICP, FORM= 'FORMATTED', 
	1	      ACCESS= 'APPEND',
	1	      STATUS= 'UNKNOWN', IOSTAT=ios)
		 write(unit=2,fmt='(1x,a,1x,a,1x,a,1x,I6)',iostat=ios)
	1	      date(1:17),' File: ',str90(1:right)//fname//
	1	      ' Point # ',pts+1
	   ENDIF !error opening data file
	END IF

            

C--------------------------- DUMP INTENSITY DATA -----------------------------C
	write(unit=str9,fmt='(I9)',iostat=ios)pts+1
	call upcase(fcap,fname(10:12))
	bstr(1:)=fcap(1:3)//':'//tab//'PT='//str9  !NG7:tab PT= and clear rest
	lb = 17 !length of filled part of bstr

	i=0
	line(1:)=' '
	ik=1
	IF (rbuf%qzi.ne.0.)THEN !nonzero qz
	   write (unit=outbuf(1:10),fmt='(f10.6)',err=300)
	1	rbuf%qz + rbuf%qzi*pts
	   if (i.eq.0)then	!first column
	      line(1:10)=outbuf(1:10)
	      ik=10
	   else
	      line(1:ik+10)=line(1:ik)//outbuf(1:10)
	      ik=ik+10
	   endif
	   i=i+1
	   bstr(1:)=bstr(1:lb)//tab//'QZ'//'='//outbuf(1:10)
	   lb = lb + 14
	ENDIF
	IF (rbuf%qxi.ne.0.)THEN !nonzero qx
	   write (unit=outbuf(1:10),fmt='(f10.6)',err=300)
	1	rbuf%qx + rbuf%qxi*pts
	   if (i.eq.0)then	!first column
	      line(1:10)=outbuf(1:10)
	      ik=10
	   else
	      line(1:ik+10)=line(1:ik)//outbuf(1:10)
	      ik=ik+10
	   endif
	   i=i+1
	   bstr(1:)=bstr(1:lb)//tab//'QX'//'='//outbuf(1:10)
	   lb = lb + 14
	ENDIF

	IF (rbuf%numx .NE. 0) THEN
	   j = rbuf%numx
	   IF (i.lt.4) THEN	!4 motors max will fit on line
	      IF (motfl(j) .and. rbuf%ix.ne.0.) THEN !non-zero increment
		 write (unit=outbuf(1:8),fmt='(f8.3)',err=300)
	1	      rbuf%sx+rbuf%ix*pts
		 if (i.eq.0)then !first column
		    line(1:8)=outbuf(1:8)
		    ik=8
		 else
		    line(1:ik+8)=line(1:ik)//outbuf(1:8)
		    ik=ik+8
		 endif
		 i=i+1
		 write(unit=str2,fmt='(I2.2)',IOSTAT=ios)j
		 bstr(1:)=bstr(1:lb)//tab//'A'//str2//'='//outbuf(1:8)
		 lb = lb + 13
	      ENDIF
	   ENDIF
	END IF			!numx ne 0
	DO kj=1,4		!check if each of 4 slit motors moving
	   j = slitlist(kj)
	   IF (i.lt.4) THEN	!4 motors max will fit on line
	      IF (motfl(j) .and. rbuf%i(kj).ne.0.) THEN !non-zero increment
		 outbuf(1:)=' '
		 rval = rbuf%s(kj)+rbuf%i(kj)*pts
		 CALL Putmax(rval,outbuf(2:6),5,3)
		 if (i.eq.0)then !first column
		    line(1:6)=outbuf(1:6)
		    ik=6
		 else
		    line(1:ik+6)=line(1:ik)//outbuf(1:6)
		    ik=ik+6
		 endif
		 i=i+1
		 write(unit=str2,fmt='(I2.2)',IOSTAT=ios)j
		 bstr(1:)=bstr(1:lb)//tab//'A'//str2//'='//outbuf(2:6)
		 lb = lb + 10
	      ENDIF
	   ENDIF
	ENDDO
	ik=ik+1			!ik should always point to next blank place
	IF (tempctr) THEN
	   CALL Putmax(temp,line(ik+2:ik+8),7,3)
	   bstr(1:)=bstr(1:lb)//tab//'T='//line(ik+2:ik+8)
	   lb = lb + 10
	   ik=ik+8
	ENDIF
	IF (monmag) THEN
	   ik = ik+1
	   call putmax(field,line(ik+2:ik+7),6,4)
	   bstr(1:)=bstr(1:lb)//tab//'HF='//line(ik+2:ik+7)
	   lb = lb + 10
	   ik=ik+7
	ENDIF
	write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(1)
	bstr(1:)=bstr(1:lb)//tab//'MON='//line(ik+1:ik+12)
	lb = lb + 17
	ik=ik+12
	write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(2)
	bstr(1:)=bstr(1:lb)//tab//'SC1='//line(ik+1:ik+12)
	lb = lb + 17
	ik=ik+12
	write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(3)
	bstr(1:)=bstr(1:lb)//tab//'DATA='//line(ik+1:ik+12)
	lb = lb + 18
	ik=ik+12
	WRITE (UNIT=2,fmt='(a)',err=300)line(1:ik)
	IF (PSD) THEN		!write position sensitive detector data
	   bstr(lb+1:)=tab//'PSD='
	   lb = lb + 5
	   outpsd(1:)=' '	!flush output buffer
	   kpsd = 2		!kpsd points to where next char should go in buffer
	   m = 0		!m points to last char in buffer
	   DO j = 1, nocells	!do for each point
	      ee = 1
	      DO WHILE (histodata(j) .GE. 10**ee)
		 ee = ee + 1
	      END DO		!here, ee will have # of digits to write
	      m = kpsd + ee-1	!m is where last digit should go
	      IF (m .GE. 80) THEN ! if no room, write string to file first
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
	   IF (m.gt.2)THEN
	      write(2,fmt='(a)')outpsd(1:m-1)	
	      bstr(1:)=bstr(1:lb)//outpsd(1:m-1)
	      lb = lb + m-1
	   ENDIF
	ENDIF
	GOTO 400
 300	CALL Putcontr(' Error writing to data file')
	Write(Unit=2,fmt='(1x,a,I10,a,I12,1x,I12)',err=400)
	1  ' Point: ',pts,' Counts: ',counts(2),counts(3)
 400	CLOSE (2)
	CALL Gobroad(lb,bstr)
	END

