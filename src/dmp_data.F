#include "icp.h"
	SUBROUTINE Dmp_Data (fname,pts,counts,tim,typ,ibuf,
     1		qbuf,tbuf,bbuf,temp,which,field,monmag)  
C	************************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Dump Intensity Data. The Intensities Are Written To A        C
C     Character String. As Soon As The Length Of That String Exceeds 80       C
C     Characters, The Whole String Is Dumped In To The Data File.             C
C     Called from: Run_I_Buf, Run_Q_Buf, Run_T_Buf                            C
C                                                                             C
C     Beat Schmid, August 1989                                                C
C     Modified by Gayle Greene Jan. 92 to have Increment data put out in      C
C       Column format							      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use histo
	use tc
	use vdata
	use asd
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
	   subroutine ISOTIME(timestamp)
           use iso_c_binding
           character(kind=c_char), intent(out)::timestamp
           end subroutine
#include "c_libwait.h"
	end interface
	INTEGER*4    pts, typ, ee, raw /1/, dummy(3) /0,0,0/,right,ios,
	1    status,notries/50/,openfai/30/ 
	INTEGER*4    i, j, k0, m, maxtry, counts(37), which, ik, kpsd, lstr90
	REAL*4       npoints, e, qh, qk, ql, temp, min, tim,
	1    zeropos/0./,field,posnow, Rdangl
	CHARACTER*5  ch_raw /'''RAW'''/
	CHARACTER*6  m_typ
	CHARACTER*10 str10(4)
	CHARACTER*12 fname,outbuf,str12
	CHARACTER*14 f_nam
	CHARACTER*28 date
	CHARACTER*3  fcap
	CHARACTER*20 datum
	CHARACTER*80 out0, line, outpsd
	CHARACTER*94 str90,str90b ! Nick inflated size of string here
	CHARACTER*1  tchar
	CHARACTER*6  tstring
	CHARACTER*8  str8
	CHARACTER*9  str9
	CHARACTER*7  str7(2)
	CHARACTER*13 str13
	CHARACTER*2 str2
	CHARACTER*40 vstr	!set up description of what is varying
	CHARACTER*40 dstr	!set up description possible "y" values
	LOGICAL      monmag, errflag
	CHARACTER*2800  bstr	!more than 256*10 + 80
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)
	INTEGER*4    lb, lv, idum1,idum2,idum3
	REAL*4       A3,A4
	REAL*4       energyvec(1024)
	CHARACTER*80 gethome

	TYPE(i_buf) ibuf      
	TYPE(t_buf) tbuf
	TYPE(q_buf) qbuf
	TYPE(b_buf) bbuf

C---------------------------INITIALIZE DATA FILE ----------------------------C

	IF (pts .EQ. 0) THEN	! first call
	   CALL DATE_TIME (date)
	   Call Getdir(str90,right)
c       add filename to monitor.rec
           maxtry = 0
	      OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	   ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
	      DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
		 CALL libwait(.2,status)
		 OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
	1	      ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
		 maxtry = maxtry+1
	      ENDDO
	      IF (ios .EQ. 0)THEN
		 write(unit=2,fmt='(1x,a,1x,a,1x,a)',err=2209)
	1	      date(1:17),' Created: ',str90(1:right)//fname
 2209		 close(unit=2,err=2210)
	      ENDIF

c       write header
 2210	   OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED',
	1	ACCESS= 'SEQUENTIAL',STATUS= 'NEW')
	   f_nam = CHAR(39) // fname // CHAR(39) 
	   datum = CHAR(39) // date(1:17) // CHAR(39) 
	   IF (typ.eq.1) THEN
	      tchar='I'
	   ELSE IF (typ.lt.4) THEN
	      tchar='Q'
	   ELSE IF (typ.eq.4) THEN
	      tchar='T'
	   ELSE IF (typ.eq.5) THEN !bragg buffer
	      tchar='B'
	   ELSE
	      tchar='U'		!unknown
	   ENDIF
	   tstring = CHAR(39) // tchar // CHAR(39)//'   '

	   IF (typ .EQ. 1) THEN	! Powder header
	      m_typ = CHAR(39) // ibuf%mt // CHAR(39)
	      IF (nsta.eq.1)then 
		 datum(13:13) = CHAR(39)
		 WRITE (2, '(a14,1x,a13,i2,3i4,2i2,f9.0,i5,2x,a6,i5,2x,a5)',
	1	      IOSTAT=ios)
	1	      f_nam, datum, typ, (dummy(i), i=1,3), nsta, raw, 
	2	      ibuf%mon, NINT(ibuf%mpf), m_typ, NINT(ibuf%pts), ch_raw
		 WRITE (2, FMT='(2x,a8,9x,a4,7x,a15,5x,a3,5x,a3,2x,a4,3x,a4,2x,a4)')
	1	      'Filename', 'Date','Exp. Parameters', 'Mon', 'Prf', 'Base',
	2	      '#pts','Type'

		 IF (magnctr) THEN
		    OPEN (UNIT=3, FILE= fname(1:10)//'H'//fname(12:12), 
	1		 FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1		 STATUS= 'NEW')
		    WRITE (3, '(a14,1x,a13,i2,3i4,2i2,f9.0,i5,2x,a6,i5,2x,a5)',
	1		 IOSTAT=ios)
	1		 f_nam, datum, typ, (dummy(i), i=1,3), nsta, raw, 
	2		 ibuf%mon, NINT(ibuf%mpf), m_typ, NINT(ibuf%pts), ch_raw
		    WRITE (3, FMT='(2x,a8,9x,a4,7x,a15,5x,a3,5x,a3,2x,a4,3x,a4,2x,a4)')
	1		 'Filename', 'Date','Exp. Parameters', 'Mon', 'Prf', 'Base',
	2		 '#pts','Type'
	  	    call putmax(ibuf%hf,str10(1),10,4)
	  	    call putmax(ibuf%ihf,str10(2),10,4)
		    write(unit=str10(3),fmt='(I10)',iostat=ios)NINT(ibuf%hfw)
c       call putmax(ibuf%hfw,str10(3),10,4)
	  	    call putmax(hfconv,str10(4),10,4)
		    write(unit=3,fmt='(1x,a)')
	1		 'H-Field = '//str10(1)//
	2		 ' Inc-H= '//str10(2)//' H-wait= '//
	3		 str10(3)//' H-conv= '//str10(4)
		    CLOSE(3)	!magnet file
		 ENDIF		!monmag
	      ELSE		!not nsta 1 
		 WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	      f_nam, datum,tstring, 
	2	      ibuf%mon, NINT(ibuf%mpf), m_typ, NINT(ibuf%pts), ch_raw
		 WRITE (2, FMT='(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)')
	1	      'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	      '#pts','Type'
	      ENDIF		!nsta 1
	      IF (polarized .AND. which .NE. 0) THEN
	         WRITE (2, '(a50,a18)') ibuf%com(1:50), '  F1: '//
	1	      flp(which,1)//'  F2: '//flp(which,2)
	      ELSE
	         WRITE (2, '(a50)') ibuf%com(1:50)
	      END IF
	      vstr(1:)='VARY='
	      lv = 5
	      DO j = 1,6
		 IF ( nsta .ne. 1 .or. j .eq. 3 .or. j .eq. 4) THEN
		    IF (motfl(j) .and. .not. fixed(j) .AND. ibuf%i(j) .NE. 0.)THEN
		       write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		       vstr(1:)=vstr(1:lv)//'A'//str2//' '
		       lv = lv + 4
		    ENDIF
		 ENDIF		!not bt1 OR motor is 3 or 4 on bt1
	      ENDDO
	      IF (tempctr .AND. ibuf%it .NE. 0 .AND. ibuf%tmp .NE. 0)THEN
		 vstr(1:)=vstr(1:lv)//'T '
		 lv = lv + 2
	      ENDIF
	      IF (magnctr .AND. ibuf%ihf .NE. 0) THEN
		 vstr(1:)=vstr(1:lv)//'HF '
		 lv = lv + 3
	      ENDIF
	      write(unit=str9,fmt='(I9)',iostat=ios)NINT(ibuf%pts)
 	      call upcase(fcap,fname(10:12))
 	      bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//
	1	   tab//'FILE='//str90(1:right)//
	2	   fname//tab//vstr(1:lv)//tab//ibuf%com(1:50)
	      lb = 78+right+lv	!length of filled part of bstr
	      call putmax(ibuf%hf,str13(1:9),9,5)
	      call putmax(ibuf%tmp,str8,8,5)
	      call putmax(ibuf%it,str7(1),7,5)
	      WRITE (unit=str90(1:75),FMT=
	1	   '(4(i4,1x),3(i3,1x),1x,f7.4,4x,a8,1x,a7,1x,a9,1x,i4)',IOSTAT=ios)
	1	   (collim(i), i= 1,4), (mosaic(i), i=1,3), wavel,
	2	   str8, str7(1), str13(1:9), scl
	      lstr90 = 75
	      WRITE (unit=str90b(1:75), FMT=
	1	   '(1x,a11,6x,a6,4x,a10,3x,a7,3x,a5,3x,a7,1x,a4)') 
	1	   'Collimation', 'Mosaic', 'Wavelength',
	2	   'T-Start', 'Incr.', 'H-field','#Det    '
	      IF (magnctr)THEN
		 call putmax(hfconv,str7(1)(1:7),7,4)
		 call putmax(ibuf%ihf,str7(2)(1:7),7,4)
		 WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
		 str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
		 lstr90=lstr90+16
	      ENDIF
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)
	      DO j= 1, 6
		 IF (nsta.eq.1) THEN
		    IF ( (j .eq. 3 .or. j .eq. 4) .and. motfl(j) )THEN
		       IF (.not. fixed(j)) THEN
			  WRITE (2, '(i3,3x,3f9.4)',IOSTAT=ios)  j, 
	1		       ibuf%a(j), ibuf%i(j),
	1		       ibuf%a(j)+ ibuf%i(j)*(ibuf%pts-1.)
		       ELSE	!is fixed
			  posnow = rdangl(j) - z(j) !go read current position 
			  WRITE (2, '(i3,3x,3f9.4)',IOSTAT=ios)  j, posnow, zeropos,posnow 
		       ENDIF	!fixed
		    ENDIF	!motfl
		 ELSE		!not BT1
		    IF (motfl(j))THEN	
		       IF (.not. fixed(j)) THEN
			  WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j, 
	1		       ibuf%a(j), ibuf%i(j),
	1		       ibuf%a(j)+ ibuf%i(j)*(ibuf%pts-1.)
		       ELSE	!fixed
			  posnow = rdangl(j) - z(j) !go read current position 
			  WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j,posnow, zeropos,posnow
		       ENDIF
		    ELSE
		       WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j, zeropos,zeropos,zeropos
		    ENDIF	!motfl or not
		 END IF		!BT1 or not
	      END DO		!for each motor
	      WRITE (2, FMT='(1x,a4,4x,a5,7x,a4,6x,a3)') 'Mot:', 'Start', 'Step', 'End'
	      IF (nsta .EQ. -5 .AND. use_asd)THEN
		 IF (keep_rawasd)THEN
		    write(2,'(a,1x,I10,1x,a)',iostat=ios)
	1		 ' #ASD values: ',noslices,' Raw Data Kept'
		 ELSE
		    write(2,'(a,1x,I10)',iostat=ios)
	1		 ' #ASD values: ',noslices
		 ENDIF
	      ENDIF
	      IF (nsta .EQ. 1) THEN ! Write zerop & scalef
	         WRITE (2, '(3(11f7.3,/))',IOSTAT=ios) (eff(j), j= 1, scl)
		 WRITE (2, '(a)') ' Detector Relative Scalefactors     '//monobt1
	         WRITE (2, '(3(11f7.2,/))',IOSTAT=ios) (zp(j),  j= 1, scl)
		 WRITE (2, '(a)') ' Detector Relative Zero Angles      '//monobt1
	         WRITE (2, '(a)') ' Data in ''N'' Detector Blocks with N = #scl'
		 IF (rockfl .AND. .NOT. fixed(rkmot))
	1	      WRITE (2,'(a,I2,a,F7.2,a,f7.2)',iostat=ios)'$Rocking motor ',
	2	      rkmot,' between ',rkpos1,' and ',rkpos2
	      ELSE
	         i=0
	         line(1:)=' '
	         ik=1
	         DO j=1,6	!check if each of first 6 motors moving
		    IF (i.lt.4) THEN !4 motors max will fit on line
		       IF (motfl(j) .and. ibuf%i(j).ne.0. .and.
	1		    .not. fixed(j)) THEN !non-zero increment
			  write (unit=outbuf(1:1),fmt='(i1)',IOSTAT=ios)j
			  if (i.eq.0)then
			     line(1:11)='     A'//outbuf(1:1)//'   '
			     ik=11
			  else
			     line(1:ik+11)=line(1:ik)//'     A'//outbuf(1:1)//'   '
			     ik=ik+11
			  endif
			  i=i+1
		       ENDIF
		    ENDIF
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
		 if (ibuf%mt .NE. 'NEUT') then
		    line(ik+1:ik+12)='    MONITOR '
		    ik=ik+12
		 endif
		 line(ik+1:ik+12)='     COUNTS '
	         ik=ik+12
		 if (scl.gt.1) then
		    line(ik+1:ik+12)='     EXTRA  '
		    ik=ik+12
		    
		 endif
	         WRITE (UNIT=2,fmt='(a)')line(1:ik)
	      END IF
	   ELSE IF (typ .EQ. 5) THEN ! Bragg header
	      m_typ = CHAR(39) // bbuf%mt // CHAR(39) 
	      WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	   f_nam, datum,tstring ,
	2	   bbuf%mon, NINT(bbuf%mpf), m_typ, NINT(bbuf%pts), ch_raw
	      WRITE (2, FMT='(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)')
	1	   'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	   '#pts','Type'
	      IF (polarized .AND. which .NE. 0) THEN
	         WRITE (2, '(a50,a18)') bbuf%com(1:50), '  F1: '//flp(which,1)//
	1	      '  F2: '//flp(which,2)
	      ELSE
	         WRITE (2, '(a50)') bbuf%com(1:50)
	      END IF
	      vstr(1:)='VARY='
	      lv = 5
	      DO j = 1,6
		 IF (motfl(j) .and. .not. fixed(j) .AND. bbuf%i(j) .NE. 0.)THEN
		    write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		    vstr(1:)=vstr(1:lv)//'A'//str2//' '
		    lv = lv + 4
		 ENDIF
	      ENDDO
	      IF (tempctr .AND. bbuf%it .NE. 0 .AND. bbuf%tmp .NE. 0)THEN
		 vstr(1:)=vstr(1:lv)//'T '
		 lv = lv + 2
	      ENDIF
	      IF (magnctr .AND. bbuf%ihf .NE. 0) THEN
		 vstr(1:)=vstr(1:lv)//'HF '
		 lv = lv + 3
	      ENDIF
	      write(unit=str9,fmt='(I9)',iostat=ios)NINT(bbuf%pts)
  	      call upcase(fcap,fname(10:12))
  	      bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//
	1	   tab//'FILE='//str90(1:right)//
	2	   fname//tab//vstr(1:lv)//tab//bbuf%com(1:50)
	      lb = 78+right+lv	!length of filled part of bstr
	      WRITE (2, '(4i4,5x,3i3,7x,3i3,f8.3,3x,3i3)',IOSTAT=ios) (collim(i), i=1,4), 
	1	   (mosaic(i), i=1,3), (NINT(bbuf%hkl1(i)), i=1,3), bbuf%a1, 
	2	   (NINT(bbuf%hkl2(i)), i=1,3)
	      WRITE (2, '(1x,a11,8x,a6,14x,a17)') 
	1	   'Collimation', 'Mosaic', 'Xstal Orientation'
	      WRITE (2, '(6f10.3)',IOSTAT=ios)
	1	   bbuf%lat_a, bbuf%b, bbuf%c, bbuf%aa, bbuf%bb, bbuf%cc
	      WRITE (2, '(a32)') 'Lattice Parameters in real space'
	      CALL Putmax(bbuf%tmp,str8,8,5)
	      CALL Putmax(bbuf%it,str9(1:8),8,5)
	      WRITE (2, '(2f10.3,f13.3,5x,2f8.3,5x,a8,2x,a8)',IOSTAT=ios) 
	1	   bbuf%ec,bbuf%es,bbuf%ef,dm,da,str8,str9(1:8)
	      IF (typ.eq.2)THEN
		 tchar='A'	!analyzer fixed
	      ELSE
		 tchar='M'	!monochrometer fixed
	      ENDIF
	      WRITE (2, '(4x,a8,2x,a7,6x,a1,a1,a6,6x,a5,3x,a5,5x,a8,3x,a7)') 
	1	   'E center', 'Delta E', 'E',tchar,' fixed', 'M-dsp', 'A-dsp',
	2	   'Tmp strt', 'Tmp inc'
	      call putmax(bbuf%hf,str13(1:9),9,4)
	      WRITE (unit=str90(1:61),FMT= 
	1	   '(3(f7.3,1x),2x,3(f7.4,1x),2x,a9)',IOSTAT=ios)
	1	   bbuf%hc, bbuf%kc, bbuf%lc, bbuf%hs, bbuf%ks, bbuf%ls,
	2	   str13(1:9)
	      WRITE (unit=str90b(1:61), FMT='(3x,a19,8x,a13,12x,a6)') 
	1	   'Q (hkl scan center)', 'Delta Q (hkl)', 'Hfield'
	      lstr90 = 61
	      IF (magnctr)THEN
		 call putmax(hfconv,str7(1)(1:7),7,4)
		 call putmax(bbuf%ihf,str7(2)(1:7),7,4)
		 WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
		 str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
		 lstr90=lstr90+16
	      ENDIF
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)
	      DO j= 1, 6
		 IF (motfl(j))THEN
		    IF (.not. fixed(j)) THEN	
	               WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j, 
	1		    bbuf%a(j), bbuf%i(j), bbuf%a(j)+ bbuf%i(j)*(bbuf%pts-1.)
		    ELSE	!fixed
	               posnow = rdangl(j) - z(j) !go read current position 
	               WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j,posnow,zeropos,posnow 
		    ENDIF	!fixed
		 ELSE
	            WRITE (2, '(i3,3x,3f11.5)',IOSTAT=ios)  j, zeropos,zeropos,zeropos
		 ENDIF		!motfl or not
	      END DO		!for each motor
	      WRITE (2, FMT='(1x,a4,4x,a5,7x,a4,6x,a3)') 'Mot:', 'Start', 'Step', 'End'
	      IF (nsta .EQ. -5 .AND. use_asd)THEN
		 IF (keep_rawasd)THEN
		    write(2,'(a,1x,I10,1x,a)',iostat=ios)
	1		 ' #ASD values: ',noslices,' Raw Data Kept'
		 ELSE
		    write(2,'(a,1x,I10)',iostat=ios)
	1		 ' #ASD values: ',noslices
		 ENDIF
	      ENDIF
	      i=0
	      line(1:)=' '
	      ik=1
	      DO j=1,6		!check if each of first 6 motors moving
		 IF (i.lt.4) THEN !4 motors max will fit on line
		    IF (motfl(j) .and. bbuf%i(j).ne.0. .and.
	1		 .not. fixed(j)) THEN !non-zero increment
	               write (unit=outbuf(1:1),fmt='(i1)',IOSTAT=ios)j
	               if (i.eq.0)then
			  line(1:11)='     A'//outbuf(1:1)//'   '
			  ik=11
		       else
			  line(1:ik+11)=line(1:ik)//'     A'//outbuf(1:1)//'   '
			  ik=ik+11
		       endif
		       i=i+1
		    ENDIF
		 ENDIF
	      ENDDO
	      IF (tempctr) THEN
		 line(ik+1:ik+8)=' TEMP  '
		 ik=ik+8
	      ENDIF
	      IF (monmag) THEN
		 line(ik+1:ik+9)=' H-Field '
		 ik=ik+9
	      ENDIF
	      line(ik+1:ik+7)='  MIN  '
	      ik=ik+7
	      line(ik+1:ik+12)='     COUNTS '
	      ik=ik+12
	      WRITE (UNIT=2,fmt='(a)')line(1:ik)
	   ELSE IF (typ .EQ. 2 .OR. typ .EQ. 3) THEN ! Const. Q Header
	      m_typ = CHAR(39) // qbuf%mt // CHAR(39) 
	      WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	   f_nam, datum,tstring ,
	2	   qbuf%mon, NINT(qbuf%mpf), m_typ, NINT(qbuf%pts), ch_raw
	      WRITE (2, FMT='(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)',
	1	   IOSTAT=ios)
	1	   'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	   '#pts','Type'
	      IF (polarized .AND. which .NE. 0) THEN
	         WRITE (2, '(a50,a18)') qbuf%com(1:50), '  F1: '//flp(which,1)//
	1	      '  F2: '//flp(which,2)
	      ELSE
	         WRITE (2, '(a50)') qbuf%com(1:50)
	      END IF
	      vstr(1:)='VARY='
	      lv = 5
	      IF (qbuf%hs .NE. 0.0)THEN
		 vstr(1:)=vstr(1:lv)//'QH '
		 lv = lv + 3
	      ENDIF
	      IF (qbuf%ks .NE. 0.0)THEN
		 vstr(1:)=vstr(1:lv)//'QK '
		 lv = lv + 3
	      ENDIF
	      IF (qbuf%ls .NE. 0.0)THEN
		 vstr(1:)=vstr(1:lv)//'QL '
		 lv = lv + 3
	      ENDIF
	      IF (qbuf%es .NE. 0.0)THEN
		 vstr(1:)=vstr(1:lv)//'ENERGY '
		 lv = lv + 7
	      ENDIF
	      IF (tempctr .AND. qbuf%it .NE. 0 .AND. qbuf%tmp .NE. 0)THEN
		 vstr(1:)=vstr(1:lv)//'T '
		 lv = lv + 2
	      ENDIF
	      IF (magnctr .AND. qbuf%ihf .NE. 0) THEN
		 vstr(1:)=vstr(1:lv)//'HF '
		 lv = lv + 3
	      ENDIF
	      write(unit=str9,fmt='(I9)',iostat=ios)NINT(qbuf%pts)

  	      call upcase(fcap,fname(10:12))
  	      bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//
	1	   tab//'FILE='//str90(1:right)//
	2	   fname//tab//vstr(1:lv)//tab//qbuf%com(1:50)
	      lb = 78+right+lv	!length of filled part of bstr
	      WRITE (2, '(4i4,5x,3i3,7x,3i3,f8.3,3x,3i3)',IOSTAT=ios) (collim(i), i=1,4), 
	1	   (mosaic(i), i=1,3), (NINT(qbuf%hkl1(i)), i=1,3), qbuf%a1, 
	2	   (NINT(qbuf%hkl2(i)), i=1,3)
	      WRITE (2, '(1x,a11,8x,a6,14x,a17)') 
	1	   'Collimation', 'Mosaic', 'Xstal Orientation'
	      WRITE (2, '(6f10.3)',IOSTAT=ios)
	1	   qbuf%a, qbuf%b, qbuf%c, qbuf%aa, qbuf%bb, qbuf%cc
	      WRITE (2, '(a32)') 'Lattice Parameters in real space'
	      CALL Putmax(qbuf%tmp,str8,8,5)
	      CALL Putmax(qbuf%it,str9(1:8),8,5)
	      WRITE (2, '(2f10.3,f13.3,5x,2f8.3,5x,a8,2x,a8)',IOSTAT=ios) 
	1	   qbuf%ec,qbuf%es,qbuf%ef,dm,da,str8,str9(1:8)
	      IF (typ.eq.2)THEN
		 tchar='A'	!analyzer fixed
	      ELSE
		 tchar='M'	!monochrometer fixed
	      ENDIF
	      WRITE (2, '(4x,a8,2x,a7,6x,a1,a1,a6,6x,a5,3x,a5,5x,a8,3x,a7)') 
	1	   'E center', 'Delta E', 'E',tchar,' fixed', 'M-dsp', 'A-dsp',
	2	   'Tmp strt', 'Tmp inc'
	      call putmax(qbuf%hf,str13(1:9),9,4)
	      WRITE (unit=str90(1:61),FMT= 
	1	   '(3(f7.3,1x),2x,3(f7.4,1x),2x,a9)',IOSTAT=ios)
	1	   qbuf%hc, qbuf%kc, qbuf%lc, qbuf%hs, qbuf%ks, qbuf%ls,
	2	   str13(1:9)
	      WRITE (unit=str90b(1:61), FMT='(3x,a19,8x,a13,12x,a6)') 
	1	   'Q (hkl scan center)', 'Delta Q (hkl)', 'Hfield'
	      lstr90 = 61
	      IF (magnctr)THEN
		 call putmax(hfconv,str7(1)(1:7),7,4)
		 call putmax(qbuf%ihf,str7(2)(1:7),7,4)
		 WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
		 str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
		 lstr90=lstr90+16
	      ENDIF
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)
	      IF (nsta .EQ. -5 .AND. use_asd)THEN
		 IF (keep_rawasd)THEN
		    write(2,'(a,1x,I10,1x,a)',iostat=ios)
	1		 ' #ASD values: ',noslices,' Raw Data Kept'
		 ELSE
		    write(2,'(a,1x,I10)',iostat=ios)
	1		 ' #ASD values: ',noslices
		 ENDIF
	      ENDIF
	      IF (tempctr)THEN
		 IF (nsta .eq. -11) THEN
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)'//
	1		 '     E     A3      A4        T-act    min       Counts'
		 ELSE IF (monmag) THEN
		    WRITE (2, '(a)') 
	1		 '    Q(x)    Q(y)    Q(z)     E       T-act'//
	2		 '    H-Field     min       Counts'
		 ELSE
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)'//
	1		 '     E       T-act    min       Counts'
		 ENDIF
	      ELSE		!not tempctr
		 IF (nsta .eq. -11) THEN
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)'//
	1		 '     E     A3      A4       min       Counts'
		 ELSE IF (monmag)THEN
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)'//
	1		 '     E       H-Field     min       Counts'
		 ELSE
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)'//
	1		 '     E       min       Counts'
		 ENDIF
	      ENDIF		!tempctr
           ELSE			! trash Header
	      m_typ = CHAR(39) // tbuf%mt // CHAR(39) 
	      WRITE (2, '(a14,1x,a19,1x,a6,f9.0,i5,2x,a6,i5,2x,a5)',IOSTAT=ios)
	1	   f_nam, datum, tstring, 
	2	   tbuf%mon, NINT(tbuf%mpf), m_typ, NINT(tbuf%pts), ch_raw
	      WRITE (2, FMT='(2x,a8,9x,a4,11x,a6,6x,a3,4x,a3,2x,a4,3x,a4,2x,a4)')
	1	   'Filename', 'Date',' Scan  ','Mon', 'Prf', 'Base',
	2	   '#pts','Type'
	      WRITE (2, '(a50)') tbuf%com(1:50)
	      vstr(1:)='VARY='
	      lv = 5
	      IF (tbuf%es .NE. 0.0)THEN
		 vstr(1:)=vstr(1:lv)//'ENERGY '
		 lv = lv + 7
	      ENDIF
	      IF (tempctr .AND. tbuf%it .NE. 0 .AND. tbuf%tmp .NE. 0)THEN
		 vstr(1:)=vstr(1:lv)//'T '
		 lv = lv + 2
	      ENDIF
	      IF (magnctr .AND. tbuf%ihf .NE. 0) THEN
		 vstr(1:)=vstr(1:lv)//'HF '
		 lv = lv + 3
	      ENDIF
	      write(unit=str9,fmt='(I9)',iostat=ios)NINT(tbuf%pts)
  	      call upcase(fcap,fname(10:12))
  	      bstr(1:)=fcap(1:3)//':START'//tab//'NPTS='//str9//

	1	   tab//'FILE='//str90(1:right)//
	2	   fname//tab//vstr(1:lv)//tab//tbuf%com(1:50)
	      lb = 78+right+lv	!length of filled part of bstr
	      WRITE (2, '(4i4,5x,3i3,10x,3i2,f8.3,3x,3i2)',IOSTAT=ios) (collim(i), i=1,4), 
	1	   (mosaic(i), i=1,3), 0,0,0, 0.0, 0,0,0 
	      WRITE (2, '(1x,a11,8x,a6,14x,a17)') 
	1	   'Collimation', 'Mosaic', 'Xstal Orientation'
	      WRITE (2, '(6f10.3)',IOSTAT=ios) 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
	      WRITE (2, '(a32)') 'Lattice Parameters in real space'
	      CALL Putmax(tbuf%tmp,str8,8,5)
	      CALL Putmax(tbuf%it,str9(1:8),8,5)
	      WRITE (2, '(2f10.3,f13.3,5x,2f8.3,5x,a8,2x,a8)',IOSTAT=ios) 
	1	   tbuf%ec,tbuf%es,tbuf%ef,dm,da,str8,str9(1:8)
	      WRITE (2, '(4x,a8,2x,a7,6x,a7,7x,a5,3x,a5,5x,a8,3x,a7)') 
	1	   'E start ', 'Delta E', 'E final', 'M-dsp', 'A-dsp',
	2	   'Tmp strt', 'Tmp inc'
	      call putmax(tbuf%hf,str13(1:9),9,4)
	      WRITE (unit=str90(1:61), FMT=
	1	   '(3(f7.3,1x),2x,3(f7.4,1x),2x,a9)',IOSTAT=ios)
	2	   0.0, 0.0, 0.0, 0.0,0.0, 0.0, str13(1:9)
	      WRITE (unit=str90b(1:61), FMT='(3x,a19,8x,a13,12x,a6)') 
	1	   'Q (hkl scan center)', 'Delta Q (hkl)', 'Hfield'
	      lstr90 = 61
	      IF (magnctr)THEN
		 call putmax(hfconv,str7(1)(1:7),7,4)
		 call putmax(tbuf%ihf,str7(2)(1:7),7,4)
		 WRITE(unit=str90(lstr90+1:),FMT='(2(1x,a7))',IOSTAT=ios)str7(1),str7(2)
		 str90b(1:)=str90b(1:lstr90)//'  H-conv   H-Inc'
		 lstr90=lstr90+16
	      ENDIF
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90(1:lstr90)
	      WRITE(2,FMT='(a)',IOSTAT=ios)str90b(1:lstr90)

	      IF (tempctr)THEN
		 IF (monmag)THEN
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)     E'//
	1		 '       T-act    H-Field     min       Counts'
		 ELSE
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)     E'//
	1		 '       T-act    min       Counts'
		 ENDIF
	      ELSE		!not tempctr
		 IF (monmag)THEN
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)     E'//
	1		 '       H-Field     min       Counts'
		 ELSE
		    WRITE (2, '(a)') '    Q(x)    Q(y)    Q(z)     E'//
	1		 '       min       Counts'
		 ENDIF
	      ENDIF		!tempctr
	   END IF		! (* initialize *)
	   CALL Gobroad(lb,bstr)
	ELSE
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
	1	   ACCESS= 'APPEND',STATUS= 'UNKNOWN', IOSTAT=ios)
	      write(unit=2,fmt='(1x,a,1x,a,1x,a,1x,I6)',iostat=ios)
	1	   date(1:17),' File: ',str90(1:right)//fname//' Point # ',pts+1
	   ENDIF		!error opening data file
	END IF


C---------------------------DUMP INTENSITY DATA -----------------------------C
	write(unit=str9,fmt='(I9)',iostat=ios)pts+1

  	call upcase(fcap,fname(10:12))
  	bstr(1:)=fcap(1:3)//':'//tab//'PT='//str9 !BT2:tab PT= and clear rest
	lb = 17			!length of filled part of bstr
	IF (nsta .NE. 1) THEN	! non BT-1 data
	   IF (typ .EQ. 1) THEN	! const. increment
	      i=0
	      line(1:)=' '
	      ik=1
	      DO j=1,6		!check if each of first 6 motors moving
		 IF (i.lt.4) THEN !4 motors max will fit on line
		    IF (motfl(j) .and. ibuf%i(j).ne.0. .and.
	1		 .not. fixed(j)) THEN !non-zero increment
	               IF (nsta.eq.7 .OR. nsta .eq. -1 .OR.
	2		    nsta .EQ. 0) THEN
			  CALL Putmax(ibuf%a(j)+ibuf%i(j)*pts,outbuf(1:11),
	1		       11,5)
		       ELSE
			  CALL Putmax(ibuf%a(j)+ibuf%i(j)*pts,outbuf(1:11),
	1		       11,3)
		       ENDIF
	               if (i.eq.0)then !first column
			  line(1:11)=outbuf(1:11)
			  ik=11
		       else
			  line(1:ik+11)=line(1:ik)//outbuf(1:11)
			  ik=ik+11
		       endif
		       write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		       bstr(1:)=bstr(1:lb)//tab//'A'//str2//'='//outbuf(1:11)
		       lb = lb + 16
		       i=i+1
		    ENDIF
		 ENDIF
	      ENDDO
	      ik = ik+1
	      IF (tempctr) THEN
		 IF (temptype .eq. tdev%cfg)THEN !working in degrees
		    CALL Putmax(temp,line(ik+2:ik+8),7,5)
		 ELSE
		    CALL Putmax(temp,line(ik+2:ik+8),7,6)
		 ENDIF
		 bstr(1:)=bstr(1:lb)//tab//'T='//line(ik+2:ik+8)
		 lb = lb + 10
		 ik=ik+8
	      ENDIF
	      IF (monmag)THEN
		 ik=ik+1
		 call putmax(field,line(ik+1:ik+7),7,5)
		 bstr(1:)=bstr(1:lb)//tab//'HF='//line(ik+1:ik+7)
		 lb = lb + 10
		 ik=ik+7
	      ENDIF
	      write(unit=line(ik+1:ik+7),fmt='(f7.2)',err=300) tim/60.
	      bstr(1:)=bstr(1:lb)//tab//'M='//line(ik+1:ik+7)
	      lb = lb + 10
	      ik=ik+7
	      if (ibuf%mt .ne. 'NEUT') THEN
		 write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(1)
		 ik=ik+12
	      endif
	      write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(2)
	      bstr(1:)=bstr(1:lb)//tab//'DATA'//'='//line(ik+1:ik+12)
	      lb = lb + 18
	      ik=ik+12
C       BOGUS	
	      if (scl .gt. 1) then
		 write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(3)
		 ik=ik+12
	      endif
C       BOGUS
	      WRITE (UNIT=2,fmt='(a)',err=300)line(1:ik)
	      IF (PSD) THEN	!write position sensitive detector data
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
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
		       lb = lb + 80
		       outpsd(1:)=' '
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
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF
	      IF (vmedata .and. dmpscl .or.
	1	   (nsta.eq.4 .and.trashmode)) THEN
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, nvhisto
		    ee = 1
		    DO WHILE (vhisto(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF
! Format was i20
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios)
	1		 vhisto(j)
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		 ENDIF
	      ENDIF
CvvvvvvvvvvvvvvNICK ADD ASD 
	      IF (USE_ASD .and. AIMPSD) THEN
		 outpsd(1:)=' '
		 kpsd = 2
		 DO j= 1, asd_xdim * asd_ydim
		    ee = 1
		    DO WHILE (asd_storage(j) .GE. 10**ee)
		       ee = ee + 1
		    END DO
		    m = kpsd + ee-1
		    IF (m .GE. 80) THEN ! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       outpsd(1:) = ' '
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) asd_storage(j)
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    IF (mod(j,asd_xdim).eq.0) THEN
		       outpsd(m:m) = ';'
		    ELSE
		       outpsd(m:m) = ','
		    ENDIF
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN ! Write what remains
		    write(2,fmt='(a)')outpsd(1:m-1)	
		 ENDIF
	      ENDIF
C^^^^^^^^^^^^^^NICK ADD ASD 
	      IF (nsta .EQ. -5 .AND. USE_ASD) THEN !write area sensitive detector data
		 bstr(lb+1:)=tab//'ASD='
		 lb = lb + 5
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, noslices
		    ee = 1
		    DO WHILE (asd_data(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
		       lb = lb + 80
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF

		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) NINT(asd_data(j)) 
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF		!ASD
	   ELSE IF (typ .EQ. 5) THEN ! Bragg
	      i=0
	      line(1:)=' '
	      ik=1
	      DO j=1,6		!check if each of first 6 motors moving
		 IF (i.lt.4) THEN !4 motors max will fit on line
		    IF (motfl(j) .and. bbuf%i(j).ne.0. .and..not. fixed(j)) THEN !non-zero increment
	               IF (nsta.eq.7 .OR. nsta.EQ.-1 .OR. nsta .eq. 0) THEN
			    CALL Putmax(bbuf%a(j)+bbuf%i(j)*pts,outbuf(1:11),
	1		    11,5)
		       ELSE
			       CALL Putmax(bbuf%a(j)+bbuf%i(j)*pts,outbuf(1:11),
	1		       11,3)
		       ENDIF
	               if (i.eq.0)then !first column
			  line(1:11)=outbuf(1:11)
			  ik=11
		       else
			  line(1:ik+11)=line(1:ik)//outbuf(1:11)
			  ik=ik+11
		       endif
		       i=i+1
		       write(unit=str2,fmt='(i2.2)',iostat=ios)j !write motno
		       bstr(1:)=bstr(1:lb)//tab//'A'//str2//'='//outbuf(1:11)
		       lb = lb + 16
		    ENDIF
		 ENDIF
	      ENDDO
	      ik = ik+1
	      IF (tempctr) THEN
		 IF (temptype .eq. tdev%cfg)THEN !working in degrees
		    CALL Putmax(temp,line(ik+2:ik+8),7,5)
		 ELSE		!in resistance
		    CALL Putmax(temp,line(ik+2:ik+8),7,6)
		 ENDIF
		 bstr(1:)=bstr(1:lb)//tab//'T='//line(ik+2:ik+8)
		 lb = lb + 10
		 ik=ik+8
	      ENDIF
	      ik = ik+1
	      IF (monmag)THEN
		 call putmax(field,line(ik+1:ik+7),7,5)
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
	      IF (PSD) THEN	!write position sensitive detector data
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
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
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
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF
	      IF (nsta .EQ. -5 .AND. USE_ASD) THEN !write area sensitive detector data
		 bstr(lb+1:)=tab//'ASD='
		 lb = lb + 5
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, noslices
		    ee = 1
		    DO WHILE (asd_data(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
		       lb = lb + 80
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) NINT(asd_data(j)) 
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF		!ASD
	   ELSE IF (typ .LT. 4) THEN ! Const-q scan
	      npoints = (qbuf%pts -1.) / 2. - pts
	      e  = qbuf%ec - npoints * qbuf%es 
	      qh = qbuf%hc - npoints * qbuf%hs
	      qk = qbuf%kc - npoints * qbuf%ks
	      ql = qbuf%lc - npoints * qbuf%ls
	      IF (nsta .eq. -11) THEN
		 a3 = rdangl(3) - z(3)
		 a4 = rdangl(4) - z(4)
	      ENDIF
	      min = tim / 60.
	      write(unit=str9,fmt='(f9.4)',iostat=ios)qh
	      bstr(1:)=bstr(1:lb)//tab//'QH='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)qk
	      bstr(1:)=bstr(1:lb)//tab//'QK='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)ql
	      bstr(1:)=bstr(1:lb)//tab//'QL='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)e
	      bstr(1:)=bstr(1:lb)//tab//'ENERGY='//str9
	      lb = lb + 17
	      IF (tempctr)THEN
		 str9(1:1)=' '
		 IF (temptype .eq. tdev%cfg)THEN !working in degrees
		    CALL Putmax(temp,str9(2:9),8,5)
		 ELSE		!in resistance
		    CALL Putmax(temp,str9(2:9),8,7)
		 ENDIF
		 bstr(1:)=bstr(1:lb)//tab//'T='//str9(2:9)
		 lb = lb + 11
		 IF (monmag)THEN
		    CALL Putmax(field,str12,12,5)
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a9,a12,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str9, str12, min, counts(2)
		    bstr(1:)=bstr(1:lb)//tab//'HF='//str12
		    lb = lb + 16
		 ELSE IF (nsta .eq. -11) THEN
		    WRITE (UNIT= 2, FMT= '(5f8.4,f9.4,a9,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, a3, a4, str9, min, counts(2)
		 ELSE
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a9,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str9, min, counts(2)
		 ENDIF
	      ELSE		!not tempctr
		 IF (monmag)THEN
		    CALL Putmax(field,str12,12,5)
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a12,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str12, min, counts(2)
		    bstr(1:)=bstr(1:lb)//tab//'HF='//str12
		    lb = lb + 16
		 ELSE IF (nsta .eq. -11) THEN
		    WRITE (UNIT= 2, FMT= '(5f8.4,f9.4,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, a3, a4, min, counts(2)
		 ELSE
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, min, counts(2)
		 ENDIF
	      ENDIF		!tempctr
	      write(unit=str12,fmt='(I12)',IOSTAT=ios)counts(2)
	      bstr(lb+1:)=tab//'DATA='//str12
	      lb = lb + 18
	      IF (PSD) THEN	!write position sensitive detector data
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
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
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
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF

	      IF (nsta .EQ. -5 .AND. USE_ASD) THEN !write area sensitive detector data
		 bstr(lb+1:)=tab//'ASD='
		 lb = lb + 5
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, noslices
		    ee = 1
		    DO WHILE (asd_data(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       bstr(1:)=bstr(1:lb)// outpsd(1:80)
		       lb = lb + 80
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) NINT(asd_data(j))
		    outpsd(kpsd:m) = datum(21-ee:20)

		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		    bstr(1:)=bstr(1:lb)// outpsd(1:m-1)
		    lb = lb + m-1
		 ENDIF
	      ENDIF		!ASD
CvvvvvvvvvvvvvvNICK ADD ASD 
	      IF (USE_ASD .and. AIMPSD) THEN
		 outpsd(1:)=' '
		 kpsd = 2
		 DO j= 1, asd_xdim * asd_ydim
		    ee = 1
		    DO WHILE (asd_storage(j) .GE. 10**ee)
		       ee = ee + 1
		    END DO
		    m = kpsd + ee-1
		    IF (m .GE. 80) THEN ! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       outpsd(1:) = ' '
		       kpsd = 2
		       m = 2 + ee-1
		    END IF
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) asd_storage(j)
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    IF (mod(j,asd_xdim).eq.0) THEN
		       outpsd(m:m) = ';'
		    ELSE
		       outpsd(m:m) = ','
		    ENDIF
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN ! Write what remains
		    write(2,fmt='(a)')outpsd(1:m-1)	
		 ENDIF
	      ENDIF
C^^^^^^^^^^^^^^NICK ADD ASD
 	      IF (vmedata .and. dmpscl) THEN
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, 37
		    ee = 1
		    DO WHILE (counts(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF
! Format was i20
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios)
	1		 counts(j)
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		 ENDIF
	      ENDIF
	   ELSE			! Trash scan

	      i = NINT(tbuf%pts)
	      call FANSVEC(tbuf%ec,i,tbuf%es,energyvec(1))
	      e  = energyvec(pts+1)
	      qh = 0.0
	      qk = 0.0
	      ql = 0.0
	      min = tim / 60.
	      write(unit=str9,fmt='(f9.4)',iostat=ios)qh
	      bstr(1:)=bstr(1:lb)//tab//'QH='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)qk
	      bstr(1:)=bstr(1:lb)//tab//'QK='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)ql
	      bstr(1:)=bstr(1:lb)//tab//'QL='//str9
	      lb = lb + 13
	      write(unit=str9,fmt='(f9.4)',iostat=ios)e
	      bstr(1:)=bstr(1:lb)//tab//'ENERGY='//str9
	      lb = lb + 17
	      IF (tempctr)THEN
		 str9(1:1)=' '
		 IF (temptype .eq. tdev%cfg)THEN !working in degrees
		    CALL Putmax(temp,str9(2:9),8,5)
		 ELSE		!in resistance
		    CALL Putmax(temp,str9(2:9),8,7)
		 ENDIF
		 bstr(1:)=bstr(1:lb)//tab//'T='//str9(2:9)
		 lb = lb + 12
		 IF (monmag)THEN
		    CALL Putmax(field,str12,12,5)
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a9,a12,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str9, str12, min, counts(2)
		    bstr(1:)=bstr(1:lb)//tab//'HF='//str12
		    lb = lb + 16
		 ELSE
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a9,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str9, min, counts(2)
		 ENDIF
	      ELSE		!not tempctr
		 IF (monmag)THEN
		    CALL Putmax(field,str12,12,5)
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,a12,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, str12, min, counts(2)
		    bstr(1:)=bstr(1:lb)//tab//'HF='//str12
		    lb = lb + 16
		 ELSE
		    WRITE (UNIT= 2, FMT= '(3f8.4,f9.4,f8.2,i12)',IOSTAT=ios) 
	1		 qh, qk, ql, e, min, counts(2)
		 ENDIF
	      ENDIF		!tempctr
	      WRITE(UNIT=str12,fmt='(I12)',IOSTAT=ios)counts(2)
	      bstr(1:)=bstr(1:lb)//tab//'DATA='//str12
	      lb = lb + 18
	      IF (vmedata.and.(nsta.eq.4 .and. trashmode)) THEN
		 outpsd(1:)=' '
		 kpsd = 2
		 m = 0
		 DO j = 1, nvhisto
		    ee = 1
		    DO WHILE (vhisto(j) .GE. 10**ee)
	               ee = ee + 1
		    END DO
		    m = kpsd + ee-1	    	     
		    IF (m .GE. 80) THEN	! write string to file
		       WRITE (2, '(a80)') outpsd(1:80)
		       outpsd(1:)=' '
	               kpsd = 2
		       m = 2 + ee-1
		    END IF
! format was i20
		    WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios)
	1		 vhisto(j)
		    outpsd(kpsd:m) = datum(21-ee:20)
		    m = m + 1
		    outpsd(m:m) = ','
		    kpsd = m + 1
		 END DO
		 IF (m.gt.2)THEN
		    write(2,fmt='(a)')outpsd(1:m-1)	
		 ENDIF
	      ENDIF		! vmedata
	   END IF
	ELSE			! BT-1 Data

	   IF (monmag) THEN
	      OPEN (UNIT=3, FILE= fname(1:10)//'H'//fname(12:12), 
     1		   FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	   STATUS= 'OLD', ERR= 875)
	      CALL Putmax(field,str12,12,5)
	      WRITE (Unit=3,fmt='(1x,I10,1x,a)',err=873)pts,str12
 873	      CLOSE(3)
	   ENDIF		!monmag
c       Set up header line
 875	   line(1:)='$'
	   ik=1
	   DO j=3,4		!check if either motor moving
	      IF (motfl(j) .and. ibuf%i(j).ne.0. .and.
	1	   .not. fixed(j)) THEN !non-zero increment
		 write (unit=outbuf(1:11),fmt='(f11.3)',err=300)
	1	      ibuf%a(j)+ibuf%i(j)*pts
		 write (unit=tchar,fmt='(I1)',err=300)j !write motor number
		 line(1:ik+15)=line(1:ik)//
	1	      ' M'//tchar//'='//outbuf(1:11)
		 bstr(1:)=bstr(1:lb)//tab//'A0'//tchar//'='//outbuf(1:11)
		 lb = lb + 15
		 ik=ik+15
	      ENDIF		!non-zero increment
	   ENDDO
	   ik = ik+1
	   IF (tempctr) THEN
	      line(1:ik+3) = line(1:ik)//' T='
	      ik = ik + 3
	      IF (temptype .eq. tdev%cfg)THEN !working in degrees
		 CALL Putmax(temp,line(ik+2:ik+8),7,5)
	      ELSE
		 CALL Putmax(temp,line(ik+2:ik+8),7,6)
	      ENDIF
	      bstr(1:)=bstr(1:lb)//tab//'T='//line(ik+2:ik+8)
	      lb = lb + 10
	      ik=ik+8
	   ENDIF
	   line(1:ik+3) = line(1:ik)//' C='
	   ik = ik + 3
	   write(unit=line(ik+1:ik+7),fmt='(f7.2)',err=300) tim/60.
	   bstr(1:)=bstr(1:lb)//tab//'M='//line(ik+1:ik+7)
	   lb = lb + 10
	   ik=ik+7
	   line(1:ik+3) = line(1:ik)//' N='
	   ik = ik + 3
	   write(unit=line(ik+1:ik+12),fmt='(I12)',err=300) counts(1)
	   bstr(1:)=bstr(1:lb)//tab//'MON='//line(ik+1:ik+12)
	   lb = lb + 17
	   ik=ik+12
	   WRITE (UNIT=2,fmt='(a)',err=300)line(1:ik)

C       Temporary code to isolate noise problem on bt1
	   line(1:)='$'
	   write(line(2:),fmt='(1x,4(1x,I15))',iostat=ios)(counts(ik),ik=34,37)
	   WRITE (UNIT=2,fmt='(a)',err=300)line(1:70)
C       End of temporary code on BT1

C       Now put out BT1 data
	   bstr(lb+1:)=tab//'DATA='
	   lb = lb + 6
   	   out0(1:)=' '
	   k0 = 2


	   DO j = 1, scl
	      ee = 1
	      DO WHILE (counts(j+1) .GE. 10**ee)
		 ee = ee + 1
	      END DO
	      m = k0 + ee-1	    	     
	      IF (m .GE. 80) THEN ! write string to file
		 WRITE (2, '(a80)') out0(1:80)
		 bstr(1:)=bstr(1:lb)// out0(1:80)
		 lb = lb + 80
		 out0(1:)=' '
		 k0 = 2
		 m = 2 + ee-1
	      END IF
! format was i20
	      WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios)
	1	   counts(j+1)
	      out0(k0:m) = datum(21-ee:20)
	      m = m + 1
	      out0(m:m) = ','
	      k0 = m + 1
	   END DO
	   IF (m.gt.2)THEN
	      write(2,fmt='(a)')out0(1:m-1)	
	   ENDIF
	   bstr(1:)=bstr(1:lb)// out0(1:m-1) !leave off final comma
	   lb = lb + m

	END IF

	GOTO 400
 300	Call Putcontr(' Error writing to data file')
	Write(Unit=2,fmt='(1x,a,I10,a,I12)',err=400)
	1  ' Point: ',pts,' Counts: ',counts(2)
 400	CLOSE (2)
	IF (tempctr .AND. temptype .EQ. 13)
	1  Call lia_wrdata(.TRUE.,errflag,pts+1)
	CALL Gobroad(lb,bstr)
	END

