#include "icp.h"
	SUBROUTINE Rock (mot, range, delta, mon, keep, arch,gomax,toler)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine scans the given motor around the current angle.               C
C        IF gomax at call, drive mot to position of max intensity             c
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, October, 1993                                           C
C        GG: to do: better check out what happens on bt4 with linear zero     C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface
	INTEGER*4    j, mot, points, counts(37), mcounts
	INTEGER*4    type, i, right, maxint
        REAL*4       range, delta, step, mon, current, start, zm, fz, toler
	REAL*4       pos, RdAngl, field, amps, waitmag, hmax, fmax, maxpos
        REAL*4       znew
 	CHARACTER*80 response, subdir, info(4)
	Logical keep, inlimits,gomax,arch, Readef

C---- variables for writing file --------------------------------------------C
	INTEGER*4     context, status
	INTEGER*4     fver, fversion
	CHARACTER*80  res_fname
	CHARACTER*12  file_name,psfile, fstring
	CHARACTER*2   str2
	CHARACTER*3   namestr
	CHARACTER*10  vstr, str10
	CHARACTER*14  str14
	INTEGER*4     lv, lb, ios
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)
	CHARACTER*80 GETHOME



	type = 1
	GOTO 10

	ENTRY Rock_H (field,range,delta,mon,keep,arch) !field is center of field
C	***************************************************		      C
C-----------------------------------------------------------------------------C
C	Routine scans the magnetic field around field                         C
C	Called from: Parse_Contr                                              C
C-----------------------------------------------------------------------------C
	type = 4
	gomax = .FALSE. !no final drive with field rock

C-----------------open file in which points are written-------------------C
 10	if (keep) then !open datafile
	  Psfile(1:5) = 'RKx00'
	  if (type.eq.4) THEN !scan on magnet
	    Psfile(3:3) = 'M'
	  ELSE
	    Write(psfile(4:5),fmt='(i2.2)',err=100)mot
	  ENDIF
	  fstring = file_name(psfile(1:5))
	  psfile = fstring
	  CALL Putline_NT (contr, 'File created: '//psfile(1:12))

	  OPEN (unit=1,file=psfile,status='new',err=100)
	  IF (type.ne.4)then !Rock on motor
	    WRITE (UNIT = 1,fmt='(a,i2,a)',err=150)
     1	     ' Motor no. ',mot,'    Intensity'
	  ELSE !Magnet scan
	    WRITE (UNIT = 1, fmt='(a)',err=150)
     1	    ' Field           Intensity'
	  ENDIF
	  fstring=psfile
	ELSE
	  fstring(1:)=' '
	END IF !datafile

	IF (nsta .gt. 0) THEN
	   namestr(1:) = 'BT%'
	ELSEIF (nsta .eq. 0) THEN         
	   namestr(1:) = 'XR%'
	ELSE
	   namestr(1:) = 'NG%'         
	ENDIF
	WRITE (UNIT= namestr(3:3), FMT= '(i1)') iabs(nsta)

	goto 150

 100	keep=.false.
	CALL Putline_NT (contr, 'Error opening file '//psfile(1:12))

C------------------------ FIND OUT WHERE WE ARE HERE -------------------------C

 150	IF (type .ne. 4) THEN !not a magnet scan
	  current = RdAngl(mot) - z(mot) !current position of motor
	ELSE !magnet scan
	  current = field
	  CALL calc_mag_wait(waitmag)
	ENDIF
	step    = delta
	points     = NINT (range/step)
	start   = current - points/2 * step
	CALL Putline_NT (contr, 'Rock:')
	IF (type .ne. 4) THEN !not a magnet scan
	  WRITE (UNIT= response(1:), FMT= 
     1	 '(''Angle no = '', i2,'',  Range = ['',f8.3, '' ...'',f8.3, '' ],  Step = '',f6.3,'', Mon = '', i8)') 
     3      mot, start, start + points*step, step, NINT(mon)
	ELSE !a magnet scan
	  hmax = start + points*step
	  fmax = fieldmax * hfconv
	  IF (abs(hmax) .GT. fmax .OR. abs(start) .GT. fmax)THEN
     	      CALL Putcontr_NT('ERROR!!! Field out of range ')
	      RETURN
	  ENDIF
	  WRITE (UNIT= response(1:), FMT= 
     1	 '(''Field scan:  '','',  Range = ['',f8.3, '' ...'',f8.3, '' ],  Step = '',f6.3,'',  Mon = '',i8)') 
     3      start, start + points*step, step, NINT(mon)
	ENDIF
	CALL Putline_NT (contr, response)

	vstr(1:)='VARY='
	lv = 5
	IF (type .EQ. 4) THEN !scan on magnet
	  vstr(1:)=vstr(1:lv)//'HF '
	  lv = lv + 3
	ELSE
	  write(unit=str2,fmt='(i2.2)',iostat=ios)mot !write motno
	  vstr(1:)=vstr(1:lv)//'A'//str2//' '
	  lv = lv + 4
	ENDIF

	write(unit=str10,fmt='(I10)',iostat=ios)points+1
	response(1:)=namestr//':START'//tab//'NPTS='//str10//
     1		tab//'FILE='//fstring//tab//vstr(1:lv)//tab//
     1		'ROCK'
	lb = 49+lv !length of filled part of string
	CALL Gobroad(lb,response)

	maxint    = 0
	maxpos = current
	DO j = 1, points + 1

!       C----------------- CHECK PAUSE & STOP FLAGS --------------------------C

	   IF (READEF (pause))THEN
	      CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
	      CALL CURSOR_OFF (paste)
	      CALL HIBER()                                   
	      CALL clref (contr_A)
	      CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 

!          C---------- DRIVE TO NEW POSITION THEN COUNT ----------------------C
	   IF (type .ne. 4) THEN !not magnet scan
	      pos = start + (j-1)*step
	        IF (nsta.eq.4 .and. z(mot).eq.-99.)THEN !special flag
		  zm = fz(mot,pos) !special zero
	        ELSE 
		  zm = z(mot)
	        ENDIF
	      IF (.NOT. inlimits(mot,pos+zm,contr))THEN
	         CALL Putcontr_NT('Rock aborted')
		 GOTO 300
	      END IF
              CALL Drive (mot, pos+zm)
	      CALL Yield
	   ELSE !scan on magnet
	      pos = start + (j-1)*step
	      amps = pos / hfconv
	      call setmag(amps)
	      call libwait(waitmag,status) !wait magwait seconds before starting count
	   ENDIF !magnet scan
	   IF (READEF(contr_A))THEN
	      CALL Putline_NT (contr, ' **** ABORT during Rock ****')
	      GOTO 300
	   END IF	   
	   IF (mon .GT. 0.) THEN
	      CALL Count(mon)
	   ELSE
              CALL CnTime (ABS(mon))
	   END IF
	   CALL Scaler (counts)
	   IF (READEF(contr_A))THEN
	      CALL Putline_NT (contr, ' **** ABORT during Rock ****')
	      GOTO 300
	   END IF	   

!          C---------------- PRINT COUNTS -----------------------------C

	   mcounts = counts(2)  !select scaler 1
	   IF ( type .NE. 4 ) THEN
	      WRITE (UNIT= response(1:), FMT= 
     1	   '(''   Angle= '', f8.3, 5x,''Intensity = '', i10)') pos, mcounts
	      if (keep) WRITE (UNIT = 1,fmt='(1x,f9.4,4x,i10)',err=200)
     1	        pos,mcounts
	   ELSE !scan on magnet
	      WRITE (UNIT= response(1:), FMT= 
     1  '(''   Field= '',f13.4,5x,''   Amps= '',f14.4,5x,''Intensity = '',i10)')
     1		 pos, amps,mcounts
	      if (keep) WRITE (UNIT = 1,fmt='(1x,f13.4,4x,i10)',err=200)
     1	        pos,mcounts
	   END IF
 200	   IF (keep)THEN
	     CALL Putline_NF (contr, response)
	   ELSE
	     CALL Putline_NT (contr, response)
	   ENDIF
	   IF (mcounts .GT. maxint) THEN
	      maxint = mcounts	
	      maxpos = pos
	   END IF
C          Broadcast point
	   write(unit=str10,fmt='(I10)',iostat=ios)j
	   response(1:)=namestr//':'//tab//'PT='//str10  !BT2:tab PT= and clear rest
	   lb = 18 !length of filled part of response
	   IF ( type .NE. 4 ) THEN
	     write(unit=str10,fmt='(F10.4)',iostat=ios)pos
	     response(1:)=response(1:lb)//tab//'A'//str2//'='//str10
	     lb = lb + 15
	   ELSE !scan on field
	     write(unit=str14,fmt='(F14.4)',iostat=ios)pos
	     response(1:)=response(1:lb)//tab//'HF='//str14
	     lb = lb + 18
	   ENDIF
	   write(unit=str10,fmt='(I10)',iostat=ios) mcounts
	   response(1:)=response(1:lb)//tab//'DATA='//str10
	   lb = lb + 16
	   CALL Gobroad(lb,response)
	END DO   
	IF (gomax)THEN
	  WRITE (UNIT=response(1:), FMT= 
     1    '(''Max Int found at '', f10.3,''  with an intensity of '', i10)',
     1	   err=300) maxpos, maxint
	  CALL Putline_NT(contr,response)
	  WRITE (UNIT=1, FMT= 
     1    '(1x,''Max Int found at '', f13.4,''  with an intensity of '', i10)',
     1	   err=300) maxpos, maxint
	  WRITE (UNIT=1, FMT= '(1x,''Driving to max intensity position!'')',
     1		 err=300)
	  CALL Putline_NT (contr, 'Driving to max intensity position!')
	  IF (nsta .EQ. 4 .AND. z(mot) .EQ. -99.)THEN !special flag
			zm = Fz(mot,maxpos)
	  ELSE
			zm = z(mot)
	  ENDIF
	  CALL Drive (mot, maxpos+zm)
          CALL Yield
	ENDIF !gomax
 300	if (keep) THEN
	  close(unit=1)
	        Call getdir(subdir,right)		! Get name of subdir
	        info(1)(1:) = ' Executing:   ROCK      '// 
	1              '                Datafile: '
	        info(1)(1:) = info(1)(1:50) //  subdir(1:right)//psfile(1:12)
	        info(2)(1:)= ' '
	        info(3)(1:)= ' '
	        info(4)(1:)= ' '
		call status_file(info)
	ENDIF !keep
	IF (keep.and.arch)CALL ArchFile(psfile,12)

	IF (gomax)THEN
c	 what about  IF (nsta.eq.4 .and. z(mot).eq.-99.)THEN !special case?
	 znew = RdAngl(mot) - current
	 IF ( ABS(znew-z(mot)) .GT. toler ) THEN
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	   z(mot) = znew
	   WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	   CLOSE (1)
	   if (trackmoves) call memcurpos(mot,0,znew)
	   WRITE (UNIT=response(1:), FMT= '(''New zero set to '', f10.4)', 
	1          err=300) znew
	   CALL PutContr_NT (response)
	 ELSE
	   CALL PutContr_NT ('Zero within tolerance.')
	 END IF
	ENDIF

	RETURN

	END
