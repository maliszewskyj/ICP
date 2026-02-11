#include "icp.h"
	SUBROUTINE HOWLONG (origcommand, iend , tmin, tmax, scrmes,
     1				flagstr)
C	***************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     Routine to calculate duration of buffers indicated in origcommand       C
C     Called from: Parse_control & time_com  				      C
C     At call: origcommand begins with HOWLONG and iend points to last        c
C      character of howlong ... command. All spaces have been removed.        C
c									      C
C     Gayle Greene August 14,1992                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use refl
	use hwlong
	IMPLICIT   none

	REAL*4		tmin,tmax

	LOGICAL	    scrmes !true if messages to be put to screen

	INTEGER*4     j, k, iend, l, m, istart, jc,numpols
	REAL          rmaxmin, rmin, e0, energy
	CHARACTER*2   str2
	CHARACTER*50  command,origcommand
	CHARACTER*80  response
	CHARACTER*7   str7, str7b
	CHARACTER*6   str6, str6b
	INTEGER*4     isecs,minsecs,maxsecs,ibmon,ios, loop
	CHARACTER*1   type
	LOGICAL	      errflag, nomove
	CHARACTER*20  flagstr
	REAL*4	      SCP, bufang, Energy_angle, dmnow


C  Variables to hold generic values of buffer entries
	REAL*4  tmp,it,tw,th0,th,pts,mpf,
     1		mon,hf,ihf,hfw, pos1, inc,
     1		ec,es,ef
	INTEGER*4	monomot
	CHARACTER*4	mt
	CHARACTER*3	eft
	CHARACTER*3     efstr/'EF='/
	CHARACTER*3     eistr/'EI='/
	character*7	festr

C  Set up string showing which flags used
	flagstr(1:) = '   T-'
C  Determine dm to use in determining motor positions
	IF (priv) THEN
	  dmnow = dm
	ELSE
	  dmnow = dlast !from mrat file
	ENDIF

C  Define which motor is associated with monochromator
	IF (nsta .EQ. 2 .OR. nsta .EQ. 4 .OR.
     1	    nsta .EQ. 9 .OR. nsta .EQ. -5)THEN
	  monomot = 2
	ELSEIF (nsta .eq. 1)THEN 
	  monomot = 4 !just pick any motor likely to be used
	ELSE
	  monomot = 2  !assume so for now
	ENDIF
C  Extract buffer numbers and type from entire command string
	command(1:)=origcommand(1:iend)
	type = command(8:8) !buffer type follows HOWLONG
	j = 8 !parse_contr has removed all blanks. Buffer type follows HOWLONG
	command(1:) = command(j+1:) !now command contains only buf nos.
	iend = iend - j
	command(1:) = ','//command(1:iend) !each buffer series must start with ','
	iend = iend + 1

        tmin = 0. !cumulative min time total for all buffers in mins
	tmax = 0. !cumulative max time total for all buffers in mins

	jc = INDEX (command(1:iend),',')	   !JC points to comma
	DO WHILE (jc. ne. 0)  !do while there is another comma & buffer series
c        Set up range of buffers to be treated in l through m
	   command(1:) = command(jc+1:)			
	   iend = iend - jc
	   jc = INDEX (command(1:iend),',')	   !JC points to phrase end + 1
	   if (jc .eq. 0)jc = iend + 1
	   j = INDEX (command(1:jc-1), '-')
	   IF (j .eq. 0 ) THEN
	     istart=1
	   ELSE
	     istart=j+1
	   ENDIF
	   IF ( j .EQ. 0) THEN    ! (* one buffer only *)
	      READ (UNIT= command(1:jc-1), FMT= *, ERR= 270) l        
	      m = l
	   ELSE                                   ! (* treat range of buffers *)
	      READ (UNIT= command(1:j-1), FMT= *, ERR= 270) l        
	      READ (UNIT= command(j+1:jc-1),  FMT= *, ERR= 270) m
	   END IF
	   IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1      m .LT. 1) GOTO 270
	   IF (l .GT. m) THEN                                  ! Exchange l & m
	      k = l
	      l = m
	      m = k
	   END IF

C	 Now get time for each buffer in range
	   DO k = l, m
	     errflag = .FALSE.
	     minsecs = 0   !minimum time total for 1 buffer in minutes
	     maxsecs = 0   !maximum  "    "
	     ibmon = 0    !cumulative mon count for buffer
	     IF (type .EQ. 'R')THEN
	       CALL read_buf(type,k,monomot,tmp,it,tw,th0, !get in delays
     1			th,hf,ihf,hfw,pts,mon,mpf,mt,numpols,
     1			pos1,inc,eft,ec,es,ef,errflag)
	       IF (mt .EQ. 'NEUT')THEN !5/14/93: Sushil says do not do.
	         write(unit=str2,fmt='(i2)',iostat=ios)k
	         call putline(contr,
     1	           'Buffer '//str2//':  Howlong not implemented for '//
     1	           'counts on NEUT. ')
		 errflag = .TRUE.
	       ELSE
	         call refl_time(k,isecs,ibmon)
	       ENDIF !counting on NEUT
	     ELSEIF (type .EQ. 'X')THEN
		call putcontr(' Howlong not implemented for Dartz buffers')
		errflag = .TRUE.
	     ELSE !not an R buffer or X buffer
	       CALL read_buf(type,k,monomot,tmp,it,tw,th0,
     1			th,hf,ihf,hfw,pts,mon,mpf,mt,numpols,
     1			pos1,inc,eft,ec,es,ef,errflag)
C  	       Calculate time of actual data collection only for buffer
	       IF (mt .EQ. 'TIME')THEN
		 isecs = pts * mon * mpf 
		 flagstr(10:) = 'Const Time'
	       ELSE !counting on neutrons
		 IF (type .NE. 'I')write(unit=festr,fmt='(F7.2)',iostat=ios)
     1			ef
		 nomove = .FALSE. !default is monochromator does not move
		 IF (type .EQ. 'I' .OR. type .EQ. 'B')THEN !is mon fixed?
		   IF (nsta .EQ. 1)THEN
		     nomove = .TRUE.
		   ELSE
		     IF (fixed(monomot) .OR. inc .EQ. 0)nomove = .TRUE.
		   ENDIF
		 ELSEIF (type .EQ. 'Q' .OR. type .EQ. 'T')THEN
		   IF (fixed(monomot) .OR. 
     1			(type.EQ.'Q'.AND.eft.EQ.'MON') )THEN
		     nomove = .TRUE.
		   ENDIF
	           e0 = ec - ((pts -1.) / 2.) * es
c		 ELSE !other types have already been eliminated
		 ENDIF
		IF (nomove)THEN !will be same rate for each point so calc
	          IF (type .EQ. 'I')THEN
		    flagstr(10:) = 'MONO CONST'
	          ELSE
     		    IF (type.EQ.'Q'.AND.eft.EQ.'MON') THEN
		      flagstr(10:) = eistr//festr
		    ELSEIF (fixed(monomot)) THEN
		      flagstr(10:) = 'Motor Fixd'
		    ENDIF 
		  ENDIF
		  IF (type .EQ. 'I' .OR. type .EQ. 'B')THEN !is mon fixed?
 		     bufang = pos1 
		  ELSEIF (type .EQ. 'Q') THEN
		     bufang = 2. * ( Energy_Angle (ef, dmnow) )
		  ELSE !(type .EQ. 'T')
	  	     bufang = 2. * ( Energy_Angle (ef+e0, dmnow) )
		  ENDIF
	          isecs = SCP(bufang,mon*mpf) * pts
		ELSE !mono moves
	          IF (type .EQ. 'I')THEN
		    flagstr(10:) = 'MONO VARYS'
	          ELSE
		    flagstr(10:) = efstr//festr
		  ENDIF
		  isecs = 0
		  DO loop = 0, NINT(pts) - 1
		    IF (type .EQ. 'I' .OR. type .EQ. 'B')THEN !is mon fixed?
 		      bufang = pos1 + inc * loop 
		    ELSE !(type .EQ. 'Q' .OR. type .EQ. 'T')THEN
		      energy = e0 + loop * es
		      bufang = 2. * ( Energy_Angle (ef+energy, dmnow) )
		    ENDIF
		    isecs = isecs + SCP(bufang,mon*mpf)
		  ENDDO
		ENDIF !no move or move
	       ENDIF !time or neutrons
	     ENDIF !type buffer
	     IF (polarized) THEN
	       isecs = isecs * numpols 
	       flagstr(1:2) = 'P+'
	     ENDIF
C	     Now add in waits and holds as appropriate
	     isecs = isecs + (pts * hwlconst)  !add in user provided constant ovrhead
	     IF (tmp .NE. 0 .OR. hf .NE. 0)THEN !will be entering new_Temp_field
	       isecs = isecs + (th0*60.) + (pts * th * 60.) !holds are in minutes
	     ENDIF
	     minsecs = isecs
	     maxsecs = isecs
	     IF (tmp .NE. 0)THEN
	       flagstr(5:5) = '+'
	       IF (it. EQ. 0)THEN
		 maxsecs = maxsecs + (tw*60.) !only for initial temp setting
	       ELSE
		 maxsecs = maxsecs + (pts * tw * 60.) !might wait at each point
	       ENDIF
	     ENDIF
	     IF (hf .NE. 0)flagstr(7:8)='H+'
	     IF (.NOT. errflag)THEN
	        rmin = float(minsecs)/60.
		rmaxmin = float(maxsecs)/60.
		tmin = tmin + rmin
	        tmax = tmax + rmaxmin
	       IF (scrmes)THEN !only write out if requested
	         write(unit=str7,fmt='(I7)',iostat=ios)NINT(rmin)
	         write(unit=str7b,fmt='(I7)',iostat=ios)NINT(rmaxmin)
	         write(unit=str6,fmt='(F6.1)',iostat=ios)tmin/60.
	         write(unit=str6b,fmt='(F6.1)',iostat=ios)tmax/60.
	         write(unit=str2,fmt='(i2)',iostat=ios)k
		 IF (tmin .NE. tmax)THEN
	           call putline(contr,
     1	             type//' '//str2//': '//str7//' - '//str7b//
     1	             ' Mins.  Total='//str6//' - '//str6b// ' Hrs. '//
     1		     flagstr)
		 ELSE
	           call putline(contr,
     1	             type//' '//str2//': '//str7//
     1	             ' Mins.  Total='//str6//' Hrs. '//flagstr)
		  ENDIF
	       ENDIF !scrmes
	     ENDIF !errflag
	   ENDDO !for each buffer in range
	   CLOSE(1) !close file

 	  jc = INDEX (command(1:iend),',')	   !JC points to next comma
	ENDDO !while there is another comma
	RETURN

 270	response(1:) = 'Error interpreting command'
	CALL PUTLINE(contr,response)
	RETURN

	END

	REAL*4 FUNCTION SCP (bufang,totmon)
C	***************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     Routine to calculate counting period of 1 point at monochromator 
C     angle bufang for totmon (= mon * mpf) neutrons                          C
c									      C
C     Gayle Greene April ,1997                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use hwlong
	IMPLICIT   none

	REAL*4		bufang,totmon,mr, RMR, monmr

	IF (nsta .EQ. 1)THEN
	  SCP = totmon / cntrate
	  RETURN
	ENDIF

	monmr = RMR (monang)
	IF (monmr .EQ. 0.)THEN
	  SCP = 0.
	  call putcontr(' ERR in SCP. Base monitor rate calculated as zero')
	ELSE
	  mr = cntrate * RMR(bufang) / monmr
	  IF (mr .EQ. 0.)THEN
	    SCP = 0.
	    call putcontr(' ERR in SCP. Monitor rate calculated as zero')
	  ELSE
	    SCP = totmon / mr
	  ENDIF
	ENDIF

	RETURN
	END

	REAL*4 FUNCTION RMR (bufang)
C	***************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     Routine to calculate countrate at monochromator 
C     angle bufang using information from mrat save file                      C
c									      C
C     Gayle Greene April ,1997                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use hwlong
	IMPLICIT   none

	REAL*4		bufang

	RMR = Chw1 + Chw2*bufang + Chw3*bufang*bufang +
     1		Chw4*bufang*bufang*bufang

	RETURN
	END
