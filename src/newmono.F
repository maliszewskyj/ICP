#include "icp.h"

	SUBROUTINE NewMono(newstr,errsig)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  NewMono: move motors to place new mono in and take old mono out            C
C   Warning: this routine must be very robust since motors are behind
C   shielding. Abort if any question about anything.
C
C	at return: errsig=false if operation finished successfully
C                                                                             C
C	Gayle Greene, January 1998                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_vme.h"
	end interface
	character*5  newstr	!name of mono to change to
	logical  errsig
	integer*4  nummonos
	parameter (nummonos=3)
	character*5  monostr(nummonos)/'CU311','GE311','GE733'/
	integer*4  filtmot
	parameter (filtmot=10)
	real*4     poserr, rderrflag
	parameter (poserr=1.,
	1    rderrflag = -999.99)
	integer*4  maxtries
	parameter (maxtries=12)
	integer*4	cu311,ge311,ge733
	parameter	(cu311 = 1,
	1    ge311 = 2,
	2    ge733 = 3)

	character*5  oldstr, label(nummonos)
	integer*4    newindex,oldindex !indices in tables corresponding to new & old
	real*4	     mot4pos(nummonos,nummonos)
	integer*4    motno(nummonos),posin(nummonos),posout(nummonos),
	1    filtpos(nummonos),mot12pos(nummonos)
	character*19	mot4file
	character*25	transfile
	character*1	head
	integer*4	i, lbuf, status, j, ios, st
	integer*4       i1 
	logical READEF
	character*80    buffer
	logical		stopped,plus,minus,onhome,comerr,errflag,
	1    lresp, LimProb
	real*4		rval(2), newpos, curpos, RdAngl
	character*9	str9
	character*2	str2
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)

	real*4	range,step,mon,twotheta/0./,rate,time
	integer*4  callid,mot,type,lglist,movelist(7), counts(37)
	character*7  st7(2)
	character*13	str13
	character*4	str4
	character*80  help_line(3)

	character*80  gethome

	mot4file = MOVE4_CFG
	transfile = TRANSLATION_CFG

	errsig = .true.		!default is error

C       Will listen for contr_a during this routine
	CALL CLREF(contr_a)
C       Each mono corresponds to an index in the various tables
C       used by this subroutine. Find the indices for the old
C       and new monochromators.
	oldstr = monobt1	!monobt1 has the string describing the current mono
	oldindex = 0
	i = 1
	do while (oldindex .eq. 0 .and. i .LE. nummonos)
	   IF (monostr(i) .eq. oldstr) oldindex = i
	   i = i + 1
	enddo
	IF (oldindex .eq. 0)THEN
	   call putcontr_NT(' Error identifying current monochromator')
	   RETURN
	ENDIF
	newindex = 0
	i = 1
	do while (newindex .eq. 0 .and. i .LE. nummonos)
	   IF (monostr(i) .eq. newstr) newindex = i
	   i = i + 1
	enddo
	IF (newindex .eq. 0)THEN
	   call putcontr_NT(' Error: new monochromator not recognized')
	   RETURN
	ENDIF
	IF (newindex .eq. oldindex)THEN
	   call putcontr_NT(' Error: current monochromator specified as new')
	   RETURN
	ENDIF

	call putcontr_nt('Changing from mono '//monostr(oldindex)//
	1    ' to '//monostr(newindex) )
	call putcontr_nf(' ')
	call putcontr_nt('Waiting for you to shut off the beam...')
	call putcontr_nt('Be sure to remove the sample changer.')

 28	CALL Read_STRING(keyb, str9,
	1    'Type GO to continue, STOP to abort monochromator change: ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft


	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   goto 28
	ENDIF
	IF (str9(1:4) .EQ. 'STOP') THEN
	   CALL Putcontr_NT('Procedure aborted.')
	   RETURN
	ELSEIF (str9(1:2) .NE. 'GO') THEN
	   IF ( READEF(contr_a) ) GOTO 800	  
	   GOTO 28
	ENDIF
C       Read in positions from bt1 setup files
c       fill mot4pos with positions from file
	OPEN(UNIT=1,FILE=MOVE4_CFG,STATUS='OLD',ERR=699)
	read(1,fmt='(a)',err=700)buffer !skip headers
	do i = 1,2
	   read(1,fmt='(a)',err=700) buffer
	   lbuf = len_trim(buffer)
	   j = 1
	   do while ( (buffer(j:j) .eq. ' '.or. buffer(j:j) .eq. tab ) .and. 
     1		j .lt. lbuf)
	      j = j + 1
	   enddo
	   label(i)=buffer(j:j+4)
	   read(unit=buffer(j+5:lbuf),fmt=*,err=700)rval(i)
	enddo
	close(unit=1)

	do i = 1,2
	   if (label(i) .NE. monostr(i+1) )THEN
	      CALL Putcontr_NT('Records must be in alphabetical order')
	      GOTO 700
	   endif
	enddo

	mot4pos(cu311,cu311) = 0.
	mot4pos(cu311,ge311) = rval(1)
	mot4pos(cu311,ge733) = rval(2)
	mot4pos(ge311,cu311) = -rval(1)
	mot4pos(ge311,ge311) = 0.
	mot4pos(ge311,ge733) = -( abs(rval(1)) + abs(rval(2)) )
	mot4pos(ge733,cu311) = -rval(2)
	mot4pos(ge733,ge311) = ( abs(rval(1)) + abs(rval(2)) )
	mot4pos(ge733,ge733) = 0.

c       fill translations

	open(unit=1,file=TRANSLATION_CFG,status='old',err=750)
	read(1,fmt='(a1)',err=750)header !skip header
	do i = 1,nummonos
	   read(1,fmt='(a)',err=750)buffer
	   lbuf=len_trim(buffer)
	   j = 1
	   do while ( (buffer(j:j) .eq. ' '.or. buffer(j:j) .eq. tab ) .and. 
     1		j .lt. lbuf)
	      j = j + 1
	   enddo
	   label(i)=buffer(j:j+4)
	   read(unit=buffer(j+5:lbuf),fmt=*,err=750)motno(i),posin(i),posout(i),
	1	filtpos(i),mot12pos(i)
	enddo
	close(unit=1)

	do i = 1,3
	   IF (label(i) .NE. monostr(i))THEN
	      CALL Putcontr_NT(' Records must be in alphabetical order')
	      GOTO 750
	   ENDIF
	enddo

	curpos = rdangl(4)
	if (curpos > 12.0) then
	   CALL putcontr_nt('Moving detector bank out of harms way. Thank Judy.')
	   call drive(4,12.0)
	   call yield
	endif


C       Check positions for 1,7,10, and 14
C       Reinitialize positions of all monochromators and pull out of the beam
	do i = 1,nummonos
	   write(unit=str2,fmt='(I2)',iostat=ios)motno(i)
	   CALL VMEEnable(motno(i))
	   CALL Go_Limit(motno(i),.true.,comerr)
	   CALL Putcontr_NT('Sending motor '//str2//' to neg limit')
	end do

	CALL Yield
	if (readef(contr_a)) goto 800
	do i = 1, nummonos
	   write(unit=str2,fmt='(I2)',iostat=ios)motno(i)
	   Call Check_Limits(motno(i),stopped,plus,minus,onhome,comerr)
	   IF (comerr .or. .not. stopped .or. .not. minus)THEN
	      CALL Putcontr_NT(' Error verifying motor '//str2//' on neg limit')
	      RETURN
	   ENDIF	  
	   CALL Iang(motno(i),0.0)
	   CALL Drive(motno(i),100.0)
	end do
	CALL Yield
	
	do i = 1, nummonos
	   CALL VMEDisable(motno(i))
	   if (readef(contr_a)) goto 800
	   if (i .eq. oldindex) then
	      curpos = rdangl(filtmot)
	      if ( abs(curpos - rderrflag) .LT. poserr)THEN
		 CALL Putcontr_NT('Cannot determine current position for Filter')
		 RETURN
	      endif
	      if ( abs( curpos - filtpos(i) ) .GT. poserr)THEN
		 CALL Putcontr_NT('Filter not in expected start position.')
		 RETURN
	      endif
	   endif
	end do

	IF (mottype(12).eq.VME) THEN
	   CALL VMEEnable(12)
	ENDIF

 38	CALL Read_STRING(keyb, str9,
	1    'Type GO to continue, STOP to abort monochromator change: ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft


	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 38
	ENDIF
	IF (str9(1:4) .EQ. 'STOP') THEN
	   CALL Putcontr_NT('Procedure aborted.')
	   RETURN
	ELSEIF (str9(1:2) .NE. 'GO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 38
	ENDIF

C       Get external hardware (motors 3,4, and 12) to position
C       Drive motor 12 to home:
	curpos = rdangl(12)	!hardware position 
	if ( abs(curpos - rderrflag) .LT. poserr)THEN
	   CALL Putcontr_NT('ERR: Cannot determine current position for Motor 12')
	   RETURN
	endif
	IF ( curpos .LT. 0.0)THEN !motor could be on wrong side of home
	   CALL Putcontr_NT('ERR: Motor 12 has negative current position')
	   CALL Putcontr_NT('      and could be on wrong side of home.')
	   RETURN
	ENDIF
	IF ( curpos .GT. 100.0+z(12) )THEN
	   IF (mottype(12).eq.VME) THEN
	      CALL VMEEnable(12)
	   ENDIF
	   CALL Drive( 12, 100.0+z(12) ) !go to software 100
	   CALL Yield
	ENDIF
	IF ( READEF(contr_a) ) GOTO 800

	IF (limprob(12)) RETURN
	i = 12			!send motor 12 to home
	str2(1:1)='-'		!go in - direction
C	CALL Home (i, str2(1:1))
	CALL Go_Limit(i,.true.,comerr)
	CALL Putcontr_NT('Waiting for motor 12 to reach neg limit')
	CALL Yield_or_Abort(12,errflag)
C	CALL Yield
	IF ( READEF(contr_a) ) GOTO 800
C	IF (limprob(12)) RETURN
	Call Check_Limits(12,stopped,plus,minus,onhome,comerr)
	IF (comerr .or. .not. stopped .or. .not. minus)THEN
	   CALL Putcontr_NT(' Error verifying motor 12 on neg limit')
	   RETURN
	ENDIF
C       Init motor 12 to 0:
	IF ( READEF(contr_a) ) GOTO 800
	CALL Iang (12, 0.0)

C       Now get motor 4 to position
        newpos = mot4pos(oldindex,newindex) !is this the right order of indexes?
	curpos = rdangl(4)	!curpos starts with hardware position 
	if ( abs(curpos - rderrflag) .LT. poserr)THEN
	   CALL Putcontr_NT('ERR: Cannot determine current position for Motor 4')
	   RETURN
	endif
	curpos = curpos - z(4)	!curpos is now software angle
	if (newpos - curpos .GT. 0.0)THEN !positive move
	   if (curpos .gt. 3)THEN !move motor 4 to 3.0 to get it out of the way
	      call drive(4,3.0+z(4))
	      call yield
	   endif
	else			!negative move
	   call drive(4,newpos+z(4))
	   call yield
	endif
	IF ( READEF(contr_a) ) GOTO 800

	IF (limprob(4)) RETURN

	CALL Putcontr_NT('Waiting for you to:')
	CALL Putcontr_NT('  Close beam')
C	CALL Putcontr_NT('  Switch monitor')
	CALL Putcontr_NT('  Switch shielding')
	CALL Putcontr_NT('  Remove beamstop')

 138	CALL Read_STRING(keyb, str9,
	1    'Type GO when ready to continue: ',   
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft

	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   goto 138
	ENDIF

	IF (str9(1:2).NE.'GO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 138
	ENDIF

	lglist = 0
	if (newpos - curpos .GT. 0.0)THEN !finish positive move
	   call drive(4,newpos+z(4))
	   lglist = lglist + 1
	   movelist(lglist) = 4
	ENDIF

C       while 4 is moving, go move others as well (except 12 which interferes with 4)
	call drive(3,newpos+z(3))
	lglist = lglist + 1
	movelist(lglist) = 3
c       move old monochromator out
	IF (mottype(motno(newindex)).eq.VME) THEN
	   CALL VMEEnable(motno(newindex))
	ENDIF
	IF (mottype(filtmot).eq.VME) THEN
	   CALL VMEEnable(filtmot)
	ENDIF

c       move the new monochromator in
	call drive( motno(newindex), float(posin(newindex)) + z(motno(newindex)) )
	lglist = lglist + 1
	movelist(lglist) = motno(newindex)
	call yield
	IF (mottype(motno(newindex)).eq.VME) THEN
	   CALL VMEDisable(motno(newindex))
	ENDIF

c       move filter to position for new monochromator

! First move to negative limit (J. Stalick email 10/21/2005)
	call go_limit(filtmot, .true., comerr)
	call yield_or_abort(filtmot,errflag)
	call iang(filtmot, 0.0) ! Rezero

	call drive( filtmot, float(filtpos(newindex)) + z(filtmot) )
C	call drive(filtmot, 100.0 + z(filtmot))
	lglist = lglist + 1
	movelist(lglist) = filtmot
c       wait for these motors to finish and check for any limits or contr_a
	call yield

	IF (mottype(filtmot).eq.VME) THEN
	   CALL VMEDisable(filtmot)
	ENDIF

	IF ( READEF(contr_a) ) GOTO 800	

	do i = 1, lglist
	   j = movelist(i)	!get number of next motor on list 
	   IF (limprob(j)) RETURN
	enddo			!for each motor in movelist

	CALL Iang (4, 0.0)
	CALL Iang (3, 0.0)

C       Now get 12 into its final position
	Call drive(12,float(mot12pos(newindex)) + z(12) )
	call yield
C	IF (mottype(12).eq.VME) THEN
C       CALL VMEDisable(12)
C	ENDIF

	IF ( READEF(contr_a) ) GOTO 800
	IF (limprob(12)) RETURN


	CALL Putcontr_NT('Waiting for you to:')
	CALL Putcontr_NT('  Remove beamstop')
	CALL Putcontr_NT('  Open the beam')

 238	CALL Read_STRING(keyb, str9,
	1    'Type GO when ready to continue: ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft

	IF (i. eq. 0)THEN

	   IF ( READEF(contr_a) ) GOTO 800

	   goto 238
	ENDIF

	IF (str9(1:2).NE.'GO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 238
	ENDIF

	CALL Putcontr_NT(' ')
	CALL Putcontr_NT('Monochromator now set to '//newstr)

	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	READ (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS)
	READ (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4),
	1    (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	CLOSE(1)
	monobt1 = newstr
	IF (newstr .EQ. 'CU311')THEN
	   ll(4) = -1.
	   ul(4) = 13.1
	   CALL Putcontr_NT('Motor 4 limits set to -1. and 13.1')
	   collim(2) = 20
	ELSEIF (newstr .EQ. 'GE311')THEN
	   ll(4) = -1.
	   ul(4) = 11.4
	   CALL Putcontr_NT('Motor 4 limits set to -1. and 11.4')
	   collim(2) = 20
	ELSEIF (newstr .EQ. 'GE733')THEN
	   ll(4) = -0.7
	   ul(4) = 13.1
	   CALL Putcontr_NT('Motor 4 limits set to -0.7 and 13.1')
	   collim(2) = 30
	ENDIF
	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1    ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	WRITE (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	WRITE (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),
	1    (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	CLOSE (1)
	IF (collim(1).eq.7)THEN
	   str13(1:)=newstr//'_7'//'.DAT'
	ELSEIF (collim(1).eq.15)THEN
	   str13(1:)=newstr//'_15'//'.DAT'
	ELSEIF (collim(1).eq.60)THEN
	   str13(1:)=newstr//'_60'//'.DAT'
	ENDIF
	IF (collim(1) .ne. 7 .and. collim(1) .ne. 15.and.collim(1).ne.60)THEN
     	     buffer='Warning: collim must be reset to 7, 15, or 60.'
	   CALL Putline_NT(contr,buffer)
	ELSE
	   OPEN (UNIT= 1, FILE= HOMEDIR//str13, STATUS= 'OLD',
	1	ACCESS='SEQUENTIAL', FORM= 'FORMATTED', ERR= 240)
	   READ (1, *,err=240) wavel, (eff(i), i= 1,scl), (zp(i), i= 1, scl) !overwrite wavel
	   CLOSE(1)
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1	ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	   WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4),	!memorize new wavel
	1	(mosaic(i), i= 1,3), wavel, dm, da, monobt1
	   CLOSE (1)
	   GOTO 245
C       noncritical error branch
 240	   close(1)
	   Call Putcontr_nt('NonFatal Err reading file sys$login:'//str13)
 245	   continue
	ENDIF
	help_line(1) = ' Type HELP for commands '
	write(unit=str4(1:4),fmt='(I4)',iostat=ios)collim(1)
	help_line(2)=' Collimation: '//str4//'  MONO='//monobt1
	help_line(3)=' Use COL= command to change collimation'
	CALL Put_Help (help_line)
	CALL Putcontr_NT(' ')

C       mrat1:
 250	time = 1.
	CALL CLREF (contr_A)

	WRITE (UNIT= st7(1)(1:5), FMT= '(i2)',iostat=ios) NINT(time)
	buffer = 'Measuring for '// st7(1)(1:2) //' seconds.  Please wait !'
	CALL CnTime (time)
	t_count = .FALSE.
	CALL Putline_NT (contr, buffer)
	CALL Scaler (counts)
	IF ( READEF(contr_a) ) GOTO 800

	WRITE (UNIT= st7(2), FMT= '(i5)',iostat=ios) NINT(time)
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) counts(1)
	buffer = st7(1)(1:7) //' counts in '// st7(2) //' seconds. '
	rate = counts(1)/time
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT (rate)
	buffer = buffer(1:32) //'===> RATE =  '// st7(1) //' cps '
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(rate*60.)
	buffer = buffer(1:57) //' or '// st7(1) //' cpm.'
	CALL Putline_NT (contr, buffer)
	IF (rate .LT. 5) THEN
	   buffer = '*** WARNING: Rate is very low!'
	   CALL Putline_NT (contr, buffer)
	ENDIF

 288	CALL Read_STRING(keyb, str9,
	1    'Is the monitor rate OK (Yes/No)? ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft


	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   goto 288
	ENDIF

	IF (str9(1:2) .EQ. 'NO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 250
	ELSEIF (str9(1:3) .NE. 'YES') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 288
	ENDIF

C       Do FPD 1,4,1,.1,-1
	callid = 1
	mot = 4
	range = 1.
	step = .1
	mon = -1
	type = -1		!detector 1
c	CALL Find_Peak (callid, mot, range, step, mon, keep,type,fitmin,fitinten,
c       1		montemp, twotheta,arch)
	CALL Find_Peak (callid, mot, range, step, mon, .FALSE.,type,
	1    .FALSE.,.FALSE.,.FALSE., twotheta, .FALSE., .FALSE.)
	IF ( READEF(contr_a) ) GOTO 800
 308	CALL Read_STRING(keyb, str9,
	1    'Are you satisfied with the find_peak results? (Yes/No)? ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft
	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   goto 308
	ENDIF



	IF (str9(1:2) .EQ. 'NO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
C       GOTO 250 !go back to mrat1
! IF MONITOR RATE'S OK, BUT THE FIT IS BAD/PEAK NONEXISTENT,
! GET SOME PROFESSIONAL HELP!
	   GOTO 900
	ELSEIF (str9(1:3) .NE. 'YES') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 308
	ENDIF


	CALL Iang (4, 0.0)

C       mrat/s
	time = 20.
	CALL CLREF(contr_a)
	WRITE (UNIT= st7(1)(1:5), FMT= '(i2)',iostat=ios) NINT(time)
	buffer = 'Measuring for '// st7(1)(1:2) //' seconds.  Please wait !'
	CALL CnTime (time)
	t_count = .FALSE.
	CALL Putline_NT (contr, buffer)
	CALL Scaler (counts)
	IF ( READEF(contr_a) ) GOTO 800

	WRITE (UNIT= st7(2), FMT= '(i5)',iostat=ios) NINT(time)
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) counts(1)
	buffer = st7(1)(1:7) //' counts in '// st7(2) //' seconds. '
	rate = counts(1)/time
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT (rate)
	buffer = buffer(1:32) //'===> RATE =  '// st7(1) //' cps '
	WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(rate*60.)
	buffer = buffer(1:57) //' or '// st7(1) //' cpm.'
	CALL Putline_NT (contr, buffer)
	IF (rate .LT. 5) THEN
	   buffer = '*** WARNING: Rate is very low! Mrat values not saved.'
	   CALL Putline_NT (contr, buffer)
	ELSE
	   CALL Save_MRAT(rate)
	END IF

	CALL Putcontr_NT('Waiting for you to:')
	CALL Putcontr_NT('  Close beam')
	CALL Putcontr_NT('  Replace beamstop')
 338	CALL Read_STRING(keyb, str9,
	1    'Type GO to continue: ',
	2    9,i1,i1,i1, i,i1, contr) !i1 is dummy param for absoft

	IF (i. eq. 0)THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   goto 338
	ENDIF

	IF (str9(1:2).NE.'GO') THEN
	   IF ( READEF(contr_a) ) GOTO 800
	   GOTO 338
	ENDIF

	CALL Putcontr_NT(' ') 
	CALL Putcontr_NT('NewMono procedure completed normally.') 

	errsig = .false.
	return
 699	call putcontr_nt(' Error opening file '//MOVE4_CFG)
	close(unit=1)
	return


 700	call putcontr_nt(' Error reading info from '//mot4file)
	close(unit=1)
	return

 750	call putcontr_nt(' Error reading info from '//transfile)
	close(unit=1)
	return
	
 800	CALL Putcontr_NT('Operation aborted by user')
	return

 900	CALL Putcontr_NT('*******************************************')
        CALL Putcontr_NT('At this point we are in dangerous territory')
        CALL Putcontr_NT('Please seek assistance from responsible staff')
	CALL Putcontr_NT('before things get worse')
	return
	end
