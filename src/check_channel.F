#include "icp.h"
	SUBROUTINE Check_Channel (btyp,mtyp,mon,prf,pts,fname,fenergy,ip,hstr)
C	****************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to check whether the channel is open. If no, print warning      C
C     message, if yes, and calling buffer is increment then estimate and      C
C     print execution time.                                                   C
C     Called from: Run_I_Buf, Run_Q_Buf, Run_Trash                            C
C     Note: ip has 1 or number of polarized configurations
C                                                                             C
C     Beat Schmid, October 1989                                               C
C     Modified by Gayle Greene several times
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use asd
	use tc
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_debugval.h"
#include "c_libwait.h"
	end interface
	REAL*4       time,  mon, prf, pts, params(6), hlevel
	INTEGER*4    btyp, counts(37), maxtry,
     1			notries/10/,openfai/30/,ios
	INTEGER*4    status,ip
	CHARACTER*4  mtyp
	CHARACTER*5  out
	CHARACTER*7  hstr 
	CHARACTER*8  str8
	CHARACTER*80 response
	CHARACTER*12 fname
	CHARACTER*28 sys_time
	LOGICAL errflag, alarm, comerr, shutdone, hldasd, Readef
	REAL*4  fenergy,countr
	CHARACTER*80 gethome

	if (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5))call upd_helium

	hldasd = use_asd
	use_asd = .FALSE. !do not want to count on asd while checking channel
	time = 5.
	CALL Reset_Display
	CALL CnTime (time)
	t_count = .FALSE.
	CALL Scaler (counts)
	use_asd = hldasd
	IF (READEF(contr_A))RETURN
	countr = counts(1)/time

C  add message to monitor.log
           maxtry = 0
 	   OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
     1	    ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	     CALL libwait(.2,status)
 	     OPEN (UNIT=2, FILE= MONITOR_REC, FORM= 'FORMATTED', 
     1	      ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
	      maxtry = maxtry+1
	   ENDDO
	   IF (ios .NE. 0)GOTO 210 !err opening file
	   CALL DATE_TIME (sys_time)
	   IF (nsta .EQ. 1)THEN
	     IF (collim(1).eq.7)THEN
	         str8(1:)=monobt1//'_7'
	     ELSEIF (collim(1).eq.15)THEN
	         str8(1:)=monobt1//'_15'
	     ENDIF
	     write(unit=2,fmt='(1x,a17,1x,f10.0,1x,f10.2,1x,a,1x,a,2x,a8)',err=209)
     1	       sys_time(1:17),
     1	       countr,fenergy,'meV','  Start of Run',str8
	   ELSE !not BT1
	     write(unit=2,fmt='(1x,a17,1x,f10.0,1x,f10.2,1x,a,1x,a)',err=209)
     1	       sys_time(1:17),
     1	       countr,fenergy,'meV','  Start of Run'
	   ENDIF
 209	   close(unit=2,err=210)

C  check count rate to see if beam open
 210	IF (countr .LT. 5) THEN
	   response = 
     1	   '**** WARNING ****  Monitor rate < 5!!  Check if beam tube is open!'
	  Call Contr_ring_bell(3)
	  CALL PUTCONTR_NT (response)      
	  CALL PUTCONTR_NF (' ')
	ENDIF
	IF (btyp .EQ. 2) THEN
	   goto 800
	ELSE 
	   IF (mtyp .EQ. 'NEUT') THEN
	      IF (countr .eq. 0)GOTO 800 !avoid division by zero
	      time = mon * prf * pts / countr / 3600.
	   ELSE
	      time = mon * prf * pts / 3600.
	   END IF
	   WRITE (UNIT= out(1:5), FMT= '(f5.1)') time * ip !ip for polarized
	   response = 'Estimated duration of that buffer:  '// 
     1		out(1:5) //' hrs.'
	END IF
	CALL PUTCONTR_NT (response)      
	CALL PUTCONTR_NF (' ')
 800	continue !jump here if Btyp eq 2
	if (tempctr)then
	  call checktemp(errflag)
	  if (errflag)then
	    response = '**** WARNING **** Tflag is set and cannot read temperature!***'
	    Call contr_ring_bell(3)
	    CALL PUTCONTR_NT (response)      
	    power=0
	  else
	    call libwait(.1,status)
	    call rdpar (params)
	    power = params(4)
	  endif
	  IF (temptype .EQ. 13)THEN
	    CALL Lia_file(fname,errflag)
	  ELSEIF (temptype .EQ. 12)THEN
	    CALL Tdb_file(fname,errflag)
	  ELSEIF (temptype .EQ. 14)THEN
	    CALL AD201_file(fname,errflag)
	  ENDIF
	endif
	if (magnctr .AND. (magtype .EQ. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5))then !sc magnet
	  hlevel = -999.
	  call check_helium(alarm,hlevel,comerr,shutdone)
 	  IF (shutdone) THEN
	    Call Contr_Ring_Bell(3)
	    hstr(1:)='Shutoff'
	  ELSEIF (comerr)THEN !communication problem
	    hstr(1:)='???????'
	  ELSE
	    CALL Putmax(hlevel,hstr,7,7)
	  ENDIF
	endif
	RETURN
	END
