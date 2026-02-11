#include "icp.h"
	SUBROUTINE Lia_Status(tofile,errflag)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, Sept 1997				              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use lia
	IMPLICIT    none
	interface
#include "c_libwait.h"
	end interface
	LOGICAL	    tofile,errflag

	CHARACTER*80  lrcommand,lrreply, response
	INTEGER*2     lreply,lcom
	LOGICAL	      getreply/.true./
	INTEGER*4     isrc,icpl,ignd,irsv,ilin,itim,islp,isnc,isns,ios
	character*10  input_config(0:3)/' A ',' A-B ','I (1Mohm)',
     1			'I (10Mohm)'/
	character*2   coupling(0:1)/'AC','DC'/
	character*6   shield_gnd(0:1)/'float','ground'/
	character*9   reserve(0:2)/'high','normal','low noise'/
	character*10  notch(0:3)/' none ',' 1X line ',' 2X line',
     1			'1X,2X line'/
	character*6   time_cnst(0:19)/'10 us','30 us','100 us','300 us',
     1			'1 ms','3 ms','10 ms','30 ms','100 ms','300 ms',
     2			'1 s','3 s','10 s','30 s','100 s','300 s',
     3			'1 ks','3 ks','10 ks','30 ks'/
	character*9   ofsl(0:3)/'6 dB/oct','12 dB/oct','18 dB/oct',
     1			'24 dB/oct'/
	character*11  sync(0:1)/'Off','On (<200Hz)'/
	character*9   senstvty(0:26)/'2 nV/fA','5 nV/fA','10 nV/fA',
     1			'20 nV/fA','50 nV/fA','100 nV/fA','200 nV/fA',
     2                	'500 nV/fA','1 uV/pA','2 uV/pA','5 uV/pA',
     3                  '10 uV/pA','20 uV/pA','50 uV/pA','100 uV/pA',
     4			'200 uV/pA','500 uV/pA','1 mV/nA','2 mV/nA',
     5			'5 mV/nA','10 mV/nA','20 mV/nA','50 mV/nA',
     6			'100 mV/nA','200 mV/nA','500 mV/nA','1 V/uA'/
	CHARACTER*20 str20
	INTEGER*4	lia_of1,lia_of2,lia_oa, status
	REAL*4		freq, phase, slvl

	errflag = .FALSE.

	IF (tofile)THEN
	  OPEN (UNIT=2, FILE= liafile, FORM= 'FORMATTED',
	1	ACCESS= 'APPEND',STATUS= 'OLD', iostat=ios)
	  IF (ios .NE. 0)THEN
	    CALL Putcontr_NT(' err opening file: '//liafile)
	    tofile = .FALSE.
	  ENDIF
	ENDIF


	lrcommand='ISRC?'  !input configuration
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	  CALL Putcontr_NT(' no reply from LIA for input configuration')
	else
	  read (lrreply(1:lreply),fmt=*,iostat=ios)isrc
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (isrc .GT. 3 .or. isrc .LT. 0)))THEN
	    CALL Putcontr_NT(' error decoding input configuration value')
	  ELSE
	    response(1:) = 'Input configuration: '//input_config(isrc)
	    if (tofile)THEN
	      write(unit=2,fmt='(1x,a25)',iostat=ios)response(1:50)
	      CALL Putcontr_NF(response(1:50))
	    ELSE
	      CALL Putcontr_NT(response(1:50))
	    ENDIF
	  ENDIF
	endif

	call libwait(.1,status)
	lrcommand='ICPL?'  !Input Coupling
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Input Coupling')
	else
	  read (lrreply(1:lreply),fmt=*,iostat=ios)icpl
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (icpl .GT. 1 .or. icpl .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Input Coupling reply')
	    ELSE
	      response(1:) = 'Input Coupling: '//coupling(icpl)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='IGND?'  !Shield Grounding
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Shield Grounding')
	else
	  read (lrreply(1:lreply),fmt=*,iostat=ios)ignd
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (ignd .GT. 1 .or. ignd .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Shield Grounding reply')
	    ELSE
	      response(1:) = 'Shield Grounding: '//shield_gnd(ignd)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='RMOD?'  ! Reserve Mode
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Reserve Mode')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)irsv
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (irsv .GT. 2 .or. irsv .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Reserve Mode reply')
	    ELSE
	      response(1:) = 'Reserve Mode: '//reserve(irsv)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='ILIN?' !Input Line Notch Filter
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Notch Filter')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)ilin
	    IF (ios .NE. 0 .OR. 
     1         (ios .eq. 0 .and. (ilin .GT. 3 .or. ilin .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding notch filter reply')
	    ELSE
	      response(1:) = 'Input Line Notch Filter: '//notch(ilin)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a40)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='OFLT?'  ! Time Constant
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Time Constant')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)itim
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (itim .GT. 19 .or. itim .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Time Constant reply')
	    ELSE
	      response(1:) = 'Time Constant: '//time_cnst(itim)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='OFSL?'  ! low-pass filter slope
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Filter Slope')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)islp
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (islp .GT. 3 .or. islp .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Filter Slope reply')
	    ELSE
	      response(1:) = 'Filter Slope: '//ofsl(islp)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='SYNC?'  ! Synchronous Filter
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Synchronous Filter')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)isnc
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (isnc .GT. 1 .or. isnc .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Synchronous Filter reply')
	    ELSE
	      response(1:) = 'Synchronous Filter: '//sync(isnc)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='SENS?'  ! gain sensitivity
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Gain Sensitivity')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)isns
	  IF (ios .NE. 0 .OR. 
     1       (ios .eq. 0 .and. (isns .GT. 26 .or. isns .LT. 0)))THEN
	      CALL Putcontr_NT(' err decoding Gain Sensitivity reply')
	    ELSE
	      response(1:) = 'Sensitivity : '//senstvty(isns)
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a50)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='FREQ?' !reference frequency
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Reference Frequency')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)freq
	    IF (ios .NE. 0) THEN
	      CALL Putcontr_NT(' err decoding reference frequency')
	    ELSE
	      response(1:)='Reference frequency: '
	      write(unit=response(24:),fmt='(f10.3)',iostat=ios)freq
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a40)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='PHAS?' !reference phase
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Reference Phase')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)phase
	    IF (ios .NE. 0) THEN
	      CALL Putcontr_NT(' err decoding reference phase')
	    ELSE
	      response(1:)='Reference Phase: '
	      write(unit=response(24:),fmt='(f10.3)',iostat=ios)phase
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a40)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	call libwait(.1,status)
	lrcommand='SLVL?' !sine output level
	lcom=5
	call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	if (lreply.eq.0)then
	    CALL Putcontr_NT(' no reply from LIA for Sine Output Level')
	else
	    read (lrreply(1:lreply),fmt=*,iostat=ios)slvl
	    IF (ios .NE. 0) THEN
	      CALL Putcontr_NT(' err decoding sine output level')
	    ELSE
	      response(1:)='Sine Output Level: '
	      write(unit=response(24:),fmt='(f5.3)',iostat=ios)slvl
	      if (tofile)THEN
		write(unit=2,fmt='(1x,a40)',iostat=ios)response(1:50)
	        CALL Putcontr_NF(response(1:50))
	      ELSE
	        CALL Putcontr_NT(response(1:50))
	      ENDIF
	    ENDIF
	endif

	IF (tofile)close(unit=2)
	RETURN
	END
