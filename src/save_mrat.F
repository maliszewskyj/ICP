#include "icp.h"
	SUBROUTINE Save_MRAT (cps)
C	****************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     At call, cps has the counts/second just determined by mrat              C
C     Memorize this value as well as current dspacing and current position    C
C     of monochromator. Also ask user if howlong should use overhead for each C
C     motor moved and/or a constant overhead for each point. If constant,     C
C     ask user for value. If overhead for each motor, read in values from     C
C     file and put into table hwlmwait. This all will be used in howlong.     C
C                                                                             C
C	Gayle Greene, Mar, 1996                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use hwlong
	use smg_prep
	IMPLICIT     none

C  Calling parameters
	REAL*4		cps !counts/second
C  External function
	REAL*4		fz, RdAngl
C  Local variables	
	REAL*4		zm, data, tmpdm, c1, c2, c3, c4
	INTEGER*4	m, ios, ig, idum
	CHARACTER*2     mstr
	CHARACTER*20    str20
	CHARACTER*1	label
	CHARACTER*80    GETHOME

C  decide motor to memorize according to instr.
	IF (nsta .eq. 2 .or. nsta .eq. 4 .or. nsta .eq. 9 .or.
     1		nsta .eq. -5)THEN
	  m = 2
	ELSEIF (nsta .eq. 1 .or. nsta .eq. 7)THEN
	  m = 0 !no motor involved
	ELSE
	  CALL Putcontr_NT(' Invalid command for this instrument.')
	  CALL Putcontr_NT(' Cannot determine monochromator motor.')
	  RETURN
	ENDIF

	cntrate = cps !memorize counts per second
	dlast = dm !memorize monochromator dspacing

	cnstovr = .FALSE.
	CALL Putcontr_NF('Howlong will add a constant overhead per point')
	CALL Putcontr_NF(' in addition to holds & waits defined in buffer.')
 10	CALL READ_STRING(keyb, str20,
     1		'Enter constant overhead per point in secs: (default = 1)',
     1		 20,idum,idum,idum, ig,idum, contr) !idum is dummy for absoft
	IF (ig .eq. 0)THEN
	  hwlconst = 1.
	  cnstovr = .TRUE.
	ELSE
	  READ(unit=str20,fmt=*,err= 10)hwlconst
	  IF (hwlconst .NE. 0.)cnstovr = .TRUE.
	ENDIF

	IF (nsta .EQ. 1 .or. nsta .eq. 7)THEN
	  monang = 0.
	  c1 = 0.
	  c2 = 0.
	  c3 = 0.
	  c4 = 0.
	ELSE
	  IF (motfl(m)) THEN
	    data = RdAngl(m)
 	    IF (nsta .EQ. 4 .AND. z(m) .EQ. -99.)THEN !special flag
		   zm = fz(m,data)
	    ELSE
		   zm = z(m)
	    ENDIF
	    monang = data - zm
	    OPEN (UNIT= 1, FILE= HWCOEFFS_CFG, STATUS= 'OLD', ACCESS= 
	1     'SEQUENTIAL', FORM= 'FORMATTED', ERR= 999)
	    READ (1,fmt='(a)',err=888)label
	    tmpdm = -537. !pick wierd value to insure no match
	    DO WHILE (abs(tmpdm - dlast) .GT. .001) 
	      READ (1, FMT= *,err=888,end=880) tmpdm, c1, c2, c3, c4
	    ENDDO 
	    CLOSE(1)
	  ELSE
	    write(unit=mstr,fmt='(I2)',iostat=ios)m
	    CALL Putcontr_NT(' Cannot determine current position of motor '//mstr)
	    RETURN
	  ENDIF
	ENDIF

	  chw1 = c1
	  chw2 = c2
	  chw3 = c3
          chw4 = c4

	  OPEN (UNIT= 1, FILE= HWLONG_BUF, STATUS= 'OLD',RECL=MOTBUF_RECL,
	1        ACCESS='DIRECT',FORM= 'UNFORMATTED', err=777)
	  WRITE(1,REC=1,err=666)dlast,monang,cntrate,cnstovr,hwlconst,
     1		chw1,chw2,chw3,chw4
	  CLOSE (1)


c	now ask user if overheads are to be added for each motor moving
C	if so,
c	  motovr = true
c	  zero hwlmwait table then read in mot,wait list from file to table
c	else
C	  motovr = false
c       endi
	
	RETURN

 666	close(1)
 777	CALL Putcontr_NT(' Cannot save mrat data to file')
	RETURN

 880	CALL Putcontr_NT
     1	  (' Error: No calibration for current dspacing found in hwcoeffs.cfg')
 888	close(1)
 999	CALL Putcontr_NT(' Cannot input coeffs for monochromator')
	RETURN

	END
