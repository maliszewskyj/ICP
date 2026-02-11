#include "icp.h"
	include COMPILE_OPTS

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

	IMPLICIT     none

C  Calling parameters
	REAL*4		cps !counts/second
C  External function
	REAL*4		fz, RdAngl
C  Local variables	
	REAL*4		zm, data, tmpdm, c1, c2, c3, c4
	INTEGER*4	m, ios, ig
	CHARACTER*2     mstr
	CHARACTER*20    str20
	CHARACTER*1	label

	INCLUDE INSTR_CMN     ! Instrument and hardware parameters
	INCLUDE 'hwlong.cmn'
	INCLUDE SMGPREP_CMN !for contr and keyb

	cntrate = 1. !memorize counts per second
	dlast = 3. !memorize monochromator dspacing

	cnstovr = .TRUE.
	  hwlconst = 1.

	   monang = 90.

	  chw1 = 1
	  chw2 = 1
	  chw3 = 1
          chw4 = 1

	  OPEN (UNIT= 1, FILE= HWLONG_BUF, STATUS= 'NEW', 
	1        ACCESS= 'DIRECT', FORM= 'UNFORMATTED',RECL=motbuf_recl)
	  WRITE(1,REC=1)dlast,monang,cntrate,cnstovr,hwlconst,
     1		chw1,chw2,chw3,chw4
	  CLOSE (1)


	END
