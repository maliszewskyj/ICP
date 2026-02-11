#include "icp.h"
	SUBROUTINE INIT_ASD
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, May 1996					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use asd
	IMPLICIT    none
	CHARACTER*30 outstr
	LOGICAL     errflg
	INTEGER*4   lstr
	INTEGER*2   ktype/0/ !determines whether a wait between chars is done
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)

C------- Set ASD to listener                          ----------------C
	Call Gpib_listen(asd_addr,errflg)

C------- Set up string in format for ASD -----------------------------C
	outstr(1:)='ct(0)'//cr
	lstr=8
	Call ASD_WrPol(outstr,lstr,errflg,ktype)

	Call Gpib_unlisten(errflg)

	RETURN
	END
