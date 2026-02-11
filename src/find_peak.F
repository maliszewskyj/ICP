#include "icp.h"
	SUBROUTINE Find_Peak (callid,mot, range, delta, mon, keep, type, 
     1		   fitmin, fitinten, montemp, twotheta, arch, halfh)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine scans the given motor around the current angle within range   C
C	and tries to find a peak. If a peak is found, we prompt for fitting   C
C	or whatelse to do.                                                    C
C	type = 1 searches one angle 
C	type = 2, does a 2:1 scan                
C	type = 3 is for monitor peak                                          C
C	type = 4 is for special xray scan. Twotheta only used for fpx         C
C	type = 5 is for RM scan
C	type = 6 is for RA scan
C	type = 7 searches one angle with sigmoidal curve 
C       type = -x is for scan at detector x (FPD and FPL)		      C
C	Called from: Parse_Align                                              C
C                                                                             C
C	Beat Schmid, March 1990  (based on Ross Erwin)                        C
C       Modified by Gayle Greene					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none
	INTEGER*4    mot
	INTEGER*4    type, ierr
	INTEGER*4	callid
        REAL*4       range, delta, mon
	REAL*4       int, RdAm, RdAngl, zm, Fz
	REAL*4	     twotheta,  callpos(2), fpos(2)
	Logical fitmin,fitinten,keep, montemp,arch, fitok, halfh

C  Memorize current position of motors in callpos in case no peak found
	IF (type.ne.4)THEN !all but special xray
	  RdAm = RdAngl(mot)
	  zm = Fz(mot,RdAm)
	  callpos(2) = RdAm - zm        ! typically A4 in 2:1, A3 else
	ELSE !special xray
	  callpos(2) = twotheta/2.
	ENDIF
	IF (type .EQ. 2 .or. type.eq.4) THEN   ! we have a 2:1 scan here or special xray
	   IF (type.eq.2)THEN !2:1 scan
		RdAm = RdAngl(mot-1)
		zm = Fz(mot-1,RdAm)
	        callpos(1) = RdAm - zm
	   ELSE		      !special xray scan
		callpos(1) = twotheta/2.
	   ENDIF
	ENDIF

C  Go do scan and fit peak if possible
	CALL GoFind_Peak (callid, mot, range, delta, mon, keep, type, 
     1		   fitmin, fitinten, montemp, twotheta, arch, callpos, 
     1		   ierr, fitok, fpos, halfh)

	RETURN
	END

