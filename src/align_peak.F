#include "icp.h"
	SUBROUTINE Align_Peak (mot, range, delta, mon, keep, type, tol, ierr,
     1		montemp,fitmin,fitinten,twotheta,arch, halfh)

C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine executes find_peak and if peak found resets angle zeros	      C
C	type = 1 searches one angle
C	type = 2, does a 2:1 scan                C
C	type = 3 is for monitor peak                                          C
C	type = 7 searches one angle with sigmoidal curve 
C	type < 0 is for scaler number or PSD channel number                   C
C	Called from: Parse_Contr    Align_Xtal				      C
C                                                                             C
C	Ross Erwin,  June 1992						      C
C	Modified by Gayle Greene
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use globals
	IMPLICIT     none
	INTEGER*4    type, mot, ierr
	LOGICAL	     montemp,	!monitor temp if set to true
	1    fitmin,		!fit to minimum if set to true
	2    fitinten,		!fit to intensity over part of array
	3    arch,		!archive data file
	4    halfh		!drive to halfheight of max intensity		

	real*4   twotheta

        REAL*4       range, delta, mon, callpos(2), wl
	REAL*4       RdAngl,  tol, znew, zm, Fz, RdAm, fpos(2)
	REAL*4       SIND
	Logical      keep, fitok
	INTEGER*4	callid/3/, ios
	CHARACTER*20    str20, str20b

c       parameters for calulating wavel
	real*8  c,h,mn,conv
	PARAMETER  (h  = 6.6260755) ! [J·s]
	PARAMETER  (mn = 1.6749286) ! [kg]
	PARAMETER  (conv = 1.60217733) ! [J/eV]

C----------------verify not trying to scan BT4 motor 2 with linear zero-------C
	IF (nsta.eq.4 .and. mot.EQ.2 .AND. z(2).eq.-99.)THEN !special flag
	   CALL Putcontr_NT (' Error. AP cannot scan motor with linear zero.')
	   ierr = 3
	   return
	ENDIF

	CALL PutContr_NT ('Align_peak:')

C       Memorize position at start of command execution in callpos
	IF (type.ne.4)THEN	!all but special xray
	   RdAm = RdAngl(mot)
	   zm = Fz(mot,RdAm)
	   callpos(2) = RdAm - zm ! typically A4 in 2:1, A3 else
	ELSE			!special xray
	   callpos(2) = twotheta/2.
	ENDIF
	IF (type .EQ. 2 .or. type.eq.4) THEN ! we have a 2:1 scan here or special xray
	   IF (type.eq.2)THEN	!2:1 scan
	      RdAm = RdAngl(mot-1)
	      zm = Fz(mot-1,RdAm)
	      callpos(1) = RdAm - zm
	   ELSE			!special xray scan
	      callpos(1) = twotheta/2.
	   ENDIF
	ENDIF


C       Go do scan and fit peak if possible
	CALL GoFind_Peak (callid, mot, range, delta, mon, keep, type, 
	1    fitmin, fitinten, montemp, twotheta,arch, callpos,
	2    ierr, fitok, fpos, halfh)
	IF ( ierr .EQ. 0 ) THEN
	   IF (type .ne. 2)THEN !do not reset zeros for apt
	      znew = RdAngl(mot) - callpos(2)
	      IF ( ABS(znew-z(mot)) .GT. tol ) THEN
		 z(mot) = znew
		 if (trackmoves) call memcurpos(mot,0,znew)
		 IF (type .EQ. 2) THEN
		    z(mot-1) = RdAngl(mot-1) - callpos(1)
		    if (trackmoves) call memcurpos(mot-1,0,z(mot-1))
		 END IF
		 CALL PutContr_NT ('New zeros determined.')
	      ELSE
		 CALL PutContr_NT ('Zeros within tolerance.')
	      END IF
	   ELSE			!type eq 2
	      IF (fitok)THEN	!fitted position within scanned range
		 IF (lastef .NE. 0.) THEN
		    c = 5.* h**2 / (mn * conv)
		    wl = SQRT (c/lastef)
		 ELSE		!no defined lastef
		    IF (wavel .ge. .5 .and. wavel .le. 10)THEN
		       wl = wavel
		    ELSE	!no valid wavelength		
		       vg(3) = fpos(1)
		       vg(4) = fpos(2)
		       CALL PutContr_NT ('Wavel undefined. VLAT not changed.')
		       RETURN
		    ENDIF
		 ENDIF
		 vg(3) = fpos(1)
		 vg(4) = fpos(2)
		 vlat = wl/(2. * SIND(vg(4)/2.)) * tol
		 if (nsta.ne.1 .and. nsta .ne. -7)wavel = wl		
		 write(unit=str20,fmt=*,iostat=ios)vlat
		 write(unit=str20b,fmt=*,iostat=ios)wl
		 call putcontr(' Vlat set to '//str20//
	1	      ' for wavelength of '//
	2	      str20b//' A')
	      ELSE		!problem with fitted position
		 vg(3) = callpos(1)
		 vg(4) = callpos(2)
		 CALL PutContr_NT ('V3, V4, VLAT set to defaults')
	      ENDIF
	   ENDIF		!type ne 2
	ELSE			!ierr ne 0
	   IF (type .EQ. 2)THEN
	      vg(3) = callpos(1)
	      vg(4) = callpos(2)
	      CALL PutContr_NT ('V3, V4, VLAT set to defaults')
	   ELSE
	      CALL PutContr_NT ('Unable to determine new zeros.')
	   ENDIF
	END IF
	RETURN
	END
