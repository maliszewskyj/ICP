#include "icp.h"
	SUBROUTINE Parse_Align (command, errflag, lastchar)
C	***********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Checks Spelling And Syntax Of The Align Commands. If Ok,        C
C     Executes  Command And Returns to Parse_Contr.			      C
C                                                                             C
C     Gayle Greene, 1997, based on R. Erwin & B. Schmidt                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use asd
	use tc
	! use xray
	IMPLICIT      none

C  Calling Parameters
	character*(*)	command
	logical		errflag
	integer*4	lastchar !at call has iend, at return has string shift position
C  Local Variables
	logical		keep, fitinten, fitmin, montemp, arch, halfh
	integer*4	m, ios, np, mot, iend, i,  type, ig, maxit
	real*4		multi(6), range, step,  mon, twotheta, toler
	character*80    response
	logical		hldasd, Readef
	character*80    gethome

	real*4	        oldz(MAXMOTS)


	errflag = .TRUE. !default is an error return. All early returns are err
	iend = lastchar
	lastchar = 2 !default

C  Which command and which options?
	keep = .TRUE.  !set up defaults
	fitinten = .FALSE.
	fitmin = .FALSE.
	montemp = .FALSE.
	arch = .TRUE.
	halfh = .FALSE.
	IF (command(2:3) .EQ. 'PT') THEN                    ! Find peak 2:1
	   IF (nsta.eq.8) Call Not_Implemented (*520) 
 	   m = 4
	   np = 5
	   type = 2
	ELSEIF (command(2:3) .EQ. 'PX') THEN       ! Find peak: polymers xray
	   IF (nsta .NE. 0)CALL Not_implemented(*520)
	   m = 4
	   np = 5
	   type = 4
	ELSEIF (command(2:2) .EQ. 'M') THEN                     ! Find Monitor
	   IF (nsta .eq. 0) CALL NoMon (*520)
	   m = 3
	   np = 5
	   type = 3
	ELSEIF (command(2:3) .EQ. 'PD') THEN          ! Find Peak at Detector
	   m = 4
	   np = 6
	ELSEIF (command(2:3) .EQ. 'PS') THEN          ! Find Peak - sigmoidal curve
	   IF (nsta .NE. 0)CALL Not_implemented(*520)
           m = 4
	   np = 5
	   type = 7
	ELSEIF (command(2:2) .EQ. 'P') THEN                        ! Find Peak
           m = 3
	   np = 5
	   type = 1
 	ELSE IF (command(2:2) .EQ. 'S' .OR. command(2:2) .EQ. 'A') THEN ! Find Monitor
	   m = 3
	   np = 6
	ENDIF !which command
	DO WHILE (command(m:m) .EQ. '/')
	  IF (command(m+1:m+1).eq.'F')then !make file
	     keep=.TRUE.
	     m = m+2
	  ELSEIF (command(m+1:m+1).eq.'A')then !archive file
	     arch=.TRUE.
	     keep=.TRUE.
	     m = m+2
	  ELSEIF (command(m+1:m+1).eq.'M')then !fit to minimum
	     fitmin = .TRUE.
	     m = m+2
	  ELSEIF (command(m+1:m+1) .EQ. 'I')THEN !fit to intensity
	     IF (.NOT. PSD)THEN
		CALL Putcontr_NT('PSD flag L must be set for AP/I')
		RETURN
	     ENDIF
	     fitinten = .TRUE.
	     m = m+2
	  ELSEIF (command(m+1:m+2) .EQ. 'RM')THEN !fit to flipper ratio
	     IF (.NOT. polarized) THEN
	       CALL Putcontr_NT('Polarization flag P must be set for FP/R')
	       RETURN
	     ENDIF
	     type = 5
	     m = m+3
	  ELSEIF (command(m+1:m+2) .EQ. 'RA')THEN !fit to flipper ratio
	     IF (.NOT. polarized) THEN
	       CALL Putcontr_NT('Polarization flag P must be set for FP/R')
	       RETURN
	     ENDIF
	     type = 6
	     m = m+3
	  ELSEIF (command(m+1:m+1) .EQ. 'T')THEN !monitor temp
	     IF (tinit_done .AND. tempctr)THEN
	       montemp = .TRUE.
	     ELSE
	       CALL Putcontr_NT(' Warning: Use tdev before temp can be monitored')
	     ENDIF
	     m = m+2
	  ELSEIF (command(m+1:m+1).eq.'H')then !half height
	     if (nsta .eq. -7)halfh = .TRUE. !only for ng7 now
	     m = m+2
	  ELSE
	     return !syntax error
	  ENDIF
	ENDDO

C  Get calling paramters into multi(6) and number of params into np
	range = 1.  !set up defaults
	step = 0.1
	mon = -1.
	toler = 0.01
	maxit = 5
	ios = 1 !ios of 0 indicates all params read successfully
	DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= command(m:iend), FMT= *, IOSTAT= ios) (multi(i), i= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1 !np has # params read in at exit
	END DO

C  Interpret params for each command
	twotheta = 0. !default for twotheta
	IF (command(2:3) .EQ. 'PT') THEN                    ! Find peak 2:1
	   toler = 1. !default constant
	   IF (np. EQ. 0) THEN
	     RETURN !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
	   IF (np .GT. 1) range = multi(2)				  ! (2) is angle range
	   IF (np .GT. 2) step = multi(3)				  ! (3) step width
	   IF (np .GT. 3) mon   = multi(4)				  ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)	! (5) is optional constant
	   IF (MOD(mot,2) .NE. 0) THEN
	      response = 'Invalid motor specificaton'
	      CALL Putcontr_NT (response)
	      RETURN
	   ENDIF
	ELSE IF (command(2:3) .EQ. 'PX') THEN                    ! Find peak: polymers xray
	   IF (np. EQ. 0) THEN
	     RETURN !error. must specify two-theta at least.
	   ELSE
	     twotheta = multi(1)
	   ENDIF
	   IF (np .GT. 1) range = multi(2)	! (2) is angle range
	   IF (np .GT. 2)step = multi(3)	! (3) step width
	   IF (np .GT. 3)mon   = multi(4)       ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)	! (5) is tolerance
	   mot = 4
	   IF (.NOT. motfl(4) .OR. .NOT. motfl(3)) THEN
	      response = 'That motor is not available on this instrument!'
	      CALL Putcontr_NT (response)
	      RETURN
	   ENDIF
	ELSE IF (command(2:2) .EQ. 'M') THEN                     ! Find Monitor
	   IF (np .EQ. 0) THEN
	     RETURN !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))	  		  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1) range = multi(2)		  ! (2) is angle range
	   IF (np .GT. 2) step = multi(3)		  ! (3) step width
           IF (np .GT. 3) mon   = multi(4)		  ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)	! (5) is tolerance
	ELSE IF (command(2:3) .EQ. 'PD') THEN              ! Find Peak at Detector
	   IF (np .LT. 2) THEN
	     RETURN !error. must specify motor & detector at least.
	   ELSE
	     ig    = IFIX (multi(1))			  ! (1) is detector no
	     mot   = IFIX (multi(2))			  ! (2) is motor number
	   ENDIF
	   IF (nsta .eq. 1) THEN
	     IF (ig .gt. scl .or. ig .lt. 1)RETURN !error in scaler number
	   ELSEIF (.not. PSD) THEN 
	     IF (ig .gt. 4 .or. ig .lt. 1)RETURN !error in scaler number
	   ENDIF
           IF (np .GT. 2) range = multi(3)	  ! (3) is angle range
	   IF (np .GT. 3) step = multi(4)	  ! (4) step width
           IF (np .GT. 4) mon   = multi(5)	  ! (5) is monitor
           IF (np .GT. 5) toler   = multi(6)	! (6) is toler
	   type = -ig !to signal type is a detector/scaler number
	ELSE IF (command(2:3) .EQ. 'PS') THEN   ! Find Peak - sigmoidal curve
	   fitmin = .TRUE. !make implicit in command
	   IF (np .EQ. 0)  THEN
	      RETURN !error. must specify motor at least.
	   ELSE
	      mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1)  range = multi(2)		  ! (2) is angle range
	   IF (np .GT. 2)  step = multi(3)		  ! (3) step width
           IF (np .GT. 3)  mon   = multi(4)		  ! (4) is monitor
	   IF (np .GT. 4)  toler = multi(5)	! (5) is tolerance
	ELSE IF (command(2:2) .EQ. 'P') THEN                        ! Find Peak
	   IF (np .EQ. 0) THEN
	     RETURN !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1) range = multi(2)		  ! (2) is angle range
	   IF (np .GT. 2) step = multi(3)		  ! (3) step width
           IF (np .GT. 3) mon   = multi(4)		  ! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)	! (5) is tolerance
 	ELSE IF (command(2:2) .EQ. 'S' .OR. command(2:2) .EQ. 'A') THEN ! Find Monitor
	   IF (np .EQ. 0) THEN
	     RETURN !error. must specify motor at least.
	   ELSE
	     mot   = IFIX (multi(1))			  ! (1) is motor number
	   ENDIF
           IF (np .GT. 1) range = multi(2)	! (2) is angle range
	   IF (np .GT. 2) step  = multi(3)	! (3) step width
           IF (np .GT. 3) mon   = multi(4)	! (4) is monitor
	   IF (np .GT. 4) toler = multi(5)	! (5) is tolerance
	   IF (np .GT. 5) maxit = IFIX (multi(6))	!(6) is maxit
	   IF ( maxit .LT. 1 )    maxit = 1
	   IF ( command(2:2) .EQ. 'S' .AND. (mot .NE. 3 .AND. mot .NE. 4) ) THEN
	      response = 'Sample alignment must start with motor 3 or 4!'
	      CALL Putcontr_NT (response)
	      RETURN !error
	   END IF
	   IF ( command(2:2) .EQ. 'A' .AND. (mot .NE. 5 .AND. mot .NE. 6) ) THEN
	      response = 'Analyzer alignment must start with motor 5 or 6!'
	      CALL Putcontr_NT (response)
	      RETURN !error
	   END IF
	ENDIF

	lastchar = m
        IF (nsta .eq. 0 .and. mon .ge. 0) CALL NoMon (*520)
        IF (mot.lt.1 .or. mot.gt.MAXMOTS)RETURN !error
	IF (mottype(mot) .EQ. VIRT)THEN !not allowed with virtual motors
	  response = 'Virtual motors not allowed with this command'
	  CALL Putcontr_NT (response)
	  RETURN
	ENDIF
        IF (.NOT. motfl(mot) ) THEN
	  response = 'That motor is not available on this instrument!'
	  CALL Putcontr_NT (response)
	  RETURN
	ENDIF
	IF (nsta .eq. 1 .AND. .not. super .and. mot .ne. 3 .and. 
     1		mot .ne. 4) CALL NoSuper (*520)
	IF (nsta .eq. -11 .AND. .not. super .and. .not.
	1    ((mot .ge. 1 .and. mot .le. 8) .or.
	1     (mot .ge. 13 .and. mot .le. 16))) CALL NoSuper (*520)
	IF ( range .LT. .005 ) range = 1.  !check params
	IF ( abs(mon) .LT. .5 ) mon = sign(1.,mon)
	IF ( toler .LT. .005 .AND. type .NE. 2) toler = .01
C  read in z. Call align-peak. Write out new z.
	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	CLOSE (1)

	if (trackmoves) then
	  do i = 1,MAXMOTS
	    oldz(i) = z(i)
	  enddo
	endif

	CALL clref (Contr_A)
 	IF (command(2:2) .EQ. 'S' .OR. command(2:2) .EQ. 'A') THEN ! Find Monitor
	        CALL Align_Xtal (mot, range, step, mon, keep, maxit, 
     1		  toler, i, montemp, fitmin,fitinten,arch,halfh)
	ELSE !all other align types
	  IF (command(2:2) .EQ. 'M') THEN                     ! Find Monitor
	    hldasd = use_asd
	    use_asd = .FALSE. !disable counting on asd for fm
	  ENDIF
	  CALL Align_Peak (mot, range, step, mon, keep, type, 
     1		  toler, i, montemp, fitmin,fitinten,twotheta,arch,halfh)
	  IF (command(2:2) .EQ. 'M') use_asd = hldasd
	ENDIF
	IF (READEF(contr_A)) CALL IcpAbort(command, *520)

	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	 ACCESS= 'DIRECT',RECL=MOTBUF_RECL, FORM= 'UNFORMATTED')
	WRITE (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS) 
	CLOSE (1)
	if (trackmoves) then
	  do i = 1,MAXMOTS
	    if (oldz(i) .ne. z(i)) call memcurpos(i,0,z(i))
	  enddo
	endif !trackmoves
 520	errflag = .FALSE.
	RETURN
	END
