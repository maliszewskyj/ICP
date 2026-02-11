#include "icp.h"
	include COMPILE_OPTS
C  DEASSIGN CHANNEL & REMOVE PRIVILEGES & stop
	SUBROUTINE RS232end 
C
	IMPLICIT NONE
c
	include '($prvdef)'
	include INSTR_CMN !for many variables
	include MOTORS_CMN
	include TC_CMN

	INTEGER*4 STATUS,SYS$DASSGN,i,ITYPE
	logical   type_used(maxtypes), no_motors
C
c  variables for removing phy_io (for set term) & sysprv (for assign) priv
	integer*4 priv_mask(0:1),isub,ipos,sys$setprv 
c
C
	do itype = 1,maxtypes
	  type_used(itype) = .FALSE.
	enddo

	no_motors = .TRUE.
	do i = 1,maxmots
	  IF (motfl(i))THEN
	    type_used(mottype(i))=.TRUE.
	    no_motors = .FALSE.
	  ENDIF
	enddo
	IF (nsta .eq. 0)THEN
	  type_used(XPROM) = .TRUE.
	  no_motors = .FALSE.
	ENDIF

	IF (.NOT. no_motors)THEN !some motors in use
	  DO itype = 1, maxtypes !set up port for each type controller
	    IF (type_used(itype))THEN !but only if type used
	      STATUS=SYS$DASSGN(%val(MCCHAN(itype)))
	      IF (.NOT.STATUS)call libsignal(status)
	    ENDIF
	  enddo
	ENDIF !some motors
C
	IF (tinit_done)THEN
	  STATUS=SYS$DASSGN(%val(tempCHAN))
	  IF (.NOT.STATUS)call libsignal(status)
	END IF
c
c  remove phy_io  & sysprv privileges
	isub = PRV$V_PHY_IO/32           ! Define wether high or low order
	ipos = MOD (PRV$V_PHY_IO,32)	 ! Define position in mask
	priv_mask(isub) = IBSET (priv_mask(isub),ipos)
	isub = PRV$V_SYSPRV/32           ! Define wether high or low order
	ipos = MOD (PRV$V_SYSPRV,32)	 ! Define position in mask
	priv_mask(isub) = IBSET (priv_mask(isub),ipos)
	status = SYS$SETPRV (%VAL(0),priv_mask,%VAL(1),)
	IF (.NOT. status) CALL LIBSIGNAL (status)
c	call setef(camerr)
c
	RETURN
	END
c
