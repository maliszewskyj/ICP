#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE RS232_INIT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c    Set up RS232 lines to characteristics required by motor		 C
c    controllers.		          				 C
c    Assign channels to these line after giving process		         C
c    neccessary privilege						 C
c									 C
C    Gayle Greene, 1989							 C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE
	include INSTR_CMN !mot_line
c
c
	integer*4 status,motline,i, itype
	character*4 motorline
	logical   type_used(maxtypes), no_motors
c
c  variables for giving process phy_io (for set term) & sysprv (for assign)
	integer*4 priv_mask(0:1),isub,ipos,sys$setprv 
c
c  variables for setting terminal characteristics
	character*192 mspcom
	integer*4 lib$spawn
c
c  variables for assigning channel
	integer*4 sys$assign,io_stat
	character*5 dhv11
c
	include MOTORS_CMN !mcchan
	include XRAY_CMN !poly
	include '($prvdef)'
	include '($iodef)'

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
c       need to set up line to xprom even if no xray motors used
	IF (nsta .eq. 0)THEN
	  type_used(XPROM) = .TRUE.
	  no_motors = .FALSE.
	ENDIF

	IF (no_motors)RETURN !no motors in use
C
c  give process phy_io privilege
	isub = PRV$V_PHY_IO/32           ! Define wether high or low order
	ipos = MOD (PRV$V_PHY_IO,32)	 ! Define position in mask
	priv_mask(isub) = IBSET (priv_mask(isub),ipos)
	isub = PRV$V_SYSPRV/32           ! Define wether high or low order
	ipos = MOD (PRV$V_SYSPRV,32)	 ! Define position in mask
	priv_mask(isub) = IBSET (priv_mask(isub),ipos)
	status = SYS$SETPRV (%VAL(1),priv_mask,%VAL(1),)
	IF (.NOT. status) CALL LIBSIGNAL (status)
c	call setef(camerr)
c
c  set characteristices for mcu RS232 port
	DO itype = 1, maxtypes !set up port for each type controller
	  IF (type_used(itype))THEN !but only if type used
	    IF (nsta .EQ. 0)THEN !xray
	      motorline='tta0'
	    ELSE
	      motorline='txa0'
	    ENDIF
	    motline=mot_line(iTYPE)
	    if (motline.gt.7)then
	      motorline='txb0'
	      motline=motline-8
	    endif
	    write(motorline(4:4),25,iostat=io_stat)motline
 25	    format(i1)
	    if (io_stat.ne.0)then
	      call putcontr
     1	       (' error setting terminal characteristics of '//motorline(1:4))
c	      call setef(camerr)
	    endif
	    IF (itype .EQ. ACSF)THEN !focusing motors on NG5
	      Mspcom(1:)='SET TERM/PERM/SPEED=4800/PARITY=EVEN/NOEIGHT_BIT'//
     1		      '/noINTERACTIVE/NOECHO/NOTYPE_AHEAD/noTTSYNC'//
     1		      '/NOSCOPE/NOBROADCAST/noFULLDUP/NOAUTOBAUD'//
     1		      '/NOLINE_EDITING/PASTHRU/nohostsync/nowrap'//
     1		      '/nolocal_echo '//MOTORline//':'
	    ELSEIF (itype .EQ. PC)THEN !CompuMotors controlled by PC
	      IF (nsta .EQ. 8)THEN !for BT8
	        Mspcom(1:)='SET TERM/PERM/SPEED=9600/PARITY=EVEN/EIGHTBIT'//
     1		        '/noINTERACTIVE/NOECHO/NOTYPE_AHEAD/noTTSYNC'//
     1		        '/NOSCOPE/NOBROADCAST/noFULLDUP/NOAUTOBAUD'//
     1		        '/NOLINE_EDITING/PASTHRU '//MOTORline//':'
	      ELSE !all others (refl & polymers xray)
	        Mspcom(1:)='SET TERM/PERM/SPEED=9600/PARITY=NONE/EIGHTBIT'//
     1		        '/noINTERACTIVE/NOECHO/NOTYPE_AHEAD/noTTSYNC'//
     1		        '/NOSCOPE/NOBROADCAST/noFULLDUP/NOAUTOBAUD'//
     1		        '/NOLINE_EDITING/PASTHRU '//MOTORline//':'
	      ENDIF
	    ELSEIF (itype .EQ. XPROM)THEN !Xray prom
		      motorline='TTA2' !instr.cfg puts mot_line in mot_line(acsf)
	      IF (.NOT. poly) THEN !not polymers
	        Mspcom(1:)='SET TERM/PERM/NOECHO/FORM/NOWRAP/SPEED=2400'//
     1		      '/HOST/NOBROAD/TYPE_AHEAD/ALTYP '//motorline//':'
	      ELSE !polymers
	        Mspcom(1:)='SET TERM/PERM/NOECHO/FORM/NOWRAP/SPEED=9600'//
     1		      '/HOST/NOBROAD/TYPE_AHEAD/ALTYP '//motorline//':'
	      ENDIF
	    ELSE !type = ACS: standard ACS controller
	      Mspcom(1:)='SET TERM/PERM/SPEED=4800/PARITY=ODD/NOEIGHT_BIT'//
     1		      '/noINTERACTIVE/NOECHO/NOTYPE_AHEAD/noTTSYNC'//
     1		      '/NOSCOPE/NOBROADCAST/noFULLDUP/NOAUTOBAUD'//
     1		      '/NOLINE_EDITING/PASTHRU/nohostsync/nowrap'//
     1		      '/nolocal_echo '//MOTORline//':'
	    ENDIF
	    status=LIB$SPAWN(mspcom)
	    if (.not.status)call libsignal(status)
c	    call setef(camerr)
c
C  assign channel to RS232 line for motor controller
	    dhv11='_'//motorline
	    status=sys$assign(dhv11,mcchan(itype),,)
	    if (.not.status)call libsignal(status)
c	    call setef(camerr)
	  ENDIF
	ENDDO
C
c  remove  sysprv privileges. must leave PHY_IO for temperature controller
	isub = PRV$V_PHY_IO/32           ! Define wether high or low order
	ipos = MOD (PRV$V_PHY_IO,32)	 ! Define position in mask
	priv_mask(isub) = IBCLR (priv_mask(isub),ipos)
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
