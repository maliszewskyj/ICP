#include "icp.h"
	include COMPILE_OPTS
	subroutine init_xmots

c  initialize xray motors to speeds found in instr.cmn

c
	implicit none
	character*80 command,reply
	integer*2 lcom,lreply
	integer*4  secwait
	logical	   rserr
c
	include INSTR_CMN
	include XRAY_CMN
c
c
C  Send out motor speeds to SCINTAG
	IF (motfl(3) .and. mottype(3) .eq. XPROM)THEN !if motor 3 in use
	  command(1:)=mstring(1)
	  IF (poly)THEN !polymers
	    lcom = 20
	  ELSE !not polymers
	    lcom=17
	  ENDIF
	  secwait=10
	  rserr = .false.
	  call rs232x(command,lcom,reply,lreply,secwait,rserr)
	ENDIF
	IF (motfl(4) .and. mottype(4) .eq. XPROM)THEN
	  command(1:)=mstring(2)
	  IF (poly)THEN !polymers
	    lcom = 20
	  ELSE !not polymers
	    lcom=17
	  ENDIF
	  secwait=10
	  rserr = .false.
	  call rs232x(command,lcom,reply,lreply,secwait,rserr)
	ENDIF

	RETURN
	end
