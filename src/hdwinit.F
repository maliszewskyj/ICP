#include "icp.h"
	SUBROUTINE HDWINIT(Mp)
C	*******************

C	Routine to initialize instrument and hardware control parameters
C       using instr.cfg and motors.buf. Setup error and control_a event 
c       flags.
	use instr
	use tc
	use polar
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_rs232_cnf.h"
#include "c_vme.h"
#include "c_rs232_acs.h"
	end interface
	CHARACTER   dummy
	REAL*4      zero/0./
	INTEGER*4    i,  Mp, contr_A_offs /79/, pause_offs /14/
	INTEGER*4    hdw_event_offs /64/, camerr_offs /32/, context, nowait/1/
	INTEGER*4    status, ios, stop_offs/8/
	CHARACTER*30 f

	integer*4  m(MAXMOTS)

	system_type = linux 
c       
C---------------CLEAR MOTOR FLAGS TO INDICATE NONE IN USE
	DO  I=1,MAXMOTS
	   MOTFL(I)=.false.
	ENDDO

	call rdfiles
	priv = .true. ! Short-circuit privilege for now
	IF (.not.priv)THEN
	   print *,' This program must be run from instrument control terminal!'
	   CALL EXIT
	ENDIF

C-----------------SET GLOBAL FLAGS TO THEIR DEFAULT VALUES ------------------C
	counting = .FALSE.
	t_count  = .FALSE.
	m_count  = .FALSE.

	call cluster64
	contr_A  = contr_A_offs + iabs(nsta)
	pause    = pause_offs + iabs(nsta)
	stop = stop_offs + iabs(nsta)

	if (nsta.eq.2.or.nsta.eq.7)
	1    amscaler=.true.
	if (nsta .eq. 4.or.nsta.eq.9.or.nsta.eq.-5) vmedata = .true.


	TEMPTYPE=1		!assume old lakeshore
	CALL RS232_CONF		!!not vms
C	IF (amscaler) CALL SCALERINIT !!not vms
	IF (vmedata) CALL RS232V_INIT
C	IF (nsta .eq. 5) CALL RS232P_INIT
	IF (mot_line(acs).ne.-1) CALL RS232_Init
	CALL Init_Motors        !set up moving flags and name strings for motors

	call smgini ! Setup signal handlers
	RETURN

	END


