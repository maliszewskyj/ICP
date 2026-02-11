#include "icp.h"
	SUBROUTINE Goodbye
C	******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to erase and unpaste the virtual displays and then exit to DCL  C
C     Called from: ICP_MAIN                                                   C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep  ! Program specific SMG-definitions
	use instr     ! Instrument and hardware parameters
	use iso_c_binding
	interface
#include "c_rs232_acs.h"
	end interface
	CHARACTER*16  filename
	INTEGER*4  lname

	filename(1:) = logname//'.LOG'
	lname = 16
	if (priv) CALL Archfile(filename,lname)

	IF (priv) THEN
	  CALL RS232end
          CALL ContrEnd
	ENDIF
	PRINT *, ' '
	PRINT *, ' '
	PRINT *, '                           ICP - Program terminated   '
	PRINT *, ' '
	PRINT *, 
     1  '   Good bye!   Adios!   Sayonara!   Auf Wiedersehen!   Arrivederci! '
	PRINT *, ' '
	PRINT *, ' '
	STOP
	END      

