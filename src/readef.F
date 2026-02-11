#include "icp.h"
	LOGICAL FUNCTION READEF(iflag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Routine which interfaces with DEC SYS$READEF     		      C
C	returns true if and only if event flag iflag was set
C                                                                             C
C	Gayle Greene, April , 1998                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C  Calling parameters
	use eventflag
	use icpsig
	use instr

	IMPLICIT NONE
	INTEGER*4	iflag, state

	IF (sigint .ne. 0) THEN !contr_a
      	   evflags(contr_A) = .TRUE.
	   evflags(pause) = .FALSE.
	   sigint = 0
           print *,'*** CTRLA ***'
	   clrstat=.TRUE.
	ELSEIF (sigtstp .ne. 0) THEN !control p
	   evflags(pause) = .TRUE.
	   sigtstp = 0
           print *,'*** CTRLP ***'
	ENDIF 

	! 
	IF (sigquit .ne. 0) THEN !control f
           print *,'*** CTRLF ***'
	   sigquit = 0
	   clrstat=.TRUE.
	   finishup = .TRUE.
	ENDIF

      	readef = evflags(iflag)

	RETURN
	END
