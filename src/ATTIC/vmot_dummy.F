#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE VINIT_BT8
C	*****************************

	IMPLICIT     none
	return
	end

      include COMPILE_OPTS
      SUBROUTINE RDV24_BT8(pos,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		pos
	LOGICAL		errflag
	RETURN
	END

      include COMPILE_OPTS
      SUBROUTINE V24_BT8(targetangle,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		targetangle
	LOGICAL		errflag

	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE V25_BT8(targetangle,errflag)
C     *************************************


	IMPLICIT     none
	REAL*4		targetangle
	LOGICAL		errflag

	RETURN
	END

      include COMPILE_OPTS
      SUBROUTINE RDV25_BT8(pos,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		pos
	LOGICAL		errflag

	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE V21_BT8(targetangle,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		targetangle
	LOGICAL		errflag


	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE RDV21_BT8(pos,errflag)
C     *************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	REAL*4		pos
	LOGICAL		errflag
	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE V22_BT8(targetangle,errflag)
C     *************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	REAL*4		targetangle
	LOGICAL		errflag
	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE RDV22_BT8(pos,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		pos
	LOGICAL		errflag
	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE V23_BT8(targetangle,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		targetangle
	LOGICAL		errflag
	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE RDV23_BT8(pos,errflag)
C     *************************************

	IMPLICIT     none
	REAL*4		pos
	LOGICAL		errflag
	RETURN
	END


      include COMPILE_OPTS
      SUBROUTINE IVM_ARRAY(error,filename)
C     *************************************

	IMPLICIT NONE
	LOGICAL ERROR
	CHARACTER*(*)  filename
	ERROR = .TRUE. !should not ever be called in dummy
	RETURN
	END
