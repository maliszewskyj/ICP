#include "icp.h"
	SUBROUTINE Find_File_End(context)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$FIND_FILE_END		      C
C                                                                             C
C	Gayle Greene, April    , 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use ISO_C_BINDING
	IMPLICIT NONE
	interface
	   subroutine GLOBEND() BIND(C,name="GLOBEND")
	   use ISO_C_BINDING
	   end subroutine GLOBEND
	end interface

C  Calling parameters
	INTEGER*4	context !search context

	CALL GLOBEND
 	RETURN
	END
