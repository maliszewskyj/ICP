#include "icp.h"
	SUBROUTINE RdKeith (reply,lreply,error)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to read character string over GPIB bus via CAMAC module         C
C            return reply in reply(1:lreply)                                  C
C     Gayle Greene, April 1994                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT      none
	INTEGER*2     i2data
	CHARACTER*50  reply
	LOGICAL       error, finished
	INTEGER*4     length, i, j
	INTEGER*2     lreply

	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)

C NOT USED!!
	return

	END      
      
