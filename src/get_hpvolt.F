#include "icp.h"
	SUBROUTINE Get_HpVolt (reply,lreply,error)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to read character string over GPIB bus via CAMAC module         C
C            return reply in reply(1:lreply)                                  C
C     Gayle Greene, Sept 1997                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use tc
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpibct.h"
	end interface
     	INTEGER*2  hpdev/0/ !HP format will be used in comm with mag controller
	CHARACTER*20 outstr
	INTEGER*4   lstr
	byte	    blf/10/,bcr/13/
	character*1 cr,lf, ichar
	equivalence (lf,blf),(cr,bcr)

	CHARACTER*80  reply
	LOGICAL       finished, error
	INTEGER*4      i, j

	INTEGER*4     lreply,errflg

	outstr(1:) = ';VOUT?;'//cr
	lstr = 8
	CALL Gpib_Listen(magport, errflg)   ! set up controller to listen
	CALL WrPol(outstr, lstr, errflg,hpdev)
	CALL Gpib_Unlisten(errflg)
	CALL Gpib_Talk(magport,errflg)
C   read back reply
	CALL GPIBCT_RD(reply,80,lreply,errflg)
	CALL Gpib_Unlisten(errflg)

	RETURN
	END
