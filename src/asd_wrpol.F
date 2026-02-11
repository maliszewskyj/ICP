#include "icp.h"
	SUBROUTINE asd_WrPol (string, length, error,pstype)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send character string over GPIB bus via CAMAC module         C
C                                                                             C
C     Gayle Greene, February 1991
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use asd
	use instr
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface

	INTEGER*2     i2data,pstype !type of power supply. ie:poltype(psno)
	CHARACTER*30  string
	INTEGER*4     error
	INTEGER*4     length, i, status

	IF (gpibenet) THEN
	   CALL GPIBEN_WR(string,length,error)
	ELSE
	   CALL GPIBCT_WR(string,length,error)
	ENDIF
	END      
      
