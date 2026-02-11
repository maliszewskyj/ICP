#include "icp.h"
	SUBROUTINE Lia_Head(tofile)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, Sept 1997				              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT    none
	LOGICAL  tofile
	CHARACTER*10  sp10/'          '/

	IF (tofile)THEN
	  CALL Putcontr_NF('           X(V)'//
     1     sp10//' Y(V)'//'       '//'FREQ'//sp10(1:9)//'SLVL')
	  CALL Putcontr_NF(
     1	   '        AUXV1       AUXV2')
	ELSE !just  in log
	  CALL Putcontr_NT('           X(V)'//
     1     sp10//' Y(V)'//'       '//'FREQ'//sp10(1:9)//'SLVL')
	  CALL Putcontr_NT(
     1	   '        AUXV1       AUXV2')
	ENDIF

	RETURN
	END
