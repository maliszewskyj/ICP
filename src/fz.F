	FUNCTION Fz(motorno,pos)
C	************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Zero: return zero for current position of motor indicated    	      C
C	Called from: Parse_Contr,Instrini,Rock, Dry*,Run*,Update*             C
C                                                                             C
C	Gayle Greene, Feb 1996                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use zcoeffs
	IMPLICIT     none
	REAL*4       Fz
	INTEGER*4    motorno
	REAL*4       pos


	IF (nsta .NE. 4 .OR. z(motorno) .NE. -99.) THEN
	  fz = z(motorno)
	  RETURN
	ENDIF

	IF (motorno .EQ. 2)THEN
	  fz = z2coeff1*pos+z2coeff2 !linear z for motor 2 on BT4
	ELSEIF (motorno .EQ. 1)THEN
	  fz = z1coeff1*pos+z1coeff2 !linear z for motor 1 on BT4
	ELSE !(motorno .NE. 1 .AND. motorno .NE. 2)
	  fz = z(motorno)
	ENDIF

	RETURN

	END


