#include "icp.h"
C       From numerical recipes. Comments are added.
C       Chebyshev evaluation: All arguments are input.
C       C(1:m) is an array of Chebyshev coefficients.


	REAL*4  FUNCTION CHEBEV(A,B,C,M,X)

	IMPLICIT NONE
	

	INTEGER*4  M,J
	REAL*4 	   A,B,X,D,DD,SV,Y,Y2,C(M)

	IF ((X-A)*(X-B).GT.0.) THEN
	   CAll Putcontr('value not in range in chebyshev function.')
	   chebev = -999.
	ENDIF
	D=0.
	DD=0.
	Y=(2.*X-A-B)/(B-A)
	Y2=2.*Y
	DO J=M,2,-1
	   SV=D
	   D=Y2*D-DD+C(J)
	   DD=SV
	ENDDO
	CHEBEV=Y*D-DD+0.5*C(1)
	RETURN
	END
