#include "icp.h"
! isdigit  --  return true if character is digit or - or +
      	LOGICAL FUNCTION isdigit (c)
C	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	isdigit -- return true if character is [0..9,+,-,.]                   C
C                                                                             C
C       Beat Schmid, May 1990                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      	CHARACTER*1  c

      	isdigit = ((lge(c,'0') .AND. lle(c,'9')) .OR. c .EQ. '+' .OR. c .EQ. '-'
     	1         .OR. c .EQ. '.')
      	END
