#include "icp.h"
	SUBROUTINE Init_Lia(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE SR830 LockIn AMP                      C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, November, 1998                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use lia
	use smg_prep
	implicit none

	CHARACTER*80  response,command,reply
	INTEGER*2     lreply,lcom
	LOGICAL	      errflag,noreply/.false./
	CHARACTER*50  spconfig(4)
	INTEGER*4  j,ios,l
	CHARACTER*6   str6

	DATA spconfig /
	1  '      NONE:       NOTHING BEING SCANNED    ',
	2  '      SLVL:       OSCILLATOR AMPLITUDE     ',
	9  '      FREQ:       OSCILLATOR FREQUENCY     ',
	3  '      AUXV:       DAC OUTPUT ON THE BACK   '/

	CALL Putcontr_NF ('Select Parameter to Scan: ')
	CALL Putcontr_NF (' ')
	DO j = 1, 4
	      CALL Putcontr_NF (spconfig(j))
	END DO
	CALL Putcontr_NF (' ')
	CALL READ_STRING (keyb, str6, 'Selection (NONE/SLVL/FREQ/AUXV): ',
     1			 6,j,j,j, l,j, contr)  !j is for unix   
 10	IF (str6(1:4) .EQ. 'NONE' .OR. str6(1:4) .EQ. 'none')THEN
	  CALL Putcontr('Buffers should always contain 0.0 for t0')
	  scanlia = 0
	ELSEIF (str6(1:4) .EQ. 'SLVL' .OR. str6(1:4) .EQ. 'slvl')THEN !slvl
	  CALL Putcontr('To scan, use OA vals (.004-5 volts) for temp params in buffers')
	  scanlia = 1
	ELSEIF (str6(1:4) .EQ. 'FREQ' .OR. str6(1:4) .EQ. 'freq')THEN !ref freq
	  CALL Putcontr('To scan, use OF vals (.001-102000Hz) for temp params in buffer')
	  scanlia = 2
	ELSEIF (str6(1:4) .EQ. 'AUXV' .OR. str6(1:4) .EQ. 'auxv')THEN
	  CALL Putcontr('Connect AUX1 output to AUX1 input & to your equip with a tee connector.')
	  CALL Putcontr
     1	   ('To scan, use AUXV output vals (-10.500->10.500V) for temp params in buffers')
	  scanlia = 3
	  command(1:)='AUXV1,0.5'
	  lcom=9
	  call srlia232(command,lcom,reply,lreply,noreply)
	ELSE
	  CALL Putcontr_NF (' ')
	  CALL READ_STRING (keyb, str6, 
     1	   'Please enter NONE,SLVL,FREQ,AUXV or return for exit: ',
     1			 6,j,j,j, l,j, contr) !j is for unix
	    IF (l .eq. 0) THEN
	      ERRFLAG = .TRUE.
	      CALL Putcontr(' Initialization not completed.')
	      RETURN
	    ENDIF
	    GOTO 10
	ENDIF

	command(1:)='OUTX0'
	lcom=5
	call srlia232(command,lcom,reply,lreply,noreply)

	ERRFLAG = .false.

	RETURN 
	END

