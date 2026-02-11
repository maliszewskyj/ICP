#include "icp.h"
	SUBROUTINE Init_SRLia(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE SR830 LockIn AMP                      C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, NOV, 1998                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use lia
	implicit none

	CHARACTER*80  response,command,reply
	INTEGER*2     lreply,lcom
	LOGICAL	      noreply/.false./
	CHARACTER*50  spconfig(4)
	INTEGER*4  j,ios,l
	INTEGER*4 ERRFLAG
	CHARACTER*4   str4
	DATA spconfig /
	1  '        NO:       NOTHING BEING SCANNED    ',
	2  '        OA:       OSCILLATOR AMPLITUDE     ',
	9  '        OF:       OSCILLATOR FREQUENCY     ',
	3  '        DAC:      DAC OUTPUT ON THE BACK   '/


c	CALL Putcontr_NF ('Select Parameter to Scan: ')
c	CALL Putcontr_NF (' ')
c	DO j = 1, 4
c	      CALL Putcontr_NF (spconfig(j))
c	END DO
c	CALL Putcontr_NF (' ')
c	CALL SMG$READ_STRING (keyb, str4, 'Selection (NO/OA/OF/DAC): ',
c     1			 4,,,, l,, contr)   
c 10	IF (str4(1:2) .EQ. 'NO' .OR. str4(1:2) .EQ. 'no')THEN
c	  CALL Putcontr('Buffers should always contain 0.0 for t0')
c	  scanlia = 0
c	ELSEIF (str4(1:2) .EQ. 'OA' .OR. str4(1:2) .EQ. 'oa')THEN
c	  CALL Putcontr('To scan, use OA vals (0-5 volts) for temp params in buffers')
c	  scanlia = 1
c	ELSEIF (str4(1:2) .EQ. 'OF' .OR. str4(1:2) .EQ. 'of')THEN
c	  CALL Putcontr('To scan, use OF vals (.5-125kHz) for temp params in buffer')
c	  scanlia = 2
c	ELSEIF (str4(1:3) .EQ. 'DAC' .OR. str4(1:3) .EQ. 'dac')THEN
c	  CALL Putcontr('Connect DAC output to ADC4 input & to your equip with a tee connector.')
c	  CALL Putcontr('To scan, use DAC vals (0-15 volts) for temp params in buffers')
c	  scanlia = 3
c	ELSE
c	  CALL Putcontr_NF (' ')
c	  CALL SMG$READ_STRING (keyb, str4, 
c    1	   'Please enter NO,OA,OF,DAC or return for exit: ',
c   1			 4,,,, l,, contr)
c	  READ (UNIT=str4(1:l), FMT= '(i)',iostat=ios) j
c	  IF (j .eq. 0) THEN
c	    ERRFLAG = .TRUE.
c	    CALL Putcontr(' Initialization not completed.')
c	    RETURN
c	  ENDIF
c	ENDIF


	ERRFLAG = 1
c	command(1:)='TRIG2'
c	lcom=5
c	call srlia232(command,lcom,reply,lreply,noreply)
c	command(1:) = 'DD44'
c	lcom = 4
c	call srlia232(command,lcom,reply,lreply,noreply)

	RETURN 
	END

