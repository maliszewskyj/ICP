#include "icp.h"
	SUBROUTINE Run_R_Polar (rbuf,polcfg, loop, keep)
C	****************************************************

C-----------------------------------------------------------------------------C
C                                                                             C
C    Routine Which Repeats A Point With A Different Flipper Configuration     C
C    Called from: Run_R_Buf                                                   C
C                                                                             C
C-----------------------------------------------------------------------------C
	use instr
	use smg_prep
	use polar
	use histo
	use refl
	use tc
	IMPLICIT      none
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, prf, accucnts(37), counts(37), state
	INTEGER*4     loop, j, ee,m,kpsd
	INTEGER*4     min, sec, qwtime(2), idx, typ, hsum
	REAL*4        dsec, elap_time, mpf, temp /0./, dummy, runmon, qznow,
     1		        ihf,hf,hfh,field,secdiff
	CHARACTER*4   mt
	CHARACTER*10   str10
	CHARACTER*12   str12
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response,outpsd
	LOGICAL	      monmag

	TYPE(r_buf) rbuf
	TYPE(polarize) polcfg

C---------------------- MAP BUFFER VARIABLES TO LOCAL ------------------------C

	IF (rbuf%mon1.ne.0)THEN
	   qznow = rbuf%qz + rbuf%qzi * loop
	   runmon = rbuf%mon0 + (rbuf%mon1 * (qznow**rbuf%exp))
	ELSE
	   runmon = rbuf%mon0
	ENDIF
	mpf = rbuf%mpf
	mt  = rbuf%mt
	hfh = rbuf%hfh
	ihf = rbuf%ihf
	hf  = rbuf%hf

#if defined (VMS)
C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr .and. ihf .NE. 0)monmag=.TRUE.

C----------------- RESET FLIPPER AND ADVANCE LINE ON OUTPUT ------------------C

	   CALL Set_Flippers (polcfg.csec)
	   idx = polcfg.csec
	   CALL DATE_TIME (sys_time)        ! which says which cross section
	   response(1:) = sys_time(13:17) //'   #####  F1: '// flp(idx,1) // 
	1                 '   F2: '// flp(idx,2) // '       *'
	   CALL SCROLL_DISPLAY (contr)
	   CALL PUTCHARS (contr, response(1:40), 19, 1)



C------------------------------ START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.


C--------------------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL RlyOn (3)			 ! Reset little display counter
	   CALL RlyOff (3)
 200	   DO prf = 1, NINT(mpf)
 201	      CALL RlyOff (4)			        ! Start display counter
	      IF (mt .EQ. 'NEUT') THEN
  	         CALL Count (runmon)
	      ELSE
  	         CALL CnTime (runmon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
C	      IF (READEF(contr_A))RETURN


!          C----------- CHECK STATISTICS IF ENABLED -------------------------C

	      IF (statistic) THEN              
	         CALL Test_Ststcs (runmon, prf, counts, *100, *201)
	      END IF

              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO

	      IF (mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(runmon)
	      END IF
	      min = INT ( elap_time / 60. )
	      sec = NINT( MOD (elap_time, 60.))
           END DO                                              ! Prefactor loop
	   IF (tempctr) then
	     IF (which_temp.eq.2)THEN !control
	       CALL RdTemp (dummy, temp, dummy)
	     ELSEIF (which_temp.eq.3) THEN !setpoint
	       CALL RdTemp (dummy, dummy, temp)
	     ELSE !sample
	       CALL RdTemp (temp, dummy, dummy)
	     ENDIF
	   ENDIF

	   IF (monmag)THEN !monitor field
	     IF (magtype .eq.4)THEN !read white superconducting only for now 
     		Call RdField(field)
		IF (field .EQ. -999.9)THEN !err reading back
		  field = fieldlast * hfconv
		ELSE !convert tesla to amps & then to user units !HOW???
		  field = field !just put out in tesla for now
		ENDIF
	     ELSEIF (magtype.eq.3 .or. magtype.eq.5)THEN !blue superconducting 
		field = fieldlast * hfconv
	     ELSE
     		field = hf + loop*ihf
	     ENDIF
	   ENDIF


!       C----------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

	   IF (tempctr) THEN
	      Call Putmax(temp,str10,10,3)
	      IF (monmag)THEN
	        CALL putmax(field,STR12,12,4)
	        WRITE (UNIT= response, FMT= 
     1		'(i4,a,i2.2,1x,i11,1x,i9,1x,a10,1x,i9,a12)') 
     1               min, ':', sec, accucnts(1),accucnts(2),str10,
     1		     accucnts(3), STR12
	      ELSE
	        WRITE (UNIT= response, FMT= 
     1		'(i4,a,i2.2,1x,i11,1x,i9,1x,a10,1x,i9,1x,a10)') 
     1               min, ':', sec, accucnts(1),accucnts(2),str10,
     1		     accucnts(3)
	      ENDIF
	   ELSE
	      IF (monmag)THEN
	        CALL putmax(field,STR12,12,4)
	        WRITE (UNIT= response, FMT= '(i4,a,i2.2,i11,i9,i9,a12)') 
     1               min, ':', sec, accucnts(1), accucnts(2),accucnts(3),
     1		     STR12
	      ELSE
	        WRITE (UNIT= response, FMT= '(i4,a,i2.2,i11,i9,i9)') 
     1               min, ':', sec, accucnts(1), accucnts(2),accucnts(3)
	      ENDIF
	   END IF
	   CALL Putline_NF (contr, response)
	   IF (PSD) THEN !write position sensitive detector data
	     outpsd(1:)=' '
	     kpsd = 2
	     DO i = 1, nocells/16 !16 groups of 16 cells
	       hsum=0
	       DO j = 1, 16 !sum adjacent 16 cells
	         hsum=hsum+(histodata((i-1)*16+j))
	       ENDDO
	       ee = 1
	       DO WHILE (hsum .GE. 10**ee)
	         ee = ee + 1
	       END DO
	       m = kpsd + ee-1	    	     
	       IF (m .GE. 80) THEN                        ! write string to screen
	         CALL Putline_NF (contr, outpsd(1:80))
c		 WRITE (2, '(a80)') outpsd(1:80)
	         kpsd = 2
		 m = 2 + ee-1
	       END IF
	       IF (kpsd .LE. 2) outpsd(1:) = ' '
	       WRITE (UNIT= datum, FMT= '(i20)') hsum
	       outpsd(kpsd:m) = datum(21-ee:20)
	       outpsd(m+1:m+1) = ','
	       kpsd = m + 2
	     END DO
c	     IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	     IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	   ENDIF
	   IF (keep) CALL Dmp_Refl (polcfg.fname, loop, accucnts, elap_time, rbuf,
	1		            temp, polcfg.csec,field,monmag)
#endif
	END
