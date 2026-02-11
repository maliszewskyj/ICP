#include "icp.h"
	SUBROUTINE Run_DPolar (dbuf, polcfg, loop, keep)
C	****************************************************
C-----------------------------------------------------------------------------C
C                                                                             C
C    Routine Which Repeats A Point With A Different Flipper Configuration     C
C    Called from: Run_D_Buf                                                   C
C                                                                             C
C    Gayle Greene, Jan 2001                                               C
C-----------------------------------------------------------------------------C
	use instr
	use smg_prep
	use histo
	use tc
	use polar
	use dartz
	IMPLICIT      none
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, prf, accucnts(37), counts(37), state
	INTEGER*4     loop, j, ee,m,kpsd
	INTEGER*4     min, sec, qwtime(2), idx, typ, hsum
	REAL*4        dsec, elap_time, mon, mpf, temp /0./, dummy,
     1			ihf,hf,hfh,field,secdiff
	CHARACTER*4   mt
	CHARACTER*9   str9
	CHARACTER*12   str12
	CHARACTER*28  sys_time
	CHARACTER*80  response,outpsd
	LOGICAL	      monmag, Readef

	TYPE(d_buf) dbuf
	TYPE(polarize) polcfg
C---------------------- MAP BUFFER VARIABLES TO LOCAL ------------------------C
	mon = dbuf%mon
	mpf = dbuf%mpf
	mt  = dbuf%mt
	ihf = dbuf%ihf
	hf  = dbuf%hf
	hfh  = dbuf%hfh
#if defined(VMS)
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
	   CALL Reset_Display       !Reset NIM count display
 200	   DO prf = 1, NINT(mpf)
 201	      CALL RlyOff (4)			        ! Start display counter
	      IF (mt .EQ. 'NEUT') THEN
  	         CALL Count (mon)
	      ELSE
  	         CALL CnTime (mon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
C	      IF (READEF(contr_A)) RETURN
!          C----------- CHECK STATISTICS IF ENABLED -------------------------C
	      IF (statistic) THEN              
	         CALL Test_Ststcs (mon, prf, counts, *100, *201)
	      END IF
              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(mon)
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
	     CALL putmax(temp,str9,9,3)
	     IF (monmag) THEN
	      CALL putmax(field,str12,12,4)
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9,a12)') 
	1            min, ':', sec, accucnts(2), str9, str12
	     ELSE
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9)') 
	1            min, ':', sec, accucnts(2), str9
	     ENDIF
	   ELSE
	     IF (monmag) THEN
	      CALL putmax(field,str12,12,4)
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a12)') 
	1            min, ':', sec, accucnts(2), str12
	     ELSE
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9)') 
	1            min, ':', sec, accucnts(2)
	     ENDIF
	   END IF
	   CALL PUTLINE_NF (contr, response)
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
	         CALL PUTLINE_NF (contr, outpsd(1:80))
c		 WRITE (2, '(a80)') outpsd(1:80)
	         kpsd = 2
		 m = 2 + ee-1
	       END IF
	       IF (kpsd .LE. 2) outpsd(1:) = ' '
	       WRITE (UNIT= outpsd(kpsd:m), FMT= '(i<ee>)') hsum
	       outpsd(m+1:m+1) = ','
	       kpsd = m + 2
	     END DO
c	     IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	     IF (m.gt.2)CALL PUTLINE_NF (contr, outpsd(1:m))
	   ENDIF
	   IF (keep) CALL Dmp_dbuf (polcfg.fname, loop, accucnts, elap_time,
	1		   typ, dbuf, temp, polcfg.csec,field,monmag)
#endif
	END
