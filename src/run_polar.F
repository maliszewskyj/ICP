#include "icp.h"

	SUBROUTINE Run_Polar (typ, ibuf, qbuf, bbuf, polcfg, loop, keep)
C	****************************************************

C-----------------------------------------------------------------------------C
C                                                                             C
C    Routine Which Repeats A Point With A Different Flipper Configuration     C
C    Called from: Run_I_Buf, Run_Q_Buf                                        C
C                                                                             C
C    Beat Schmid, January 1991                                                C
C-----------------------------------------------------------------------------C
	use instr
	use smg_prep
	use polar
	use histo
	use tc
	use asd
	use lia
	IMPLICIT      none
	LOGICAL	      keep !true to keep data file
	INTEGER*4     i, prf, accucnts(37), counts(37), state
	INTEGER*4     loop, j, ee,m,kpsd,idum
	INTEGER*4     min, sec, qwtime(2), idx, typ, hsum
	REAL*4        dsec, elap_time, mon, mpf, temp /0./, dummy,
     1			ihf,hf,hfh,field,secdiff
	CHARACTER*4   mt
	CHARACTER*9   str9
	CHARACTER*12   str12
	CHARACTER*28  sys_time
	CHARACTER*20  datum
	CHARACTER*80  response,outpsd
	LOGICAL	      monmag, prftemp, Readef, savepos

	REAL*4	posit(MAXMOTS) !dummy used only for call to dmp_qpos

	TYPE(i_buf) ibuf
	TYPE(q_buf) qbuf
	TYPE(b_buf) bbuf
	TYPE(polarize) polcfg

C---------------------- MAP BUFFER VARIABLES TO LOCAL ------------------------C

	savepos = .FALSE. !are we saving motor positions to a sep file?
	IF (typ .EQ. 1) THEN !i-buffer
           mon = ibuf%mon
	   mpf = ibuf%mpf
	   mt  = ibuf%mt
	   ihf = ibuf%ihf
	   hf  = ibuf%hf
	   hfh  = ibuf%hfh
	ELSE IF (typ .EQ. 5) THEN !bragg 
           mon = bbuf%mon
	   mpf = bbuf%mpf
	   mt  = bbuf%mt
	   ihf = bbuf%ihf
	   hf  = bbuf%hf
	   hfh  = bbuf%hfh
	ELSE			!q-buffer
           mon = qbuf%mon
	   mpf = qbuf%mpf
	   mt  = qbuf%mt
	   ihf = qbuf%ihf
	   hf  = qbuf%hf
	   hfh  = qbuf%hfh
	   if (verbose_run .and. (qbuf%es .ne. 0.0 .or. qbuf%hs .ne. 0.0 .or.
     1	     qbuf%ks .ne. 0.0 .or. qbuf%ls .ne. 0.0) )savepos = .true.
	END IF

	IF (tempctr .AND. temptype .EQ. 13)THEN
	  prftemp = .TRUE. !read temp each prefactor
	  prfpoint = 0
	ELSE
	  prftemp = .FALSE. !no need to read temp each prefactor
	ENDIF

C--------DETERMINE IF MONITORING FIELD----------------------------------------C
	monmag = .FALSE.
	IF (magnctr .and. ihf .NE. 0) monmag=.TRUE.

C----------------- RESET FLIPPER AND ADVANCE LINE ON OUTPUT ------------------C

	CALL Set_Flippers (polcfg%csec)
	idx = polcfg%csec
	CALL DATE_TIME (sys_time) ! which says which cross section
	response(1:) = sys_time(13:17) //'   #####  F1: '// flp(idx,1) // 
	1    '   F2: '// flp(idx,2) // '       *'
	CALL SCROLL_DISPLAY (contr)
	CALL PUTCHARS (contr, response(1:40), 19, 1)



C------------------------------START COUNTING -------------------------------C

 100	DO i = 1, scl + 1
	   accucnts(i) = 0
	END DO
	elap_time = 0.


C---------------------------LOOP OVER PREFACTOR -----------------------------C

	CALL Reset_Display	!Reset NIM count display
	IF (nsta .EQ. -5 .AND. use_asd) CALL Zero_ASD !zero data array
 200	DO prf = 1, NINT(mpf)
 201	   CALL RlyOff (4)	! Start display counter
	   IF (mt .EQ. 'NEUT') THEN
	      IF (nsta .EQ. -5 .AND. use_asd) CALL asd_on(1)
	      CALL Count (mon)
	   ELSE
	      IF (nsta .EQ. -5 .AND. use_asd) CALL asd_timed(1,mon)
	      CALL CnTime (mon)
	   END IF
	   CALL Scaler (counts)
	   CALL RlyOn (4)	! Stop display counter

	   IF ((nsta .eq. -5 .and. use_asd).or.(nsta.eq.-1.and.psd)
	1	.or.(nsta .eq. -11 .and. use_asd))
	1	counts(2)=NINT(fpwindow)

!       C----------- CHECK STATISTICS IF ENABLED -------------------------C

	   IF (statistic) THEN              
	      CALL Test_Ststcs (mon, prf, counts, *100, *201)
	   END IF

	   DO i = 2, scl + 1
	      accucnts(i) = accucnts(i) + counts(i)
	   END DO
	   IF (mt .EQ. 'NEUT') THEN
	      dsec = SecDiff()	!function to get stop_time - start_time
	      elap_time = elap_time + dsec
	   ELSE
	      accucnts(1) = accucnts(1) + counts(1)
	      elap_time = elap_time + NINT(mon)
	   END IF
	   min = INT ( elap_time / 60. )
	   sec = NINT( MOD (elap_time, 60.))
	   IF (prftemp) CALL RdTemp (temp, dummy, dummy)
	END DO			! Prefactor loop
	IF (tempctr) then
	   IF (.not. prftemp) THEN
	      IF (which_temp.eq.2)THEN !control
		 CALL RdTemp (dummy, temp, dummy)
	      ELSEIF (which_temp.eq.3) THEN !setpoint
		 CALL RdTemp (dummy, dummy, temp)
	      ELSE		!sample
		 CALL RdTemp (temp, dummy, dummy)
	      ENDIF
	   ENDIF		!prftemp
	ENDIF

	IF (monmag)THEN		!monitor field
	   IF (magtype .eq.4)THEN !read white superconducting only for now 
     		Call RdField(field)
	      IF (field .EQ. -999.9)THEN !err reading back
		 field = fieldlast * hfconv
	      ELSE		!convert tesla to amps & then to user units !HOW???
		 field = field	!just put out in tesla for now
	      ENDIF
	   ELSEIF (magtype.eq.3 .or. magtype.eq.5)THEN !blue superconducting 
C       field = fieldlast * hfconv
	      Call Rdfield(field)
	   ELSEIF (magtype.eq.2)THEN !read back voltage for HP
	      Call Rdfield(field)
	   ELSE
	      field = hf + loop*ihf
	   ENDIF
	ENDIF

!       C----------- PRINTOUT DATA TO TERMINAL AND FILE ---------------------C

	IF (tempctr) THEN
	   IF (temptype .EQ. tdev%cfg) THEN !using degrees
	      CALL putmax(temp,str9,9,4)
	   ELSE			!using resistance
	      str9(1:1)=' '
	      CALL putmax(temp,str9(2:9),8,7)
	   ENDIF
	   IF (monmag) THEN
	      CALL putmax(field,str12,12,4)
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9,a12)') 
	1	   min, ':', sec, accucnts(2), str9, str12
	   ELSE
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a9)') 
	1	   min, ':', sec, accucnts(2), str9
	   ENDIF
	ELSE
	   IF (monmag) THEN
	      CALL putmax(field,str12,12,4)
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9,a12)') 
	1	   min, ':', sec, accucnts(2), str12
	   ELSE
	      WRITE (UNIT= response, FMT= '(2x,i4,a,i2.2,i9)') 
	1	   min, ':', sec, accucnts(2)
	   ENDIF
	END IF
	CALL Putline_NF (contr, response)
	IF (PSD) THEN		!write position sensitive detector data
	   outpsd(1:)=' '
	   kpsd = 2
	   DO i = 1, nocells/16 !16 groups of 16 cells
	      hsum=0
	      DO j = 1, 16	!sum adjacent 16 cells
	         hsum=hsum+(histodata((i-1)*16+j))
	      ENDDO
	      ee = 1
	      DO WHILE (hsum .GE. 10**ee)
	         ee = ee + 1
	      END DO
	      m = kpsd + ee-1	    	     
	      IF (m .GE. 80) THEN ! write string to screen
	         CALL Putline_NF (contr, outpsd(1:80))
	         kpsd = 2
		 m = 2 + ee-1
	      END IF
	      IF (kpsd .LE. 2) outpsd(1:) = ' '
	      WRITE (UNIT= datum, FMT= '(i20)') hsum
	      outpsd(kpsd:m) = datum(21-ee:20)
	      outpsd(m+1:m+1) = ','
	      kpsd = m + 2
	   END DO
	   IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	ENDIF
	IF (nsta .EQ. -5 .AND. USE_ASD) THEN !write area sensitive detector data
	   outpsd(1:)=' '
	   kpsd = 2
	   DO i = 1, noslices
	      hsum=asd_data(i)
	      ee = 1
	      DO WHILE (hsum .GE. 10**ee)
		 ee = ee + 1
	      END DO
	      m = kpsd + ee-1	    	     
	      IF (m .GE. 80) THEN ! write string to screen
		 CALL Putline_NF (contr, outpsd(1:80))
c       WRITE (2, '(a80)') outpsd(1:80)
		 kpsd = 2
		 m = 2 + ee-1
	      END IF
	      IF (kpsd .LE. 2) outpsd(1:) = ' '
	      WRITE (UNIT= datum, FMT= '(i20)') hsum
	      outpsd(kpsd:m) = datum(21-ee:20)
	      outpsd(m+1:m+1) = ','
	      kpsd = m + 2
	   END DO
c       IF (m.gt.2)write(2,fmt='(a)')outpsd(1:m)	
	   IF (m.gt.2)CALL Putline_NF (contr, outpsd(1:m))
	ENDIF
	IF (keep) THEN
	   CALL Dmp_Data (polcfg%fname, loop, accucnts, elap_time,
	1	typ, ibuf, qbuf,idum,bbuf, temp,
	2	polcfg%csec,field,monmag)
	   IF (nsta .EQ. -5 .AND. USE_ASD .AND. Keep_RawASD)
	1	CALL Dmp_ASD(polcfg%fname,loop)
	   IF (savepos) CALL Dmp_Qpos(polcfg%fname,loop,accucnts,qbuf,posit,
     1		polcfg%csec)
	ENDIF			!keep
	END
