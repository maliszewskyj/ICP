#include "icp.h"
	SUBROUTINE Run_P_Buf (comfile,lname)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes POLFIG    Buffer No. buf_no                                  C
C	Called from: Parse_Contr and only in nsta .eq. 6                      C
C									      C
C	Gayle Greene, Nov. 1992                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use tc
	! use polfig
	IMPLICIT      none
	INTEGER*4     i, j, k, status, st,  typ, ver, m
	INTEGER*4     prf, loop, buf_no, state, counts (37), accucnts(37)
 	INTEGER*4     context,ig,i1,i2
	INTEGER*4     min, sec, right, left, iwhere, lname
	REAL*4        posit(10), oldpos(10), RdAngl, dummy, secdiff
	REAL*4        elap_time, dsec, temp /0./
	LOGICAL       limit, wrap, inlimits, Readef
 	CHARACTER*2   str2(3), bufno
	CHARACTER*3   str3
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr
	CHARACTER*8   out, new_line /'        '/
	CHARACTER*9   str9
	CHARACTER*12  fname, File_Name
	CHARACTER*28  sys_time
	CHARACTER*50  comfile
	CHARACTER*80  response, info(4), error, help_line(3), subdir
	REAL*4 fenergy

#if defined(VMS)
C  Variables unique to polfig buffer
	real*4 offset,rave,rin,frac1,frac2,chistart
	integer*4 loopi,loopj,loopk, lastk,nrun,l,
     1	   nmph(19)/2,8,18,24,24,36,36,72,72,72,72,72,72,72,72,
     1	     72,72,72,72/,icnts(73),iycnts(73),npts,inm
	logical fast
    	DATA  mlist/4,3,8,7/ !list of motors in use


	OPEN (UNIT= 1, FILE= comfile(1:lname), STATUS= 'OLD',err=111)
	READ (1,fmt='(a)')pbuf.outname(1:)
	READ (1,fmt='(a)',err=111)pbuf.com
	READ (1,fmt=*)pbuf.tmp,pbuf.th,pbuf.hf
	READ (1,FMT=*)pbuf.mon
	READ (1,FMT='(a)')pbuf.mt
	READ (1,FMT=*)npol,ispeed,chistart
	DO i = 1,npol
	  READ (1,FMT=*)iph(i),ipk(i),ipl(i),mpre(i),tth(i)
	ENDDO
	CLOSE (1)
	info(2) = pbuf.com
	info(4) = ' '
	GOTO 112
c  Treat error finding command file
 111	   response(1:)=' **** Error reading input file: '//comfile//' ****'
	   CALL PUTLINE (contr,response)
	   CALL PUTLINE (contr, ' **** RUN ABORTED ****')
	   RETURN

C------------------ PRINT SCAN PARAMETERS AND COMMENT --------------------------C

 112	CALL PUTLINE (contr, new_line)
	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = sys_time(1:17) // '  '//pbuf.com
	CALL PUTLINE (contr, response)
	CALL Putmax(pbuf.tmp,str6(1),6,3)
	CALL Putmax(pbuf.hf,out,8,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') pbuf.hf
	WRITE (UNIT= str6(2)(1:6), FMT= '(f6.0)') pbuf.th
	response = 'Temp  = '// str6(1) //' T-Hold= '//str6(2)//
	2        ' min  H-Field= '// out(1:8)
	CALL PUTLINE (contr, response)
	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(pbuf.mon)
	response = 'Monitor= '// str7(1) //
	1        '    Type= '// pbuf.mt 
	IF (file)response = response(1:46) //'     Datafile Series: '// pbuf.outname
	CALL PUTLINE (contr, response)


	IF (npol .lt. 1 .or. npol .gt. 9 ) THEN
	      CALL PUTLINE (contr, new_line)
	      info(3)  = 'Buffer aborted, npol outside of limits'
	      CALL PUTLINE (contr, info(3))
	      RETURN
	ENDIF

	IF (.not.motfl(3))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 3 not declared in instr.cfg  ****')
	IF (.not.motfl(4))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 4 not declared in instr.cfg  ****')
	IF (.not.motfl(7))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 7 not declared in instr.cfg  ****')
	IF (.not.motfl(8))CALL PUTLINE 
     1	    (contr, ' **** WARNING: Motor 8 not declared in instr.cfg  ****')
	IF (.not. file) CALL PUTLINE
     1	    (contr, ' **** WARNING: DATA WILL NOT BE SAVED IN FILE. Set D+ ****')


	DO LOOPI = 1 , npol !Do each polfig
	 numscan=loopi
	 nrun = loopi * 100
c	 Determine output filename
	  IF (file) THEN                                     ! Data storage is on
	   write(unit=str3,fmt='(i3.3)')nrun
	   fname = pbuf.outname//str3//'.bt6'
	   Call getdir(subdir,right)
	   CALL PUTLINE(contr,new_line)
	   CALL PUTLINE (contr,subdir(1:right)//fname)
 	  END IF
c	 Update help screen
	  help_line(1)(1:) = ' Executing Polfig Scan.  Datafile: '
	  IF (file) THEN 
	    help_line(1) = help_line(1)(1:50) //  subdir(1:right)//fname(1:12)
	  ELSE
	    help_line(1) = help_line(1)(1:50) // 'none        '
	  END IF
	  help_line(2)(1:) = ' '// pbuf.com(1:50)
	  help_line(3)     = ' '
	  CALL Put_Help (help_line)
	  info(1) = help_line(1)(2:)
C	 Write Q-center
	  CALL PUTLINE (contr, new_line)
	  WRITE (UNIT= str2(1), FMT= '(i2)') iph(loopi)
	  WRITE (UNIT= str2(2), FMT= '(i2)') ipk(loopi)
	  WRITE (UNIT= str2(3), FMT= '(i2)') ipl(loopi)   
	  response = 'Q-center = ('// str2(1) //','// str2(2) //','//str2(3)//')'
	  CALL PUTLINE (contr, response)
c        Position motors 3 & 4
	  posit(4)=tth(loopi)
	  posit(3)=.5*tth(loopi)

	DO loopj = 1, 19
c	 Position motor 8
	  IF (chistart .gt. 0) THEN
	    posit(8) = chistart - (5. * floatj(loopj-1))
	  ELSE
	    posit(8) = chistart + (5. * floatj(loopj-1))
	  ENDIF
c	  nrun = (loopi * 100) + loopj
c	 Print screen header line
	  i = 0  	! (* number of motors written *)
	  wrap = .FALSE.
	  response(1:) = 'Time     Pt# '
	  k = 13
	  DO l = 1,4
	    j=mlist(l)
	    IF (motfl(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:k+8) = response(1:k) //'    A'// out(1:1) //'  '
	      k = k + 8
	      i = i + 1
	    END IF
	  END DO
	  response(1:k+12) = response(1:k) //'     TIME   '
	  k = k + 12
	  response(1:k+10) = response(1:k) //'INTEN     '
	  k = k + 10
	  IF (tempctr) response(1:k+4) = response(1:k) //'TEMP'
	  CALL PUTLINE (contr, response)
	  CALL PUTLINE (contr, new_line)

C        Normal-phi or fast-phi scan?
	  IF (ispeed.eq.1)THEN
	    fast = .true.
	  ELSE
	    fast = .false.
	  ENDIF

	  IF (.not. FAST) THEN
	    lastk = 73
	    offset = 5.
	  ELSE
	    lastk = nmph(loopj)
	    offset = 360./lastk
	  ENDIF
	  DO loopk = 1, lastk
c	   Check pause flag
	    IF (READEF (pause)) THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	    END IF 
c	   Position motor 7
	    posit(7) = floatj(loopk - 1) * offset
c	   Type out position of each motors on screen
	    CALL DATE_TIME (sys_time)
	    WRITE(UNIT=out(1:7),fmt='(i3,a1,i3)')loopk,'/',lastk
	    response(1:) = sys_time(13:17) //' '//out(1:7)
	    i = 0		! (* number of motors written *)
	    k = 13
	    limit = .FALSE.
	    DO L = 1,4
	      j = mlist(l) 		             ! (* print new position first *)
	      IF (motfl(j)) THEN
	         WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
	         response(1:)= response(1:k) // out(1:8) 
	         k = k + 8
	         i = i + 1
		 IF (.NOT. inlimits(j,posit(j)+z(j),contr))THEN
	           limit = .TRUE.
	           error = 'Buffer aborted !!'
	         END IF
	      END IF
	    END DO
	    response = response(1:k) //' *'
	    k = k + 2
	    CALL PUTLINE (contr, response(1:k))
	    IF (i .LT. 6) THEN
	      CALL CURSOR_ABS (contr, 19, k+1)
	      iwhere = k+1
	    ELSE
	      iwhere = 1
	    ENDIF
	    info(3)(1:) = response(1:k)
	    IF (limit) THEN
	      CALL PUTLINE (contr, new_line)
	      CALL PUTLINE (contr, error)
	      CALL PUTLINE (contr, new_line)
	      IF (file .AND. loopk .GT. 2) CALL 
	1       POLFILE (fname,loopj, loopk-1,posit(8),posit(4),icnts)
	      info(3)  = 'Buffer aborted, angle outside software limits!'
	      RETURN
	    END IF
c	   Move all motors
	    DO l = 1 , 4                 ! (* drive motors in reverse order *)
	      IF (READEF (contr_A)) GOTO 50
	      j = mlist(l)
	      IF (loopi .EQ. 1 .and. loopj.eq.1 .and. loopk .eq. 1) THEN
  	        IF (motfl(j)) CALL Drive (j, posit(j) + z(j))
	      ELSE
	        IF ( posit(j) .NE. oldpos(j) .AND. motfl(j))
	1            CALL Drive (j, posit(j) + z(j))
	      END IF
	      oldpos(j) = posit(j)
	      memposit(l)=posit(j)
	    END DO
 50	    CALL Yield
	    IF (READEF (contr_A)) THEN
	      IF (file .and. loopk .gt.2) 
     1	        CALL POLFile (fname,loopj, loopk-1,posit(8),posit(4),icnts)
	      CALL PUTLINE (contr, ' **** ABORT during drive ****')
	      RETURN
   	    END IF

c	   Set temperature and wait if first point
	    IF (loopi .eq. 1 .and. loopj .eq. 1 .and. loopk .eq. 1 )THEN
	      IF (tempctr .AND. pbuf.tmp .NE. 0.) THEN
	        CALL New_Temp_Field (temp,pbuf.tmp,0.,0.,0.,
     1	             pbuf.th,0.,pbuf.hf, loop,0.,0.,0.,0.)
	        IF (READEF (contr_A)) THEN
	          IF (file .and. loopk .gt.2) 
     1	            CALL POLFile (fname,loopj, loopk-1,posit(8),posit(4),icnts)
	          CALL PUTLINE (contr, ' **** ABORT during temp change ****')
	          RETURN
   	        END IF
	      END IF
	    END IF

C------------------- START COUNTING -------------------------------C

 100	   DO i = 1, scl + 1
	      accucnts(i) = 0
	   END DO
	   elap_time = 0.


C---------------- LOOP OVER PREFACTOR -----------------------------C

	   CALL Reset_Display       !Reset NIM count display
 200	   DO prf= 1, mpre(loopi)
 201	      CALL RlyOff (4)			        ! Start display counter
              IF (pbuf.mt .EQ. 'NEUT') THEN
  	         CALL Count (pbuf.mon)
	      ELSE
  	         CALL CnTime (pbuf.mon)
	      END IF
  	      CALL Scaler (counts)
	      CALL RlyOn (4)				 ! Stop display counter
	      IF (READEF (contr_A)) THEN
	         IF (file .and. loopk .gt.2) 
     1	          CALL POLFile (fname,loopj, loopk-1,posit(8),posit(4),icnts)
	         CALL PUTLINE (contr, ' **** ABORT during count ****')
	         RETURN
	      END IF

c	   Check pause flag
	    IF (READEF (pause)) THEN
              CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
              CALL CURSOR_OFF (paste)
	      CALL HIBER()                                      ! Hibernate 
              CALL clref (contr_a)
              CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
              CALL CURSOR_ON (paste)
	      IF (iwhere .eq. 1) THEN
	        CALL SCROLL_DISPLAY (contr)
	        CALL CURSOR_ABS (contr, 19, 1)
	      ELSE
	        CALL CURSOR_ABS (contr, 19, iwhere)
	      ENDIF
	    END IF 

              DO i = 2, scl + 1
                 accucnts(i) = accucnts(i) + counts(i)
	      END DO
	      IF (pbuf.mt .EQ. 'NEUT') THEN
		 dsec = SecDiff() !function to get stop_time - start_time
	         elap_time = elap_time + dsec
	      ELSE
	         accucnts(1) = accucnts(1) + counts(1)
	         elap_time = elap_time + NINT(pbuf.mon)
	      END IF
	      min = INT (elap_time / 60.)
	      sec = NINT ( MOD (elap_time, 60.))
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


!          C-------- PRINTOUT DATA TO TERMINAL --------------------------------C

	      IF (tempctr) THEN
		 CALL Putmax(temp,str9,9,3)
	         WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9,a9)') 
	1               min, ':', sec, accucnts(2), str9
	      ELSE
	         WRITE (UNIT= response, FMT= '(i4,a,i2.2,i9)') 
	1               min, ':', sec, accucnts(2)
	      END IF
	      IF (wrap) response = '  '// response
	      CALL PUTLINE (contr, response)
	   info(3)(k+1:) = response

	   icnts(loopk) = accucnts(2)

!          C------------------ UPDATE STATUS FILE -----------------------------C

	   CALL Status_File (info)

	  END DO  ! (LOOPK)

	  IF (Fast) then
	    icnts(lastk+1) = icnts(1)
	    IF (loopj .eq. 1) THEN
	      rave = float(icnts(1) + icnts(2)) / 2.
	      DO i = 1,73
	        icnts(i) = nint(rave)
	      ENDDO
	    ELSE !loopj .ne. 1
	      DO I = 1,73
		rin = (5. * float(i-1)) / offset
		inm = int (rin)
		frac1 = (rin - inm)
		frac2 = 1. - frac1
		L = inm+1
		iycnts(i) = NINT(icnts(L) * frac2 + icnts(L+1) * frac1)
	      ENDDO
	      DO I = 1,73
	        icnts(i) = iycnts(i)
	      ENDDO
	    ENDIF !loopj .eq. 1
	  ENDIF
	  npts=73
	  IF (file) Call POLFILE(fname,loopj,npts,posit(8),posit(4),icnts)
	ENDDO !(loopJ)
	ENDDO !(loopi)
#endif
	RETURN
	END

