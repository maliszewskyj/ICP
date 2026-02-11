#include "icp.h"
	SUBROUTINE Howl_File (fcommand,dopart)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     the file named in fcommand and passes it to the time_com routine        C 
C     to check. It continues to read and check each command.  		      C
C     NOTE: whole file is checked unless dopart is true at call. In this case C
C       file is only checked from * onwards.				      C
C     Called from: Parse_Contr						      C
C                                                                             C
C     Gayle Greene, February 1996                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

C NOTE FILES ARE NOT OPENED WITH SHARED STATUS
	use instr
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface

	LOGICAL	      dopart !true if only after * to be treated
	INTEGER*4     i, lbuf, status, ios, context
	INTEGER*4     maxtry,spos,scom,ecom,lcom, asc, io_stat
	CHARACTER*50  command, fcommand
	CHARACTER*80  response, res_fname
	CHARACTER*120 buffer
	LOGICAL	      putstar,findstar, Readef
	REAL*4		time,tottime,timemax,totmax
	CHARACTER*20   flagstr

	tottime = 0.
	totmax = 0.
	findstar = dopart

C Get filename to beginning of fcommand
	response(1:)=' '
	response(1:20) = fcommand
	response(23:)='Minutes:  Minimum    Maximum'
	call putcontr(response(1:))
	i=1
	DO WHILE (fcommand(i:i).eq.' ' .and. i .lt. 50)
	  i=i+1
	ENDDO
	fcommand(1:)=fcommand(i:50)

C Open file.
  90	maxtry = 0
 100	OPEN (UNIT = 2, FILE= fcommand, STATUS='OLD',
	1    iostat=io_stat)
	IF (io_stat .ne. 0) THEN
  	  context=0
  	  CALL FIND_FILE (fcommand, res_fname, context,status)
  	  CALL FIND_FILE_END (context)
  	  IF (status.eq.0) GOTO 990 !file not found
	  GOTO 150 !FILE LOCKED BY ANOTHER USER
	ELSE
	  GOTO 300 !FILE OPEN OK
	ENDIF
 150	maxtry = maxtry + 1
	IF (READEF (contr_A))THEN
	  CALL clref (contr_a)
	  CALL clref (hdw_event)  !remote abort
	  GOTO 990
	ENDIF
	CALL PUTCONTR(' File in use. Retrying.')
	CALL libwait(10.,status) !wait 10 seconds and try again
	IF (maxtry .LT. 100) THEN
	  GOTO 100
	ELSE
	  OPEN (UNIT = 2, FILE= fcommand, STATUS='OLD',ERR=990)
	ENDIF

 300	READ (2,fmt='(a)',end=500,err=990)buffer
	lbuf=len_trim(buffer)
	IF (findstar) THEN !skip all lines until star is found
	  spos = INDEX (buffer(1:lbuf), '*')
	  DO WHILE (spos .EQ. 0)
 	    READ (2,fmt='(a)',end=185,err=990)buffer
	    lbuf=len_trim(buffer)
	    DO WHILE (buffer(1:1).eq.'!') !ignore comment lines
	      READ (2,fmt='(a)',end=185,err=990)buffer
	      lbuf=len_trim(buffer)
	    ENDDO
	    spos = INDEX (buffer(1:lbuf), '*')
	  ENDDO
 185	  IF (spos .EQ. 0) THEN
 	    rewind(2)
	    READ (2,fmt='(a)',end=500,err=990)buffer
	    lbuf=len_trim(buffer)
	    CALL Putcontr_NT('No star found. Treating entire file.')
	  ELSEIF (spos .eq. 1) THEN !entire line to be treated
	      buffer(1:)=buffer(2:lbuf)
	      lbuf=lbuf-1
	  ELSEIF (spos .EQ. lbuf)THEN !just go to next line to start
	      READ (2,fmt='(a)',end=500,err=990)buffer
	      lbuf=len_trim(buffer)
	  ELSE !remove line up to (& including) star
	      buffer(1:) = buffer(spos+1:lbuf)
	      lbuf = lbuf - spos 
	  ENDIF
	  findstar = .FALSE. !only find beginning star once
	ENDIF !findstar
C  Have first line to treat in buffer.
C  Remove any * in it to ignore them.
 	    DO WHILE (buffer(1:1) .eq. '!') !ignore comment lines
 	      READ (2,fmt='(a)',end=500,err=990)buffer
	      lbuf=len_trim(buffer)
	    ENDDO
	    spos = INDEX(buffer(1:lbuf), '*')
	    DO WHILE (spos.ne.0)
	      IF (spos .eq. 1) THEN
	        buffer(1:)=buffer(2:lbuf)
	      ELSE IF (spos .ne. lbuf) THEN
		buffer(1:)=buffer(1:spos-1)//buffer(spos+1:lbuf)
	      ENDIF
	      lbuf=lbuf-1
	      spos = INDEX(buffer(1:lbuf), '*')
	    ENDDO
	    putstar = .true.

C  Find next command in buffer  
 400          IF (putstar) THEN !add star at start of line if appropriate
	        buffer(1:)='*'//buffer(1:lbuf)
	        lbuf = lbuf + 1
	        putstar = .false.
	      END IF !putstar
	      spos = INDEX (buffer(1:lbuf), '*') !remember where star is
	      IF (spos .EQ. 0) THEN !How can this happen? 
	        call PUTCONTR(' Rechecking: '//buffer(1:lbuf))
	        putstar = .true.
	        goto 400
	      ENDIF
	      scom=spos+1 !scom will mark start of command
	      DO WHILE (buffer(scom:scom).eq.' ' .OR.  !skip ; # and space
     1		  buffer(scom:scom).eq.'#' .OR. !ignore end for check
     1		  buffer(scom:scom).eq.';' .AND. scom .LE. lbuf)
	            scom=scom+1
	      ENDDO
	      IF (scom.gt.lbuf) goto 300 !get next line
	      ecom=scom !ecom will mark end of command
	      DO WHILE (buffer(ecom:ecom).ne.';' .and. !find end of command
     1	            buffer(ecom:ecom).ne.'#' .and. ecom .LE. lbuf)
	              ecom=ecom+1
	      ENDDO
	      ecom=ecom-1
	      command(1:)=buffer(scom:ecom)
	      lcom=ecom-scom+1

C  Change command to uppercase and check it.
	      DO i=1,lcom
	        asc = ichar(command(i:i))
	        IF (asc .GE. 97 .AND. asc .LE. 122) THEN !convert to uppercase
		  asc = asc - 32
		  command(i:i) = char(asc)
	        ENDIF
	      ENDDO
	      CALL Time_com (command,lcom,time,timemax,flagstr)
	      IF (time.NE. 0. .OR. timemax .NE. 0)THEN
	        tottime=tottime+time
		totmax = totmax + timemax
		response(1:) = ' '
		response(1:30) = command(1:lcom)
		IF (time .EQ. timemax)THEN
		  write(response(32:),FMT='(F8.1,11x,a)',iostat=ios)
     1		  time,flagstr
		ELSE
	          WRITE(UNIT=response(32:),fmt='(F8.1,a3,F8.1,a)',
     1			iostat=ios)time,'   ',timemax,flagstr
		ENDIF
 	        CALL PUTCONTR ( response )
	      ENDIF

 710	      IF (READEF (contr_A))THEN
	        CALL clref (contr_a)
	        CALL clref (hdw_event) !remote abort
	        CLOSE(2)
	        response=' Howlong terminated by control A'
		CALL PUTCONTR ( response)
		RETURN 
	      ENDIF

C  Set up rest of line so can move on to next command. 
	      IF (ecom.lt.lbuf) THEN !finish line
		    buffer(1:)=buffer(1:spos-1)//buffer(spos+1:ecom)//'*'//
     1		      buffer(ecom+1:lbuf)
		    goto 400
	      ELSE !get next line
		    goto 300
	      ENDIF

C  Close File
 500	  CLOSE(2)
	  response(1:)='Total Execution Time in Hours: '
	  IF (tottime .EQ. totmax)THEN
	    WRITE(UNIT=response(32:),fmt='(1x,F7.1)',iostat=io_stat)tottime/60.
 	    CALL PUTCONTR ( response)
	  ELSE
	    WRITE(UNIT=response(32:),fmt='(1x,F7.1,1x,a3,F7.1)',iostat=io_stat)
     1		tottime/60.,' - ',totmax/60.
 	    CALL PUTCONTR ( response)
	  ENDIF
	  RETURN 

C-------------------Handle file error-----------------------------------------C
 990	response = 'Error reading file: '//fcommand
	CALL PUTCONTR ( response)
	RETURN 
 995	response = 'End of file found unexpectedly. # should be last command.'
	CALL PUTCONTR ( response)
	RETURN 
	END      
