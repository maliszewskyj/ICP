#include "icp.h"
	SUBROUTINE Do_File_Sequence (fcommand,*)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     the file named in fcommand and passes it to the parser where it is      C 
C     executed. It continues to read another command until   		      C
C     it reads the end_of_command character '#'.                              C
C     Called from: Control                                                    C
C                                                                             C
C     Gayle Greene, July 1992                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

C NOTE FILES ARE NOT OPENED WITH SHARED STATUS
	use instr
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface
	INTEGER*4     i, j, lbuf, status,lnew
	INTEGER*4     maxtry,spos,s2pos,scom,ecom,lcom,  io_stat,
     1			recnum,soipos
	CHARACTER*80  command, response, fcommand
	CHARACTER*120 buffer,newbuf
	LOGICAL	      putstar,comflag,endflag,retry,nulltreat,
     1		inclflag,overwrite,Readef
	CHARACTER*80  newfile, res_fname, comname,syscmd,tmpfile
	INTEGER*4    context, lfn, ltmp
	INTEGER	     retn,system

C Get filename to beginning of fcommand
	i=4
	DO WHILE (fcommand(i:i).eq.' ' .and. i .lt. 50)
	  i=i+1
	ENDDO
C Check for /C meaning overwrite local file with community one
	overwrite = .FALSE.
	IF (fcommand(i:i+1).eq.'/C')THEN
	  overwrite = .TRUE. !memorize /c was there
	  i = i + 2 !strip off /c
	  DO WHILE (fcommand(i:i).eq.' ' .and. i .lt. 50)
	    i=i+1
	  ENDDO
	ENDIF
	fcommand(1:)=fcommand(i:50)
C Get length of actual filename
	lfn = 50 !get length of filename in lfn
	do while (fcommand(lfn:lfn).eq.' ' .and. lfn .gt.1)
	  lfn = lfn - 1
	enddo
C Get name of temporary file
	tmpfile(1:) = fcommand(lfn:lfn)//'.tmp'
	ltmp = 5
C Overwrite local file if so instructed
	IF (overwrite) THEN
	  comname(1:)='/usr/local/icp/community/'//fcommand
	  context = 0
	  CALL FIND_FILE (comname, res_fname, context,status)
	  CALL FIND_FILE_END (context)
	  IF (status.ne.0) THEN !file found
          syscmd(1:)='rm -f '//fcommand//char(0) !rm old file if there
          retn = system(syscmd)
 	    OPEN (UNIT = 2, FILE= fcommand, STATUS = 'NEW',
     1	         access='sequential')
	    comname(1:) = 'Include /usr/local/icp/community/'//fcommand(1:lfn)
	    WRITE(UNIT=2,fmt='(a)',iostat=io_stat)comname(1:lfn+18)
	    CLOSE(UNIT=2)
	  ELSE
	    CALL Putcontr_NT('File not found in community directory')
	    GOTO 990
	  ENDIF
	ENDIF

C Open file and see if star present; putstar <- true if not
  	maxtry = 0
	retry=.true.
 100	OPEN (UNIT = 1, FILE= fcommand, STATUS='OLD',iostat=io_stat)
	IF (io_stat .ne. 0) THEN
  	  context = 0
  	  CALL FIND_FILE (fcommand, res_fname, context,status)
  	  CALL FIND_FILE_END (context)
  	  IF (status.eq.0) THEN !file not found
	    if (retry) then !check in community directory
	      retry = .false.
	      comname(1:)='/usr/local/icp/community/'//fcommand
	      context = 0
	      CALL FIND_FILE (comname, res_fname, context,status)
	      CALL FIND_FILE_END (context)
	      IF (status.ne.0) THEN !file found
		call putcontr_nt('Looking for community version')
 	        OPEN (UNIT = 2, FILE= fcommand, STATUS = 'NEW',
     1	         access='sequential')
		comname(1:) = 'Include /usr/local/icp/community/'//fcommand(1:lfn)
		WRITE(UNIT=2,fmt='(a)',iostat=io_stat)comname(1:lfn+18)
		CLOSE(UNIT=2)
	        GOTO 100
	      ELSE
	        goto 990
	      ENDIF
	    else !not retry
	      goto 990
	    endif
	  endif
	  GOTO 150 !FILE LOCKED BY ANOTHER USER
	ELSE
	  GOTO 180 !FILE OPEN OK
	ENDIF
 150	maxtry = maxtry + 1
	IF (READEF (contr_A))THEN
	 IF (.NOT. READEF (hdw_event))THEN !not remote
	  CALL clref (contr_a)
	  response(1:)='Control A detected'
	  GOTO 999
	 ELSE  !remote abort so ignore in this case
	  CALL clref (contr_a)
	  CALL clref (hdw_event)
	 ENDIF !remote abort
	ENDIF
	CALL putcontr(' File in use. Retrying.')
	CALL libwait(10.,status) !wait 10 seconds and try again
	IF (maxtry .LT. 100) THEN
	  GOTO 100
	ELSE
	  GOTO 990
	ENDIF

 180	putstar = .true.
	endflag = .false.

 182	  READ (1,fmt='(a)',end=185,err=990)buffer
	  lbuf=len_trim(buffer)
	  DO WHILE (buffer(1:1).eq.'!') !ignore comment lines
	    READ (1,fmt='(a)',end=185,err=990)buffer
	    lbuf=len_trim(buffer)
	  ENDDO
	  spos = INDEX (buffer(1:lbuf), '*')
	  if (spos.ne.0)then
	    s2pos = INDEX (buffer(spos+1:lbuf), '*')
	    if (s2pos.ne.0 .or. .not.putstar) then
	      call putcontr(' Multiple * in command file')
	      GOTO 990
	    endif
	    putstar=.false. !star found in file
	  endif	    
	  goto 182
 185	lbuf=len_trim(buffer)
	close(1)


C Repeatedly open file and copy into new file, treating command following star
c  each time. Move star along in file. Repeat until '#' found of entire
c  file treated
	inclflag = .FALSE. !no include series ready to be compressed
	DO WHILE (.not. endflag .OR. inclflag)
	  retry = .true.
	  maxtry = 0
 200	  OPEN (UNIT = 1, FILE= fcommand, STATUS='OLD',
     1		IOSTAT=IO_STAT)
	IF (io_stat .ne. 0) THEN

  	  context = 0
  	  CALL FIND_FILE (fcommand, res_fname, context,status)
  	  CALL FIND_FILE_END (context)
  	  IF (status.eq.0) THEN !file not found
	    if (retry) then !try twice when not found
	      retry = .false.
	      CALL libwait(5.,status) !wait 5 seconds and try again
	      goto 200
	    else
	      goto 990
	    endif
	  endif
	  GOTO 250 !FILE LOCKED BY ANOTHER USER
	ELSE
	  GOTO 280 !FILE OPEN OK
	ENDIF
 250	  maxtry = maxtry + 1
	  IF (READEF (contr_A))THEN
	   IF (.NOT. READEF (hdw_event))THEN !not remote
	    CALL clref (contr_a)
	    response(1:)='Control A detected'
	    GOTO 999
	   ELSE  !remote abort so ignore in this case
	    CALL clref (contr_a)
	    CALL clref (hdw_event)
	   ENDIF !remote abort
	  ENDIF
	  CALL putcontr(' File in use. Retrying.')
	  CALL libwait(10.,status) !wait 10 seconds and try again
	  IF (maxtry .LT. 100) THEN
	    GOTO 200
	  ELSE
	    GOTO 990 !error opening file
	  ENDIF
	  
 280	  syscmd(1:)='rm -f '//tmpfile !rm old file if there
	  retn = system(syscmd)
	  OPEN (UNIT = 2, FILE= tmpfile, STATUS = 'NEW')

	  comflag = .false.	!no command to treat yet
	  nulltreat = .false.	!no command with null treatment yet

	  IF (inclflag) THEN !compress a finished include series this pass
	    nulltreat = .TRUE.
	    inclflag = .FALSE.
	    READ (1,fmt='(a)',end=990,err=990) buffer
	    lbuf=len_trim(buffer)
	    do i = 1, soipos-1
	      WRITE (2, '(a)') buffer(1:lbuf)
	      READ (1,fmt='(a)',end=500,err=990) buffer
	      lbuf=len_trim(buffer)
	    enddo
	    IF (buffer(1:6) .EQ. '!SOI: ')THEN
	      WRITE(2, '(a)') buffer(7:lbuf) !write line without !SOI: 
	      READ (1,fmt='(a)',end=990,err=990) buffer
	      lbuf=len_trim(buffer)
	      do while (buffer(1:5) .NE. '!EOI:') !skip lines 
	        READ (1,fmt='(a)',end=990,err=990) buffer
		lbuf=len_trim(buffer)
	      enddo
	      READ (1,fmt='(a)',end=500,err=990) buffer
	    ENDIF
 300	    WRITE (2, '(a)') buffer(1:lbuf)
	    READ (1,fmt='(a)',end=500,err=990) buffer
	    lbuf=len_trim(buffer)
	    GOTO 300
	  ELSE !not compressing an include series this pass
	    recnum = 0 !restart pointer in file used to memorize include series
	    soipos = 0 !no start of include position memorized yet
C !Loop until end of file found
 400	    READ (1,fmt='(a)',end=500,err=990)buffer
	    lbuf=len_trim(buffer)
	    recnum = recnum + 1 !we are looking at next record
	    DO WHILE (buffer(1:1).eq.'!') !handle comment lines
	      IF (.NOT. inclflag .AND. buffer(1:6) .EQ. '!SOI: ')THEN
		soipos = recnum !memorize position of last SOI before EOI
	      ENDIF !SOI
	      IF (.NOT. inclflag .AND. buffer(1:5) .EQ. '!EOI:' .AND.
     1		   soipos .NE. 0 .AND.
     1		   .NOT. comflag .AND. .NOT. nulltreat) THEN !not active series
	        inclflag = .TRUE. !compress starting with soipos next pass
	      ENDIF !EOI
	      WRITE (2, '(a)')buffer(1:lbuf)
	      READ (1,fmt='(a)',end=500,err=990)buffer
	      lbuf=len_trim(buffer)
	      recnum = recnum + 1 !we are looking at next record
	    ENDDO
	    IF (.NOT. comflag .AND. .NOT. endflag .AND. .NOT. nulltreat) THEN
	      IF (putstar) THEN !add star at start of line if appropriate
	        buffer(1:)='*'//buffer(1:lbuf)
	        lbuf = lbuf + 1
	        putstar = .false.
	      END IF !putstar
	      spos = INDEX (buffer(1:lbuf), '*') !remember where star is
	      IF (spos .NE. 0) THEN !this line contains next command
	        scom=spos+1 !scom will mark start of command
	        DO WHILE (buffer(scom:scom).eq.' ' .OR.
     1		  buffer(scom:scom).eq.';' .AND. scom .LE. lbuf)
	            scom=scom+1
	        ENDDO
	        IF (scom.gt.lbuf) then
	          putstar = .true. !* was at end of line, put at start of next
		  IF (spos.lt.lbuf)then
		    buffer(1:)=buffer(1:spos-1)//buffer(spos+1:lbuf)
		  ENDIF
		  lbuf=lbuf-1 !star removed
	        ELSE IF (buffer(scom:scom).eq.'#')THEN
		  endflag=.true. !End of execution mark found
		  buffer(1:)=buffer(1:spos-1)//buffer(spos+1:lbuf) !remove star
		ELSE !really have a command this line
		  ecom=scom !ecom will mark end of command
	          DO WHILE (buffer(ecom:ecom).ne.';' .and.
     1	            buffer(ecom:ecom).ne.'#' .and. ecom .LE. lbuf)
	              ecom=ecom+1
		  ENDDO
	          ecom=ecom-1
		  command(1:)=buffer(scom:ecom)
	 	  lcom=ecom-scom+1
		  CALL UPCASE (command, command)
		  j = 1
		  DO i = 1, lcom !get rid of blanks
		    IF (command(i:i) .NE. ' ') THEN
		      command(j:j) = command(i:i)
		      j = j + 1
		    END IF
		  END DO
		  command(j:) = ' ' 
		  lcom = j - 1
	          IF (command(1:7) .EQ. 'INCLUDE')THEN
		    IF (spos .GT. 1)WRITE (2, '(a)')buffer(1:spos-1) !write precommand
		    WRITE (2, '(a)')'!SOI: '//buffer(scom:ecom) !write include command
		    IF (command(8:9) .eq. '/C')THEN
		      newfile(1:)='/usr/local/icp/community/'//command(10:)
		    ELSE
		      newfile(1:)=command(8:)
		    ENDIF
		    maxtry = 0
		    retry = .TRUE.
 430		    OPEN (UNIT=3, FILE=newfile, STATUS='OLD',iostat=io_stat)
		    IF (io_stat .ne. 0) THEN
  	  	      context = 0
  	  	      CALL FIND_FILE (newfile, res_fname, context,status)
  	  	      CALL FIND_FILE_END (context)
  	  	      IF (status.ne.0) THEN !file not found
			  IF (retry)THEN
			    retry = .FALSE.
			    IF (newfile(1:25) .ne. '/usr/local/icp/community/')THEN
			      call putcontr_nt('Looking for community version')
			      newfile(1:)='/usr/local/icp/community/'//newfile
			      GOTO 430
			    ELSE
			      GOTO 980
			    ENDIF
			  ELSE
			    GOTO 980 !No file. Fatal error.
			  ENDIF
		      ELSE ! file found, but locked
 			maxtry = maxtry + 1
			IF (READEF (contr_A))THEN
			    IF (.NOT. READEF (hdw_event))THEN !not remote
				CALL clref (contr_a)
				CALL Putcontr('Control A detected')
				GOTO 990 !close files 1 & 2
			    ELSE  !remote abort so ignore in this case
				CALL clref (contr_a)
				CALL clref (hdw_event)
			    ENDIF !remote abort
			ENDIF !control a
			IF (maxtry .LT. 60) THEN
			    CALL putcontr(' File in use. Retrying.')
			    CALL libwait(5.,status) !wait 5 seconds and try again
			    GOTO 430
			ELSE
			    GOTO 980 !Fatal error
			ENDIF
		      ENDIF !file not found or locked
		    ENDIF !iostat ne 0
 		    READ (3,fmt='(a)',end=475,err=980)lnew,newbuf
		    lnew=len_trim(newbuf)
		    DO WHILE (newbuf(1:1).eq.'!') !do not star initial comment lines
		      WRITE (2, '(a)')newbuf(1:lnew)
		      READ (3,fmt='(a)',end=475,err=980)newbuf
		      lnew = len_trim(newbuf)
		    ENDDO
		    newbuf(1:)='*'//newbuf(1:lnew)
		    lnew = lnew+1
		    WRITE (2, '(a)')newbuf(1:lnew)
		    nulltreat = .true. !signal no treatment this time through
 450		    READ (3,fmt='(a)',end=475,err=980)newbuf
		    lnew=len_trim(newbuf)
		    WRITE (2, '(a)')newbuf(1:lnew)
		    GOTO 450
 475		    lnew=len_trim(newbuf)
		    CLOSE(UNIT=3) !all of included file copied into com file
		    IF (ecom .LT. lbuf)THEN ! add command after INCLUDE to file
		      WRITE (2, '(a)')'!EOI: '//buffer(scom:ecom)
		      scom = ecom + 1 !point to next instruction now
	              DO WHILE (buffer(scom:scom).ne.';' .and.
     1	                   scom .LT. lbuf)
	                scom=scom+1
		      ENDDO
		      IF (scom .GE. lbuf) THEN
			buffer(1:)='!' !pick innocuous filler
			lbuf = 1
		      ELSE
		        buffer(1:)=buffer(scom+1:lbuf)
		        lbuf = lbuf - scom
		      ENDIF
		    ELSE !INCLUDE was last command on line
		      buffer(1:)='!EOI: '//buffer(scom:ecom)
		      lbuf = ecom - spos + 7
		    ENDIF ! ecom lt lbuf
		  ELSE !command is not an INCLUDE
	  	    comflag=.true.
		    IF (ecom.lt.lbuf) THEN
		      buffer(1:)=buffer(1:spos-1)//buffer(spos+1:ecom)//'*'//
     1		        buffer(ecom+1:lbuf)
		    ELSE
		      buffer(1:)=buffer(1:spos-1)//buffer(spos+1:ecom)//'*'
		    ENDIF
		  ENDIF !command = INCLUDE
	        ENDIF !have command
	      END IF !spos ne 0
	    END IF !not comflag and not endflag and not nulltreat
	    WRITE (2, '(a)')buffer(1:lbuf)
	    GOTO 400 !resume LOOP
	ENDIF !inclflag set
C !End of File reached 

C         Close both files
 500	CLOSE(1)
	CLOSE(2)
  	  syscmd(1:)='cp '//tmpfile(1:ltmp)//' '//fcommand(1:lfn)
  	  retn = system(syscmd)
	  IF (putstar)endflag = .true. !if no more lines to put star at

C 	  TREAT COMMAND
	  IF (comflag) THEN
	   CALL Putcontr('RSF: '//command(1:lcom))
C Upcase the command string so that STUPID FORTRAN can parse it
	   CALL upcase(command,command)
	   CALL Parse_Contr (command, *710)
 710	   comflag=.false.
	  ELSEIF (nulltreat) THEN !no treatment this time through file
	   nulltreat = .false.
	  ELSE !no comflag
	    IF (.not. endflag) THEN !ignore if endflag set too
	      response=' Star missing in command file. Stopped.'
	      goto 999
	    ENDIF
	  ENDIF
	  IF (READEF (contr_A))THEN
	    IF (.NOT. READEF (hdw_event))THEN !not remote
	      CALL clref (contr_a)
	      response=' Sequence terminated by control A'
	      goto 999
	    ELSE  !remote abort. Go get next command
	      CALL clref (contr_a)
	      CALL clref (hdw_event)
	      response=' Remote abort detected. Going to next command.'
	      CALL putcontr ( response)
	    ENDIF  !remote abort
	  ENDIF

	END DO !not endflag. repeat process until # or all file treated

C-------------------------------- WELL DONE ----------------------------------C

 140	response = 'Command sequence terminated !'
	GOTO 999

C-------------------Handle file error-----------------------------------------C
 980	response = 'Error reading file: '//newfile
	GOTO 999
 990	response = 'Error reading file: '//fcommand
 999	CALL putcontr ( response)
	CLOSE(UNIT=3)
 	CLOSE(UNIT=2)
 	CLOSE(UNIT=1)
	RETURN 1
	END      
