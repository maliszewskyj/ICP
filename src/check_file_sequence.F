#include "icp.h"
	SUBROUTINE Check_File_Sequence (fcommand,*)
C	**************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine repeats to read one command from the command_sequence in        C
C     the file named in fcommand and passes it to the check_parser            C 
C     to check. It continues to read and check each command.  		      C
C     NOTE: whole file is checked. Internal * or # are ignored commented on   C
C       and ignored.                                                          C
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
	INTEGER*4     i, j, st,  lbuf, status, context
	INTEGER*4     maxtry,spos,s2pos,scom,ecom,lcom, asc, io_stat, cpos
	CHARACTER*50  command, response, fcommand
	CHARACTER*80  res_fname
	CHARACTER*120 buffer
	LOGICAL	      putstar,isdigit, Readef

C Get filename to beginning of fcommand
	i=5
	DO WHILE (fcommand(i:i).eq.' ' .and. i .lt. 50)
	  i=i+1
	ENDDO
	fcommand(1:)=fcommand(i:50)

C Open file.
 90	maxtry = 0
 100	OPEN (UNIT = 1, FILE= fcommand, STATUS='OLD',
	1    iostat=io_stat)
	IF (io_stat .ne. 0) THEN

	   context = 0
	   CALL FIND_FILE (fcommand, res_fname, context,status)
	   CALL FIND_FILE_END (context)
	   IF (status.eq.0) GOTO 990 !FILE NOT FOUND

	   GOTO 150		!FILE LOCKED BY ANOTHER USER
	ELSE
	   GOTO 180		!FILE OPEN OK
	ENDIF
 150	maxtry = maxtry + 1
	IF (READEF (contr_A)) THEN
	   CALL clref (contr_a)
	   CALL clref (hdw_event) !remote abort
	   GOTO 990
	ENDIF
	CALL PUTCONTR(' File in use. Retrying.')
	CALL libwait(10.,status) !wait 10 seconds and try again
	IF (maxtry .LT. 100) THEN
	   GOTO 100
	ELSE
	   OPEN (UNIT = 1, FILE= fcommand, STATUS='OLD',ERR=990)
	ENDIF

C       Check each line for *s and #. Message to user if found.
C       Check commas in each line to verify followed by digit. Message if not.
 180	putstar = .false.
 182	READ (1,fmt='(a)',end=185,err=990)buffer
	lbuf=len_trim(buffer)
	DO WHILE (buffer(1:1) .eq. '!') !ignore comment lines
	   READ (1,fmt='(a)',end=185,err=990)buffer
	   lbuf=len_trim(buffer)
	ENDDO
	spos = INDEX (buffer(1:lbuf), '*')
	ecom = INDEX (buffer(1:lbuf), '#')
	if (ecom.ne.0)then
	   call PUTCONTR(' # (execution terminator) found in line: ')
	   call PUTCONTR(buffer(1:lbuf))
	endif
	if (spos.ne.0)then
	   call PUTCONTR(' Star found in line: ')
	   call PUTCONTR(buffer(1:lbuf))
	   s2pos = INDEX (buffer(spos+1:lbuf), '*')
	   if (s2pos.ne.0 .or. .not.putstar) then
	      call PUTCONTR(' ERROR: Multiple * in command file')
	   endif
	   putstar=.false.	!star found in file
	endif
	cpos = INDEX(buffer(1:lbuf), ',') !check for commas
	DO WHILE (cpos .ne. 0)
	   if (cpos.ge.lbuf)goto 182 !, at end of line ok
	   cpos=cpos+1		!find next nonblank character:
	   DO WHILE (buffer(cpos:cpos).eq.' '.and.cpos .le.lbuf) 
	      cpos=cpos+1
	   ENDDO
	   IF (cpos.ge.lbuf)goto 182 !, at end of line ok
	   IF (.not. isdigit(buffer(cpos:cpos)))THEN
	      CALL PUTCONTR(
	1	   ' Verify commas used for parameters only in line:')
	      CALL PUTCONTR('  '//buffer(1:lbuf))
	      goto 182
	   ENDIF
	   spos=cpos		!remember offset in buffer
	   cpos = INDEX(buffer(spos:lbuf), ',')
	   if (cpos .ne. 0) cpos = cpos  + spos -1
	ENDDO
	goto 182

C  Return to beginning of file
 185	REWIND(1,err=990)
	
C       Read line into buffer and remove any * put in by user
 300	READ (1,fmt='(a)',end=500,err=990)buffer
	lbuf=len_trim(buffer)
	DO WHILE (buffer(1:1) .eq. '!') !ignore comment lines
	   READ (1,fmt='(a)',end=500,err=990)buffer
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

C       Find next command in buffer  
 400	IF (putstar) THEN	!add star at start of line if appropriate
	   buffer(1:)='*'//buffer(1:lbuf)
	   lbuf = lbuf + 1
	   putstar = .false.
	END IF			!putstar
	spos = INDEX (buffer(1:lbuf), '*') !remember where star is
	IF (spos .EQ. 0) THEN	!How can this happen? 
	   call PUTCONTR(' Rechecking: '//buffer(1:lbuf))
	   putstar = .true.
	   goto 400
	ENDIF
	scom=spos+1		!scom will mark start of command
	DO WHILE (buffer(scom:scom).eq.' ' .OR.	!skip ; # and space
	1    buffer(scom:scom).eq.'#' .OR. !ignore end for check
	2    buffer(scom:scom).eq.';' .AND. scom .LE. lbuf)
	   scom=scom+1
	ENDDO
	IF (scom.gt.lbuf) goto 300 !get next line
	ecom=scom		!ecom will mark end of command
	DO WHILE (buffer(ecom:ecom).ne.';' .and. !find end of command
	1  buffer(ecom:ecom).ne.'#' .and. ecom .LE. lbuf)
	   ecom=ecom+1
	ENDDO
	ecom=ecom-1
	command(1:)=buffer(scom:ecom)
	lcom=ecom-scom+1

C       Change command to uppercase and check it.
	DO i=1,lcom
	   asc = ichar(command(i:i))
	   IF (asc .GE. 97 .AND. asc .LE. 122) THEN !convert to uppercase
	      asc = asc - 32
	      command(i:i) = char(asc)
	   ENDIF
	ENDDO
	i = 1
	do while (command(i:i) .eq. ' ' .and. i .lt. lcom)
	   i = i + 1
	enddo
	IF (command(i:i+6) .eq. 'INCLUDE')GOTO 710 !include is ok
	CALL Parse_Check (command, *710)
 710	IF (READEF (contr_A)) THEN
	   CALL clref (contr_a)
	   CALL clref (hdw_event) !remote abort
	   CLOSE(1)
	   response=' Sequence check terminated by control A'
	   goto 996
	ENDIF

C       Set up rest of line so can move on to next command. 
	IF (ecom.lt.lbuf) THEN	!finish line
	   buffer(1:)=buffer(1:spos-1)//buffer(spos+1:ecom)//'*'//
	1	buffer(ecom+1:lbuf)
	   goto 400
	ELSE			!get next line
	   goto 300
	ENDIF

C       Close File
 500	CLOSE(1)



C--------------------------------WELL DONE ----------------------------------C

 140	response = 'Entire Command Sequence checked !'
	GOTO 996

C-------------------Handle file error-----------------------------------------C
 990	response = 'Error reading file: '//fcommand
	GOTO 996
 995	response = 'End of file found unexpectedly. # should be last command.'
 996	CALL PUTCONTR ( response)
	RETURN 1
	END      
