#include "icp.h"
      SUBROUTINE ArchFile(file_name,lname)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     C
C     C
C     Gayle Greene, November 1996					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use ISO_C_BINDING
      IMPLICIT     none
      interface
#include "c_libwait.h"
      end interface

C     Calling parameters
      CHARACTER*(*)   file_name !name of file to be added to archive list
      INTEGER*4	lname           !length of name in name

      INTEGER*4	ios,right,status,notries,openfai/30/
      REAL*4		countr/1./,fenergy/0./
      CHARACTER*90	str90
      CHARACTER*28    sys_time

      INTEGER*4     context
      CHARACTER*80  res_fname
      CHARACTER*80  GETHOME

C     See if file was ever made. If not, return.

      context = 0
      Call Getdir(str90,right)
      CALL FIND_FILE (File_Name(1:lname), res_fname, context,status)
      CALL FIND_FILE_END (context)
      if (status.eq.0) RETURN   !no file found with that name

C     Get current time
      CALL DATE_TIME (sys_time)
C     open monitor.rec
      notries = 10
      OPEN (UNIT=2, FILE=MONITOR_REC, FORM= 'FORMATTED',
     $     ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
      DO WHILE (ios .EQ. openfai .AND. notries .GT. 0)
         notries = notries - 1
         CALL libwait(.2,status)
         OPEN (UNIT=2, FILE=MONITOR_REC, FORM= 'FORMATTED',
     $        ACCESS= 'APPEND', STATUS='UNKNOWN',IOSTAT=ios)
      ENDDO
      IF (ios .NE. 0) GOTO 210  !error opening file
C     add filename to monitor.rec
      write(unit=2,fmt='(1x,a17,1x,f10.0,1x,f10.2,1x,a,1x,a)',err=209)
     1     sys_time(1:17),
     1     countr,fenergy,'meV','  Start of Run'
      write(unit=2,fmt='(1x,a,1x,a,1x,a)',err=209)
     1     sys_time(1:17),' Created: ',str90(1:right)//file_name(1:lname)
      write(unit=2,fmt='(1x,a17,1x,a)',err=209)sys_time(1:17),
     1     'End of Run'
C     close monitor.rec
      close(unit=2)

      RETURN

C     Error treatments
 209  close(unit=2)
 210  CALL Putcontr(' Error adding file '//file_name(1:lname)//
     1     ' to archive list')
      RETURN

      END
