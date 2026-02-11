#include "icp.h"
c     Sit in a loop, waiting for the temperature to creep within some
c     acceptable margin of error from the 
      subroutine wait_temp(error,polltime)

c  furnace id string
      use instr
      use tc
      use lia
      use tdb
      use iso_c_binding
      interface
#include "c_libwait.h"
      end interface
      real*4 sample, control, setpoint, error, polltime
      integer*4 status
      logical READEF

      CALL clref (contr_A)	
      if (polltime .lt. 60.0) polltime = 60.0
      if (error .lt. 1.0) error = 1.0
      call rdtemp(sample, control, setpoint)
      do while (abs(sample - setpoint) .gt. error) 
         IF (READEF(contr_a) ) goto 500
         call rdtemp(sample, control, setpoint)
         call euro_heat(control)
         call libwait(60.0,status)
      end do

 500  return
      end

C Set output heater power based on current temperature
      subroutine euro_heat(temp)
      use instr
      use tc
      use lia
      use tdb
      real*4 temp, output
      character*20 str20
      character*40 lrcommand, reply
      integer i,ios
      integer*2 lcom,lreply, termchar

c  furnace id string
      character*4	idstr/'0011'/

      output = 15.0
      if (temp .ge. 1300) then
         output = 85.0
      else if (temp .ge. 950) then
         output = 50.0
      else if (temp .ge. 700) then
         output = 30.0
      endif

      write(unit=str20,fmt='(i20)',iostat=ios) NINT(output)	
      i = 1
      do while(str20(i:i).eq.' ' .AND. i .LT. 20)
         i = i + 1
      enddo
      lrcommand(1:) = CHAR(04)//idstr//CHAR(02)//'Hi'//
     1     str20(i:20)//CHAR(03)
      lcom = 30 - i
      CALL FRN232(lrcommand,lcom,reply,lreply,termchar,.TRUE.) !add cksum
C      IF (termchar .NE. 6)call putcontr
C     1     (' err writing temp to controller')      
      
      end

      
