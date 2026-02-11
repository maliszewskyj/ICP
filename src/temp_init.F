#include "icp.h"
	SUBROUTINE TEMP_INIT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c    Set up RS232 lines to characteristics required by temperature	 C
c    controller.		          				 C
c    Assign channel to this line after giving process		         C
c    neccessary privilege						 C
c									 C
C    Gayle Greene, 1989							 C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	use tc
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_rs232_tc.h"
	end interface
c
c
	integer*4 status,tmpline
	character*4 templine


	if (temptype.eq.1) then
	   call RS232TC_INIT(300, 7, 2)  ! Lakeshore 82C
	else if (temptype .eq. 2) then
	   call RS232TC_INIT(300, 7, 2)  ! Lakeshore 93C
	else if (temptype .eq. 3) then
	   call RS232TC_INIT(300, 7, 1)  ! Lakeshore 330
	else if (temptype .eq. 4) then
	   call RS232TC_INIT(300, 7, 2)  ! LR400
	else if (temptype .eq. 5) then
	   call RS232TC_INIT(1200, 7, 2) ! Omega
	else if (temptype .eq. 6) then
	   call RS232TC_INIT(300, 7, 0)  ! NESLAB
	else if (temptype .eq. 7) then
	   call RS232TC_INIT(9600, 8, 0) ! LR700
	else if (temptype .eq. 8) then
	   call RS232TC_INIT( 300, 8, 0) ! New NESLAB
	else if (temptype .eq. 9) then
	   call RS232TC_INIT(1200, 7, 1) ! Omega 8500
	else if (temptype .eq. 10) then
	   call RS232TC_INIT(9600,7,1)   ! Lakeshore 340/331
	else if (temptype .eq. 11) then
	   call RS232TC_INIT(2400,7,2)   ! Eurotherm
	else if (temptype .eq. 15) then  
	   call RS232TC_INIT(9600, 8, 0) ! Oxford ITC-503
	else if (temptype .eq. 16) then  
	   call RS232TC_INIT(300, 8, 0)  ! LTC-21
	else if (temptype .eq. 17) then  
	   call RS232TC_INIT(9600, 8, 0) ! Shear cell
	else if (temptype .eq. 18) then  
	   call RS232TC_INIT(300, 7, 1)  ! Omega 6070
        else if (temptype .eq. 19) then
           call RS232TC_INIT(9600, 8, 0) ! 12 T Magnet
	else if (temptype .eq. 20) then
	   call RS232TC_INIT(9600, 8, 0) ! Omega CN3200 series
	else if (temptype .eq. 21) then
	   call RS232TC_INIT(9600, 8, 0) ! Cryocon model 34
        else if (temptype .eq. 22) then
           call RS232TC_INIT(9600, 8, 0) ! Thermo NC (Sushil)
	else if (temptype .eq. 23) then
	   call RS232TC_INIT(9600, 8, 0) ! Omega CNi series
	else if (temptype .eq. 24) then  
	   call RS232TC_INIT(9600, 7, 1) ! Lakeshore 370 Resistance Bridge
	else if (temptype .eq. 25) then
	   call RS232TC_INIT(9600, 8, 0) ! Petite Fleur bath
	else
	   call putcontr_nt(' Sorry. This controller not yet supported.')
	   return
	end if

	RETURN
	END
