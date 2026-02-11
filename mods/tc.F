	MODULE tc
	character*2 tcsp2,tcsp2b,tcsp2c
	integer*2   tempchan  !channel assigned to temp controller
	integer*2   magchan   !channel assigned to mag controller
	integer*4   temptype  !0=unknown,1=old lakeshore,2=new lakeshore
	integer*4   magtype   !0=undefined,1=mcu,2=hp/ps,3=scmag
	real*4	    power     !power setting at start of scan
	logical     tinit_done,dbcurve,minit_done
	character*10 hdwname,snsname
	integer*4  maxpnts,hdwlong,snslong,magport
	parameter (maxpnts=100)
	real*4      hdwvolts(maxpnts),hdwtemp(maxpnts)
	real*4      snsvolts(maxpnts),snstemp(maxpnts)
	real*4      excitlr,rangelr
	real*4      hfconv  !Conv to gauss.
	integer*4   which_temp !which (1=sample,2=control,3=setpoint) to store
	integer*4   hfmode     !0=non persitant mode. 1= persistant mode
	real*4	    fieldmax   !maximum field to which magnet can be set
	real*4	    fieldlast   !last value to which magnet controller has been set
	real*4      rmag1    !variable for memorizing whether helium level to be checked
	real*4      sweeprate ! sweep rate for 12T magnet
	logical     persist   ! Are we in persistent mode?
	TYPE lr_coeffs
	  real*4   lotemp,hitemp,loz,hiz
	  integer*4 numb,start
	END TYPE
	TYPE(lr_coeffs) incfs(3),outcfs(3)
	LOGICAL	   tcareful
	integer*2  tempchan2  !channel assigned to 2nd temp controller
	integer*4  temptype2  !0=unknown,1=old lakeshore,2=new lakeshore
	logical    tinit_done2
	INTEGER*4  tmp_line2     ! RS232-line for 2nd temp-control
	INTEGER*4  samp_sensor, contr_sensor
	END MODULE tc
