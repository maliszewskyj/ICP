#include "icp.h"
c
	subroutine rdtemp(sample,control,setpoint)
c	Type temperature controller must be specified in typetemp which
c        is found in tc.cmn. Currently recognized types and their typetemp values:
c        Old Lakeshore (DRC 82C): 1
c	 New Lakeshore (DRC 93C): 2
c        330 AutoTuning:          3
c	 LR400                  : 4
c	 Omega			: 5
c        Neslab                 : 6
c	 LR700                  : 7
c        New Neslab		: 8
c	 Omega 8500		: 9
c        Lakeshore 340		:10
c	 Eurotherm Furnace	:11
c	 Digital Transducer Amp :12
c	 SR830   LockIn Amp     :13
c	 AD201			:14
c	 Oxford			:15
c	 LTC-21			:16
c	 SHEAR CELL		:17
c	 Omega 6070		:18
c        12T Magnet (Oxford)    :19
c        Omega 3200 series      :20
c        Cryocon Model 34       :21
c        Thermo NC              :22
c	 Omega CNi		:23
c	 Lakeshore 370		:24
c        Petite Fleur Bath      :25
c
	use instr
	use tc
	use lia
	use tdb
	use ad201
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	real*4 sample,control,setpoint,tempk,ohms_to_k,k_to_ohms, templ
	integer*4 itemp,ndigits,status,maxpower/5/
	real*4 rpar1(4)/90.,40.,20.,5./,rpar2(4)/40.,20.,10.,3./
	real*4 ltcpar1(5)/50.,20.,5.,0.,100./,ltcpar2(5)/10.,100.,15.,0.,10./
	character*1 cpar(4)/'P','D','I','R'/
	character*2 omstring(3)/'PB','RA','RE'/
	real*4 cvolts,hdw_t_v,sns_v_t
	logical notok,isdigit
	integer*4 getreply/1/,noreply/0/
c
	character*10 command,outstr
	character*20 omega_command, lkcommand
	character*35 reply
	integer*4 lcom,lreply,termchar
	integer*4 i, ios
	character*1 size

c  variables for lr400
	real*4   vranges(9)/.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   vexcit(6)/.06,.2,.6,2.0,6.0,20.0/
	logical  found
	integer*4 dat1,range,b1,b2,b3,s1,s3,s4,S5,s6,dvm

c  variables for lr700
	real*4   v7ranges(10)
     1		/.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./
	integer*4  ival
	logical quitloop,oktowrite
	character*11	str11

c  variables for lr400 and lr700 and oxford
	character*80 lrcommand,lrreply

	real*4 lrsetp,lr7setp
	common/lr400/lrsetp,lr7setp !used just to memorize last setpoint written

c  furnace id string
	character*4	idstr/'0011'/
c  variables for SR830
	real*4	rlia_xy
c  variables for ad201
	real*4  adv, adconst/.0000002980232/
	integer*4 outb(2),extccode/0/
c
c  read cryostat temp controller
	if (temptype.eq.4) then !LR400
	  lrcommand(1:)='P5XR0'
	  lcom = 5
	  call lr232(lrcommand,lcom,lrreply,lreply,getreply)
	  read(lrreply(1:lreply),fmt=*,err=96)B1
	  lrcommand(1:)='P4XR0'
	  lcom = 5
	  call lr232(lrcommand,lcom,lrreply,lreply,getreply)
	  read(lrreply(1:lreply),fmt=*,err=96)B2
	  lrcommand(1:)='P3XR0'
	  lcom = 5
	  call lr232(lrcommand,lcom,lrreply,lreply,getreply)
	  read(lrreply(1:lreply),fmt=*,err=96)B3
C  	  S1 = B1 .AND. 15
c	  S1 = MOD(B1,16)
	  s1 = iand(b1,15)
C	  S3 = B2 .AND. 15
c	  S3 = MOD(B2,16)
	  s3 = iand(b2,15)
C	  S4 = (B2 .AND. 240)/16
c	  S4 = MOD(INT(B2/16)*16,256)/16
	  s4 = iand(b2,240)/16
C	  S5 = B3 .AND. 15
c	  S5 = MOD(B3,16)
	  s5 = iand(b3,15)
C	  S6 = (B3 .AND. 240)/16
c	  S4 = MOD(INT(B3/16)*16,256)/16
	  s6 = iand(b3,240)/16
c	  type *,' s1: ',s1,' s3: ',s3,' s4: ',s4,' s5: ',s5,' s6: ',s6
	  DVM = S5 + S6 * 10 + S3 * 100 + S4*1000 + iand(s1,1) * 10000
c	  type *,' dvm: ',dvm
	  found = .false.
	  do i = 1,9
	    if (rangelr.eq.vranges(i))THEN
	      range=6-(i-1) !this is factor to divide by
	      if (range.eq.-2)range=-1
	      found = .true.
	      ival = i	      
	    endif
	  enddo

	  IF (found) then
c	    write(unit=str30,fmt=*)float(dvm)/(10.**range)
c	    call putcontr(' resistance read: '//str30)
	    IF (tdev%cfg .EQ. temptype)THEN !converting
	      sample= ohms_to_k(FLOAT(DVM)/(10.**range))
	    ELSE !using resistance
c	      IF  (ival .le. 3) then
c		i = range + 3
c	      ELSEIF (ival .le. 6) THEN
c		i = range
c	      ELSE
c		i = range - 3
c	      ENDIF
c	      sample= FLOAT(DVM)/(10.**i)
	      sample= FLOAT(DVM)/(10.**range)
	    ENDIF
c	    write(unit=str30,fmt=*)sample
c	    call putcontr(' converted to sample: '//str30)
	  ELSE
	    call putcontr(' range not found for sample temp')
	    sample = 0.
	  ENDIF
	  setpoint = lrsetp
	  control = sample !since cannot read control. Used in new_temp_field
	else if (temptype.eq.7) then !lr700
	  quitloop = .false.
	  oktowrite = .false.
	  do while (.not. quitloop) !loop to find & set correct range
	    quitloop = .TRUE. !quit is default. Only keep on if in wrong range & know it
	    lrcommand(1:)='G 7' !see if we are in correct range before reading
	    lcom = 3
	    call lr700_232(lrcommand,lcom,lrreply,lreply)
	    IF (lreply .NE. 0)THEN
	      read(unit=lrreply(1:lreply),fmt='(I3)',err=17)ival
c	      write(unit=str30,fmt=*,iostat=ios)ival
c	      CALL Putcontr(' err code: '//str30)
	      IF (btest(ival,6) .OR. btest(ival,7))THEN !we are not in correct range
	        IF (.not.oktowrite)THEN !need to find first range & read setpoint
	          found = .false.
	          do i = 1,10
	            if (rangelr.eq.v7ranges(i))THEN
	              range=i-1
	              found = .true.
	            endif
	          enddo
c		  write(unit=str30,fmt=*,iostat=ios)range
c		  CALL putcontr(' first range index found: '//str30)
	          oktowrite = .true.
	      	ENDIF !not oktowrite
		IF (range .LT. 9 .and. oktowrite)THEN !only continue if setpoint ok
	          range = range +1
		    IF (temptype .EQ. tdev%cfg)THEN !using defined sensor
	  	      tempk = k_to_ohms(lr7setp)
	  	      IF (tempk .EQ. -999.)THEN
	    	        call putcontr(' WARNING: err in converting setpoint')
		        GOTO 17
		      ENDIF
		    ELSE !using resistance
	  	      tempk = lr7setp
		    ENDIF
	  	      itemp = NINT(tempk*10.**(8-range))
	  	      write(unit=outstr(2:7),fmt='(I6.6)',iostat=ios)iabs(itemp)
	  	      IF (ios.NE.0)GOTO 17
	  	      IF (itemp .LT. 0)THEN
	                outstr(1:1) = '-'
	              ELSE
	                outstr(1:1) = '+'
	  	      ENDIF
	  	      str11='O R='//outstr(1:7) !resend setpoint
c		      call putcontr(' set point to be written: '//outstr(1:7))
	          ENDIF !found & range lt 9

	  	  write(unit=outstr(1:1),fmt='(i1)')range
	  	  lrcommand(1:)='R '//outstr(1:1)
	  	  lcom = 3
	  	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  	  IF (lrreply(1:2) .NE. 'OK')THEN
	    	    call putcontr(' Err writing new range to lr700')
		    call putcontr(' Check setpoint on controller. It may be zero!!!')
	    	    GOTO 17
	  	  ELSE
		    CALL Putcontr(' WARNING: Range changed')
	    	    rangelr = v7ranges(range)
	  	  ENDIF
	  	  lrcommand(1:13)=str11
	  	  lcom = 13
	  	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  	  IF (lrreply(1:2) .NE. 'OK')THEN
	            Call Putcontr(' Err rewriting setpoint! Look at controller.')
		    CALL Putcontr(' It is probably at zero!!!')
		  ELSE
		    CALL Putcontr(' Setpoint rewritten OK')
	  	  ENDIF
		  quitloop = .FALSE.
	      ENDIF !range problem since bit for 64 is set
	    ENDIF !got a reply (lreply ne 0)
	  ENDDO


 17	  lrcommand(1:)='G 0'
	  lcom = 3
	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  IF (lreply .NE. 0)THEN
c	    CALL Putcontr(' reply: '//lrreply(1:lreply))
	    read(unit=lrreply(1:lreply),fmt=*,err=96)templ !templ has Resistance
	    IF (lrreply(10:10) .EQ. 'M')THEN
	      templ = templ*.001
	    ELSEIF (lrreply(10:10) .EQ. 'K')THEN
	      templ = templ*1000
	    ENDIF
	  ENDIF
c	  write(unit=str30,fmt=*,iostat=ios)templ
c	  CALL Putcontr(' templ: '//str30)
	  IF (tdev%cfg .EQ. temptype)THEN !using defined sensor
	    sample= ohms_to_k(templ)
	  ELSE !using resistance
	    sample = templ
	  ENDIF
c	  write(unit=str30,fmt=*,iostat=ios)sample
c	  CALL Putcontr(' After conversion: '//str30)
	  setpoint = lr7setp
	  control = sample !since cannot read control. Used in new_temp_field
	else if (temptype.eq.5) then !old omega
	  control=0.0 !no control
	  omega_command='*:RPV0:' !get sample
	  lcom=7
	  call om232(omega_command,lcom,reply,lreply)
	  read(reply(6:9),fmt='(i4)',err=96)itemp
	  if (reply(5:5).eq.'-')then
	    sample=-float(itemp)/10.
	  else 
	    sample=float(itemp)/10.
	  endif
	  call libwait(.1,status)
	  omega_command='*:RCSP:'
	  lcom=7
	  call om232(omega_command,lcom,reply,lreply)
	  read(reply(6:9),fmt='(i4)',err=96)itemp
	  if (reply(5:5).eq.'-')then
	    setpoint=-float(itemp)/10.
	  else 
	    setpoint=float(itemp)/10.
	  endif
	else if (temptype.eq.6) then !Neslab
	  control=0.0 !no control
	  command='MHD' !set up handshake mode
	  lcom=3
	  call nl232(command,lcom,reply,lreply)
	  call libwait(.1,status)
	  command='RA'		!get sample temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  read(reply(1:LREPLY),fmt='(f6.1)',err=96)sample
	  call libwait(.1,status)
	  command='RD'		!get setpoint temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  read(reply(1:LREPLY),fmt='(f6.1)',err=96)setpoint
	else if (temptype.eq.8) then !NEW Neslab
	  control=0.0  !no control
	  sample =0.0  ! default "bad" value
	  setpoint=0.0 ! default "bad" value
	  command='RT'		!get sample temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  IF (lreply .le. 0) goto 96
	  read(reply(1:LREPLY),fmt=*,err=96)sample
	  call libwait(.1,status)
	  command='RS'		!get setpoint temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  IF (lreply .le. 0) goto 96
	  read(reply(1:LREPLY),fmt=*,err=96)setpoint
	elseif (temptype .eq. 1)then !older lakeshore
	  if (temptype.eq.1)then !DRC 82C 
	    command='WO'
	  else
	    command='W0'
	  endif 
	  lcom=2
	  call rd232(command,lcom,reply,lreply)
	  if (lreply.eq.0)then
c	    call putcontr(' no reply from temp controller')
	    sample=0.0
	  else
	    if (temptype.eq.1)then !old DRC 82C lakeshore or unknown
	      read (reply,fmt=*,err=96)sample,control,setpoint
	    else !DRC 93C lakeshore
	      read (reply,fmt=*,err=96)sample,control,setpoint
	    endif
	    IF (dbcurve) THEN !convert temp
	      cvolts = hdw_t_v(sample)
	      sample = sns_v_t(cvolts)
	      cvolts = hdw_t_v(control)
	      control = sns_v_t(cvolts)
	      cvolts = hdw_t_v(setpoint)
	      setpoint = sns_v_t(cvolts)
	    ENDIF !dbcurve
	  endif
        elseif (temptype .eq. 2)then !lakeshore 93C
	  sample   = 0.0
	  control  = 0.0
	  setpoint = 0.0

          lkcommand='WC'
	  lcom=2
	  call rd232(lkcommand,lcom,reply,lreply)
	  if (lreply.gt.0)then
	     if (reply(lreply:lreply).eq.'K') lreply = lreply -1
	     read(reply(1:lreply),fmt=*,err=96)control
	  endif

	  lkcommand='WS'
	  lcom=2
	  call rd232(lkcommand,lcom,reply,lreply)
	  if (lreply.gt.0)then
	     if (reply(lreply:lreply).eq.'K') lreply = lreply -1
	     read(reply(1:lreply),fmt=*,err=96)sample
	  endif

	  lkcommand='WP'
	  lcom=2
	  call rd232(lkcommand,lcom,reply,lreply)
	  if (lreply.gt.0)then
	     if (reply(lreply:lreply).eq.'K') lreply = lreply -1
	     read(reply(1:lreply),fmt=*,err=96)setpoint
	  endif

	  if (sample .ne. 0.0 .and.
	1      control .ne. 0.0 .and.
	1      setpoint .ne. 0.0 .and.
	2      dbcurve) THEN	!convert temp
	     cvolts = hdw_t_v(sample)
	     sample = sns_v_t(cvolts)
	     cvolts = hdw_t_v(control)
	     control = sns_v_t(cvolts)
	     cvolts = hdw_t_v(setpoint)
	     setpoint = sns_v_t(cvolts)
	  ENDIF			!dbcurve
	  
	elseif (temptype .eq. 3)then !lakeshore 330
	  lkcommand='SDAT?'
	  lcom=5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    sample=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)sample
	  endif
	  lkcommand='CDAT?'
	  lcom=5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    control=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)control
	  endif
	  lkcommand='SETP?'
	  lcom=5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    setpoint=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)setpoint
	  endif
	elseif (temptype .eq. 10)then !lakeshore 340
c	  lkcommand='SDAT?'
	  IF (samp_sensor .eq. 2) THEN
	    lkcommand='KRDG? B'
	  ELSEIF (samp_sensor .eq. 3) THEN
	    lkcommand='KRDG? C'
	  ELSEIF (samp_sensor .eq. 4) THEN
	    lkcommand='KRDG? D'
	  ELSE
	    lkcommand='KRDG? A'  !default
	  ENDIF
	  lcom=7
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    sample=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)sample
	  endif
	  IF (contr_sensor .eq. 1) THEN
	    lkcommand='KRDG? A'
	  ELSEIF (contr_sensor .eq. 3) THEN
	    lkcommand='KRDG? C'
	  ELSEIF (contr_sensor .eq. 4) THEN
	    lkcommand='KRDG? D'
	  ELSE
	    lkcommand='KRDG? B'  !default
	  ENDIF
	  lcom=7
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    control=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)control
	  endif
	  lkcommand='SETP? 1'
	  lcom=7
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
c	    type *,' no reply from temp controller'
	    setpoint=0.0
	  else
	    read (reply(1:lreply),fmt=*,err=96)setpoint
	  endif
	else if (temptype.eq.9) then !new omega
	  omega_command='#01R00' !get sample
	  lcom=6
	  call newom_232(omega_command,lcom,reply,lreply)
	  i = 9
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply-1),fmt=*,err=96)itemp
	  sample = itemp
	  control = sample !so can set temp in buffer
	  setpoint = 0.0 !no setpoint for now
	else if (temptype.eq.18) then !omega 6070
	  omega_command='#01R0' !get sample
	  lcom=5
	  call om232(omega_command,lcom,reply,lreply)
	  IF (lreply .lt. 0) THEN
	     reply(1:) = ' '
	     sample = -999
	     goto 96
	  ENDIF
c	  call putcontr(' sample reply in rdtemp: %'//reply(2:lreply)//'%' )
	  i = 7
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply-1),fmt=*,err=96)itemp
	  sample = itemp
	  call libwait(.1,status)
	  omega_command='#01R1' !get setpoint
	  lcom=5
	  call om232(omega_command,lcom,reply,lreply)
	  IF (lreply .lt. 0) THEN
	     reply(1:) = ' '
	     setpoint = -999
	     goto 96
	  ENDIF
c	  call putcontr(' setpoint reply in rdtemp: %'//reply(2:lreply)//'%' )
	  i = 7
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply-1),fmt=*,err=96)itemp
	  setpoint = itemp
	  control = sample !so can set temp in buffer
	else if (temptype.eq.11) then !furnace
	  sample = -999.
	  lrcommand(1:) = CHAR(04)//idstr//'PV'//CHAR(05)
	  lcom = 8
	  CALL frn232(lrcommand,lcom,reply,lreply,termchar,0) !no cksum
	  if (lreply .eq. 0)GOTO 96
	  read(reply(4:lreply-1),fmt=*,err=96)templ
	  sample = templ
	  control = sample !so can set temp in buffer
	  lrcommand(1:) = CHAR(04)//idstr//'SL'//CHAR(05)
	  lcom = 8
	  CALL frn232(lrcommand,lcom,reply,lreply,termchar,0) !no cksum
	  read(reply(4:lreply-1),fmt=*,err=96)templ
	  setpoint = templ
	elseif (temptype .eq. 12)then !tdboard
	  tdbpoint = tdbpoint + 1
	  tdbdata(tdbpoint) = -999. !default value to signal error
	  lrcommand='MSV?'
	  lcom=4
	  call tdb232(lrcommand,lcom,lrreply,lreply)
	  if (lreply.eq.0)then
	    CALL PUTCONTR(' no reply from TDBoard for MSV?')
	  else
	    read (lrreply(1:lreply),fmt=*,err=96)sample,tempk,templ
	    tdbdata(tdbpoint) = sample*tdbconv !or some function thereof
	    sample = tdbdata(tdbpoint)
	    control = sample
	    setpoint = 0.0
	  endif
	elseif (temptype .eq. 13)then !SR830
	  IF (prfpoint .LT. maxprf)prfpoint = prfpoint + 1 !overwrite last if exceed
	  rlia_x(prfpoint)    = -999. !default value to signal error
	  rlia_y(prfpoint)    = -999. !default value to signal error
	  rlia_of(prfpoint)   = -999. !default value to signal error
	  rlia_oa(prfpoint)   = -999. !default value to signal error
	  rlia_adc1(prfpoint) = -999. !default value to signal error
	  rlia_adc2(prfpoint) = -999. !default value to signal error
	  rlia_adc3(prfpoint) = -999. !default value to signal error
	  rlia_adc4(prfpoint) = -999. !default value to signal error
	  lrcommand='SNAP?1,2,5,6,9'
	  lcom=14
	  call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	  if (lreply.eq.0)then
	    CALL PUTCONTR(' no reply from SRLIA for X,Y,etc')
	  else
	    read (lrreply(1:lreply),fmt=*,err=96)rlia_x(prfpoint),
     1		rlia_y(prfpoint),rlia_adc1(prfpoint),rlia_adc2(prfpoint),
     1		rlia_of(prfpoint)
	    rlia_xy = rlia_x(prfpoint)**2 + rlia_y(prfpoint)**2
	    IF (rlia_xy .NE. 0.0)rlia_xy = SQRT(rlia_xy)
	  endif
	  call libwait(.1,status)
	  lrcommand='SLVL?'
	  lcom=5
	  call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	  if (lreply.eq.0)then
	    CALL PUTCONTR(' no reply from LIA for SLVL')
	  else
	    read (lrreply(1:lreply),fmt=*,err=96)rlia_oa(prfpoint)
	  endif
	  call libwait(.1,status)
	  setpoint = rlia_x(prfpoint) !arbitrarily picking which value
	  IF (scanlia .EQ. 0)THEN !nothing being scanned
	    control = rlia_xy 
	  ELSEIF (scanlia .EQ. 1)THEN !Oa scan
	    control = rlia_oa(prfpoint)
	  ELSEIF (scanlia .EQ. 2)THEN !OF scan
	    control = rlia_of(prfpoint)
	  ELSEIF (scanlia .EQ. 3)THEN !ADC4 (DAC) scan
	    control = rlia_adc1(prfpoint)
	  ENDIF
	  sample = control 
	elseif (temptype .eq. 14)then !AD201
	  IF (ad2point .LT. maxad2)ad2point = ad2point + 1 !overwrite last if exceed
	  do i = 1,adnumchan
	    advolts(i,ad2point) = -999. !default value to signal error
	  enddo
	  do i = 1,adnumchan
	     ival = chanarr(i)
CCCCCCCCCCCCCCCCC NICK FIX ME - probably not a problem	     
C	    outb(1) = char(Z1)
	    outb(2) = (ival*16) + extccode
	    ndigits = 4
	    call sendpacket(outb,lrreply,ndigits,noreply)
C	    outb(1) = char(Z81) !send read conversion command
C	    outb(2) = char(Z0)
	    ndigits = 4
	    lrreply(1:) = ' '
	    call sendpacket(outb,lrreply,ndigits,getreply)
	    if (ndigits .eq. 0)then
	      Call putcontr_nt(' no reply from AD201 when trying to read data')
	    elseif (ndigits .lt. 4)then
	      call putcontr_nt
     1		(' reply from AD201 too short when trying to read data')
	    else !have a reply
CCCCCCCCCCCCCCCC NICK FIX ME
C	      b1 =  iand( ichar(lrreply(2:2)), 'FF'X) 
C	      b2 =  iand( ichar(lrreply(3:3)), 'FF'X) 
C	      b3 =  iand( ichar(lrreply(4:4)), 'FF'X) 
	      dat1 = b1 + b2*256 + b3 * 65536
	      adv = float(dat1) * adconst
	      adv = adv * 2. - 5. !bipolar mode
	      advolts(i,ad2point) = adv
	    endif
	  enddo
	  setpoint = dat1 !arbitrarily picking which value
	  control = adv
	  sample = control
	else if (temptype.eq.15) then !oxford
	  lrcommand(1:)='R0' !read set point
	  lcom=2
	  call rs232tc_ox(lrcommand,lcom,lrreply,lreply)
	  IF (lreply .eq. 0)GOTO 96 !no reply
	  IF (lrreply(1:1) .ne. 'R') GOTO 96 !err in reply
	  read(unit=lrreply(2:lreply),fmt=*,err=96)setpoint
	  lrcommand(1:)='R' !now add which sample channel:
	  write(unit=lrcommand(2:2),fmt='(I1)',iostat=ios)NINT(excitlr)
	  lcom=2
	  call rs232tc_ox(lrcommand,lcom,lrreply,lreply)
	  IF (lreply .eq. 0)GOTO 96 !no reply
	  IF (lrreply(1:1) .ne. 'R') GOTO 96 !err in reply
	  read(unit=lrreply(2:lreply),fmt=*,err=96)sample
	  control = sample !so can set temp in buffer
	elseif (temptype.eq.16) then !ltc-21
	  lrcommand(1:)='QSAMP?1;'
	  lcom = 8
	  call ltc232(lrcommand,lcom,lrreply,lreply,getreply)
	  ival = INDEX(lrreply(1:lreply),'K')
	  if (ival .ne. 0) then
	    read(unit=lrreply(1:ival-1),fmt=*,err=96)templ
	    sample =templ
	  else
	    sample = 0.0 !signal error
	  endif
	  control = sample !so can set temp in buffer
	  lrcommand(1:)='QSETP?1;'
	  lcom = 8
	  call ltc232(lrcommand,lcom,lrreply,lreply,getreply)
	  ival = INDEX(lrreply(1:lreply),'K')
	  if (ival .ne. 0) then
	    read(unit=lrreply(1:ival-1),fmt=*,err=96)templ
	    setpoint =templ
	  else
	    setpoint = 0.0 !signal error
	  endif
	elseif (temptype.eq.17) then !shear cell
	  command(1:)='1S'  !speed
	  lcom = 2
	  call shear232(command,lcom,reply,lreply,getreply)
	  if (lreply .eq. 0) THEN
	   setpoint = -999. ! signal communication error
	   call putcontr(' Error communicating with Shear Cell')
	   control = -999.
	   sample = -999.
	   return
	  endif
	  i = 1
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	      i = i + 1
	  enddo
	  read(reply(i:lreply-1),fmt=*,err=26)control
	  sample=control
	  setpoint=control
	  command(1:)='1E'  !status
	  lcom = 2
	  call shear232(command,lcom,reply,lreply,noreply)
	  if (lreply .eq. 0) then
	    size = ' '
	  else
	    size=reply(1:1)
	  endif
	  if (size .eq. '+')then !running
	    setpoint = 1. !signal running
	  elseif (size .eq. '-')then !not running
	    setpoint = -1. !signal not running
	    control = 0.
	    sample = 0.
	  elseif (size .eq. '#')then !fault condition
	   call putcontr(' FAULT condition returned by Shear Cell')
	   setpoint = 0. !signal fault condition
	   control = -999.
	   sample = -999.
	  else !error communicating
	   setpoint = -999. ! signal communication error
	   call putcontr(' Error communicating with Shear Cell')
	   control = -999.
	   sample = -999.
	  endif 
	elseif (temptype .eq.19) then ! 12T magnet
	  command(1:) = 'get temp sample'
	  lcom = 15
	  call rs232tc_lv(command,lcom,reply,lreply,1)
	  if (lreply .eq. 0) then
	    setpoint = -999.0
	    call putcontr(' Error communicating with Oxford LV magnet')
	    control = -999.0
	    sample  = -999.0
	    return
	  else
	     read(reply(1:lreply),fmt=*,err=26)control
	     sample = control
	  endif
	elseif (temptype.eq.20) then ! Omega CN3200 series
	  call OMCN_ReadTemps(sample,control,setpoint,ios)
	elseif (temptype.eq.21) then ! Cryocon Model 34
	  sample   = -999.
	  control  = -999.
	  setpoint = -999.
	  IF (samp_sensor .eq. 2) THEN
	    lkcommand='INPUT B:TEMP?'
	  ELSEIF (samp_sensor .eq. 3) THEN
	    lkcommand='INPUT C:TEMP?'
	  ELSEIF (samp_sensor .eq. 4) THEN
	    lkcommand='INPUT D:TEMP?'
	  ELSE
	    lkcommand='INPUT A:TEMP?'  !default
	  ENDIF	   
	  lcom = 13
	  CALL rs232tc_cr(lkcommand,lcom,reply,lreply,1)
	  if (lreply .gt. 0) then
	     read(reply(1:lreply),fmt=*,err=26)sample
	  endif

	  IF (contr_sensor .eq. 2) THEN
	    lkcommand='INPUT B:TEMP?'
	  ELSEIF (contr_sensor .eq. 3) THEN
	    lkcommand='INPUT C:TEMP?'
	  ELSEIF (contr_sensor .eq. 4) THEN
	    lkcommand='INPUT D:TEMP?'
	  ELSE
	    lkcommand='INPUT A:TEMP?'  !default
	  ENDIF	   
	  lcom = 13
	  CALL rs232tc_cr(lkcommand,lcom,reply,lreply,1)
	  if (lreply .gt. 0) then
	     read(reply(1:lreply),fmt=*,err=26)control
	  endif
	  
	  lkcommand = 'HEATER:SETPT?'
	  lcom = 13
	  CALL rs232tc_cr(lkcommand,lcom,reply,lreply,1)
	  if (lreply .gt. 0) then
	     ! Need to strip off trailing units
	     if (reply(lreply:lreply).eq.'K') lreply=lreply-1
	     read(reply(1:lreply),fmt=*,err=26)setpoint
	  endif
	elseif (temptype.eq.22) then ! Thermo temperature bath
          ! "templ"  is external sensor
          ! "tempk" is internal sensor
	  call Thermo_ReadTemps(templ,tempk,setpoint,ios)
	  IF (samp_sensor.eq.2) THEN
	     sample = templ
	  ELSE
	     sample = tempk
          ENDIF
          IF (contr_sensor.eq.2) THEN
	     control = templ
          ELSE
	     control = tempk
	  ENDIF
	elseif (temptype.eq.23) then ! OMEGA CNi
	  call OMCNI_ReadTemps(sample,control,setpoint,ios)	
	elseif (temptype .eq.24) then ! Lakeshore 370
	  getreply = 1
	  sample   = -999.
	  control  = -999.
	  setpoint = -999.
	  ! Read setpoint
	  lkcommand='SETP?'
	  lcom = 5
	  CALL lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(reply(1:lreply),fmt=*,err=26)setpoint	  

	  lkcommand='RDGK? '
	  lcom = 8
	  write(unit=lkcommand(7:8),fmt='(i2)') samp_sensor
	  CALL lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(reply(1:lreply),fmt=*,err=26)templ
	  sample = templ
	  
	  lkcommand='RDGK? '
	  lcom = 8
	  write(unit=lkcommand(7:8),fmt='(i2)') contr_sensor
	  CALL lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(reply(1:lreply),fmt=*,err=26)templ
	  control = templ

        elseif (temptype.eq.25) then ! Petite Fleur Bath
	  sample   = -999.0
	  control  = -999.0
	  setpoint = -999.0
	  templ = -1

	  call PF_QUERY(templ,setpoint, sample, control,ios)

	else
	  call putcontr(' Read for that controller not yet implemented')
	endif
	return
 26	notok = .true.
	return
 96	call putcontr(' temp read fault: '//reply)
	return
	end


c check if communications ok with temp controller
	subroutine checktemp (notok)

	use instr
	use tc
	use lia
	use tdb
	use ad201
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	real*4 sample,tempk,templ
	integer*4 itemp,status,maxpower/5/
	real*4 rpar1(4)/90.,40.,20.,5./,rpar2(4)/40.,20.,10.,3./
	real*4 ltcpar1(5)/50.,20.,5.,0.,100./,ltcpar2(5)/10.,100.,15.,0.,10./
	real*4 rdummy(3)
	character*1 cpar(4)/'P','D','I','R'/
	character*2 omstring(3)/'PB','RA','RE'/
	logical notok,isdigit
	integer*4 getreply/1/,noreply/0/
c
	character*10 command
	character*20 omega_command, lkcommand
	character*35 reply
	integer*4 lcom,lreply,termchar
	integer*4 i, ios

c  variables for lr400
	real*4   vranges(9)/.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   vexcit(6)/.06,.2,.6,2.0,6.0,20.0/
	integer*4 b1

c  variables for lr700
	real*4   v7ranges(10)
     1	  /.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./
	integer*4  ival

c  variables for lr400 and lr700 and oxford
	character*80 lrcommand,lrreply

c  furnace id string
	character*4	idstr/'0011'/
c  variables for ad201
	real*4  adconst/.0000002980232/
	integer*4 extccode/0/

	notok=.false.
c  read cryostat temp controller
	if (temptype.eq.4) then !LR400
	  lrcommand(1:)='P5XR0'
	  lcom = 5
	  call lr232(lrcommand,lcom,lrreply,lreply,getreply)
	  read(lrreply(1:lreply),fmt=*,err=126)B1
	  notok=.false.
	else if (temptype.eq.7) then !lr700
	  lrcommand(1:)='G 0'
	  lcom = 3
	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  read(unit=lrreply(1:lreply),fmt=*,err=126)templ
	  notok=.false.
	else if (temptype.eq.5) then !omega
	  omega_command='*:RPV0:' !get sample
	  lcom=7
	  call om232(omega_command,lcom,reply,lreply)
	  read(reply(6:9),fmt='(i4)',err=126)itemp
	  notok=.false.
	else if (temptype.eq.6) then !Neslab
	  command='MHD' !set up handshake mode
	  lcom=3
	  call nl232(command,lcom,reply,lreply)
	  call libwait(.1,status)
	  command='RA'		!get sample temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  read(reply(1:LREPLY),fmt='(f6.1)',err=126)sample
	  notok=.false.
	else if (temptype.eq.8) then !New Neslab
	  command='RS'		!get sample temp
	  lcom=2
	  call nl232(command,lcom,reply,lreply)
	  IF (lreply .le. 0) goto 126
	  read(reply(1:LREPLY),fmt=*,err=126)sample
	  notok=.false.
	elseif (temptype .eq. 12)then !tdboard
	  lrcommand='MSV?'
	  lcom=4
	  call tdb232(lrcommand,lcom,lrreply,lreply)
	  if (lreply.eq.0)then
	    CALL PUTCONTR(' no reply from TDBoard for MSV?')
	    notok = .TRUE.
	  else
	    read (lrreply(1:lreply),fmt=*,err=126)sample,tempk,templ
	    notok=.false.
	  endif
	else if (temptype.eq.13) then !SR830
	  lrcommand='SLVL?'
	  lcom=5
	  call srlia232(lrcommand,lcom,lrreply,lreply,getreply)
	  if (lreply.eq.0)then
	    CALL PUTCONTR(' no reply from SR LIA')
	    notok = .TRUE.
	  else
	    read (lrreply(1:lreply),fmt=*,err=126)templ
	    notok=.false.
	  endif
	elseif (temptype.eq.1)then !older lakeshore
	  if (temptype.eq.1)then !DRC 82C
	    command='WO'
	  else
	    command='W0'
	  endif 
	  lcom=2
	  call rd232(command,lcom,reply,lreply)
	  if (lreply.eq.0)then
	    notok=.true.
	  else
	    if (temptype.eq.1)then !old DRC 82C lakeshore or unknown
C	      read (reply,fmt='(3(f6.0,2x)))',err=126)
C     1	        rdummy(1),rdummy(2),rdummy(3)
	    else !DRC 93C lakeshore
C	      read (reply,fmt='(3(f7.0,2x)))',err=126)
C     1	        rdummy(1),rdummy(2),rdummy(3)
	    endif
	    notok=.false.
	  endif
	elseif (temptype .eq. 2)then !lakeshore 93C
	  lkcommand='WS'
	  lcom=2
	  call rd232(lkcommand,lcom,reply,lreply)
	  if (lreply.eq.0)then
	     notok=.true.
	  else
	     if (reply(lreply:lreply).eq.'K') lreply=lreply-1
	     read(reply(1:lreply),fmt=*,err=126)sample
	     notok=.false.
	  endif
	elseif (temptype .eq. 3)then !lakeshore 330
	  lkcommand='SDAT?'
	  lcom=5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
	    notok = .true.
	  else
	    read (reply,fmt=*,err=126)sample
	    notok=.false.
	  endif
	elseif (temptype .eq. 10)then !lakeshore 340
	  IF (samp_sensor .eq. 2) THEN
	    lkcommand='KRDG? B'
	  ELSEIF (samp_sensor .eq. 3) THEN
	    lkcommand='KRDG? C'
	  ELSEIF (samp_sensor .eq. 4) THEN
	    lkcommand='KRDG? D'
	  ELSE
	    lkcommand='KRDG? A'  !default
	  ENDIF
	  lcom=7
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply.eq.0)then
	    notok = .true.
	  else
#if defined(VMS)
	    read (reply(1:lreply),fmt=*,err=126)sample
#else
	    read (reply,fmt=*,err=126)sample
#endif
	    notok=.false.
	  endif
	elseif (temptype .eq. 9)THEN !new omega
	  omega_command='#01R00' !get sample
	  lcom=6
	  call newom_232(omega_command,lcom,reply,lreply)
	  i = 9
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply-1),fmt=*,err=126)itemp
	  notok = .false.
	else if (temptype.eq.18) then !omega 6070
	  omega_command='#01R0' !get sample
	  lcom=5
	  call om232(omega_command,lcom,reply,lreply)
	  i = 7
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply-1),fmt=*,err=126)itemp
	  notok = .false.
	elseif (temptype.eq.11) then !furnace
	  lrcommand(1:) = CHAR(04)//idstr//'PV'//CHAR(05)
	  lcom = 8
	  CALL frn232(lrcommand,lcom,reply,lreply,termchar,0) !no cksum
	  read(reply(4:lreply-1),fmt=*,err=126)templ
	  notok = .false.
	elseif (temptype.eq.15) then !oxford
	  lrcommand(1:)='R' !now add which sample channel:
	  lrreply = ' '
	  write(unit=lrcommand(2:2),fmt='(I1)',iostat=ios)NINT(excitlr)
	  lcom=2
	  call rs232tc_ox(lrcommand,lcom,lrreply,lreply)
C CHANGE ME PLEASE
	  IF (lreply .eq. 0)GOTO 126 !no reply
	  print *,'lreply = ',lreply,'reply = ',lrreply
	  IF (lrreply(1:1) .ne. 'R') GOTO 126 !err in reply
	  print *,'reply(2:)=',lrreply(2:)
	  read(unit=lrreply(2:lreply),fmt=*,err=126)sample
	  notok = .false.
	elseif (temptype .eq. 16) then !ltc
	  lrcommand(1:)='QSAMP?1;'
	  lcom = 8
	  call ltc232(lrcommand,lcom,lrreply,lreply,getreply)
	  ival = INDEX(lrreply(1:lreply),'K')
	  if (ival .ne. 0) notok = .false.
	elseif (temptype .eq. 17) then !shear cell
	  command(1:)='1S'  !speed
	  lcom = 2
	  call shear232(command,lcom,reply,lreply,getreply)
	  if (lreply .ne. 0)notok = .false.
	elseif (temptype .eq.19) then ! 12T magnet
	  command(1:) = 'GETTEMP'
	  lcom = 7
	  call rs232tc_lv(command,lcom,reply,lreply,1)
	  if (lreply .ne. 0) notok = .false.
	elseif (temptype .eq.20) then ! Omega CN3200 series
	  call OMCN_ReadTemps(sample,tempk,templ,ios)
	  if (ios .eq. 0) notok = .false.
	elseif (temptype .eq.21) then ! Cryocon model 34
	  sample   = -999.
	  IF (samp_sensor .eq. 2) THEN
	    lkcommand='INPUT B:TEMP?'
	  ELSEIF (samp_sensor .eq. 3) THEN
	    lkcommand='INPUT C:TEMP?'
	  ELSEIF (samp_sensor .eq. 4) THEN
	    lkcommand='INPUT D:TEMP?'
	  ELSE
	    lkcommand='INPUT A:TEMP?'  !default
	  ENDIF	   
	  lcom = 13
	  CALL rs232tc_cr(lkcommand,lcom,reply,lreply,1)
	  if (lreply .ne. 0) notok=.false.
	elseif (temptype .eq.22) then ! Thermo RTE7
	  call THERMO_READTEMPS(sample, tempk,templ,ios)
	  if (ios .eq. 0) notok = .false.
	elseif (temptype .eq.23) then ! OMEGA CNi
	  call OMCNI_READTEMPS(sample, tempk,templ,ios)
	  if (ios .eq. 0) notok = .false.
	elseif (temptype .eq. 24) then ! Lakeshore 370
	  getreply=1
	  lkcommand='SETP?'
	  lcom = 5
	  CALL lk232(lkcommand,lcom,reply,lreply,getreply)
	  if (lreply .ne. 0) notok = .false.
	elseif (temptype .eq. 25) then ! Petite Fleur Bath
	  templ = -1

	  call PF_QUERY(templ,rdummy(1), rdummy(2), rdummy(3),ios)
	  if (ios.eq.0) notok=.false.
	else
	  call putcontr(' verification of that controller not yet implemented')
	endif
	goto 127
 126	notok=.true.
 127	continue
	return
	end
c
c  read controller parameters
	subroutine rdpar(rpar)
	use instr
	use tc
	use lia
	use tdb
	use ad201
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	integer*4 npar(6),itemp,ndigits,status,maxpower/5/
	real*4 rpar1(4)/90.,40.,20.,5./,rpar2(4)/40.,20.,10.,3./
	real*4 ltcpar1(5)/50.,20.,5.,0.,100./,ltcpar2(5)/10.,100.,15.,0.,10./
	real*4 rpar(6)
	character*1 cpar(4)/'P','D','I','R'/
	character*2 omstring(3)/'PB','RA','RE'/
	logical isdigit
	integer*4 getreply/1/,noreply/0/
c
	character*10 command
	character*20 omega_command, lkcommand
	character*35 reply
	integer*4 lcom,lreply
	integer*4 i, ios

c  variables for lr400
	real*4   vranges(9)/.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   vexcit(6)/.06,.2,.6,2.0,6.0,20.0/

c  variables for lr700
	real*4   v7ranges(10)
     1		/.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./

c  variables for lr400 and lr700 and oxford
	character*80 lrcommand,lrreply

	real*4 lrsetp,lr7setp
	common/lr400/lrsetp,lr7setp !used just to memorize last setpoint written

c  furnace id string
	character*4	idstr/'0011'/
c  variables for ad201
	real*4  adconst/.0000002980232/
	integer*4 extccode/0/

	if (temptype.eq.4 .or. temptype .eq. 6 .or. temptype .eq. 8
     1	     .or. temptype .eq.7 .or. temptype .eq. 9 .or.
     1	     temptype .eq. 11 .or. temptype .eq. 12 .or.
     1	     temptype .eq. 13 .or. temptype .eq. 17 .or.
     1	     temptype .eq. 19 )THEN
c	  call putcontr(' Params not implemented for this controller.')
	  do i=1,6
	    rpar(i)=0.0 !no need to use last three params
	  enddo
c	  not implemented	  
	else if (temptype.eq.5) then !omega
	  do i=4,6
	    rpar(i)=0.0 !no need to use last three params
	  enddo
	  do i=1,3
	    omega_command='*:R'//omstring(i)//'1:' !get parameter
	    lcom=7
	    call om232(omega_command,lcom,reply,lreply)
	    read(reply(6:9),fmt='(i4)',err=196)itemp
	    rpar(i)=float(itemp) 
	    call libwait(.1,status)
	  enddo
	else if (temptype.eq.18) then !omega 6070
	  do i=3,6
	    rpar(i)=0.0 !no need to use last three params
	  enddo
	  omega_command='#01R5'
	  lcom=5
	  call om232(omega_command,lcom,reply,lreply)
c	  call putcontr(' gain reply in rdtemp: '//reply(2:lreply))
	  i = 7
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply),fmt=*,err=196)itemp
	  rpar(1)=float(itemp) 
	  call libwait(.1,status)
	  omega_command='#01R4'
	  lcom=5
	  call om232(omega_command,lcom,reply,lreply)
c	  call putcontr(' rate reply in rdtemp: '//reply(2:lreply) )
	  i = 7
	  DO WHILE (i .LT. lreply .AND. .NOT. isdigit(reply(i:i)))
	    i = i + 1
	  ENDDO
	  read(reply(i:lreply),fmt=*,err=196)itemp
	  rpar(2)=float(itemp) 
	  call libwait(.1,status)
	else if (temptype.eq.3) then !lk330
	  do i=1,6
	    rpar(i)=0.0
	  enddo
	  lkcommand(1:)='GAIN?'
	  lcom = 5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(1)
	  lkcommand(1:)='RATE?'
	  lcom = 5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(2)
	  lkcommand(1:)='RSET?'
	  lcom = 5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(3)
	  lkcommand(1:)='HEAT?'
	  lcom = 5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(4)
	  lkcommand(1:)='TUNE?'
	  lcom = 5
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(5) !return tune in 5
	else if (temptype.eq.10) then !lk340
	  do i=1,6
	    rpar(i)=0.0
	  enddo
	  lkcommand(1:)='PID?'
	  lcom = 4
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(1),rpar(3),rpar(2)
	  lkcommand(1:)='RANGE?'
	  lcom = 6
	  call lk232(lkcommand,lcom,reply,lreply,getreply)
	  read(unit=reply(1:lreply),fmt=*,iostat=ios)rpar(4)
	elseif (temptype.le.2)then !lakeshore
	  if (temptype.eq.1)then !old DRC 82C lakeshore or unknown
	    command='W1'	
	  else
	    command='W3'
	  endif
	  lcom=2
	  call rd232(command,lcom,reply,lreply)
	  if (lreply.eq.0)then
c	    call setef(camerr) !set event flag for hdw error
	    goto 196 !type out message
	  else
	    if (temptype.eq.1)then !old DRC 82C lakeshore or unknown
	      read (reply,fmt=*,err=196)
     1	      (npar(i),i=1,6) !gain,rate,reset,range,%manpower,%power
	      do i=1,6
	        rpar(i)=float(npar(i))
	      enddo
	    else !DRC 93C lakeshore
C We could die here with this screwed up controller!!!!!!
C Death to FORTRAN formats!!!!!
	      read (reply,fmt=*,err=196)
     1	        rpar(1),rpar(2),rpar(3),npar(4),npar(6),npar(5)
	      rpar(4)=float(npar(4))
	      rpar(5)=float(npar(5))
	      rpar(6)=float(npar(6))
	    endif
	  endif
	elseif (temptype.eq.16) then !ltc-21
	  lrcommand(1:)='QPID?1;'
	  lcom = 7
	  call ltc232(lrcommand,lcom,lrreply,lreply,getreply)
	  do i=1,4
	    ndigits = INDEX(lrreply(1:lreply),';')
	    if (ndigits .eq. 0) goto 196
	    read(unit=lrreply(1:ndigits-1),fmt=*,err=196)rpar(i)
	    lrreply(1:) = lrreply(ndigits+1:lreply)
 	    lreply = lreply - ndigits
	  enddo
	else
	  call putcontr
     1		(' Reading params for that controller not yet implemented')
	endif
	return
 196	call putcontr(' temp read fault: '//reply)	
	return
	end
c
c  write setpoint temperature
	subroutine wrtemp(temp)

	use instr
	use tc
	use lia
	use tdb
	use ad201
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	real*4 temp,tempk,k_to_ohms, templ
	integer*4 itemp,ndigits,status,maxpower/5/
	character*4 integer_fmt
	real*4 rpar1(4)/90.,40.,20.,5./,rpar2(4)/40.,20.,10.,3./
	real*4 ltcpar1(5)/50.,20.,5.,0.,100./,ltcpar2(5)/10.,100.,15.,0.,10./
	character*1 cpar(4)/'P','D','I','R'/
	character*2 omstring(3)/'PB','RA','RE'/
	real*4 cvolts,hdw_v_t,sns_t_v
	real*4 setpoint,sample,control
	integer*4 getreply/1/,noreply/0/
c
	character*10 command,outstr
	character*20 omega_command, lkcommand, str20
	character*80 lvcommand,reply
	integer*4 lcom,lreply,termchar
	integer*4 i, ios
	character*1 size

c  variables for lr400
	real*4   vranges(9)/.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   vexcit(6)/.06,.2,.6,2.0,6.0,20.0/
	logical  found
	integer*4 range,rset,hb,lb

c  variables for lr700
	real*4   v7ranges(10)
     1		/.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./

c  variables for lr400 and lr700 and oxford
	character*80 lrcommand,lrreply
	real*4 lrsetp,lr7setp
	common/lr400/lrsetp,lr7setp !used just to memorize last setpoint written
c  furnace id string
	character*4	idstr/'0011'/

	if (temptype.eq.13)THEN !SR830
C         where temp is written depends on what is being scanned
	  IF (scanlia .EQ. 1)THEN !scan OA
	    IF (temp .LT. 0.004 .OR. temp .GT. 5.)THEN
	      CALL Putcontr
     1	      (' Osc Level not written. Out of range 0.004-5 Volts')
	      RETURN
	    ENDIF
	    lrcommand(1:)='SLVL'
	    write(unit=lrcommand(5:),fmt='(F5.3)',iostat=ios)temp
	    IF (ios .NE. 0)THEN
	      CALL Putcontr
     1		(' err converting value for output. Osc Lev not written')
	    ELSE
	      lcom=9
	      call srlia232(lrcommand,lcom,lrreply,lreply,noreply)
	    ENDIF
	  ELSEIF (scanlia .EQ. 2)THEN !scan OF
	    IF (temp .LT. .001 .OR. temp .GT. 102000.) THEN
	      CALL Putcontr
     1 	     (' Osc Freq Value not written. Out of range .001 - 102000')
	      RETURN
	    ENDIF
	    lrcommand(1:)='FREQ'
	    write(unit=lrcommand(5:),fmt='(f10.3)',iostat=ios)temp !band
	    IF (ios .NE. 0)THEN
	      CALL Putcontr
     1		(' err converting value for output. Osc Freq not written')
	    ELSE
	      lcom=14
	      call srlia232(lrcommand,lcom,lrreply,lreply,noreply)
	    ENDIF
	  ELSEIF (scanlia .EQ. 3)THEN !scan DAC on ADC1
	    IF (temp .LT. -10.5 .OR. temp .GT. 10.5)THEN
	      CALL Putcontr
     1		(' DAC not written. Out of acceptable range -10.5-10.5 Volts')
	      RETURN
	    ENDIF
	    lrcommand(1:)='AUXV1,'
	    write(unit=lrcommand(7:),fmt='(f7.3)',iostat=ios)temp
	    IF (ios .NE. 0)THEN
	      CALL Putcontr
     1		(' err converting value for output. DAC not written')
	    ELSE
	      lcom=13
	      call srlia232(lrcommand,lcom,lrreply,lreply,noreply)
	    ENDIF
	  ELSE !nothing being scanned
	    CALL Putcontr('Cannot write value to 5210. Nothing being scanned.')
	  ENDIF
	elseif (temptype.eq.4) then !LR400
c	    write(unit=str30,fmt=*)temp
c	    call putcontr(' temp to write: '//str30)
	    IF (tdev%cfg .EQ. temptype)THEN !using defined sensor so convert
	      tempk = k_to_ohms(temp)
	      IF (tempk .EQ. -999.)THEN
	        call putcontr(' err in converting temp to ohms')
	        RETURN
	      ENDIF
	    ELSE !using resistance
	      tempk = temp
	    ENDIF
c	    write(unit=str30,fmt=*)tempk
c	    call putcontr(' using ohms: '//str30)
	  RSET = INT(65536.* (tempk/rangelr) )
	  HB = RSET/256
	  IF (HB .GT. 255) HB = 255
	  LB = RSET - (256*HB)
	  IF (LB .GT. 255) LB = 255
	  write(unit=outstr(1:3),fmt='(i3.3)')LB
	  lrcommand(1:)='P2XD'//outstr(1:3)//'ZXP1XD2ZXH2X'
	  lcom = 19
 	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
	  write(unit=outstr(1:3),fmt='(i3.3)')HB
	  lrcommand(1:)='P2XD'//outstr(1:3)//'ZXP1XD3ZXH2X'
	  lcom = 19
	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
c	  lrcommand(1:)=rangecom(1:rangelcom)
c	  lcom=rangelcom
c	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
c	  lrcommand(1:)='P2XD16ZXP1XD1ZXH2X'
c	  lcom=18
c	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
	  lrcommand(1:)='P1XD4ZXH2X'
	  lcom = 10
	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
	  lrsetp = temp !memorize in degrees or resistance
	elseif (temptype.eq.7) then !LR700
	  IF (tdev%cfg .EQ. temptype)THEN !using defined sensor
	    tempk = k_to_ohms(temp)
	    IF (tempk .EQ. -999.)THEN
	      call putcontr(' err in converting temp to ohms')
	      RETURN
	    ENDIF
	  ELSE !using resistance
	    tempk = temp
	  ENDIF
	  found = .false.
	  do i = 1,10
	    if (rangelr.eq.v7ranges(i))THEN
	      range=i-1
	      found = .true.
	    endif
	  enddo
	  if (.not.found)then
	    call putcontr(' range error. use setp0')
	    return
	  endif
	  itemp = NINT(tempk*10.**(8-range))
	  write(unit=outstr(2:7),fmt='(I6.6)',iostat=ios)iabs(itemp)
	  IF (ios.NE.0)THEN
	    CALL Putcontr(' Err in calculating resistance output')
	    Return
	  ENDIF
	  IF (itemp .LT. 0)THEN
	    outstr(1:1) = '-'
	  ELSE
	    outstr(1:1) = '+'
	  ENDIF
	  lrcommand(1:)='O R='//outstr(1:7)
	  lcom = 11
	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  IF (lrreply(1:2) .NE. 'OK')THEN
	    Call Putcontr(' Err communicating with lr controller')
	  ELSE
	    lr7setp = temp !memorize in degrees or resistance
	  ENDIF
	else if (temptype.eq.6) then !Neslab
	  command='MHD' !set up handshake mode
	  lcom=3
	  call nl232(command,lcom,reply,lreply)
	  call libwait(.1,status)
	  command(1:2) = 'WD'
	  write(command(3:8),fmt='(F6.1)')temp
	  lcom=8
	  call nl232(command,lcom,reply,lreply)
	else if (temptype.eq.8) then !New Neslab
	  command(1:2) = 'SS'
	  write(command(3:8),fmt='(F6.2)')temp
	  lcom=8
	  do while (command(3:3).eq.' '.and.lcom.gt.3) !remove blanks
	    command(3:lcom-1)=command(4:lcom)
	    lcom=lcom-1
	  enddo
	  call nl232(command,lcom,reply,lreply)
	else if (temptype.eq.5) then !old omega
	  omega_command='*:WCSP/'
	  lcom=7
	  if (temp.lt.0)then
	    omega_command(7:7)='-'
	    lcom=7
	  endif
	  itemp=nint(abs(temp)*10.)
          ndigits = 1
          DO WHILE (10**ndigits .LE. itemp) !find no. of digits to write
            ndigits = ndigits + 1
          END DO
          WRITE(size, FMT= '(i1)') ndigits
          integer_fmt='(i'//size//')'
          WRITE(omega_command(lcom+1:lcom+ndigits), 
     1		FMT= integer_fmt) itemp 
 	  lcom=lcom+ndigits+1
	  omega_command(lcom:lcom)=':'
	  call om232(omega_command,lcom,reply,lreply)
	else if (temptype.eq.9) then !new omega
	  omega_command='#01M01 '
	  lcom=7
	  if (temp.lt.0)omega_command(7:7)='-'
	  itemp=nint(abs(temp))
          WRITE(omega_command(8:11), FMT= '(I4.4)') itemp 
 	  omega_command(12:12)='C'
	  lcom=12
	  call newom_232(omega_command,lcom,reply,lreply)
	  return
	else if (temptype.eq.18) then !omega 6070
	  omega_command='#01E1 '
	  lcom=6
	  if (temp.lt.0)omega_command(6:6)='-'
	  itemp=nint(abs(temp))
          WRITE(omega_command(7:10), FMT= '(I4.4)',err=86) itemp 
 	  omega_command(11:11)='C'
	  lcom=11
	  call om232(omega_command,lcom,reply,lreply)
	  return
	elseif (temptype.le.2)then !older lakeshore
	  IF (dbcurve) THEN !convert temp
	   cvolts = sns_t_v(temp)
	   temp   = hdw_v_t(cvolts)
	  ENDIF
	  command='+S'
	  lcom=7
	  if (temp.ge.100.)then
	    write(command(3:7),fmt='(F5.1)')temp
 	  else
	    if (temp.ge.10.)then
	      write(command(3:7),fmt='(F5.2)')temp
	    else
	      if (temp.ge.1.)then    
	        write(command(3:7),fmt='(F5.3)')temp
	      else
	        write(command(3:7),fmt='(F5.4)')temp
	      endif
	    endif
	  endif
	  call wr232(command,lcom)
	elseif (temptype.eq.3)then !lakeshore 330
	  if (temp.lt.0)THEN
	    templ = abs(temp)
	    lkcommand='SETP-'
	    lcom = 5
	  ELSE
	    templ = temp
	    lkcommand='SETP'
	    lcom = 4
	  ENDIF
	  if (templ.ge.100.)then
	    write(lkcommand(lcom+1:lcom+7),fmt='(F7.3)')templ
	    lcom = lcom + 7
 	  else
	    if (templ.ge.10.)then
	      write(lkcommand(lcom+1:lcom+6),fmt='(F6.3)')templ
	      lcom = lcom + 6
	    else
	      write(lkcommand(lcom+1:lcom+5),fmt='(F5.3)')templ
	      lcom = lcom + 5
	    endif
	  endif
	  call lk232(lkcommand,lcom,reply,lreply,noreply)
	elseif (temptype.eq.10)then !lakeshore 340
	  if (temp.lt.0)THEN
	    templ = abs(temp)
	    lkcommand='SETP 1,-'
	    lcom = 8
	  ELSE
	    templ = temp
	    lkcommand='SETP 1,'
	    lcom = 7
	  ENDIF
	  if (templ.ge.100.)then
	    write(lkcommand(lcom+1:lcom+7),fmt='(F7.3)')templ
	    lcom = lcom + 7
 	  else
	    if (templ.ge.10.)then
	      write(lkcommand(lcom+1:lcom+6),fmt='(F6.3)')templ
	      lcom = lcom + 6
	    else
	      write(lkcommand(lcom+1:lcom+5),fmt='(F5.3)')templ
	      lcom = lcom + 5
	    endif
	  endif
	  call lk232(lkcommand,lcom,reply,lreply,noreply)
	else if (temptype.eq.11) then !Furnace
	  write(unit=str20,fmt='(i20)',iostat=ios)NINT(temp)
	  i = 1
	  do while(str20(i:i).eq.' ' .AND. i .LT. 20)
	    i = i + 1
	  enddo
	  lrcommand(1:) = CHAR(04)//idstr//CHAR(02)//'SL'//
     1		str20(i:20)//CHAR(03)
	  lcom = 30 - i
	  CALL FRN232(lrcommand,lcom,reply,lreply,termchar,1) !add cksum
	  IF (termchar .NE. 6)call putcontr
     1		(' err writing temp to controller')
	else if (temptype.eq.15) then !oxford
	  str20(1:)=' '
	  call putmax(temp,str20,5,3)
	  lrcommand(1:) = 'T'//str20(1:5)
	  lcom = 6
	  call rs232tc_ox(lrcommand,lcom,lrreply,lreply)
	  IF (lreply .eq. 0)GOTO 86 !no reply
	  IF (lrreply(1:1) .ne. 'T') GOTO 86 !err in reply
	else if (temptype .eq. 16) then !ltc
	  lrcommand(1:) = 'SETP1,'
	  write(lrcommand(7:15),fmt='(F9.3)',err=86)temp
	  lrcommand(16:16)=';'
	  lcom=16
	  call ltc232(lrcommand,lcom,lrreply,lreply,noreply)
	else if (temptype .eq. 17) then !shear cell
	  if (temp .lt. 0. .or. temp .gt. 100.)THEN
	    call putcontr(' error. Shear cell speed must be 0-100')
	    RETURN
	  endif
	  if (NINT(temp) .eq. 0) THEN !stop shear cell
	    command(1:)='1I'
	    lcom = 2
	    call shear232(command,lcom,reply,lreply,noreply)
	    if (lreply .eq. 0 .or. reply(1:1) .ne. '*') then
	      call putcontr(' err stopping shear cell')
	    endif
	  endif
	  write(unit=integer_fmt(1:4),fmt='(i4.4)',err=86)NINT(temp)
	  command(1:) = '1S'//integer_fmt
	  lcom=6
	  call shear232(command,lcom,reply,lreply,noreply)
	  if (lreply .eq. 0 .or. reply(1:1) .ne. '*') then
	    call putcontr(' err writing speed to shear cell')
	    return
	  endif
	  if (NINT(temp) .gt. 0) THEN !start shear cell
	    command(1:)='1H'
	    lcom = 2
	    call shear232(command,lcom,reply,lreply,noreply)
	    if (lreply .eq. 0 .or. reply(1:1) .ne. '*') then
	      call putcontr(' err starting shear cell')
	    endif
	  endif
        else if (temptype .eq. 19) then ! 12T magnet
C	   call lv_ready
	   lvcommand(1:18) = 'set temp setpoint '
	   write(unit=lvcommand(19:26),fmt='(f8.2)',err=86) temp
	   lcom = 26
	   call rs232tc_lv(lvcommand,lcom,reply,lreply,1)
	   if (lreply .eq. 0) then
	     call putcontr(' err writing temperature to 12T magnet')
	     return
           endif
	else if (temptype .eq. 20) then ! Omega Cn3200 series
	   call OMCN_WRITETEMP(temp,lreply)
	   if (lreply .ne. 0) then
	      call putcontr(' err writing temperature to Omega controller')
	      return
           endif
	else if (temptype .eq. 21) then ! Cryocon model 34
	   str20(1:11) = 'HEAT:SETPT '
	   write(unit=str20(12:20),fmt='(f8.2)',err=86) temp
	   lcom = 20
	   call rs232tc_cr(str20,lcom,reply,lreply,0)
	   ! Should read back to verify
        else if (temptype .eq. 22) then ! Thermo temperature bath
	   call THERMO_WRITETEMP(temp,lreply)
	   if (lreply .ne. 0) then
	      call putcontr(' err writing temperature to Thermo controller')
	      return
           endif	
	else if (temptype .eq. 23) then ! OMEGA CNi Series
	   call OMCNI_WRITETEMP(temp,lreply)
	   if (lreply .ne. 0) then
	      call putcontr(' err writing temperature to OMEGA CNi controller')
	      return
           endif
	else if (temptype .eq. 24) then ! Lakeshore 370 Resistance Bridge
	   lkcommand = 'SETP '
	   write(unit=lkcommand(6:14),fmt='(f8.2)',err=86) temp
	   lcom = 14
	   call lk232(lkcommand,lcom,reply,lreply,noreply)
	else if (temptype .eq. 25) then ! Petite Fleur Bath
	   sample   = -999.0
	   control  = -999.0
	   setpoint = -999.0

	   call PF_QUERY(temp,setpoint, sample, control,ios)

	else
	  call putcontr(' writing temp to that controller not yet implemented') !lk330 & lr700
	endif
	return
 86	call putcontr(' error writing temp to controller')
	return
	end
c
c  write power  parameter to max (5)
	subroutine wrmaxpower
	use tc
	implicit none
	character*1 cpar(4)/'P','D','I','R'/
	character*10 command
	integer*4 maxpower/5/
	integer*4 lcom

	IF (temptype.le.2) then !older lakeshore
	  if (temptype.eq.1) then !old DRC 82C lakeshore or unknown
	    lcom=3  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')maxpower 
	    call wr232(command,lcom)
	  else !DRC 93C lakeshore
	    lcom=4  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')maxpower
	    call wr232(command,lcom)
	  endif
	endif	
	return
	end

c  write power  parameter to value at start of scan stored in power
	subroutine wrpower
	use tc
	implicit none
	character*1 cpar(4)/'P','D','I','R'/
	character*10 command
	integer*4 mpower
	integer*4 lcom

	IF (temptype.le.2) then !older lakeshore
	  mpower=nint(power)
	  if (temptype.eq.1) then !old DRC 82C lakeshore or unknown
	    lcom=3  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')mpower 
	    call wr232(command,lcom)
	  else !DRC 93C lakeshore
	    lcom=4  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')mpower
	    call wr232(command,lcom)
	  endif
	endif	
	return
	end

c  write controller parameters
	subroutine wrpar(icd,rpar)
	use tc
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	real*4 rpar1(4)/90.,40.,20.,5./,rpar2(4)/40.,20.,10.,3./
	real*4 ltcpar1(5)/50.,20.,5.,0.,100./,ltcpar2(5)/10.,100.,15.,0.,10./
	real*4 rpar(6)

c  variables for lr400
	real*4   vranges(9)/.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   vexcit(6)/.06,.2,.6,2.0,6.0,20.0/

c  variables for lr700
	real*4   v7ranges(10)
     1		/.002,.02,.2,2.,20.,200.,2000.,20000.,200000.,2000000./
	real*4   v7excite(7)/.02,.06,.2,.6,2.,6.,20./

	integer*4 icd,dat1,excite,i,ios
	character*10 command,outstr
	character*80 lrcommand,lrreply
	character*20 omega_command, lkcommand
	character*35 reply
	integer*4 lcom,lreply,status,range,ndigits,itemp
	integer*4 getreply/1/,noreply/0/
	integer*4 err
	character*1 cpar(4)/'P','D','I','R'/
	character*2 omstring(3)/'PB','RA','RE'/
	character*1 size
	character*4 integer_fmt
	logical  found

	if (icd.eq.1)then !then take default set 1
	  if (temptype .eq. 16) then !ltc
	    do i=1,5
	      rpar(i)=ltcpar1(i)
	    enddo
	  else
	    do i=1,4
	      rpar(i)=rpar1(i)
	    enddo
	  endif
	else
	  if (icd.eq.2)then !then use default set 2
	    if (temptype .eq. 16) then !ltc
	      do i=1,5
	        rpar(i)=ltcpar2(i)
	      enddo
	    else
	      do i=1,4
	        rpar(i)=rpar2(i)
	      enddo
	    endif
	  endif
	endif
	if (temptype.eq.4) then !LR400
c	  rpar(1) has range, rpar(2) has excitation
	  found = .false.
	  do i = 1,9
	    if (rpar(1).eq.vranges(i))THEN
	      range=i-1
	      if (range.eq.8)range=15
	      found = .true.
	    endif
	  enddo
	  if (.not.found)then
	    call putcontr(' range invalid for lr400')
	    return
	  endif
	  found = .false.
	  do i = 1,6
	    if (rpar(2).eq.vexcit(i))THEN
	      excite=(i-1)*16
	      found = .true.
	    endif
	  enddo
	  if (.not.found)then
	    call putcontr(' excitation value invalid for lr400')
	    return
	  endif
	  dat1 = range + excite
	  IF (dat1.lt.10)then
	    write(unit=outstr(1:1),fmt='(i1)')dat1
	    lrcommand(1:)='P2XD'//outstr(1:1)//'ZXP1XD0ZXH2X'
	    lcom = 17
	  else
	    write(unit=outstr(1:2),fmt='(i2)')dat1
	    lrcommand(1:)='P2XD'//outstr(1:2)//'ZXP1XD0ZXH2X'
	    lcom = 18
	  endif
	  call lr232(lrcommand,lcom,lrreply,lreply,noreply)
	  excitlr = rpar(2)
	  rangelr = rpar(1)
c	    rangecom(1:)=lrcommand !memorize range command so send everytime
c	    rangelcom=lcom
	ELSEIF (temptype.eq.7) then !LR700
c	  rpar(1) has range, rpar(2) has excitation
	  found = .false.
	  do i = 1,10
	    if (rpar(1).eq.v7ranges(i))THEN
	      range=i-1
	      found = .true.
	    endif
	  enddo
	  if (.not.found)then
	    call putcontr(' range invalid for lr700')
	    return
	  endif
	  found = .false.
	  do i = 1,7
	    if (rpar(2).eq.v7excite(i))THEN
	      excite=i-1
	      found = .true.
	    endif
	  enddo
	  if (.not.found)then
	    call putcontr(' excitation value invalid for lr400')
	    return
	  endif
	  write(unit=outstr(1:1),fmt='(i1)')range
	  lrcommand(1:)='R '//outstr(1:1)
	  lcom = 3
	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  IF (lrreply(1:2) .NE. 'OK')THEN
	    call putcontr(' Err writing params to lr700')
	    return
	  ELSE
	    rangelr = rpar(1)
	  ENDIF
	  write(unit=outstr(1:1),fmt='(i1)')excite
	  lrcommand(1:)='E '//outstr(1:1)
	  lcom = 3
	  call lr700_232(lrcommand,lcom,lrreply,lreply)
	  IF (lrreply(1:2) .NE. 'OK')THEN
	    call putcontr(' Err writing params to lr700')
	    return
	  ELSE
	    excitlr = rpar(2)
	  ENDIF
c	  IF (rpar(3).EQ. 1.)THEN !used to have filter time
c	    outstr(1:1)='0' !1 sec
c	  ELSEIF (rpar(3).EQ. 3.)THEN
c	    outstr(1:1)='1' !3 sec
c	  ELSEIF (rpar(3).EQ. 10.)THEN
c	    outstr(1:1)='3' !10 sec
c	  ELSE
c	    call putcontr(' invalid filter param')
c	  ENDIF
c	  lrcommand(1:)='F '//outstr(1:1)
c	  lcom = 3
c	  call lr700_232(lrcommand,lcom,lrreply,lreply)
c	  IF (lrreply(1:2) .NE. 'OK')THEN
c	    call putcontr(' Err writing params to lr700')
c	    return
c	  ENDIF
	else if (temptype .EQ. 3) then !LK330
	    lkcommand(1:)='TUNE' !set autotuning to proper state
	    write(UNIT=lkcommand(5:5),FMT='(I1)',iostat=ios)NINT(rpar(5))
	    lcom = 5
	    CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    lkcommand(1:)='RANG' !write range                   RANG
	    write(UNIT=lkcommand(5:5),FMT='(I1)',iostat=ios)NINT(rpar(4))
	    lcom = 5
	    CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    IF (rpar(5) .LT. 3.)THEN !write rate              RATE
	      lkcommand(1:)='RATE' !write rate
	      write(UNIT=lkcommand(5:7),FMT='(I3.3)',iostat=ios)NINT(rpar(2))
	      lcom = 7
	      CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    ENDIF  
	    IF (rpar(5) .LT. 2.)THEN !write reset             RESET
	      lkcommand(1:)='RSET' !write reset
	      write(UNIT=lkcommand(5:7),FMT='(I3.3)',iostat=ios)NINT(rpar(3))
	      lcom = 7
	      CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    ENDIF  
	    IF (rpar(5) .LT. 1.)THEN !write gain              GAIN
	      lkcommand(1:)='GAIN' !write gain
	      write(UNIT=lkcommand(5:7),FMT='(I3.3)',iostat=ios)NINT(rpar(1))
	      lcom = 7
	      CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    ENDIF  
	else if (temptype .EQ. 10) then !LK340
	    lkcommand(1:)='PID 1,' !
	    lcom = 6
	    write(UNIT=lkcommand(lcom+1:lcom+3),FMT='(I3.3)',iostat=ios)
     1		NINT(rpar(1)) !Gain
	    lcom = lcom + 4
	    lkcommand(lcom:lcom) = ','
	    write(UNIT=lkcommand(lcom+1:lcom+3),FMT='(I3.3)',iostat=ios)
     1		NINT(rpar(3)) !Reset
	    lcom = lcom + 4
	    lkcommand(lcom:lcom) = ','
	    write(UNIT=lkcommand(lcom+1:lcom+3),FMT='(I3.3)',iostat=ios)
     1		NINT(rpar(2))  !Rate
	    lcom = lcom + 3
	    CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	    lkcommand(1:)='RANGE' !write range                   RANGE
	    write(UNIT=lkcommand(5:5),FMT='(I1)',iostat=ios)NINT(rpar(4))
	    lcom = 6
	    CALL lk232(lkcommand,lcom,reply,lreply,noreply)
	else if (temptype.eq.6 .OR. temptype .EQ. 9 .OR.
     1		 temptype .eq. 11 .or. temptype .eq. 12 .or. 
     1		 temptype .eq. 13 .or. temptype .eq. 17) then !Neslab & LK330 & New Omega & furnace
	  call putcontr(' Params not implemented on this temp controller')
	else if (temptype.eq.5) then !omega
	  do i=1,3
	    omega_command='*:W'//omstring(i)//'1/' !get param
	    itemp=nint(rpar(i))
	    lcom=7
            ndigits = 1
            DO WHILE (10**ndigits .LE. itemp) !find no. of digits to write
              ndigits = ndigits + 1
            END DO
            WRITE(size, FMT= '(i1)') ndigits
            integer_fmt='(i'//size//')'
            WRITE(omega_command(lcom+1:lcom+ndigits), FMT= integer_fmt) itemp 
 	    lcom=lcom+ndigits+1
	    omega_command(lcom:lcom)=':'
	    call om232(omega_command,lcom,reply,lreply)
	    call libwait(.1,status)
	  enddo
	else if (temptype.eq.18) then !omega 6070
	    omega_command='#01E5 ' !gain
	    itemp=nint(rpar(1))
            WRITE(omega_command(7:10), FMT= '(I4.4)',err=86) itemp 
	    omega_command(11:11)=' ' !must terminate with a space
	    lcom=11
	    call om232(omega_command,lcom,reply,lreply)
	    call libwait(.1,status)
	    omega_command='#01E4 ' !rate/reset
	    itemp=nint(rpar(2))
            WRITE(omega_command(7:10), FMT= '(I4.4)',err=86) itemp 
	    omega_command(11:11)=' ' !must terminate with a space
	    lcom=11
	    call om232(omega_command,lcom,reply,lreply)
	ELSEIF (temptype.le.2) then !older lakeshore
	  if (temptype.eq.1) then !old DRC 82C lakeshore or unknown
	    lcom=3  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')nint(rpar(4))
	    call wr232(command,lcom)
	    lcom=4
	    do i=1,3
	      command='+'//cpar(i)
	      write(command(3:4),fmt='(i2)')nint(rpar(i))
	      call wr232(command,lcom)
 	    enddo
	else !DRC 93C lakeshore
	    lcom=4  !write parameter 4 first
	    command='+'//cpar(4)
	    write(command(3:3),fmt='(i1)')nint(rpar(4))
	    call wr232(command,lcom)
	    lcom=5
	    do i=1,3
	      command='+'//cpar(i)
	      if (rpar(i).lt.1.0)then
	        write(command(3:5),fmt='(f3.2)')rpar(i)
	      else
	        if (rpar(i).lt.10.0) then
		  write(command(3:5),fmt='(f3.1)')rpar(i)
	        else
		  write(command(3:5),fmt='(f3.0)')rpar(i)
	        endif
	      endif
	      call wr232(command,lcom)
 	    enddo
	  endif
	else if (temptype .EQ. 16) then !LTC
	    lrcommand(1:)='SPID1,' !
	    lcom = 6
	    do i = 1,5
	      write(UNIT=lrcommand(lcom+1:lcom+5),FMT='(I5)',iostat=ios)
     1		NINT(rpar(i)) 
	      lcom = lcom + 6
	      lrcommand(lcom:lcom) = ','
	    enddo
	    lrcommand(lcom:lcom)=';'
	    call ltc232(lrcommand,lcom,lrreply,lreply,noreply)
	else if (temptype .eq. 20) then ! Omega 3200 series
	    call OMCN_PIDR(rpar(1),rpar(3),rpar(2),rpar(4),err)
	ELSE
	  call putcontr
     1		(' Writing params for that controller not yet implemented')
	endif	
	return
 86	call putcontr(' error writing temp to controller')
	return
	end

	subroutine wrramp(ramp)
	use tc
	character*80 command, reply
	integer*4 lcom, lreply, noreply/0/
	real*4 ramp

	if (temptype .EQ. 10) then ! Lakeshore 340
	   if ((ramp .lt. 0) .or. (ramp .gt. 999)) goto 87
	   command(1:) = 'RAMP 1, 1,'
	   write(UNIT=command(11:17),fmt='(F6.2)')ramp
	   lcom = 17
	   CALL lk232(command,lcom,reply,lreply,noreply)
	else
	   call putcontr(' Writing ramp for that controller not yet implemented')
	endif
	return
 87	call putcontr(' error writing ramp to controller')
	return
	end

c
	subroutine lr_initsetp
c	  initialize setpoint to unknown flag
	implicit none

	real*4 lrsetp,lr7setp
	common/lr400/lrsetp,lr7setp !used just to memorize last setpoint written

	lrsetp = -999.
	lr7setp = -999.

	return
	end

	! Make sure 
	subroutine lv_ready_f
	use instr
	use tc
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
#include "c_libwait.h"
#include "c_debugval.h"
	end interface
	logical readef, finished, firstloop
	integer status, dbg
	character*80 command, reply
	integer*4 lcom, lreply

	finished=.false.
	firstloop=.true.
	status=0	
	CALL DEBUGVAL(dbg)
	if (dbg.ne.0) print *,'lv_ready'
	DO WHILE (.not.finished)
	   CALL DEBUGVAL(dbg)
	   if (readef(contr_a)) return
	   CALL LIBWAIT(2.0,status)
	   command(1:16)='get field status'
	   lcom=16
	   call rs232tc_lv(command,lcom,reply,lreply,1)
	   IF (lreply .gt. 0) THEN
	      status = 0
	      lcom=0
	      if (dbg.ne.0) print *,'lreply=',lreply
	      if (dbg.ne.0) print *,'reply =',reply(1:lreply),' lreply=',lreply
	      lcom = INDEX(reply(1:lreply),'COMPLETE')
	      if (dbg.ne.0) print *,'Matched =',lcom
	      IF (lcom .gt. 0) THEN
	      	 finished = .true.
		 return
	      ENDIF
C	      IF (lcom .eq.0 .and. lreply .gt. 3) THEN
C	      	 if (reply(1:1) .eq. 'C') then
C		   if (reply(2:2) .eq. 'O') then
C		     if (reply(3:3) .eq. 'M') then
C		        lcom = 1
C                     endif
C		   endif
C	        endif
C		IF (lcom .gt. 0) finished=.true.
C	      ENDIF
	   ENDIF
	enddo
	end
