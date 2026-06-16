#include "icp.h"
	SUBROUTINE InstrIni
C	*******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	                                                                      C
C	Routine to initialize instrument and hardware control parameters      C
C	and global flags.                                                     C
C	Called from: ICP_MAIN                                                 C
C	                                                                      C
C	Beat Schmid, July 1989                                                C
C	Gayle Greene: misc modifications. Latest: 1/94                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use motors
	use histo
	use smg_prep
	use polar
	use tc
	use refl
	use xray
	use focus
	use zcoeffs
	use dartz
	use hwlong
	use asd 
	use tc_careful
	use globals
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_rs232_acs.h"
#include "c_pi.h"
#include "c_ez.h"
#include "c_pico.h"
#include "c_vme.h"
#include "c_zeta.h"
#include "c_libwait.h"
#include "c_gpiben.h"
#include "c_gpibct.h"
#include "c_center.h"
#include "c_amscaler.h"
	   subroutine RS232_CONF() bind(C,name="RS232_CONF")
	   use iso_c_binding
	   end subroutine RS232_CONF
	   subroutine UDPINIT() bind(C,name="UDPINIT")
	   use iso_c_binding
	   end subroutine UDPINIT
	   subroutine LXGETSTRING_INIT() bind(C,name="LXGETSTRING_INIT")
	   use iso_c_binding
	   end subroutine LXGETSTRING_INIT
	end interface
	INTEGER*4    i,  contr_A_offs /79/, pause_offs /14/
	INTEGER*4    hdw_event_offs /64/, camerr_offs /32/, context, nowait/1/
	INTEGER*4    status, ios, stop_offs/8/, centered, j, l
	LOGICAL      privat, errflag, updatefetch
	REAL*4       zero /0./,data
	CHARACTER    dummy
	CHARACTER*80 ftmp
	CHARACTER*80 buffer, syscmd
	CHARACTER*4  str4
  	integer		retn,system
	CHARACTER*80  gethome

	integer*4	m(MAXMOTS),
	1    dbase(MAXMOTS), dramp(MAXMOTS), dtop(MAXMOTS),
	2    dpulse(MAXMOTS), dback(MAXMOTS), dmerr(MAXMOTS),
	3    dmottype(MAXMOTS)
	real*4		dvmot1(MAXMOTS),dvmot2(MAXMOTS),dvmot3(MAXMOTS)


	TYPE(i_buf) ibuf
	TYPE(q_buf) qbuf
	TYPE(t_buf) tbuf
	TYPE(b_buf) bbuf
	TYPE(r_buf) rbuf
	TYPE(x_buf) xbuf
	TYPE(d_buf) dbuf

c  	system_type = linux !cpp seems to replace linux with 1 !!!
	system_type = 2
  	rkmot = 3
        call sethome
	call LXGETSTRING_INIT	!read/initialize command history
C       initialization of common variables
c       instr.cmn
	tdev%cfg = 1
	tdev%mot = 0
	vmedata = .FALSE.
	amscaler= .FALSE.
	trashmode = .FALSE.
	gpibenet = .FALSE.
	aimpsd = .FALSE.
	private = .FALSE.
	dmpscl  = .FALSE.
c       polar.cmn:
	do i = 1,6		!for polar.cmn
	   fullamp(i) = 6.
	   F_on(i)%cur = 0.
	   F_on(i)%engy = 13.7
	enddo
c       refl.cmn
	PGlist(1)= 1
	PGlist(2)= 2
	PGlist(3)= 4
	PGlist(4)= 6
	PGlist(5)= 7
	PGlist(6)= 9
	PGlist(7)= 10
	PGlist(8)= 11
	PGlist(9)= 1
	PGmove = 8
	Belist(1) = 1
	Belist(2) = 2
	Belist(3) = 5
	Belist(4) = 7
	Belist(5) = 9
	Belist(6) = 10
	Belist(7) = 11
	Belist(8) = 1
	Bemove = 7
    	slitlist(1)= 8 
	slitlist(2)= 16
	slitlist(3)= 17
	slitlist(4)= 18
c       tc.cmn
	hfconv = 1.
	fieldmax = 90.
	fieldlast = -999.

c       histo.cmn
	nocells = 256

c       asd.cmn
	fpwindow = 0.0

	call lr_initsetp

C-----------------READ INSTRUMENT AND HARDWARE PARAMETERS -------------------C

	OPEN (UNIT= 1, FILE= INSTR_CFG, STATUS= 'OLD', ACCESS= 
	1    'SEQUENTIAL', FORM= 'FORMATTED', ERR= 100)

	READ (1, FMT= '(a)') header ! Instrument
	READ (1, FMT= *) nsta,	! Station number
	1    mots,		! Number of motors
	2    mon_add,		! CAMAC module address of preset_counter
	3    scl,		! Number of scalers
	4    scl_add,		! CAMAC module address of first scaler
	5    scl_chn,		! number of first scaler for instrum.
	6    input_add,		! CAMAC module address of relay driver
	7    (misc_switches(i), i= 1, 5) ! bit nos. used for misc. switches
	READ (1, FMT= '(a1)') dummy      
	READ (1, FMT= *) sw_add, ! CAMAC module address of i/o register
	2    sw_reg,		! I/O register used by instrument
	3    sw_mon_bit,	! bit no. of mon inhibit line
	4    (sw_relays(i), i = 1, 5), ! bit nos. used for relays
	5    sw_scl_bit		! bit no. of scl inhibit line
	READ (1, FMT= '(a1)') dummy      
	READ (1, FMT= *) mot_line(acs),	! RS232 line for motor controller
	1    tmp_line,		! RS232 line for temperature controller
	2    pols,		! Number of power supplies for flippers
	3    pol_add,		! Camac address of gpib interface
	4    pol_chn,		! number of first power supply
	5    (poltype(i), i = pol_chn, pol_chn + pols - 1) ! defines type of power supply
	READ (1, FMT= '(a1)') dummy      

  	IF (nsta.eq.-7 .OR. nsta.eq.8)then
	   READ (1, FMT= *) histo_add, ! CAMAC address for LC histo module
	1	rout_add,	! CAMAC address for LC router module
	2	sw_histo_bit	! BIT in output strip to NIM module for histo control
	   READ (1, FMT= '(a1)') dummy      
	ELSE
	   histo_add = 0	!signal no LC histo hardware
	ENDIF
	READ (1, FMT= *)  term_line, ! rs232 line for priv term
	1    mag_line,		! rs232 line for magnet
	2    mot_line(pc),	! rs232 line for 2nd mot type
	3    mot_line(acsf)	! rs232 line for 3rd mot type
	READ (1, FMT= '(a1)') dummy
	READ (1, FMT= '(a1)') dummy !read new motor header line
	DO i = 1, mots
	   READ (1, FMT= *) m(i), dbase(m(i)), dramp(m(i)), dtop(m(i)),
	1	dpulse(m(i)), dback(m(i)), dmerr(m(i)), dmottype(m(i)),
	2	dvmot1(m(i)),dvmot2(m(i)),dvmot3(m(i))
	END DO
	CLOSE(1)

	IF (nsta .EQ. 99)THEN	!polymers xray
	   nsta = 0		!same as our xray
	   poly = .TRUE.
	ELSE
	   poly = .FALSE.
	ENDIF

 	IF ((nsta .eq. 4).or.
	1    (nsta .eq. 5).or.
	1    (nsta .eq. 1).or.
	2    (nsta .eq. 8).or.
	2    (nsta .eq. 9).or.
	3    (nsta .eq. -1).or.
	3    (nsta .eq. -5).or.
	4    (nsta .eq. -7).or.
	4    (nsta .eq. -11)) then
	   vmedata = .true.
 	endif
	IF (nsta .eq. 5) then
	   dmpscl = .true.
	ENDIF
	IF (nsta.eq.-1.or.nsta.eq.-11.or.nsta.eq.8.or.nsta.eq.-7.or.nsta.eq.-5) then
	   aimpsd = .true.
	ENDIF
	IF (nsta.eq.8) then
	   USE_ASD= .true.
	ENDIF
	IF (nsta.eq.-5) then
	   gpibenet = .true.
	   nocells = 64
	ENDIF

	IF (nsta .eq. 0) then
	   extension(1:3) = 'xr0'
	else if (nsta .eq. 1) then
	   extension(1:3) = 'bt1'
	else if (nsta .eq. 4) then
	   extension(1:3) = 'bt4'
	else if (nsta .eq. 5) then
	   extension(1:3) = 'bt5'
	else if (nsta .eq. 8) then
	   extension(1:3) = 'bt8'
	else if (nsta .eq. -7) then
	   extension(1:3) = 'ng7'
	else if (nsta .eq. -5) then
	   extension(1:3) = 'ng5'
	else if (nsta .eq. -1) then
	   extension(1:3) = 'ngd'
	else if (nsta .eq. -11) then
	   extension(1:3) = 'cgd'
	else
	   extension(1:3) = 'icp' ! Need a sensible default extension
	end if

	negqpos = .FALSE.	!in case it is not in motors.buf yet
C--------------------------CHECK FOR MOTORS.BUF -----------------------------C

	CALL FIND_FILE (MOTORS_BUF, ftmp, context,status)
	IF (status.eq.0) THEN
 10 	   OPEN (UNIT= 1, FILE=MOTORS_BUF, STATUS= 'NEW',
	1	ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= MOTBUF_RECL)
	   DO i = 1, MAXMOTS
	      WRITE (1,REC=i) dbase(i), dramp(i), dtop(i), dpulse(i), dback(i),
	1	   dmerr(i), dmottype(i), dvmot1(i),dvmot2(i),dvmot3(i),
	1	   zero
	   END DO
	   WRITE (1,REC=MAXMOTS+1) (zero, i= 1, MAXMOTS) !z 
	   WRITE (1,REC=MAXMOTS+2) (zero, i= 1, MAXMOTS) !ul 
	   WRITE (1,REC=MAXMOTS+3) (zero, i= 1, MAXMOTS) !ll 
	   IF (nsta.eq.1)THEN
	      monobt1='CU311'	!default values
	      collim(1) = 15
	   ELSE
	      monobt1(1:)=' '
	      collim(1) = 0
	   ENDIF
	   WRITE (1,REC=MAXMOTS+4) collim(1),(IFIX(zero), i= 1, 6),
	1	zero, 3.3540, 3.3540,monobt1
	   WRITE (1,REC=MAXMOTS+5) (.FALSE., i= 1,MAXMOTS),
	1	.TRUE., .TRUE., .FALSE., .FALSE.,
	2	.false.,	!psd
	3	.FALSE., 1, 0, .FALSE.
	   WRITE (1,REC=MAXMOTS+6) (6.0, i=1, pols), (0., i=1, pols),
	1	(13.7, i=1, pols)
	   WRITE (1, REC=MAXMOTS+7) .FALSE.,33.5 ,92.4,	!focus5, fdad, fdsa
	1	1,1,256,256,128,128,.FALSE. !asd_xmin,ymin,xmax,ymax,numx,numy,negqpos
	   CLOSE (1)
        END IF
	CALL FIND_FILE_END(context)

C----------READ PARAMETERS, ZEROS AND LIMITS FOR MOTORS AND FLAGS -----------C

 20	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', ACCESS= 
	1    'DIRECT', FORM= 'UNFORMATTED', RECL=MOTBUF_RECL,ERR= 110)
	! Originally jumped to statement 10
	DO i = 1, mots
	   READ (1,REC=m(i)) base(m(i)), ramp(m(i)), top(m(i)), pulse(m(i)), 
	1	back(m(i)), merr(m(i)), mottype(m(i)), vmot1(m(i)),
	1	vmot2(m(i)), vmot3(m(i)), vmot4(m(i))
	END DO
	READ (1,REC=MAXMOTS+1) (z(i), i= 1, MAXMOTS)
	READ (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	READ (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS)
	READ (1,REC=MAXMOTS+4) 
	1    (collim(i), i= 1,4), (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	READ (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), file, statistic,
	1    tempctr, polarized, psd,
	1    tdev, magnctr
	READ (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
	2    (f_on(i)%engy, i=1, pols)
	READ (1,REC=MAXMOTS+7,iostat=ios) focus5, fdad , fdsa, !fdsa(samp->analy); fdad (analy->detector)
	1    asd_xmin,asd_ymin,asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos
	CLOSE (1)

	file = .TRUE.		!always take data
	focus5 = .FALSE.	!start with false always.
	rockfl = .FALSE.	!start up with false always (add to motors.buf?)
	motovr = .FALSE.	!def is do not use overhead for motors in howlong
	cnstovr = .FALSE.	!def is no constant/pt overhead used in howlong
	hwlconst = 1.		!no constant overhead defined yet
	cntrate = 0.		!no mrat/s done yet
	tempctr = .false.	!t flag is always off at start of ICP
	magnctr = .false.	!m flag is always off at start of ICP
	tinit_done = .false.	!tdev must be done each time ICP is restarted
	tinit_done2 = .false.	!tdev2 must be done each time ICP is restarted
	minit_done = .false.	!mdev must be done each time ICP is restarted
	dbcurve = .false.	!not using database temperature curves
	magtype = 0		!undefined magnet controller
	liqwait = 0.		!wait in secs before counting after moving in ng7 scans
	pgaa = .false.		!no coord between Lindstrom pgaa and ng7
	focus5=.false.
	fieldmax = 90.		!default max allowed field (used in prep field tests)
	noback = .FALSE.	!default to always adjust motor drive to take out backlash
	use_asd = .FALSE.		!default is not using position sensitive detector
	asd_addr = 10
	super = .FALSE.
	lastef = 0.0
	tcareful = .false.	!not using Ross temp algo: special_approach
	caution = 0.		!variable for careful_approach
	offset = 0.		!variable for careful_approach
	maxoffset = 0.		!variable for careful_approach
	setemperr = 0.		!variable for careful_approach
	setempwait = 0.		!variable for careful_approach
	IF (nsta .EQ. 1)THEN
	   autolim = .TRUE.	!bt1 motor 4 limits & scan range set for mono
	ELSE
	   autolim = .FALSE.
	ENDIF
	IF (nsta.eq.8) then
	   psd=.false.
	   use_asd=.true.
	   nocells = 960
	   asd_xdim = 960
	   asd_ydim = 20
	ENDIF


	verbose_run = .false.

C--------------------------CHECK FOR IBUFFER.BUF ----------------------------C

	CALL FIND_FILE ('IBUFFER.BUF', ftmp, context,status)
	IF (status.eq.0)THEN
	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'NEW',
	1	ACCESS= 'DIRECT',FORM= 'UNFORMATTED',
	1	RECL= IBUF_RECL)
	   ibuf%mt = 'TIME'
	   ibuf%pts = 1
	   ibuf%com(1:) = 'EMPTY'
	   DO i = 1, NBUF
	      WRITE (1,REC=i) ibuf
	   END DO
	   WRITE (1,REC=NBUF+1) '#'
	   CLOSE(1)
        END IF
	CALL FIND_FILE_END(context)


C--------------------------CHECK FOR QBUFFER.BUF ----------------------------C
	IF (nsta .NE. 1)THEN
	   CALL FIND_FILE ('QBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0)THEN
	      OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= QBUF_RECL)
	      qbuf%mt = 'TIME'
	      qbuf%pts = 1
	      qbuf%com(1:) = 'EMPTY'
	      DO i = 1, NBUF
		 WRITE (1,REC=i) qbuf
	      END DO
	      CLOSE(1)
	   ENDIF		!not status
	END IF			!nsta .ne. 1
	CALL FIND_FILE_END(context)

C--------------------------CHECK FOR BBUFFER.BUF ----------------------------C

	IF (nsta .NE. 1)THEN
	   CALL FIND_FILE ('BBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0)THEN
	      OPEN (UNIT= 1, FILE= 'BBUFFER.BUF',STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= BBUF_RECL)
	      bbuf%mt = 'TIME'
	      bbuf%pts = 1
	      bbuf%com(1:) = 'EMPTY'
	      DO i = 1, NBUF
		 WRITE (1,REC=i) bbuf
	      END DO
	      CLOSE(1)
	   ENDIF
	END IF
	CALL FIND_FILE_END(context)
C--------------------------CHECK FOR RBUFFER.BUF ----------------------------C

	IF (nsta .EQ. -7) THEN
	   CALL FIND_FILE ('RBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0)THEN
  	      OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= RBUF_RECL)
	      DO i = 1, NBUF
		 WRITE (1,REC=i) rbuf
	      END DO
	      CLOSE(1)
	   ENDIF
	END IF
	CALL FIND_FILE_END(context)

C--------------------------CHECK FOR TBUFFER.BUF ----------------------------C

	IF (nsta .EQ. 4) THEN 
	   CALL FIND_FILE ('TBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0)THEN
  	      OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= TBUF_RECL)
	      tbuf%mt = 'TIME'
	      tbuf%ec = 25.0
	      tbuf%es = 1.0
	      tbuf%pts = 1
	      tbuf%com(1:) = 'EMPTY'
	      DO i = 1, NBUF
		 WRITE (1,REC=i) tbuf
	      END DO
	      CLOSE(1)
	   ENDIF
	END IF
	CALL FIND_FILE_END(context)

C--------------------------CHECK FOR XBUFFER.BUF ----------------------------C

	IF (nsta .EQ. 8)THEN
	   CALL FIND_FILE ('XBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0)THEN
  	      OPEN (UNIT= 1, FILE='XBUFFER.BUF', STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= XBUF_RECL)
	      xbuf%mt = 'TIME'
	      xbuf%pts = 1
	      xbuf%com(1:) = 'EMPTY'
	      DO i = 1, NBUF
		 WRITE (1,REC=i) xbuf
	      END DO
	      CLOSE(1)
	   ENDIF
	END IF
	CALL FIND_FILE_END(context)
C--------------------------CHECK FOR DBUFFER.BUF ----------------------------C
	IF (nsta .EQ. 8)THEN
	   CALL FIND_FILE ('DBUFFER.BUF', ftmp, context,status)
	   IF (status.eq.0) THEN
  	      OPEN (UNIT= 1, FILE='DBUFFER.BUF', STATUS= 'NEW',
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= DBUF_RECL)
	      DO i = 1, NBUF
		 WRITE (1,REC=i) dbuf
	      END DO
	      CLOSE(1)
	   ENDIF
	END IF
	CALL FIND_FILE_END(context)
C--------------------------CHECK FOR HWLONG.BUF -----------------------------C

	CALL FIND_FILE (HWLONG_BUF, ftmp, context,status)
	IF (status.eq.0)THEN
 28 	   OPEN (UNIT= 1, FILE=HWLONG_BUF, STATUS= 'NEW', 
	1	ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	RECL= MOTBUF_RECL)
	   WRITE(1,REC=1) -2.,90.,1.,.TRUE.,1.,
	2	.01,.001,.0001,.00001
	   CLOSE (1)
        END IF
	CALL FIND_FILE_END(context)

  	OPEN (UNIT= 1, FILE=HWLONG_BUF, STATUS= 'OLD',
	1    ACCESS='DIRECT', FORM= 'UNFORMATTED',
	1    RECL=MOTBUF_RECL,ERR= 115)
	READ(1,REC=1)dlast,monang,cntrate,cnstovr,hwlconst,
	1    chw1,chw2,chw3,chw4
	CLOSE (1)

C--------------------------CHECK FOR Distances.NG7----------------------------C
	IF (nsta .EQ. -7) THEN
	   OPEN (UNIT= 1, FILE=DISTANCES_ICPR, STATUS= 'OLD',
	1	ACCESS= 'SEQUENTIAL', FORM= 'FORMATTED', ERR= 130)
	   READ (1, FMT= '(a)') dummy ! Header 
	   READ (1, FMT= *) rxPG !monochromator to PG filter: horizontal 
	   READ (1, FMT= *) rxBe !monochromator to Be filter: horizontal
	   READ (1, FMT= *) rxs1 !monochromator to slit 1: horizontal
	   READ (1, FMT= *) rxp1 !monochromator to shield wall clevis point: horizontal
	   READ (1, FMT= *) rzp1 !monochromator to shield wall clevis point: vertical
	   READ (1, FMT= *) rrp	!floating pivot radius on shield wall track
	   READ (1, FMT= *) rthetap !floating pivot angle at alpha=0 on shield wall track
	   READ (1, FMT= *) rxp2 !monochromator to front flight tube clevis: horizontal
	   READ (1, FMT= *) rxt	!monochromator to sample table: horizontal
	   READ (1, FMT= *) rxp3 !monochromator to detector clevis point: horizontal
	   READ (1, FMT= *) rh1	!Height of wheel center above floating clevis #2
	   READ (1, FMT= *) rh2	!Height of beam above wheels at alpha = 0.
	   READ (1, FMT= *) rd2	!Distance from wheel midpt to slit 2
	   READ (1, FMT= *) rd3	!Distance from wheel midpt to slit 3
	   READ (1, FMT= *) rd4	!Distance from rear fixed clevis to slit 4
	   CLOSE(1)
	ENDIF			!NG7


C--------------------------CHECK FOR ZCoeffs.cfg----------------------------C
	IF (nsta .EQ. 4) THEN
	   OPEN (UNIT= 1, FILE= ZCOEFFS_CFG, STATUS= 'OLD',
	1	ACCESS='SEQUENTIAL', FORM= 'FORMATTED',
	1	ERR= 135)
	   READ (1, FMT= '(a)') dummy ! Header 
	   READ (1, FMT= *,err=135) z2coeff1 ! z2coeff1 
	   READ (1, FMT= *,err=135) z2coeff2 ! z2coeff2 
	   READ (1, FMT= '(a)') dummy ! Header 
	   READ (1, FMT= *,err=135) z1coeff1 ! z1coeff1 
	   READ (1, FMT= *,err=135) z1coeff2 ! z1coeff2 
	   CLOSE(1)
	ENDIF			!BT4

C-------------------SET PRIVILEGES ACCORDING TO TERMINAL --------------------C
	
	Call getpriv

 30	IF (.not. priv) THEN	!check for undocumented bypass
	   CALL Priv_Bypass (privat)
	   IF (privat) priv = .TRUE.		
	END IF

	trackmoves = .false.
	if (priv .and. (system_type .ne. DEC .or. nsta .eq. -5) )
	1    trackmoves = .true.

C--------------------------CHECK FOR MOTPOS.BUF -----------------------------C
	updatefetch = .false.
	IF (trackmoves) THEN
	   CALL FIND_FILE (MOTPOS_BUF, ftmp, context,status)
	   IF (status.eq.0) THEN
	      updatefetch = .true.
	      OPEN (UNIT= 1, FILE= MOTPOS_BUF, STATUS= 'NEW', 
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL= MOTBUF_RECL)
	      data = -999.99	!flag to signal unknown position
	      DO i = 1, MAXMOTS
		 WRITE (1,REC=i) data, zero
	      END DO
	      CLOSE (1)
	   END IF
	   CALL FIND_FILE_END(context)
	ENDIF

C-----------------SET GLOBAL FLAGS TO THEIR DEFAULT VALUES ------------------C


	call cluster64		! communication
	pause    = pause_offs + iabs(nsta)
	def_ver  = 1
	counting = .FALSE.
	t_count  = .FALSE.
	m_count  = .FALSE.
	DO i= 1,MAXMOTS		!set all motors to not in use by default
	   motfl(i) = .false.
	ENDDO
	DO i= 1, mots
	   motfl(m(i)) = .TRUE.
	END DO
	CALL Setup_VM		!Setup Virtual motor lists if used

	DO i= pol_chn, pol_chn + pols - 1
	   polfl(i) = .TRUE.
	END DO
	IF (nsta .EQ. 1) THEN
c	   IF (((collim(1) .NE. 7) .and.
c	1	(collim(1) .NE. 15) .and.
c	1	(collim(1) .NE. 60)) .OR.
c	1	((monobt1 .ne. 'CU311') .AND.
c	1	(monobt1 .ne. 'GE311') .and.
c	2	(monobt1 .ne. 'GE733') .AND.
c	2	(monobt1 .ne. 'DUMMY'))) THEN !use default
	      collim(1)=15
	      monobt1='CU311'
c	   ENDIF		!need to use default values
c	   IF (collim(1).eq.7) THEN
c	      buffer(1:)=monobt1//'_7'//'.DAT'
c          ELSE IF (collim(1).eq.60) THEN
c              buffer(1:)=monobt1//'_60'//'.DAT'
c	   ELSE
c	      buffer(1:)=monobt1//'_15'//'.DAT'
c	   ENDIF
	   OPEN (UNIT= 1, FILE= HOMEDIR//buffer(1:13), STATUS= 'OLD', ACCESS= 
	1	'SEQUENTIAL', FORM= 'FORMATTED', ERR= 120)
	   READ (1, *) wavel, (eff(i), i= 1,scl), (zp(i), i= 1, scl)
	   CLOSE(1)
	   data = wavel		!memorize new value of wavel while reading in record
	   IF (priv) THEN
	      OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	   ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	   RECL=MOTBUF_RECL)
	      READ (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
	1	   (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	      wavel = data	!set wavel to new value
	      WRITE (1,REC=MAXMOTS+4) (collim(i), i= 1,4), 
	1	   (mosaic(i), i= 1,3), wavel, dm, da, monobt1
	      CLOSE (1)
	   ENDIF		!priv
	   which_one = 1
	ELSEIF (nsta .eq. -7)THEN
	   OPEN (UNIT= 1, FILE= SCLFAC_DAT, STATUS= 'OLD',
	1	ACCESS= 'SEQUENTIAL', FORM= 'FORMATTED', ERR= 50)
	   READ (1, *)eff(1)
	   CLOSE(1)
	   GOTO 75
 50	   eff(1) = 5.0		!default value
	   OPEN (UNIT=1, FILE= SCLFAC_DAT,STATUS='NEW',
	1	ACCESS='SEQUENTIAL',FORM='FORMATTED')
	   WRITE(1,*)EFF(1)
	   CLOSE(UNIT=1)
 75	   which_one = 5
	ELSEIF (nsta.eq.0.or.nsta.eq.7.or.nsta.eq.-1)THEN !i_buf for BT7, XRAY, ng1
	   which_one = 1
	ELSEIF (nsta .EQ. 8) THEN !Dartz buffer
	   which_one = 6
	ELSE
	   which_one = 2
	END IF
	contr_A  = contr_A_offs + iabs(nsta)
	stop = stop_offs + 1 
	IF (nsta .eq. 0)THEN	!XRAY will use unique evflags
	   pause = contr_A_offs + 1
	   contr_A = pause_offs + 1
	   qioevflag = contr_a + 1
	ENDIF
	CALL CLREF (contr_A)


C-----------------------INITIALIZE PRIVILEGED STUFF -------------------------C

	IF (priv) THEN
	   CALL verify_motors(dmottype,dpulse,errflag)
	   IF (errflag) CALL EXIT
	   timer_id = 98

	   hdw_event= hdw_event_offs + iabs(nsta)
	   IF (nsta .eq. 0) hdw_event = hdw_event + 1 !xray
	   CALL CLREF (hdw_event)
	   camerr = camerr_offs + iabs(nsta)
	   IF (nsta .eq. 0) camerr = camerr + 1 !xray
	   CALL CLREF (camerr)

  	   CALL RS232_CONF

	   IF (nsta .ne. 0) CALL Camac_Init

C           IF (nsta .eq. 4) trashmode = .true.
 	   IF (amscaler) CALL SCALERINIT
	   IF (nsta .eq. 5) CALL RS232P_INIT
	   IF ((nsta .eq. -1 .or. nsta.eq.-11 ).and. term_line .gt. 0)
	1	CALL RS232G_INIT
	   IF (nsta .eq. -5) CALL ENET_INIT
   	   IF (system_type .ne. DEC) CALL UDPINIT
	   IF (mot_line(PC).gt.0) THEN
	      IF (vmedata) THEN 
		 CALL RS232V_INIT
              ENDIF
	   ENDIF

	   IF (mot_line(ACS).gt.0) THEN
	      IF (nsta.eq.-5) THEN
		 CALL RS232Z_INIT
	      ELSE IF (nsta .eq. 5) THEN
		 CALL RS232I_INIT
	      ELSE	
		 CALL RS232_Init
	      ENDIF
	   ENDIF
	   
	   IF (mot_line(ACSF).gt.0) CALL RS232EZ_INIT
	   CALL Init_Motors	!set up moving flags and name strings for motors
	   Call logfile		!get name of new logfile
	   makelog=.TRUE.	!default at startup
	END IF

	IF (updatefetch) call fetch_tofile(.false.)
	IF (NSTA .EQ. 1 .and. PRIV) THEN !safeguard on BT1
	   IF (monobt1 .EQ. 'CU311')THEN
	      ll(4) = -1.
	      ul(4) = 13.1
	   ELSEIF (monobt1 .EQ. 'GE311')THEN
	      ll(4) = -1.
	      ul(4) = 11.4
	   ELSEIF (monobt1 .EQ. 'GE733')THEN
	      ll(4) = -0.7
	      ul(4) = 13.1
	   ELSE
	      if (ul(4) .gt. 13.) ul(4) = 13.
	      if (ll(4) .lt. -1.) ll(4) = -1.
	   ENDIF		!select limits to go with monochromator
	   OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1	ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	RECL=MOTBUF_RECL,ERR= 90)
	   WRITE (1,REC=MAXMOTS+2) (ul(i), i= 1, MAXMOTS) 
	   WRITE (1,REC=MAXMOTS+3) (ll(i), i= 1, MAXMOTS) 
	   CLOSE(1)
!       Check translations
	   CALL CENTER_CHECK(centered)
	   IF (centered .eq. 0) THEN
!       Not centered
	      CALL Read_STRING (keyb, str4, 
	1	   'Sample Stage Not Centered. Center now? (T/F): ',
	2	   4,j,j,j, l,j, contr) !j is dummy param for absoft   
	      IF (str4(1:1).eq.'T'.or.str4(1:1).eq.'t') THEN
		 CALL CENTER(0)
	      ENDIF
	   ENDIF
	ENDIF			!BT1 priv

 90	continue
	RETURN

 100	print *,'   Error opening "INSTR.CFG"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT 
 110	print *,'   Error opening "MOTORS.BUF"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT 
 115	print *,'   Error opening "HWLONG.BUF"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT 
 120	print *, '   Error opening "SCALFAC.DAT"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT
 125	print *,'   Error opening "CU311.MON"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT
 130	print *,'   Error opening "DISTANCES.NG7"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT
 135	print *,'   Error opening "ZCOEFFS.CFG"!'//
	1    ' Check for existence and correct data.'
	CALL EXIT
	END
        
