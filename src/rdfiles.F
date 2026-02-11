#include "icp.h"
	SUBROUTINE Rdfiles
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
	use histo
	use motors
	use polar
	use focus
	IMPLICIT     none
	INTEGER*4    i,  contr_A_offs /79/, pause_offs /14/
	INTEGER*4    hdw_event_offs /64/, camerr_offs /32/, context, nowait/1/
	INTEGER*4    status, ios
	LOGICAL      privat, errflag
	REAL*4       zero /0./,data
	CHARACTER    dummy
	CHARACTER*80 ftmp
	CHARACTER*80 buffer, syscmd
	CHARACTER*80 gethome

	integer*4	m(MAXMOTS)
	integer*4       retn


	call sethome


C----------------- READ INSTRUMENT AND HARDWARE PARAMETERS -------------------C

	OPEN (UNIT= 1, FILE= trim(gethome())//'INSTR.CFG',
	1    STATUS= 'OLD', ACCESS= 'SEQUENTIAL',
	1    FORM= 'FORMATTED', ERR= 100)

	READ (1, FMT= '(a)') header     ! Instrument
	READ (1, FMT= *) nsta,          ! Station number
	1                mots,          ! Number of motors
	2                mon_add,       ! CAMAC module address of preset_counter
	3                scl,           ! Number of scalers
	4                scl_add,       ! CAMAC module address of first scaler
	5                scl_chn,       ! number of first scaler for instrum.
	6                input_add,     ! CAMAC module address of relay driver
	7  (misc_switches(i), i= 1, 5)  ! bit nos. used for misc. switches
	READ (1, FMT= '(a1)') dummy      
	READ (1, FMT= *) sw_add,        ! CAMAC module address of i/o register
	2                sw_reg,        ! I/O register used by instrument
	3                sw_mon_bit,    ! bit no. of mon inhibit line
	4     (sw_relays(i), i = 1, 5), ! bit nos. used for relays
	5                sw_scl_bit     ! bit no. of scl inhibit line
	READ (1, FMT= '(a1)') dummy      
	READ (1, FMT= *) mot_line(acs),      ! RS232 line for motor controller
	1                tmp_line,      ! RS232 line for temperature controller
	2                pols,          ! Number of power supplies for flippers
	3                pol_add,       ! Camac address of gpib interface
	4                pol_chn,       ! number of first power supply
	5    (poltype(i), i = pol_chn, pol_chn + pols - 1)  ! defines type of power supply
	READ (1, FMT= '(a1)') dummy      
	IF (nsta.eq.-7 .OR. nsta .EQ. -1.OR.nsta.eq.8)then
	  READ (1, FMT= *) histo_add,     ! CAMAC address for LC histo module
     1	                 rout_add,      ! CAMAC address for LC router module
     2	                 sw_histo_bit   ! BIT in output strip to NIM module for histo control
	  READ (1, FMT= '(a1)') dummy      
	ELSE
	  histo_add = 0 !signal no LC histo hardware
	ENDIF
	READ (1, FMT= *)  term_line,                    ! rs232 line for priv term
     1			  mag_line,			! rs232 line for magnet
     1			  mot_line(pc),			! rs232 line for 2nd mot type
     1			  mot_line(acsf)			! rs232 line for 3rd mot type
	READ (1, FMT= '(a1)') dummy
	READ (1, FMT= '(a1)') dummy  !read new motor header line
	DO i = 1, mots
	   READ (1, FMT= *) m(i), base(m(i)), ramp(m(i)), top(m(i)),
     1       pulse(m(i)), back(m(i)), merr(m(i)), mottype(m(i)),
     1	     vmot1(m(i)),vmot2(m(i)),vmot3(m(i))
	END DO
	CLOSE(1)

	IF (nsta .EQ. 99)THEN !polymers xray
	  nsta = 0 !same as our xray
C	  poly = .TRUE.
	ELSE
C	  poly = .FALSE.
	ENDIF

	negqpos = .FALSE. !in case it isn't in motors.buf yet
C-------------------------- CHECK FOR MOTORS.BUF -----------------------------C

	CALL FIND_FILE (trim(gethome())//'MOTORS.BUF', ftmp, context,status)
	IF (status.eq.0) THEN
 10 	   OPEN (UNIT= 1, FILE= trim(gethome())//'/MOTORS.BUF',
	1	STATUS= 'NEW', ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	RECL= MOTBUF_RECL)
	DO i = 1, MAXMOTS
	   WRITE (1,REC=i) base(i), ramp(i), top(i), pulse(i), back(i),
     1		merr(i), mottype(i), vmot1(i),vmot2(i),vmot3(i),
     1		zero
	END DO
	WRITE (1,REC=MAXMOTS+1) (zero, i= 1, MAXMOTS) !z 
	WRITE (1,REC=MAXMOTS+2) (zero, i= 1, MAXMOTS) !ul 
	WRITE (1,REC=MAXMOTS+3) (zero, i= 1, MAXMOTS) !ll 
	IF (nsta.eq.1)THEN
	  monobt1='CU311' !default values
	  collim(1) = 15
	ELSE
	  monobt1(1:)=' '
	  collim(1) = 0
	ENDIF
	WRITE (1,REC=MAXMOTS+4) collim(1),(IFIX(zero), i= 1, 6), 
     1	   zero, 3.3540, 3.3540,monobt1
	WRITE (1,REC=MAXMOTS+5) (.FALSE., i= 1,MAXMOTS),
     1	   .TRUE., .TRUE., .FALSE., .FALSE.,
     1	              .false., !psd
	1             .FALSE., 1, 0, .FALSE.
	WRITE (1,REC=MAXMOTS+6) (6.0, i=1, pols), (0., i=1, pols),
	2	      (13.7, i=1, pols)
	WRITE (1, REC=MAXMOTS+7) .FALSE.,33.5 ,92.4,  !focus5, fdad, fdsa
     1	  1,1,256,256,128,128,.FALSE. !asd_xmin,ymin,xmax,ymax,numx,numy,negqpos
 	CLOSE (1)
        END IF
	CALL FIND_FILE_END(context)


	DO i= 1, mots
	   motfl(m(i)) = .TRUE.
	END DO

C------------------- SET PRIVILEGES ACCORDING TO TERMINAL --------------------C
 
	priv=.true.
 	RETURN

 100    print *,' error opening instr.cfg'
	CALL EXIT
	END
