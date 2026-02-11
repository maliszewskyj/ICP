#include "icpdef.h"
      MODULE instr
      use ISO_C_BINDING
      CHARACTER*35 header       ! header for program
      CHARACTER*1  insp1, insp1b
      CHARACTER*2  insp2
      CHARACTER*3  insp3
      INTEGER(c_int),bind(C,name="nsta")::nsta         ! station number
      INTEGER*4    mots         ! number of motors
      INTEGER*4    mon_add      ! address of down-count module
      INTEGER(c_int),bind(C,name="scl")::scl          ! number of scalers
      INTEGER*4    scl_add      ! address of 1. scaler module
      INTEGER*4    scl_chn      ! number of first scaler
      INTEGER*4    input_add    ! address of input module
      INTEGER*4    misc_switches(5) ! bit nos. used for misc. sws.
      INTEGER*4    sw_add       ! address of IO-register module
      INTEGER*4    sw_reg       ! IO register used by instrum.
      INTEGER*4    sw_mon_bit   ! bit no. of mon inhibit line
      INTEGER*4    sw_scl_bit   ! bit no. of scl inhibit line
      INTEGER*4    sw_relays(5)    ! bit nos. used for relays
      INTEGER*4    pols         ! number of power supplies
      INTEGER*4    pol_add      ! camac address of GPIB interf
      INTEGER*4    pol_chn      ! number of first power supply
      INTEGER*4    histo_add    ! Camac add of LC histo (or 0 if none)
      INTEGER*4    sw_histo_bit ! bit no. to NIM module for histo control
      INTEGER*4    rout_add     ! CAMAC add of LC router
      INTEGER(c_int),bind(C,name="mot_line")::mot_line(MAXTYPES) ! RS232-line for mot-control
! 1=ACS, 2=PC, 3=Focus
      INTEGER(c_int),bind(C,name="tmp_line")::tmp_line     ! RS232-line for temp-control
      INTEGER(c_int),bind(C,name="mag_line")::mag_line     ! RS232/GPIB-ID for magnet-control
      INTEGER(c_int),bind(C,name="term_line")::term_line    ! RS232-line for priv terminal
C      CHARACTER(kind=c_char,len=5) , bind(C,name="monobt1")::monobt1      ! bt1 mon used: cu311,cu220,si531
      CHARACTER*5 monobt1 ! bt1 mon used: cu311,cu220,si531
      INTEGER*4    base(MAXMOTS)
      INTEGER*4    ramp(MAXMOTS)
      INTEGER*4    top(MAXMOTS)
      INTEGER(c_int), bind(C, name="pulse")::pulse(MAXMOTS) ! dynamic motor parameters
      INTEGER(c_int), bind(C, name="back")::back(MAXMOTS)
      INTEGER(c_int), bind(C, name="merr")::merr(MAXMOTS) !           "
      INTEGER*4    mottype(MAXMOTS) ! type of motor controller
      REAL(c_float), bind(C,name="vmot1")::vmot1(MAXMOTS) ! mot var (H+ for type 3)
      REAL(c_float), bind(C,name="vmot2")::vmot2(MAXMOTS) ! mot var (H- for type 3)
      REAL(c_float), bind(C,name="vmot3")::vmot3(MAXMOTS) ! mot var (address for type 3)
      REAL(c_float), bind(C,name="vmot4")::vmot4(MAXMOTS) ! mot var (pos for type 3)
      INTEGER*4    collim(4)    ! collimation comment
      INTEGER*4    mosaic(3)    ! mosaic comment
      INTEGER*4    def_ver      ! default file version	
      CHARACTER*7  terminal     ! terminal line
      CHARACTER*12 logname      ! filename based on date
      REAL*4       dm, da       ! monochr & analyzer d-spacing
      ! zeropoints, up & lw limits
      REAL(c_float), bind(C,name="z")::z(MAXMOTS)   
      REAL*4       ul(MAXMOTS)
      REAL*4       ll(MAXMOTS)  
      REAL*4       eff(32), zp(32) ! efficencies & zeros for BT1
      REAL*4       wavel        ! wavelength in Å
      REAL*4       rkpos1, rkpos2 ! position for rocking
      INTEGER*4    rkmot        ! motor to rock during count
      
      INTEGER*4 ACS,PC,ACSF,XPROM,DF,VIRT,NOTYETVM,
     $     PIMOT,VME,ZETA,EZ,PICO
      PARAMETER (ACS = 1)       !standard Adv Contr Systs motor controller
      PARAMETER (PC  = 2)       !Compumotor run via PC
      PARAMETER (ACSF = 3)      !focusing motor on spins
      PARAMETER (XPROM = 4)     !xray motr
      PARAMETER (DF = 5)        !Drabkin flipper on NG5
      PARAMETER (VIRT = 6)      !virtmot
      PARAMETER (NOTYETVM = -6) !uninitialized virtual motor
      PARAMETER (PIMOT = 8)     !Physik Instrumente Motor
      PARAMETER (VME = 9)       !VME based OMS58
      PARAMETER (ZETA = 10)     !Parker Compumotor
      PARAMETER (EZ=11)         !Allmotion EZ stepper
      PARAMETER (PICO=12)       !Newfocus Pico 87xx motor control

      LOGICAL     priv          ! .TRUE. when TX or OP terminal
      LOGICAL     super         ! .TRUE. when super privs set
      LOGICAL     counting      ! set when counting
      LOGICAL     t_count       ! set when counting with time
      LOGICAL     m_count       ! set when counting with monitor
      LOGICAL     file          ! set if data storage enabled
      LOGICAL     statistic     ! set if statistics check enabled
      LOGICAL     polarized     ! set if polarized beam
      LOGICAL	  psd           ! set if position sensitive dect on bt7
      LOGICAL     tempctr       ! set if temperature control enabled
      LOGICAL     magnctr       ! set if magnet control enabled
      LOGICAL	  focus5        ! using focusing motors on ng5
      LOGICAL     fixed(MAXMOTS)         ! set if a motor shall not move
      LOGICAL     makelog       ! allow creation of a logfile
      LOGICAL     motfl(MAXMOTS)         ! motor mask
      LOGICAL     polfl(8)         ! power supply mask
      LOGICAL     rockfl        ! set if motor rocks while counting
      INTEGER*4   timer_id      ! Timer request number
      INTEGER*4   camerr        ! Number in local event flag cluster
      INTEGER*4   hdw_event     ! Number in local event flag cluster
      INTEGER*4   contr_a       ! Number in local event flag cluster
      INTEGER*4   stop          ! Number in common event flag cluster
      INTEGER*4   pause         ! Number in common event flag cluster
      CHARACTER*28 start_time   ! Start-time in monitor-counts
      CHARACTER*28 stop_time    ! End-time in monitor-counts
      LOGICAL     verbose_run   ! set if extra info during buffer exe


      LOGICAL	autolim, !if true, bt1 motor 4 limits & scan range set for mono
     1     finishup,     !ctrle sets to true. current point in buffer to be last
     1     clrstat       !redo status line
      INTEGER*4 startsecs,stopsecs !time when prep mode entered
      INTEGER*4 system_type, dec, linux
      REAL*4 duration
      PARAMETER (dec=1)
      PARAMETER (linux=2)
      LOGICAL  vmedata,trashmode,trashmove,trackmoves,amscaler
      LOGICAL gpibenet
      LOGICAL aimpsd
      CHARACTER*12 cfgdir

      TYPE tmpdev
      integer*4 cfg
      integer*4 mot
      END TYPE
      
      TYPE(tmpdev) tdev
      END MODULE instr
