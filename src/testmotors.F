#include "icp.h"
      PROGRAM TESTMOTORS
      use instr
      use motors
      use globals
      use iso_c_binding
      implicit none
      interface
#include "c_rs232_cnf.h"
#include "c_vme.h"
#include "c_debugval.h"
      end interface

      CHARACTER    dummy      
      character*80 gethome
      integer*4 i
      integer*4	m(MAXMOTS),
     1     dbase(MAXMOTS), dramp(MAXMOTS), dtop(MAXMOTS),
     1     dpulse(MAXMOTS), dback(MAXMOTS), dmerr(MAXMOTS),
     1     dmottype(MAXMOTS)
      real*4	dvmot1(MAXMOTS),dvmot2(MAXMOTS),dvmot3(MAXMOTS)


      call sethome

      vmedata = .FALSE.
      amscaler= .FALSE.
      trashmode = .FALSE.
      gpibenet = .FALSE.
      aimpsd = .FALSE.

      print *,'INSTR_CFG ='//INSTR_CFG
      OPEN (UNIT= 1, FILE= INSTR_CFG, STATUS= 'OLD',ERR= 100)

      READ (1, FMT= '(a)') header ! Instrument
      READ (1, FMT= *) nsta,    ! Station number
     $     mots,                ! Number of motors
     $     mon_add,             ! CAMAC module address of preset_counter
     $     scl,                 ! Number of scalers
     $     scl_add,             ! CAMAC module address of first scaler
     $     scl_chn,             ! number of first scaler for instrum.
     $     input_add            ! CAMAC module address of relay driver
      READ (1, FMT= '(a1)') dummy      
      READ (1, FMT= *) sw_add,  ! CAMAC module address of i/o register
     $     sw_reg,              ! I/O register used by instrument
     $     sw_mon_bit,          ! bit no. of mon inhibit line
     $     sw_scl_bit           ! bit no. of scl inhibit line
      READ (1, FMT= '(a1)') dummy      
      READ (1, FMT= *) mot_line(acs), ! RS232 line for motor controller
     $     tmp_line,            ! RS232 line for temperature controller
     $     pols,                ! Number of power supplies for flippers
     $     pol_add,             ! Camac address of gpib interface
     $     pol_chn             ! number of first power supply
C     $     poltype(1),poltype(2),poltype(3),poltype(4),poltype(5),
C     $     poltype(6)           ! defines type of power supply
      READ (1, FMT= '(a1)') dummy      

      IF (nsta.eq.-7 .OR. nsta .EQ. -1.OR.nsta.eq.8)then
         READ (1, FMT= *) histo_add, ! CAMAC address for LC histo module
     1        rout_add,         ! CAMAC address for LC router module
     2        sw_histo_bit      ! BIT in output strip to NIM module for histo control
         READ (1, FMT= '(a1)') dummy      
      ELSE
         histo_add = 0          !signal no LC histo hardware
      ENDIF
      READ (1, FMT= *)  term_line, ! rs232 line for priv term
     1     mag_line,            ! rs232 line for magnet
     1     mot_line(pc),        ! rs232 line for 2nd mot type
     1     mot_line(acsf)       ! rs232 line for 3rd mot type
      READ (1, FMT= '(a1)') dummy
      READ (1, FMT= '(a1)') dummy !read new motor header line
      DO i = 1, mots
         READ (1, FMT= *) m(i), dbase(m(i)), dramp(m(i)), dtop(m(i)),
     1        dpulse(m(i)), dback(m(i)), dmerr(m(i)), dmottype(m(i)),
     1        dvmot1(m(i)),dvmot2(m(i)),dvmot3(m(i))
      END DO
      CLOSE(1)

      vmedata = .true.
      IF (nsta.eq.-1.or.nsta.eq.-11.or.nsta.eq.8.or.nsta.eq.-7) then
         aimpsd = .true.
      ENDIF

      IF (nsta.eq.-5) then
         gpibenet = .true.
      ENDIF


      call rs232_conf
      call DEBUGLEVEL(1)
      call RS232V_INIT

      CALL EXIT
 100  print *,'   Error opening "INSTR.CFG"!'//
     1	   ' Check for existence and correct data.'
      CALL EXIT 
 120  print *, '   Error opening "SCALFAC.DAT"!'//
     1     ' Check for existence and correct data.'
      CALL EXIT
 125  print *,'   Error opening "CU311.MON"!'//
     1	   ' Check for existence and correct data.'
      CALL EXIT
 130  print *,'   Error opening "DISTANCES.NG7"!'//
     1	   ' Check for existence and correct data.'
      CALL EXIT
 135  print *,'   Error opening "ZCOEFFS.CFG"!'//
     1     ' Check for existence and correct data.'
      CALL EXIT


      END
