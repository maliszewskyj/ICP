#include "icp.h"
	SUBROUTINE Read_Buf(type,numb,monomot,gen_tmp,gen_it,gen_tw,gen_th0,
     1		gen_th, gen_hf,gen_ihf,gen_hfw,gen_pts,gen_mon, 
     1		gen_mpf, gen_mt, ipol, gen_pos1, gen_inc,
     1		gen_eft,gen_ec,gen_es,gen_ef,errflag)
	use smg_prep
	use refl
	use instr
	IMPLICIT NONE

C  Calling parameters
	CHARACTER*1 type
	INTEGER*4	numb, ipol, monomot
	REAL*4  gen_tmp,gen_it,gen_tw,gen_th0,gen_th, gen_pts, gen_mpf,
     1		gen_mon, gen_hf,gen_ihf,gen_hfw, gen_pos1, gen_inc,
     1	  	gen_ec, gen_es, gen_ef
	CHARACTER*4	gen_mt
	CHARACTER*3	gen_eft
	LOGICAL errflag

C  Local variables
	INTEGER*4	j

	TYPE(i_buf) ibuf
	TYPE(q_buf) qbuf
	TYPE(t_buf) tbuf
	TYPE(b_buf) bbuf
	TYPE(r_buf) rbuf

	errflag = .FALSE.

	IF (type .EQ. 'I')THEN
	  OPEN (UNIT= 3, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 
     1         'DIRECT', FORM= 'UNFORMATTED', RECL=IBUF_RECL)
	  READ (3,REC=numb) ibuf
	  gen_tmp = ibuf%tmp
	  gen_it  = ibuf%it
	  gen_tw  = ibuf%tw
	  gen_th0 = ibuf%th0
	  gen_th  = ibuf%th
	  gen_hf  = ibuf%hf
	  gen_ihf = ibuf%ihf
	  gen_hfw = ibuf%hfw
	  gen_pts = ibuf%pts
	  gen_mon = ibuf%mon
	  gen_mpf = ibuf%mpf
	  gen_mt  = ibuf%mt
	  gen_pos1 = ibuf%a(monomot)
	  gen_inc = ibuf%i(monomot)
	  ipol = 0 !get total number of flipper positions in case polar
	  DO j = 1, 4
	      IF (ibuf%flip_state(j))ipol = ipol + 1
	  END DO
	  IF (ipol .eq. 0)   ipol = 1
	ELSEIF (type .EQ. 'Q')THEN
	  OPEN (UNIT= 3, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 
     1                'DIRECT', FORM= 'UNFORMATTED', RECL=QBUF_RECL)
	  READ (3,REC=numb) qbuf
	  gen_tmp = qbuf%tmp
	  gen_it  = qbuf%it
	  gen_tw  = qbuf%tw
	  gen_th0 = qbuf%th0
	  gen_th  = qbuf%th
	  gen_hf  = qbuf%hf
	  gen_ihf = qbuf%ihf
	  gen_hfw = qbuf%hfw
	  gen_pts = qbuf%pts
	  gen_mon = qbuf%mon
	  gen_mpf = qbuf%mpf
	  gen_mt  = qbuf%mt
	  gen_eft = qbuf%eft
	  gen_ec = qbuf%ec
	  gen_es = qbuf%es
	  gen_ef = qbuf%ef
	  ipol = 0 !get total number of flipper positions in case polar
	  DO j = 1, 4
	      IF (qbuf%flip_state(j))ipol = ipol + 1
	  END DO
	  IF (ipol .eq. 0)   ipol = 1
	ELSEIF (type .EQ. 'B')THEN
	  OPEN (UNIT= 3, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 
     1                'DIRECT', FORM= 'UNFORMATTED', RECL=BBUF_RECL)
	  READ (3,REC=numb) bbuf
	  gen_eft = bbuf%eft
	  gen_ef = bbuf%ef
	  gen_tmp = bbuf%tmp
	  gen_it  = bbuf%it
	  gen_tw  = bbuf%tw
	  gen_th0 = bbuf%th0
	  gen_th  = bbuf%th
	  gen_hf  = bbuf%hf
	  gen_ihf = bbuf%ihf
	  gen_hfw = bbuf%hfw
	  gen_pts = bbuf%pts
	  gen_mon = bbuf%mon
	  gen_mpf = bbuf%mpf
	  gen_mt  = bbuf%mt
	  gen_pos1 = bbuf%a(monomot)
	  gen_inc = bbuf%i(monomot)
	  ipol = 0 !get total number of flipper positions in case polar
	  DO j = 1, 4
	      IF (bbuf%flip_state(j))ipol = ipol + 1
	  END DO
	  IF (ipol .eq. 0)   ipol = 1
	ELSEIF (type .EQ. 'T')THEN
	  OPEN (UNIT= 3, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 
     1                'DIRECT', FORM= 'UNFORMATTED')
	  READ (3,REC=numb) tbuf
	  gen_tmp = tbuf%tmp
	  gen_it  = tbuf%it
	  gen_tw  = tbuf%tw
	  gen_th0 = tbuf%th0
	  gen_th  = tbuf%th
	  gen_hf  = tbuf%hf
	  gen_ihf = tbuf%ihf
	  gen_hfw = tbuf%hfw
	  gen_pts = tbuf%pts
	  gen_mon = tbuf%mon
	  gen_mpf = tbuf%mpf
	  gen_mt  = tbuf%mt
	  gen_eft = 'MON'
	  gen_ec = tbuf%ec
	  gen_es = tbuf%es
	  gen_ef = tbuf%ef
	  ipol = 1 !no flippers with t buffer
	ELSEIF (type .EQ. 'R')THEN
	  OPEN (UNIT= 3, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 
     1                'DIRECT', RECL=RBUF_RECL,FORM= 'UNFORMATTED')
	  READ (3,REC=numb) rbuf
	  gen_tmp = rbuf%tmp
	  gen_it  = rbuf%it
	  gen_tw  = rbuf%tw
	  gen_th0 = rbuf%th0
	  gen_th  = rbuf%th
	  gen_hf  = rbuf%hf
	  gen_ihf = rbuf%ihf
	  gen_hfw = rbuf%hfw
	  gen_pts = rbuf%pts
	  gen_mon = rbuf%mon0
	  gen_mpf = rbuf%mpf
	  gen_mt  = rbuf%mt
	  ipol = 0 !get total number of flipper positions in case polar
	  DO j = 1, 4
	      IF (rbuf%flip_state(j))ipol = ipol + 1
	  END DO
	  IF (ipol .eq. 0)   ipol = 1
	ELSE !error
	   errflag = .TRUE.
	ENDIF !buffer type

	CLOSE (UNIT = 3)

	RETURN
	END
