C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     smg&prep.cmn:                                                           C
C                                                                             C
C     COMMON and VARIABLE declaration used mostly but not only by SMG-        C
C     routines in program ICP                                                 C
C     This part is used via an INCLUDE statement                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	MODULE smg_prep
	INTEGER*4  paste, keyb, main, help, menu, iorq, menui, prpi, menuq 
	INTEGER*4  prpq, menut, prpb, prpt, moni, sequ, contr, stat, ctrly
	INTEGER*4  m_nenr, m_nenrcl, mon_line, which_one, pola, menub
	INTEGER*4  ascan,hfdisp
	LOGICAL*4  flip
	CHARACTER*3     flp(4,2) /'OFF', 'ON ', 'OFF', 'ON ',
	1    'OFF', 'OFF', 'ON ', 'ON '/

	TYPE i_buf
	   CHARACTER*50 com
	   INTEGER*2    dum
	   REAL*4       a(6)
	   REAL*4       i(6)
	   REAL*4       pts , hf 
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       mon , mpf 
	   CHARACTER*4  mt 
	   LOGICAL*4    flip_state(4)
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE

	TYPE q_buf
	   CHARACTER*50 com
	   INTEGER*2    dum
	   REAL*4	a , b , c 
	   REAL*4	aa , bb , cc 
	   REAL*4       ec , es , ef 
	   CHARACTER*3  eft 
	   CHARACTER*1  pad
	   REAL*4	hc , kc , lc 
 	   REAL*4       hs , ks , ls , pts 
	   REAL*4	hkl1(3), a1 , hkl2(3)
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       hf , mon , mpf 
	   CHARACTER*4  mt 
	   LOGICAL    flip_state(4) 
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE

	TYPE b_buf
	   CHARACTER*50 com
	   INTEGER*2    dum 
	   REAL*4	lat_a , b , c 
	   REAL*4	aa , bb , cc 
	   REAL*4       ec , es , ef 
	   CHARACTER*3  eft 
	   CHARACTER*1  pad
	   REAL*4	hc , kc , lc 
 	   REAL*4       hs , ks , ls , pts 
	   REAL*4	hkl1(3), a1 , hkl2(3)
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       hf , mon , mpf 
	   CHARACTER*4  mt 
	   LOGICAL    flip_state(4) 
	   REAL*4       a(6) 
	   REAL*4       i(6)
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE

	TYPE t_buf
	   CHARACTER*50 com 
	   INTEGER*2    dum
	   REAL*4       ec , es , ef 
	   LOGICAL*1    m3 
	   CHARACTER*3  pad
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       hf , mon , mpf , pts 
	   CHARACTER*4  mt
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE

	TYPE lattice
	   REAL*4       a, b, c, aa, bb, cc
	END TYPE

	TYPE(q_buf) man_q
	END MODULE smg_prep

