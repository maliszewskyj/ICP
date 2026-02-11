#include "icp.h"
	SUBROUTINE Parse_Dir (typebuf)
C	***********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes DIR Command And Returns to Parse_Contr.			      C
C                                                                             C
C     Gayle Greene, 1997			                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	IMPLICIT      none

C  Calling Parameters
	character*1	typebuf !contains character I,B,T,Q,R, or X at call

C  Local Variables
	character*80	response
	integer*4	ios, i
	character*6	str6
	character*8	str8

	TYPE(i_buf) ibuf
	TYPE(q_buf) qbuf
	TYPE(b_buf) bbuf
	TYPE(t_buf) tbuf

	IF (typebuf .EQ. 'I') THEN                           ! DirI
	      response(1:) = '                     Directory of the Increment Buffers:'
	      CALL Putcontr_NT ( response)
	      response(1:) = 'No  comment      A-4 beg   I-4     PTS     MON  MPF    TMP'
	      CALL Putcontr_NT ( response)
	      CALL Putcontr_NT ( ' ')
	      OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1           'DIRECT',RECL=IBUF_RECL, FORM= 'UNFORMATTED')
	      DO i = 1, NBUF
		 READ (1,REC=i) ibuf
		 CALL Putmax(ibuf%tmp,str6,6,3)
	         WRITE (UNIT=response(1:), FMT= 
     1		 '(i2,2x,a10,2x,f8.3,2x,f6.3,2x,i4,2x,i7,1x,i3,2x,a6)',
     1		  iostat=ios)
	2              i, ibuf%com(1:10), ibuf%a(4), ibuf%i(4), NINT(ibuf%pts),
	3	       NINT(ibuf%mon), NINT(ibuf%mpf), str6
		 CALL Putcontr_NT ( response)
	      END DO
	      CLOSE(1)
	ELSE IF (typebuf .EQ. 'T') THEN                           ! DirT
	      IF (nsta .NE. 4) CALL Not_Implemented (*520)
	      response(1:) = '                      Directory of the Trash Buffers:'
	      CALL Putcontr_NT ( response)
	      response(1:) = 'No Comment                   EC     ES    FixE   Mon     Tmp     Pts '
	      CALL Putcontr_NT ( response)
	      CALL Putcontr_NT ( ' ')
	      OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1           'DIRECT',RECL=TBUF_RECL, FORM= 'UNFORMATTED')
	      DO i = 1, NBUF
		 READ (1,REC=i) tbuf       
		 CALL Putmax(tbuf%tmp,str8,8,3)                                       
		 WRITE (UNIT=response(1:), FMT= 
     1		 '(i2,1x,a20,1x,f8.2,2f7.2,i7,a8,i7)',iostat=ios)
	2              i, tbuf%com (1:20), tbuf%ec, tbuf%es, tbuf%ef, 
	3	       NINT(tbuf%mon), str8, NINT(tbuf%pts)
		 CALL Putcontr_NT ( response)
	      END DO
	      CLOSE(1)
	ELSE IF (typebuf .EQ. 'B') THEN                          ! DirB
	      IF (nsta .EQ. 1) CALL Not_Implemented (*520)
	      response(1:) = '                      Directory of the Bragg Buffers:'
	      CALL Putcontr_NT ( response)
	      response(1:) = 'No comment        HC    KC    LC        FE     MON   Inc3  Inc4    PTS    TMP'
	      CALL Putcontr_NT ( response)
	      CALL Putcontr_NT ( ' ')
	      OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1           'DIRECT',RECL=BBUF_RECL, FORM= 'UNFORMATTED')
	      DO i = 1, NBUF
		 READ (1,REC=i) bbuf
		 CALL Putmax(bbuf%tmp,str8,8,3)
	 	 WRITE (UNIT=response(1:), FMT= 
     1		 '(i2,2x,a10,2x,3f6.2,f8.2,i8,2f7.2,i5,a8)',iostat=ios)
	2	       i, bbuf%com(1:10), bbuf%hc, bbuf%kc, bbuf%lc, bbuf%ef,
	3	        NINT(bbuf%mon),bbuf%i(3), bbuf%i(4), NINT(bbuf%pts), 
	4	       str8
		 CALL Putcontr_NT ( response)
	      END DO
	      CLOSE(1)
	ELSE                                                          ! DirQ
	      IF (nsta .EQ. 1) CALL Not_Implemented (*520)
	      response(1:) = '                      Directory of the Const-q Buffers:'
	      CALL Putcontr_NT ( response)
	      response(1:) = 'No  comment       HC    KC    LC      FE     EC     ES    PTS    MON     TMP'
	      CALL Putcontr_NT ( response)
	      CALL Putcontr_NT ( ' ')
	      OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1           'DIRECT', RECL=QBUF_RECL,FORM= 'UNFORMATTED')
	      DO i = 1, NBUF
		 READ (1,REC=i) qbuf
		 CALL Putmax(qbuf%tmp,str8,8,3)
	 	 WRITE (UNIT=response(1:), FMT= 
     1		 '(i2,2x,a10,2x,3f6.2,f8.2,2f7.2,i5,i8,a8)',iostat=ios)
	2	       i, qbuf%com(1:10), qbuf%hc, qbuf%kc, qbuf%lc, qbuf%ef,
	3	       qbuf%ec, qbuf%es, NINT(qbuf%pts), NINT(qbuf%mon), 
	4	       str8
		 CALL Putcontr_NT ( response)
	      END DO
	      CLOSE(1)
	END IF !typebuf
 520	RETURN

	END
