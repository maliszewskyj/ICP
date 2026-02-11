#include "icp.h"
C     
C     This routine is part of the suit of programs of the fit
C     software.
C     
C     Taken from:
C     NUMERICAL RECIPES,
C     The Art of Scientific Computing
C     
C     William H. Press
C     Brian P. Flannery
C     Saul A. Teukolsky
C     William T. Vetterling
C     
C     Cambridge University Press 1986
C     
      SUBROUTINE GAUSSj3 ( A , N , NP , B , M , MP , ERROR)
      IMPLICIT NONE
      INTEGER M, MP, N, NP, NMAX
      REAL A(NP,NP) , B(NP,MP)
      PARAMETER (NMAX=3)
C     
C     Linear equation solution by Gauss-Jordan elimination. A is an
C     input matrix of NxN elements, stored in an array of physical
C     dimensions NPxNP. B is an input matrix of NxM containing the M
C     right-handed side vectors, stored in an array of physical
C     dimensions NPxMP. On output, A is replaced by its matrix inverse,
C     and B is replaced by the corresponding set of solution vectors.
C     
      INTEGER I, ICOL, IROW, J, K, L, LL, INDXR(NMAX), INDXC(NMAX),
     >     IPIV(NMAX)
      REAL BIG, DUM, PIVINV
      LOGICAL ERROR
      ERROR = .FALSE.
      DO 11 , J=1 , N
         IPIV(J) = 0
 11   CONTINUE
      DO 22 , I=1 , N
         BIG = 0.
         DO 13 , J=1 , N
            IF ( IPIV(J) .NE. 1 ) THEN
               DO 12 , K=1 , N
                  IF (IPIV(K) .EQ. 0 ) THEN
                     IF ( ABS(A(J,K)) .GE. BIG ) THEN
                        BIG  = ABS(A(J,K))
                        IROW = J
                        ICOL = K
                     ENDIF
                  ELSE IF (IPIV(K) .GT. 1 ) THEN
                     ERROR = .TRUE.
C     
C---  This means the matrix is singular (useless for virtual motors)
C     
                     RETURN
                  ENDIF
 12            CONTINUE
            ENDIF
 13      CONTINUE
         IPIV(ICOL) = IPIV(ICOL)+1
         IF (IROW .NE. ICOL) THEN
            DO 14 , L=1 , N
               DUM       = A(IROW,L)
               A(IROW,L) = A(ICOL,L)
               A(ICOL,L) = DUM
 14         CONTINUE
            DO 15 , L=1 , M
               DUM       = B(IROW,L)
               B(IROW,L) = B(ICOL,L)
               B(ICOL,L) = DUM
 15         CONTINUE
         ENDIF
         INDXR(I) = IROW
         INDXC(I) = ICOL
         IF (A(ICOL,ICOL) .EQ. 0.) THEN
            ERROR = .TRUE.
C     
C---  This means the matrix is singular (useless for virtual motors)
C     
            RETURN
         ENDIF
         PIVINV = 1./A(ICOL,ICOL)
         A(ICOL,ICOL) = 1.
         DO 16 , L=1 , N
            A(ICOL,L) = A(ICOL,L) * PIVINV
 16      CONTINUE
         DO 17 , L=1 , M
            B(ICOL,L) = B(ICOL,L) * PIVINV
 17      CONTINUE
         DO 21 , LL=1 , N
            IF ( LL .NE. ICOL ) THEN
               DUM        = A(LL,ICOL)
               A(LL,ICOL) = 0.
               DO 18 , L=1 , N
                  A(LL,L) = A(LL,L) - A(ICOL,L)*DUM
 18            CONTINUE
               DO 19 , L=1 , M
                  B(LL,L) = B(LL,L) - B(ICOL,L)*DUM
 19            CONTINUE
            ENDIF
 21      CONTINUE
 22   CONTINUE
      DO 24 , L=N , 1 , -1
         IF ( INDXR(L) .NE. INDXC(L) ) THEN
            DO 23 , K=1 , N
               DUM           = A(K,INDXR(L) )
               A(K,INDXR(L)) = A(K,INDXC(L))
               A(K,INDXC(L)) = DUM
 23         CONTINUE
         ENDIF
 24   CONTINUE
      RETURN
      END

      SUBROUTINE maprod3 ( A, V , AV)
C     
C---  Calculates product of matrix A and vector V, puts result in vector AV
C     
      IMPLICIT NONE
      REAL A(3,3), V(3), AV(3)
      INTEGER I,J
      DO I=1,3
         AV(I) = 0.
         DO J=1,3
            AV(I) = AV(I) + A(I,J)*V(J)
         ENDDO
      ENDDO
      RETURN
      END


      SUBROUTINE VINIT_BT8
C     ***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     C
C     VINIT_BT8: initialize bt8 virtual motors				      C
C     C
C     Gayle Greene, July , 1996                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

      INTEGER*4	i, lpnt

      lpnt = 1

C     Motor # 21 is a virtual motor which causes motors 11, 12, 13 to be moved
      IF (motfl(21) .AND. mottype(21) .EQ. VIRT)THEN
         numreal(21) = 3
         pntlist(21) = lpnt
         motlist(lpnt) = 11
         motlist(lpnt+1) = 12
         motlist(lpnt+2) = 13
         lpnt = lpnt + 3
         mottype(21) = NOTYETVM !not vm until matrix initialized
         motfl(21) = .FALSE.    !will remain false until matrix initialized
      ENDIF

C     Motor # 22 is a virtual motor which causes motors 11, 12, 13 to be moved
      IF (motfl(22) .AND. mottype(22) .EQ. VIRT)THEN
         numreal(22) = 3
         pntlist(22) = lpnt
         motlist(lpnt) = 11
         motlist(lpnt+1) = 12
         motlist(lpnt+2) = 13
         lpnt = lpnt + 3
         mottype(22) = NOTYETVM !not vm until matrix initialized
         motfl(22) = .FALSE.    !will remain false until matrix initialized
      ENDIF

C     Motor # 23 is a virtual motor which causes motors 11, 12, 13 to be moved
      IF (motfl(23) .AND. mottype(23) .EQ. VIRT)THEN
         numreal(23) = 3
         pntlist(23) = lpnt
         motlist(lpnt) = 11
         motlist(lpnt+1) = 12
         motlist(lpnt+2) = 13
         lpnt = lpnt + 3
         mottype(23) = NOTYETVM !not vm until matrix initialized
         motfl(23) = .FALSE.    !will remain false until matrix initialized
      ENDIF

C     Motor # 24 is a virtual motor which causes motor 14 to be moved 
C     The position of motor 15 is used in calculating 14's destination
      IF (motfl(24) .AND. mottype(24) .EQ. VIRT)THEN
         numreal(24) = 1
         pntlist(24) = lpnt
         motlist(lpnt) = 14
         lpnt = lpnt + 1
      ENDIF

C     Motor # 25 is a virtual motor which causes motors 14 & 15 to be moved
      IF (motfl(25) .AND. mottype(25) .EQ. VIRT)THEN
         numreal(25) = 2
         pntlist(25) = lpnt
         motlist(lpnt) = 15
         motlist(lpnt+1) = 14
         lpnt = lpnt + 1
      ENDIF

      return
      end

      SUBROUTINE V21_BT8(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V21_BT8: calculate positions for real motors associated with virt motor    C
C     21 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		rpos(3), vpos(3), RdAngl

      motorno = 21

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      vpos(1) = targetangle
      vpos(2) = rdangl(22) - z(22)
      vpos(3) = rdangl(23) - z(23)

      call maprod3(vm_a_inv,vpos,rpos)


      poslist(lpointer)   = rpos(1) 
      poslist(lpointer+1) = rpos(2) 
      poslist(lpointer+2) = rpos(3)

      virtpos(motorno) = targetangle
      errflag = .FALSE.


      RETURN

      END


      SUBROUTINE RDV21_BT8(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV21_BT8: calculate position of virtual motor 21 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		rpos(3), vpos(3), RdAngl


      motorno = 21

      lpointer = pntlist(motorno)

      rpos(1) = rdangl(11) - z(11) !get software angle of motor 11
      rpos(2) = rdangl(12) - z(12) !get software angle of motor 12
      rpos(3) = rdangl(13) - z(13) !get software angle of motor 13


      IF (rpos(1) .EQ. -999.99 -z(11) .OR. rpos(2) .EQ. -999.99 - z(12) .OR.
     1     rpos(3) .EQ. -999.99 - z(13))THEN
         CALL PUTCONTR(' error reading positions of real motors')
         pos = -999.99
         errflag = .TRUE.
      ELSE
         CALL Maprod3(vm_a,rpos,vpos)
         errflag = .FALSE.
         pos = vpos(1)
      ENDIF


      RETURN

      END

      SUBROUTINE V22_BT8(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V22_BT8: calculate positions for real motors associated with virt motor    C
C     22 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		rpos(3), vpos(3), RdAngl

      motorno = 22

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      vpos(1) = rdangl(21) - z(21)
      vpos(2) = targetangle
      vpos(3) = rdangl(23) - z(23)

      call maprod3(vm_a_inv,vpos,rpos)

      poslist(lpointer)   = rpos(1) 
      poslist(lpointer+1) = rpos(2) 
      poslist(lpointer+2) = rpos(3)

      virtpos(motorno) = targetangle
      errflag = .FALSE.


      RETURN

      END

      SUBROUTINE RDV22_BT8(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV22_BT8: calculate position of virtual motor 22 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		rpos(3), vpos(3), RdAngl


      motorno = 22

      lpointer = pntlist(motorno)

      rpos(1) = rdangl(11) - z(11) !get software angle of motor 11
      rpos(2) = rdangl(12) - z(12) !get software angle of motor 12
      rpos(3) = rdangl(13) - z(13) !get software angle of motor 13


      IF (rpos(1) .EQ. -999.99 -z(11) .OR. rpos(2) .EQ. -999.99 - z(12) .OR.
     1     rpos(3) .EQ. -999.99 - z(13))THEN
         CALL PUTCONTR(' error reading positions of real motors')
         pos = -999.99
         errflag = .TRUE.
      ELSE
         CALL Maprod3(vm_a,rpos,vpos)
         errflag = .FALSE.
         pos = vpos(2)
      ENDIF


      RETURN

      END

      SUBROUTINE V23_BT8(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V23_BT8: calculate positions for real motors associated with virt motor    C
C     23 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		rpos(3), vpos(3), RdAngl

      motorno = 23

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      vpos(1) = rdangl(21) - z(21)
      vpos(2) = rdangl(22) - z(22)
      vpos(3) = targetangle

      call maprod3(vm_a_inv,vpos,rpos)


      poslist(lpointer)   = rpos(1) 
      poslist(lpointer+1) = rpos(2) 
      poslist(lpointer+2) = rpos(3)

      virtpos(motorno) = targetangle
      errflag = .FALSE.


      RETURN

      END

      SUBROUTINE RDV23_BT8(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV23_BT8: calculate position of virtual motor 23 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		rpos(3), vpos(3), RdAngl


      motorno = 23

      lpointer = pntlist(motorno)

      rpos(1) = rdangl(11) - z(11) !get software angle of motor 11
      rpos(2) = rdangl(12) - z(12) !get software angle of motor 12
      rpos(3) = rdangl(13) - z(13) !get software angle of motor 13


      IF (rpos(1) .EQ. -999.99 -z(11) .OR. rpos(2) .EQ. -999.99 - z(12) .OR.
     1     rpos(3) .EQ. -999.99 - z(13))THEN
         CALL PUTCONTR(' error reading positions of real motors')
         pos = -999.99
         errflag = .TRUE.
      ELSE
         CALL Maprod3(vm_a,rpos,vpos)
         errflag = .FALSE.
         pos = vpos(3)
      ENDIF

      RETURN
      END


      SUBROUTINE V24_BT8(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V24_BT8: calculate positions for real motors associated with virt motor    C
C     24 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		poschi, pos14, RdAngl

C     Constant for motors 14 & 15 coupling
      REAL*4   vconst
      Parameter (vconst = 1./12.)

      motorno = 24

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      poschi= rdangl(25) - z(25) !get software angle of virt motor chi

      pos14 = targetangle - vconst * poschi

      poslist(lpointer) = pos14

      virtpos(motorno) = targetangle
      errflag = .FALSE.


      RETURN

      END

      SUBROUTINE RDV24_BT8(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV24_BT8: calculate position of virtual motor 24 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		pos14, pos15

      REAL*4		RdAngl

C     Constant for Virtual motor calculations
      REAL*4		vconst
      PARAMETER	(vconst = 1./12.)

      motorno = 24
      lpointer = pntlist(motorno)

      pos14 = RdAngl(14) - z(14) !get software position of motor 14
      pos15 = RdAngl(15) - z(15) !get software position of motor 15
c     Using positions in rlist, com up with position for virtual motorno 24
      IF (pos14 .EQ. -999.99 -z(14) .OR. pos15 .EQ. -999.99 -z(15))THEN
         CALL PUTCONTR(' error reading positions of real motors')
         errflag = .TRUE.
      ELSE
         pos = pos14 + vconst * pos15
         errflag = .FALSE.
      ENDIF


      RETURN

      END

      SUBROUTINE V25_BT8(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V25_BT8: calculate positions for real motors associated with virt motor    C
C     25 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		posphi, RdAngl


C     Constant in equations
      REAL * 4 vconst
      Parameter  (vconst = 1./12.)

      motorno = 25

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      posphi = rdangl(24) - z(24) !get software position of virt motor phi

      poslist(lpointer) = targetangle !mot 15
      poslist(lpointer+1) = posphi - vconst*targetangle !mot 14

      virtpos(motorno) = targetangle
      errflag = .FALSE.
      RETURN
      END

      SUBROUTINE RDV25_BT8(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV25_BT8: calculate position of virtual motor 25 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		pos14, pos15

C     Constant for Virtual motor calculations
      REAL*4		vconst
      PARAMETER	(vconst = 1./12.)

      REAL*4		RdAngl

      motorno = 25

      lpointer = pntlist(motorno)

      pos15 = RdAngl(15) - z(15) !get software position of motor 15
c     Using positions in rlist, com up with position for virtual motorno 25
      IF (pos15 .EQ. -999.99 -z(15))THEN
         CALL PUTCONTR(' error reading positions of motor 15')
         errflag = .TRUE.
      ELSE
         pos = pos15
         errflag = .FALSE.
      ENDIF


      RETURN

      END


      SUBROUTINE IVM_ARRAY(error,filename)
      USE INSTR
      USE VIRTMOT
      IMPLICIT NONE
      LOGICAL ERROR
      CHARACTER*(*)  filename

      INTEGER*4  I,J, IOS
      REAL*4  A_INV(3,3), A(3,3), B(3,3)

C     
C     /                        \
C     | A(1,1)  A(1,2)  A(1,3) |
C---  matrix shape in file and in FORTRAN: A =  | A(2,1)  A(2,2)  A(2,3) |
C     | A(3,1)  A(3,2)  A(3,3) |
C     \                        /
C     
      ERROR = .FALSE.           !default return

      OPEN (UNIT = 1, FILE = filename, FORM = 'FORMATTED',
     >     STATUS = 'UNKNOWN',IOSTAT = ios)
      IF (ios .NE. 0)THEN
         CALL Putcontr(' Err opening '//filename)
         ERROR = .TRUE.
         RETURN
      ENDIF

      DO I=1,3
         READ ( 1 , * , iostat = ios) (A(I,J),J=1,3)
         IF (ios .NE. 0)THEN
            CALL Putcontr(' Err reading '//filename)
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDDO

      CLOSE ( 1 )

C---  Put unit matrix in B (Sort of dummy array for this purpose of GAUSSJ)
      DO I=1,3
         DO J=1,3
            B(I,J) = 0.
         ENDDO
         B(I,I) = 1.
      ENDDO


C---  Put A in A_INV and let replace by inverse through GAUSSJ

      DO I=1,3
         DO J=1,3
            A_INV(I,J) = A(I,J)
         ENDDO
      ENDDO

      CALL GAUSSJ3 (A_INV, 3, 3, B, 3, 3, ERROR)
      IF (ERROR) THEN
         CALL Putcontr(' ERROR  !! Singular matrix')
         RETURN
      ENDIF

      DO I=1,3                  !move local variables to common ones since matrix OK
         DO J=1,3
	    VM_A(I,j) = A(I,J)
            VM_A_INV(I,J) = A_INV(I,J)
         ENDDO
      ENDDO

C     Since arrays initialized OK at least this once, reset flags to allow 
C     virtual motors 21, 22, and 23 to be used
      IF (mottype(21) .EQ. NOTYETVM)THEN 
         mottype(21) = VIRT
         motfl(21) = .TRUE.
      ENDIF
      IF (mottype(22) .EQ. NOTYETVM)THEN 
         mottype(22) = VIRT
         motfl(22) = .TRUE.
      ENDIF
      IF (mottype(23) .EQ. NOTYETVM)THEN 
         mottype(23) = VIRT
         motfl(23) = .TRUE.
      ENDIF


      RETURN
      END

      SUBROUTINE VINIT_BT9
C     ***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     C
C     VINIT_BT9: initialize bt9 virtual motors				      C
C     C
C     Nick Maliszewskyj, April 2007                                           C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

      INTEGER*4	i, lpnt

      lpnt = 1


      IF (motfl(24) .AND. mottype(24) .EQ. VIRT)THEN
         numreal(24) = 1
         pntlist(24) = lpnt
         motlist(lpnt) = 7
         lpnt = lpnt + 1
      ENDIF

C     Motor # 25 is a virtual motor which causes motors 14 & 15 to be moved
      IF (motfl(25) .AND. mottype(25) .EQ. VIRT)THEN
         numreal(25) = 2
         pntlist(25) = lpnt
         motlist(lpnt) = 8
         motlist(lpnt+1) = 7
         lpnt = lpnt + 1
      ENDIF

      return
      end

      SUBROUTINE V24_BT9(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V24_BT8: calculate positions for real motors associated with virt motor    C
C     24 on BT8 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Gayle Greene, July 1996		                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		poschi, pos14, RdAngl

C     Constant for motors 7 & 8 coupling
      REAL*4   vconst
      Parameter (vconst = 1./12.)

      motorno = 24

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      poschi= rdangl(25) - z(25) !get software angle of virt motor chi

      pos14 = targetangle - vconst * poschi

      poslist(lpointer) = pos14

      virtpos(motorno) = targetangle
      errflag = .FALSE.


      RETURN

      END

      SUBROUTINE RDV24_BT9(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV24_BT9: calculate position of virtual motor 24 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Nick Maliszewskyj, February 2007	                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		pos14, pos15

      REAL*4		RdAngl

C     Constant for Virtual motor calculations
      REAL*4		vconst
      PARAMETER	(vconst = 1./12.)

C      print *,'RDV24_BT9'
      motorno = 24
      lpointer = pntlist(motorno)

      pos14 = RdAngl(7) - z(7) !get software position of motor 14
      pos15 = RdAngl(8) - z(8) !get software position of motor 15
c     Using positions in rlist, com up with position for virtual motorno 24
      IF (pos14 .EQ. -999.99 -z(7) .OR. pos15 .EQ. -999.99 -z(8))THEN
         CALL PUTCONTR(' error reading positions of real motors')
         errflag = .TRUE.
      ELSE
         pos = pos14 + vconst * pos15
         errflag = .FALSE.
      ENDIF


      RETURN

      END

      SUBROUTINE V25_BT9(targetangle,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     V25_BT9: calculate positions for real motors associated with virt motor    C
C     25 on BT9 at position targetangle. Real positions are put in poslist.  C
C     C
C     C
C     Nick Maliszewskyj, February 2007	                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		targetangle
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno, i
      REAL*4		posphi, RdAngl


C     Constant in equations
      REAL * 4 vconst
      Parameter  (vconst = 1./12.)

      motorno = 25

      nummots = numreal(motorno)
      lpointer = pntlist(motorno)

      posphi = rdangl(24) - z(24) !get software position of virt motor phi

      poslist(lpointer) = targetangle !mot 8
      poslist(lpointer+1) = posphi - vconst*targetangle !mot 7

      virtpos(motorno) = targetangle
      errflag = .FALSE.
      RETURN
      END

      SUBROUTINE RDV25_BT9(pos,errflag)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     
C     RDV25_BT8: calculate position of virtual motor 25 using actual positions
C     of associated motors.  Calculated position returned in pos
C     unless errflag is set.
C     C
C     C
C     Nick Maliszewskyj, February 2007	                                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use virtmot
      IMPLICIT     none

C     Calling parameters
      REAL*4		pos
      LOGICAL		errflag


C     Local Variables
      INTEGER*4	nummots, lpointer, motorno
      REAL*4		pos14, pos15

C     Constant for Virtual motor calculations
      REAL*4		vconst
      PARAMETER	(vconst = 1./12.)

      REAL*4		RdAngl

C      print *,'RDV25_BT9'
      motorno = 25

      lpointer = pntlist(motorno)

      pos15 = RdAngl(8) - z(8) !get software position of motor 8
c     Using positions in rlist, com up with position for virtual motorno 25
      IF (pos15 .EQ. -999.99 -z(8))THEN
         CALL PUTCONTR(' error reading positions of motor 8')
         errflag = .TRUE.
      ELSE
         pos = pos15
         errflag = .FALSE.
      ENDIF


      RETURN

      END
