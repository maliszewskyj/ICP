#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Count (moncounts)
	IMPLICIT    none
	REAL*4	moncounts
	INCLUDE INSTR_CMN
	EXTERNAL Preset_Lam    ! Subroutine to execute when preset reaches zero
	m_count = .TRUE.
	counting = .TRUE.                           ! set flag for main program
	CALL DATE_TIME (start_time)
	call preset_lam
	RETURN
	END
      
	SUBROUTINE CnTime (time)
	IMPLICIT    none
	REAL*4      time           ! at call, contains time to count in seconds
	INTEGER*4  status
	INCLUDE INSTR_CMN    ! module addresses for this instrument
 	  t_count = .TRUE.            
 	  counting = .TRUE.           ! set flag to indicate counting
 	  CALL DATE_TIME (start_time)
	  call libwait(time,status)  !maybe simulate time?
	  call timer_ast
  	RETURN
	END


	SUBROUTINE Scaler (counts)
	IMPLICIT     none
	INTEGER*4    counts(37)
	LOGICAL   READEF
	INCLUDE INSTR_CMN
 50	IF (counting) THEN                               ! ast not yet executed
	   call preset_lam
	   call timer_ast
	END IF
	CALL DATE_TIME (stop_time)
  	IF (.NOT. counting) THEN                               
	   IF (m_count .OR. t_count) CALL ReadScl (counts)    
	   IF (.NOT. m_count .or. nsta .eq. -7) CALL ReadMon (counts) 
	   IF (PSD) Call Readhisto
	   GOTO 200
	ELSE 
	   IF (READEF(contr_a) ) THEN
	      CALL SclInh
	      CALL MonClr
	      GOTO 200
	   ELSE
	      GOTO 50
	   END IF
	END IF             
 200  	m_count = .FALSE.
	t_count = .FALSE.
	RETURN
	END

	SUBROUTINE ReadScl (counts)
	IMPLICIT  none
	INTEGER*2 nscaler, ascaler
	INTEGER*4 i, j, isdata, counts(37)
	INCLUDE INSTR_CMN
	IF (nsta .EQ. 1) THEN                             !go read LeCroy scalers
	   DO j= 0, 2
	      nscaler = scl_add + j
	      DO i= 0, 11                      ! clears module & lam only at a=11
	         ascaler = i
	         counts(j*12+i+2) = i*10+j*100
	      END DO                              ! for each subaddress in module
	   END DO                                               ! for each module
	ELSE                                                 !read other scalers
	   nscaler = scl_add
	   DO i= 1, scl
	     ascaler = scl_chn + i - 1
	     counts(i+1) = i*10
	   END DO
	END IF
	RETURN
	END
      

	SUBROUTINE ReadMon (counts)
	IMPLICIT none
	INTEGER*4 counts(37)
	counts(1) = 10
	RETURN
	END
      
	SUBROUTINE Camac_Init
	IMPLICIT none
	return
	END	

	SUBROUTINE RLYON(NORELAY)
	IMPLICIT NONE
	INTEGER*4 NORELAY
	RETURN 
	END

	SUBROUTINE RLYOFF(NORELAY)
	IMPLICIT NONE
	INTEGER*4 NORELAY
	RETURN 
	END

        SUBROUTINE ReadHisto
        IMPLICIT  none
	integer*4 d0
	INCLUDE HISTO_CMN
	do d0 = 1,256
	  histodata(d0)=d0
	enddo
        RETURN
        END

	SUBROUTINE Monclr
	IMPLICIT none
	RETURN
	END

	SUBROUTINE Gcam16 (gchan, gc, gn, ga, gf, gdata2, gcamstat,qflag)
	IMPLICIT none
	INTEGER*2 gchan, gc, gn, ga, gf, gdata2
	INTEGER*4 gdata4, gcamstat(6)
	LOGICAL   c16
	LOGICAL*1 qflag
	return
	END

	SUBROUTINE Gcam24 (gchan, gc, gn, ga, gf, gdata2, gcamstat,qflag)
	IMPLICIT none
	INTEGER*2 gchan, gc, gn, ga, gf, gdata2
	INTEGER*4 gdata4, gcamstat(6)
	LOGICAL   c16
	LOGICAL*1 qflag
	return
	END

	SUBROUTINE Sclinh
	return
	END	

	SUBROUTINE READ_RELAY(inval)
	IMPLICIT NONE
	INTEGER*4 INVAL
	inval = 'ffffffff'x
	RETURN 
	END

	SUBROUTINE AMDIO_OUT(ibyte)
	INTEGER*1 ibyte
	RETURN
	END

	SUBROUTINE SCALERINIT
	RETURN
	END
