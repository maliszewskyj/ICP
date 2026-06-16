#include "icp.h"
      SUBROUTINE ReadHisto
C     ***************************


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Reads the histogram memory                                              C
C     Called from: Scaler                                                     C
C                                                                             C
C     Gayle Greene, September 1991                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      use histo
      use tc
      use asd
      use iso_c_binding
      IMPLICIT  none
      interface
#include "c_debugval.h"
#include "c_tcp_aim.h"
      end interface

      integer*4 d0, i, j, k,retn
      integer*2 mode/16/        !type of transfer  - QRPT
      integer*4 cb(4)
      INTEGER*4  d2/2/,d00/0/      
      integer*4 dbg,element
      real*4    temphist(256)

      CALL DEBUGVAL(dbg)
C--------Be sure histo and router are disabled if using tdboard---------
      IF (aimpsd) THEN
	 CALL AIM_DISARM()
         IF (psd) THEN
            retn = AIM_XFER(histodata, nocells)
            DO i=1,nocells
	       IF (dbg.gt.1) write(*,fmt='(a,i3,a,i8)') 'Element # ',i,
     $              ' CTS = ',histodata(i)
            ENDDO
         ELSE IF (use_asd) THEN
            IF (nsta .eq. -11) THEN
               asd_xdim = 512
               asd_ydim = 512
            ELSE IF (nsta .eq. 8) THEN
               asd_xdim = 960
               asd_ydim = 20
            ELSE
               retn = AIM_DIMS(asd_xdim, asd_ydim) ! Get size of histogram to transfer
            ENDIF
! make sure that we have a nonzero asd_xdim & asd_ydim
            element = asd_xdim * asd_ydim ! total size of storage
            retn = AIM_XFER(asd_storage, element)
            DO i = 1, asd_xdim
               DO j = 1, asd_ydim
                  k = i + (j - 1) * asd_xdim 
                  asd_rawdata(i,j) = asd_storage(k)
               ENDDO
            ENDDO
            IF (keep_rawasd) THEN
               CALL AIM_IMAGE(asd_storage,asd_xdim,asd_ydim)
               CALL AIM_SAVE(asd_storage,asd_xdim,asd_ydim)
            ENDIF
         ENDIF
      ENDIF

C     Get intensity sum
      fpwindow = 0.0
      if (psd) then
         do i = asd_xmin, asd_xmax
            fpwindow = fpwindow + histodata(i)
         enddo
      else if (use_asd) then
         if (asd_xmin .lt. 1) asd_xmin = 1
         if (asd_xmin .ge. asd_xdim) asd_xmin = asd_xdim - 1
         if (asd_xmax .lt. asd_xmin) asd_xmax = asd_xmin + 1
         if (asd_xmax .gt. asd_xdim) asd_xmax = asd_xdim
         if (asd_ymin .lt. 1) asd_ymin = 1
         if (asd_ymax .gt. asd_ydim) asd_ymax = asd_ydim

         do i = asd_xmin, asd_xmax
            do j = asd_ymin, asd_ymax
               fpwindow = fpwindow + asd_rawdata(i,j)
            enddo
         enddo
      endif

      RETURN
      END
