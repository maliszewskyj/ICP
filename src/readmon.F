#include "icp.h"
      SUBROUTINE ReadMon (counts)
C     ***************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Reads the preset counter after a time count                             C
C     Called from: Scaler                                                     C
C                                                                             C
C     Gayle Greene, Beat Schmid,  July 1989                                   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      use instr
      IMPLICIT none
      
      INTEGER*4 counts(37)
      INTEGER*2 ireg(0:1)
      INTEGER*4 i4reg
      EQUIVALENCE (i4reg,ireg(0))

      ! Fake it out
      counts(1) = 10
      RETURN
      END
