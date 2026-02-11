C
C Set of functions to replace the VAX "degrees" versions of trigonometric
C functions
C
#define RAD_IN_DEG   57.2957795
#define DEG_IN_RAD    0.01745329

      REAL*4 FUNCTION COSD (degrees)
      IMPLICIT NONE
      REAL*4 pi,deg_in_rad,degrees
      pi=atan2(0.0,-1.0)
      deg_in_rad = pi / 180.0
      COSD = COS(deg_in_rad * degrees)
      RETURN
      END

      REAL*4 FUNCTION SIND (degrees)
      IMPLICIT NONE
      REAL*4 pi,deg_in_rad,degrees
      pi=atan2(0.0,-1.0)
      deg_in_rad = pi / 180.0
      SIND = SIN(deg_in_rad * degrees)
      RETURN
      END

      REAL*4 FUNCTION TAND (degrees)
      IMPLICIT NONE
      REAL*4 pi,deg_in_rad,degrees
      pi=atan2(0.0,-1.0)
      deg_in_rad = pi / 180.0
      TAND = TAN(deg_in_rad * degrees)
      RETURN
      END

      REAL*4 FUNCTION ACOSD (arg)
      IMPLICIT NONE
      REAL*4 arg, pi,rad_in_deg, degrees
      pi = atan2(0.0,-1.0)
      rad_in_deg = 180.0/pi
      degrees = rad_in_deg * ACOS(arg)
      ACOSD = degrees
      RETURN
      END

      REAL*4 FUNCTION ASIND (arg)
      IMPLICIT NONE
      REAL*4 arg, pi, rad_in_deg,degrees
      pi = atan2(0.0,-1.0)
      rad_in_deg = 180.0/pi
      degrees = rad_in_deg * ASIN(arg)
      ASIND = degrees
      RETURN
      END

      REAL*4 FUNCTION ATAND (arg)
      IMPLICIT NONE
      REAL*4 arg, pi,rad_in_deg,degrees
      pi = atan2(0.0,-1.0)
      rad_in_deg = 180.0/pi
      degrees = rad_in_deg * ATAN(arg)
      ATAND=degrees
      RETURN
      END

      REAL*4 FUNCTION ATAN2D (arg1, arg2)
      IMPLICIT NONE
      REAL*4 arg1,arg2,pi,rad_in_deg,degrees
      pi = atan2(0.0,-1.0)
      rad_in_deg = 180.0/pi
      degrees = rad_in_deg * ATAN2(arg1,arg2)
      ATAN2D=degrees
      RETURN
      END
