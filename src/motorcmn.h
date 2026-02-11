/*
	logical*1 moving(maxmots),overshot(maxmots)
	real*4 destination(maxmots)
	integer*2 mcchan(maxtypes) !channels assigned to motor controllers
	integer*4 motor_wait_time(2)
	character*2 motorstring(maxmots)
	logical	  noback !no backlash correction
	common /mccom/mcchan,moving,overshot,destination,motor_wait_time,
     1	   motorstring, noback
*/
#define MAXMOTS  30
#define MAXTYPES 8
struct {
  unsigned char moving[MAXMOTS];
  unsigned char overshot[MAXMOTS];
  float destination[MAXMOTS];
  short mcchan[MAXTYPES];
  int motor_wait_time[2];
  char motorstring[2][MAXMOTS];
  unsigned int noback;
} _CMCCOM;
