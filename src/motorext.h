/*
 *
 */
#define MAXMOTS  30
#define MAXTYPES 8
extern struct {
  unsigned char moving[MAXMOTS];
  unsigned char overshot[MAXMOTS];
  float destination[MAXMOTS];
  short mcchan[MAXTYPES];
  int motor_wait_time[2];
  char motorstring[2][MAXMOTS];
  unsigned int noback;
} _CMCCOM;
