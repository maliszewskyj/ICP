#define NUMRS232 10

typedef struct {
  char * dev;
  int baud;
  int bits;
  int parity;
  int flowcontrol;
} RS232PARM;


