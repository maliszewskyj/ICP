#ifndef _trace_h
#define _trace_h
extern int debug;
void TIMESTAMP();
int  DTRACE(char *, int *);
void DEBUGLEVEL(int *);
void DEBUGVAL(int *);

#define TRACE(x...) { TIMESTAMP(); printf( x ); }
#endif
