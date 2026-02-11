#ifndef __SERIAL_H
#define __SERIAL_H
/* $Id$ */
/* Define general error conditions. Save -1..-9 for specific errors */
#define READ_ERROR       -10
#define READ_BUF_OVFL    -11
#define READ_TIMEOUT     -12

#define SERIALTIMEOUT      5

#define SOH 0x01
#define STX 0x02
#define ENQ 0x05
#define ACK 0x06
#define ETX 0x03
#define EOT 0x04
#define DLE 0x10
#define NAK 0x15
#define ETB 0x17

int serialOpen(char *, int, int, int, int);  /* Set serial characteristics */
int serialFlush(int);
int timed_read(int, char *, int);            /* Read w/ timeout */
int terminated_read(int, char *, int, char); /* Read until get terminator */
int termset_read(int , char * , int , char * ); /* Read until we see any
                                                   one of a set of terminating
                                                   chars*/
#endif
