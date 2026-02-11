
/* $Id$ */
#ifndef _TCP_IO_H
#define _TCP_IO_H

/*
 * Define opcodes
 */
#define OP_CMD  1 /* Plaintext command */
#define OP_ACK  2 /* Acknowledgement */
#define OP_DAT  3 /* Data */
#define OP_ERR  4 /* Error, see error codes */
#define OP_SET  5 /* Set parameters */
#define OP_GET  6 /* Get parameters */

/*
 * Define error codes
 */
#define ERR_UNDEF  0
#define ERR_ACCESS 1
#define ERR_BADOP  2
#define ERR_BADID  3

/*
 * Command Syntax defines
 */
#define CMD_NOP      0 /* No operation */
#define CMD_START    1 /* Arm */
#define CMD_STOP     2 /* Disarm */
#define CMD_CLEAR    3 /* Clear remote histogram */
#define CMD_XFER     4 /* Transfer histogram data */
#define CMD_STAT     5 /* Get status */
#define CMD_SPY      6 /* Get time of last transfer */
#define CMD_WRITE    7 /* Write XML file with settings */

#define SET_SIZ     0

#define GET_RANK    0 /* Get rank of the data array */
#define GET_DIMS    1 /* Get dimension array */

/*
 * "Boundary" defines
 */

#define MAXBUF     2048        /* Xmit and recv buffer length (in longwords)*/
#define MAXBLOCK    256
#define EOS           0

/*
 * Function prototypes
 */
void err_ret(char *);
void err_dump(char *);
void phex(char *, int);
int  net_open (char *, char *, int);
int  net_server (unsigned short);
int  net_listen(int );
void net_close(int);
void net_debug(int );
int  sendPKG(int, int, int, void *, int);
int  recvPKG(int, int *, int *, int *, void *);
int  sendHIST(int, int *, int, int);
int  recvHIST(int, long *, int);
#endif /* _TCP_IO_H */
