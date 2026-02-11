#ifndef _dfs_h
#define _dfs_h

#define DFS_TIMEOUT     5

#define DAS_NOTIFY_PORT 8311
#define DAS_DATAFW_PORT 8312
#define DAS_CMDREQ_PORT 8313

typedef struct _HEADER {
  int iReceiveID;
  int iCommandID;
  int length;
  int Spare1;
  int Spare2;
  int Spare3;
} HEADER;

#ifndef MAXUDPBUFFER
#define MAXUDPBUFFER 32768
#endif

typedef struct _RXPACKET {
  HEADER header;
  char buffer[MAXUDPBUFFER-sizeof(HEADER)];
} RXPACKET;

typedef struct _NEUTRON_EVENT {
  unsigned int tof;
  unsigned int pixelid;
} NEUTRON_EVENT;

/* Requests for port DAS_NOTIFY_PORT */
#define DASSTARTED 0x100
#define DASSTOPPED 0x101
#define DASPAUSED  0x102
#define DASRESUMED 0x105
#define DASSAVED   0x201

/* Valid requests for port DAS_CMDREQ_PORT */
#define REQUESTSTART  0x2100
#define REQUESTSTOP   0x2101
#define REQUESTPAUSE  0x2102
#define REQUESTRESUME 0x2106
#define REQUESTSAVE   0x2107
#define REQUESTSTATUS 0x2561
#define LOGIN         0x10
#define ECHO          0x12

#define ACKNOWLEDGE   0x40000000
#define ERROR         0x80000000

#endif /* _dfs_h */
