/*
 * udpbroad - perform UDP broadcasts of ICP data
 *            Nick Maliszewskyj
 */
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define BROADCASTPORT 8080
#ifdef TEST
#define BROADCASTADDR "127.0.0.1"
#else
#define BROADCASTADDR "129.6.123.255"
#endif

/* Global variables */
static int udpsockfd=-1;
struct sockaddr_in cli_addr, serv_addr;
extern int debug;

int UDPINIT() 
{
  int bcastopt = 1;
  /* printf("Initializing socket.\n");*/
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family      = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_BROADCAST;
  serv_addr.sin_port        = htons(BROADCASTPORT);

  /* 
   * Open a UDP socket (an Internet datagram socket)
   */
  if ((udpsockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    fprintf(stderr,"UDPINIT: Can't open datagram socket.");
    exit(1);
  }
  
  setsockopt(udpsockfd, SOL_SOCKET, SO_BROADCAST, &bcastopt, sizeof(bcastopt));

  bzero((char *) &cli_addr, sizeof(cli_addr));
  cli_addr.sin_family      = AF_INET;
  cli_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  cli_addr.sin_port        = htons(0);
  if (bind(udpsockfd, (struct sockaddr *) &cli_addr, sizeof(cli_addr)) < 0) {
    fprintf(stderr,"UDPINIT: Can't bind local address.");
    exit(2);
  }

  return 0;
}

void BROADCAST(int * len, char * wrstring) 
{

  char prstring[120];
  int prlen;
  prlen = (*len > 119) ? 119 : *len;
  bzero(prstring,sizeof(prstring));
  strncpy(prstring,wrstring,prlen);
  if (debug) printf("Sending: %s\n",prstring); 

  if (sendto(udpsockfd, wrstring, *len, 0, 
	     (struct sockaddr *)&serv_addr, sizeof(serv_addr)) 
      != *len) {
    if (debug) perror("Unable to broadcast:");
  }
}








