/*
 *  Read serial port configuration information from config files and
 *  associate this information with serial port "numbers" 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "rs232.h"
#include "instrcmn.h"
RS232PARM rs232_cnf[NUMRS232];

extern int debug;

/*
 * Trim string on the right hand side.
 */
void strtrimr(char *buf) 
{
  int buflen;
  char ch;
  
  buflen = strlen(buf);
  while(1) {
    if (isspace(buf[buflen-1])) {
      buf[buflen-1] = 0;
      buflen--;
    } else if ((ch = buf[buflen-1]) == ',') {
      buf[buflen-1] = 0;
      buflen--;
    } else {
      break;
    }
  }
}


void RS232_CONF()
{
  FILE * fp=NULL;
  char * home_dir;
  char conf_file[80];
  int i,portno;

  char inbuf[80], *ptr, *inptr, *end, ch;

  memset(rs232_cnf,0,sizeof(rs232_cnf));

  if ((home_dir = getenv("HOME")) == NULL) {
    sprintf(conf_file,"..");
  } else {
    strcpy(conf_file,home_dir);
  }
  //CHANGE ME
  strcpy(conf_file,"/usr/local/icp");
  strcat(conf_file,"/cfg/rs232.conf");

  fp = fopen(conf_file,"r");
  /* 
   * If we don't find a configuration file, just arbitrarily assign 
   * port number "1" to /dev/ttyS1,9600,8,none
   */
  if (fp == NULL) {
    if (debug) printf("Could not open serial configuration file %s\n",
		      conf_file);
    rs232_cnf[1].dev    = "/dev/ttyS0";
    rs232_cnf[1].baud   = 9600;
    rs232_cnf[1].bits   = 8;
    rs232_cnf[1].parity = 0;
    return;
  }

  /* 
   * If we find a configuration file, read it and fill in the entries in
   * the array rs232_cnf
   *
   * I think this may be a little fragile
   *
   */
  while(fgets(inbuf,80,fp) != NULL) {
    ptr = &inbuf[0];
    if (!isdigit((ch=*ptr))) continue;
    portno = (int) strtol(ptr,&end,0);
    if ((portno >= NUMRS232) || (portno < 0)) {
      fprintf(stderr,"RS232_CONF: portno out of range\n");
      continue;
    }
    ptr = end;
    while(isspace((ch=*ptr))) ptr++;
    inptr = ptr;
    i=0;
    while(!isspace((ch=*ptr))) {
      ptr++;
      i++;
    }
    rs232_cnf[portno].dev = (char *) malloc((i+1)* sizeof(char));
    strncpy(rs232_cnf[portno].dev,inptr,i);
    rs232_cnf[portno].dev[i] = 0; /* Make sure we include the trailing null*/
    rs232_cnf[portno].baud = (int) strtol(ptr,&end,0);
    ptr = end;
    rs232_cnf[portno].bits = (int) strtol(ptr,&end,0);
    while(!isalpha((ch=*ptr))) ptr++;
    switch(ch){
    case 'E':
      rs232_cnf[portno].parity = 2; break;
    case 'O':
      rs232_cnf[portno].parity = 1; break;
    case 'N':
    default:
      rs232_cnf[portno].parity = 0; 
    }
  }

  fclose(fp);
}






