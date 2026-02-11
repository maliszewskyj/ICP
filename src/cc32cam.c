/* $Id$ */
#define DEVICE_NAME "/dev/cc32_1"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <ctype.h>
#include "libcc32.h" /* the header of the shared library */

extern int debug;
static CC32_HANDLE cc32;

int CAMINIT (void) {
  int retn;
  char *ptr;
  cc32=0;

  if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"CAMINIT: cc32=0, no CAMAC services will be available\n");
      return 0;
  }
  if ((retn = cc32_open(DEVICE_NAME, &cc32)) < 0) {
    fprintf(stderr,"CAMINIT: Cannot initialize CAMAC system\n");
    exit(1);
  }
  return retn;
}

/*
  Function codes (F & 0x10) != 0 imply WRITING, otherwise we read
 */

int CAM16(short * gchan, short * gc, short * gn, short * ga, short * gf, short * gdata2, int gcamstat[]) {
  int wr, X, Q, inh, lam;

  wr = (*gf & 0x10) ? 1 : 0;
  /* Should somehow get X and Q values */
  if (debug) printf("CAM16: n = %d a = %d f = %d ",*gn,*ga,*gf);
  if (wr) {
    cc32_write_word(cc32, (unsigned int) *gn, (unsigned int) *ga,
		    (unsigned int) *gf, *gdata2);
  } else {
    *gdata2 = cc32_read_word(cc32, (unsigned int) *gn, (unsigned int) *ga,
			   (unsigned int) *gf);
  }
  if (debug) printf("data = %hd ",*gdata2);
  *gdata2 = cc32_read_word(cc32, 0, 0, 0);
  Q   = (*gdata2 & 0x08) ? 1 : 0;
  X   = (*gdata2 & 0x04) ? 1 : 0;
  inh = (*gdata2 & 0x02) ? 1 : 0;
  lam = (*gdata2 & 0x01) ? 1 : 0;
  if (debug) printf("X = %d Q = %d\n",X,Q);
  gcamstat[0] = 1; /* ERR: 1 for successful transfer */
  gcamstat[1] = 0; /* CSR: == 0 for successful transfer */
  gcamstat[2] = 0; /* ERS */
  gcamstat[3] = 0; /* LCSR: always 0 */
  gcamstat[4] = (0x2 & !X) | (0x1 & !Q); /* QXSUM:0 if transfers have X=1,Q=1*/
  gcamstat[5] = 1; /* TC: Transaction count */
  return 0;
}

int CAM24(short * gchan, short * gc, short * gn, short * ga, short * gf, int * gdata4, int gcamstat[]) {
  int wr, X, Q, inh, lam;
  short gdata2;

  wr = (*gf & 0x10) ? 1 : 0;
  if (debug) printf("CAM24: n = %d a = %d f = %d ",*gn,*ga,*gf);
  if (wr) {
    cc32_write_long(cc32, (unsigned int) *gn, (unsigned int) *ga, 
		    (unsigned int) *gf, *gdata4);
  } else {
    *gdata4 = cc32_read_long(cc32, (unsigned int) *gn, (unsigned int) *ga, 
			     (unsigned int) *gf, &X, &Q);
  }
  gdata2 = cc32_read_word(cc32, 0, 0, 0);
  Q   = (gdata2 & 0x08) ? 1 : 0;
  X   = (gdata2 & 0x04) ? 1 : 0;
  inh = (gdata2 & 0x02) ? 1 : 0;
  lam = (gdata2 & 0x01) ? 1 : 0;
  if (debug) printf("data = %d X = %d Q = %d\n",*gdata4,X,Q);
  /* Figure out what to return in gcamstat[] */
  gcamstat[0] = 1; /* ERR: 1 for successful transfer */
  gcamstat[1] = 0; /* CSR: == 0 for successful transfer */
  gcamstat[2] = 0; /* ERS */
  gcamstat[3] = 0; /* LCSR: always 0 */
  gcamstat[4] = (0x2 & !X) | (0x1 & !Q); /* QXSUM:0 if transfers have X=1,Q=1*/
  gcamstat[5] = 1; /* TC: Transaction count */
  return 0;
}

/* CALL CAB24(cachan,crate,histo_add,a0,f2,mode,histodata(1),cb,camstat)*/
int CAB24(short * gchan, short * gc, short * gn, short * ga, short * gf, int * mode, int gdata[], int cb[], int gcamstat[]) {
  int wr, i;
  int nreads, gdata4, X, Q, inh, lam;
  short gdata2;
  wr = (*gf & 0x10) ? 1 : 0;
  nreads = cb[0];
  if (debug) printf("CAB24: n = %d a = %d f = %d nreads=%d\n",
		    *gn,*ga,*gf,nreads);
  /*  if (wr) return 0;*/


  for (i=0;i<nreads;i++) {
    gdata4 = cc32_read_long(cc32, (unsigned int) *gn, (unsigned int) *ga, 
			    (unsigned int) *gf, &X, &Q);
    if (debug) printf("CAB24: i= %3d X = %d Q=%d data=%d\n",i,X,Q,gdata4);
    gdata[i] = gdata4;
  }
  

  /* Fill in elements in status array */
  gdata2 = cc32_read_word(cc32, 0, 0, 0);
  Q   = (gdata2 & 0x08) ? 1 : 0;
  X   = (gdata2 & 0x04) ? 1 : 0;
  inh = (gdata2 & 0x02) ? 1 : 0;
  lam = (gdata2 & 0x01) ? 1 : 0;
  /* Figure out what to return in gcamstat[] */
  gcamstat[0] = 1; /* ERR: 1 for successful transfer */
  gcamstat[1] = 0; /* CSR: == 0 for successful transfer */
  gcamstat[2] = 0; /* ERS */
  gcamstat[3] = 0; /* LCSR: always 0 */
  gcamstat[4] = (0x2 & !X) | (0x1 & !Q); /* QXSUM:0 if transfers have X=1,Q=1*/
  gcamstat[5] = nreads; /* TC: Transaction count */
  return 0;
}
