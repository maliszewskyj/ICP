#include <stdio.h>
#include <string.h>
#include <math.h>
#include "enet_gpib.h"
/*
  SPINS field cancellation coil routines
  Sorenson DCS 50-60E

 */

#define  M7T    5 /* Anything will do here */
#define  M12T   6

#define  PS_BOT 7
#define  PS_TOP 8
#define  I_VOLT 20.0
#define  I_MAX  60.0
#define  COIL_CAL_FIELD 6.0  /* Calibration field in Tesla */
float    coil_cal_field = COIL_CAL_FIELD; /* Default */

typedef struct cancellation {
  float energy;
  float top_current;
  float bot_current;
} cancellation;

static cancellation cancelcoil[5] = {
  { 2.3,  6.0,  2.5},
  { 3.6,  7.0,  3.5},
  { 5.0,  7.0,  3.5},
  { 8.0,  8.0,  4.0},
  {14.0,  9.0,  4.5}
};

void ICOILCONFIG(int *which, int *err) {
  char * cfgfile = "/usr/local/icp/cfg/ICOIL.CFG";
  char mstring[20],mbuf[80],*ptr,*end;
  float energy, top_current, bot_current;
  int i, npts;
  FILE * fp;
  
  switch(*which) {
  case M12T:
    strcpy(mstring,"11.5T");
    break;
  case M7T:
  default:
    strcpy(mstring,"7T");
  }
  *err = 0;
  if ((fp=fopen(cfgfile,"r")) == NULL) {
    *err = -2;
    return;
  }

  while(fgets(mbuf,sizeof(mbuf)-1,fp)!= NULL) {
    if (strstr(mbuf,mstring) != NULL) break;
  }
  if (feof(fp)) { /* Configuration not found */
    *err = -3; 
    return;
  }
  if (fgets(mbuf,sizeof(mbuf)-1,fp)==NULL) {
    *err=-4;
    return;
  }
  sscanf(mbuf,"%f %d",&coil_cal_field,&npts);

  for(i=0;i<npts;i++) {
    if (fgets(mbuf,sizeof(mbuf)-1,fp)==NULL) {
      *err = -4;
      return;
    }
    sscanf(mbuf,"%f %f %f",&energy, &top_current,&bot_current);
    cancelcoil[i].energy      = energy;
    cancelcoil[i].top_current = top_current;
    cancelcoil[i].bot_current = bot_current;
  }

  fclose(fp);
  *err=0;
}


int ICOIL(float * itop, float * ibot, int * error)
{
  char outbuf[128];
  int addr, lbuf,err;
  
  *error = 0;
  if ((*itop < 0) || (*itop >= I_MAX)) {
    *error = 1;
    return 1;
  }
  if ((*ibot < 0) || (*ibot >= I_MAX)) {
    *error = 2;
    return 2;
  }
  
  
  // Set bottom coil current
  addr = PS_BOT;
  GPIBEN_LISTEN(&addr, &err);
  sprintf(outbuf,"SOURCE:VOLTAGE %f",I_VOLT);
  lbuf = strlen(outbuf);
  GPIBEN_WR(outbuf,&lbuf,&err);
  sprintf(outbuf,"SOURCE:CURRENT %f",*ibot);
  lbuf = strlen(outbuf);
  GPIBEN_WR(outbuf,&lbuf,&err);
  GPIBEN_UNL(&err);

  addr = PS_TOP;
  GPIBEN_LISTEN(&addr, &err);
  sprintf(outbuf,"SOURCE:VOLTAGE %f",I_VOLT);
  lbuf = strlen(outbuf);
  GPIBEN_WR(outbuf,&lbuf,&err);
  sprintf(outbuf,"SOURCE:CURRENT %f",*itop);
  lbuf = strlen(outbuf);
  GPIBEN_WR(outbuf,&lbuf,&err);
  GPIBEN_UNL(&err);

  *error = err;
  return err;
}

void CANCELCOIL(float *field, float *energy, int *err)
{
  int i;
  float itop, ibot;
  float lenergy, renergy, xenergy;
  float scale;

  scale = *field / coil_cal_field;
  if (*energy <= cancelcoil[0].energy) {
    itop = cancelcoil[0].top_current;
    ibot = cancelcoil[0].bot_current;
  } else if (*energy >= cancelcoil[4].energy) {
    itop = cancelcoil[4].top_current;
    ibot = cancelcoil[4].bot_current;
  } else {
    for (i=1;i<5;i++) {
      if ((*energy >= cancelcoil[i-1].energy) &&
	  (*energy < cancelcoil[i].energy)) break;
    }
    renergy = cancelcoil[i].energy;
    lenergy = cancelcoil[i-1].energy;
    xenergy = *energy - cancelcoil[i-1].energy;
    itop = ((cancelcoil[i].top_current - cancelcoil[i-1].top_current)/
	    (renergy - lenergy)) * xenergy + cancelcoil[i-1].top_current;
    ibot = ((cancelcoil[i].bot_current - cancelcoil[i-1].bot_current)/
	    (renergy - lenergy)) * xenergy + cancelcoil[i-1].bot_current;
  }

  itop *= scale;
  ibot *= scale;

  // Now set currents 
  //  printf("H = %5.2f E = %5.2f I_top = %5.2f I_bot = %5.2f\n",
  //	 *field,*energy,itop, ibot);

  ICOIL(&itop, &ibot, err);
  
}

