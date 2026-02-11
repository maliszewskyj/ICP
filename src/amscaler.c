#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
//#include <tcl.h>

#include "am9513.h"
#include "trace.h"

/* Structure controlling counter operation */
static Am9513_type am;
#define COUNT_TIME 0
#define COUNT_MON  1
static char * cdev = "/dev/ctr0";
static int fd=-1;
static int ctrmode = 0;
static int amticks;
static int ammonitor;
static int amdetector;
static int ampreset;
static unsigned char iobyte;

void AMDIO_OUT(unsigned char * );

static char buf[82];		/* 64 bits plus 16 spaces plus null ...*/
/* Print 16 bit binary number */
char *
pbin(long unsigned int val,int bits){
    int j=bits;
    char *p=buf;
    if (bits>64) return NULL;
    while( --j >= 0 ){
      *p++ = ((val & (1 << j)) ? '1' : '0');
      if ((j%4) == 0) *p++ =' ';
    }
    *--p = '\0';		/* cancel space at j=0 */
    return buf;
}

void
am_dump(Am9513_type am) {
  int i;
  printf("Signature       = %d\n",am.signature);
  printf("Master mode reg = %s\n",pbin(am.uMaster.sMaster,16));
  for (i=0;i<5;i++) {
    printf("Counter[%d] mode = %s\n",i,pbin(am.counter[i].uMode.sMode,16));
  }
  for (i=0;i<5;i++) {
    printf("Counter[%d] load = %s\n",i,pbin(am.counter[i].load,16));
  }
  for (i=0;i<5;i++) {
    printf("Counter[%d] hold = %s\n",i,pbin(am.counter[i].hold,16));
  }  
  printf("Status          = %s\n",pbin(am.uStatus.cStatus,8));
}

void
CounterConfigure(Am9513_type *am) {

  /* Configure master mode register */
  am->uMaster.master.data_ptr = DP_SEQ_DSBL;
  am->uMaster.master.scaler   = DIV_BCD;  

  /* Configure the first counter as a timer */
  am->counter[0].uMode.mode.gate      = GATE_ABSENT;
  am->counter[0].uMode.mode.source    = CNT_SRC_F5;
  am->counter[0].uMode.mode.direction = CNT_DOWN;
  am->counter[0].uMode.mode.output    = OUT_TC_TOGGLE;
  am->counter[0].uMode.mode.control   = 0;
  am->counter[0].load = 0;
  am->counter[0].hold = 0;

  /* Configure scaler counter */
  am->counter[1].uMode.mode.gate      = GATE_HIL_GTN;
  am->counter[2].uMode.mode.gate      = GATE_HIL_GTN_M;
  am->counter[1].uMode.mode.source    = CNT_SRC_SRC2;
  am->counter[2].uMode.mode.source    = CNT_SRC_TCN_M;
  am->counter[1].uMode.mode.output    = OUT_INACTIVE_H;
  am->counter[2].uMode.mode.output    = OUT_INACTIVE_H;
  am->counter[1].uMode.mode.direction = CNT_UP;
  am->counter[2].uMode.mode.direction = CNT_UP;
  am->counter[1].uMode.mode.control   = CNT_REPEAT | RELOAD_LOAD;
  am->counter[2].uMode.mode.control   = 0;
  am->counter[1].load = 0;
  am->counter[2].load = 0;
  am->counter[1].hold = 0;
  am->counter[2].hold = 0;

  /* Configure scaler counter */
  am->counter[3].uMode.mode.gate      = GATE_HIL_GTN;
  am->counter[4].uMode.mode.gate      = GATE_HIL_GTN_M;
  am->counter[3].uMode.mode.source    = CNT_SRC_SRC4;
  am->counter[4].uMode.mode.source    = CNT_SRC_TCN_M;
  am->counter[3].uMode.mode.output    = OUT_INACTIVE_H;
  am->counter[4].uMode.mode.output    = OUT_INACTIVE_H;
  am->counter[3].uMode.mode.direction = CNT_UP;
  am->counter[4].uMode.mode.direction = CNT_UP;
  am->counter[3].uMode.mode.control   = CNT_REPEAT | RELOAD_LOAD;
  am->counter[4].uMode.mode.control   = 0;
  am->counter[3].load = 0;
  am->counter[4].load = 0;
  am->counter[3].hold = 0;
  am->counter[4].hold = 0;

}

void
SCALERINIT() {
  char * ptr;
  iobyte = 0;

  if (debug) { TRACE("SCALERINIT\n"); }
  if ((fd = open(cdev,O_RDWR)) < 0) {
    if ((ptr = getenv("TESTING")) == NULL) {
      fprintf(stderr,"Unable to open counter device. Exiting now...\n");
      exit(1);
    }
    return;
  }

  /* Reset counter */
  if (ioctl(fd,AM9513_RESET,0) < 0) {
    fprintf(stderr,"ioctl failed!\n");
  }

  /* Configure counter */
  memset(&am,0,sizeof(Am9513_type));
  /* Set Am9513_type signature --- VERY important */
  am.signature = AM9513_SIGNATURE;
  CounterConfigure(&am);
  
  if (ioctl(fd,AM9513_CNF_ALL,&am) < 0) {
    perror("SCALERINIT-ioctl():");
  }

  /* Initialize the digital I/O */
  AMDIO_OUT(&iobyte);

}

void
SCALERCLOSE() {
  /* Reset counter */
  if (ioctl(fd,AM9513_RESET,0) < 0) {
    perror("SCALERCLOSE-ioctl():");
  }

  close(fd);
  fd = -1;
}

void
SCALERSTART(int * preset, int * mode) {
  unsigned short increment;
  /*
   * Count for time:    mode = 0
   * Count for monitor: mode = 1
   */
  CounterConfigure(&am);
  ctrmode = (*mode!=0) ? 1 : 0;
  ampreset   = *preset;
  amticks  = 0;
  ammonitor  = 0;
  amdetector = 0;
  increment = (unsigned short) ((ampreset >= 65000) ? 65000 : ampreset);
  ampreset  -= increment;
  if (debug) 
    printf("SCALERSTART: mode = %s preset = %d increment = %d ampreset = %d\n",
		    ((ctrmode)?"monitor":"time"),*preset, increment,ampreset);
  if (ctrmode) {
    am.counter[0].uMode.mode.source = CNT_SRC_SRC2;
    am.counter[1].uMode.mode.source = CNT_SRC_F5;
    am.counter[0].load              = increment-1;
  } else {
    am.counter[0].uMode.mode.source = CNT_SRC_F5;
    am.counter[1].uMode.mode.source = CNT_SRC_SRC2;
    am.counter[0].load              = increment-1;
  }

  /* Disarm counters */
  if (ioctl(fd,AM9513_DIS_ALL,&am) < 0) {
    perror("SCALERSTART-disarm:");
  }    

  if (ioctl(fd,AM9513_CNF_ALL,&am) < 0) {
    perror("SCALERSTART-conf:");
  }

  if (ioctl(fd,AM9513_LOAD_ALL,&am) < 0) {
    perror("SCALERSTART-load:");
  }    

  if (ioctl(fd,AM9513_SET_TC(1),0) < 0) {
    perror("SCALERSTART-gate:");
  }
  
  if (ioctl(fd,AM9513_ARM_ALL,&am) < 0) {
    perror("SCALERSTART-arm:");
  }
   
}

int
SCALERPROGRESS() {
  int val1, val2, i;
  unsigned short increment;
  
  if (ioctl(fd,AM9513_DIS_ALL,&am)) {
    perror("SCALERPROGRESS-disarm:");
  }

  if (ioctl(fd,AM9513_READ_ALL,&am)) {
    perror("SCALERPROGRESS-read:");
  }

  val1 = am.counter[0].load + 1; /* Take preset as the real count */
  val2 = (((int) am.counter[2].hold) << 16) + (int) am.counter[1].hold;

  if (ctrmode) {
    amticks += val2 ; /* Assuming BCD, 1 MHz OSC, F5 source */
    ammonitor += val1;
  } else {
    amticks += val1 ; /* Assuming BCD, 1 MHz OSC, F5 source */
    ammonitor += val2;
  }   
  amdetector += (((int) am.counter[4].hold) << 16) + 
     (int) am.counter[3].hold;   
  
  if (debug) printf("SCALERPROGRESS: ticks = %8d, mon = %8d, det = %8d\n",
		    amticks, ammonitor, amdetector);
  if (ampreset > 0) {
    increment = (ampreset >= 65000) ? 65000 : ampreset;
    ampreset -= increment;
    for (i=0;i<5;i++) {
      am.counter[i].load = 0;
      am.counter[i].hold = 0;
    }
    am.counter[0].load   = increment-1;

    if (debug) printf("SCALERPROGRESS: increment = %8d remaining = %8d\n",
		      increment, ampreset);
  
    /* Disarm counters */
    if (ioctl(fd,AM9513_DIS_ALL,&am) < 0) {
      perror("SCALERPROGRESS-disarm:");
      return 0;
    }    

    if (ioctl(fd,AM9513_CNF_ALL,&am) < 0) {
      perror("SCALERPROGRESS-conf:");
      return 0;
    }

    if (ioctl(fd,AM9513_LOAD_ALL,&am) < 0) {
      perror("SCALERPROGRESS-load:");
      return 0;
    }    

    if (ioctl(fd,AM9513_SET_TC(1),0) < 0) {
      perror("SCALERPROGRESS-gate:");
      return 0;
    }
  
    if (ioctl(fd,AM9513_ARM_ALL,&am) < 0) {
      perror("SCALERPROGRESS-arm:");
      return 0;
    }

    return 1;
  }

  if (debug) printf("SCALERPROGRESS: Counting finished\n");
  return 0;
}

/*
 * Test to see whether counting is in progress
 */
void
SCALERSTATUS(int *iscounting) {
  if (ioctl(fd,AM9513_STATUS,&am) < 0) {
    perror("SCALERSTATUS-status:");
  }

  if (debug&8) printf("SCALERSTATUS: am.uStatus.cstatus = 0x%02x\n",
		    am.uStatus.cStatus);
  *iscounting = 0;    /* No counting in progress */   
  /* Test the output for counter 1 */
  if (am.uStatus.cStatus & 0x02) {
    *iscounting = 1;  /* Counting in progress */
    usleep(10);       /* Insert an itty-bitty wait state */
  } else {
    if (SCALERPROGRESS()) *iscounting = 1;
  }
}

void
SCALERREAD(float * seconds, int *moncounts, int *scalercounts) {
  if (debug&1) printf("SCALERREAD:");

  /* Simply transfer the values stored in amseconds, ammonitor, amdetector */
  *seconds      = ((float) amticks)/100.0;
  *moncounts    = (float) ammonitor;
  *scalercounts = (float) amdetector;

  if (debug&1) printf(" sec = %.2f mon = %d det = %d\n", 
		      *seconds, *moncounts, *scalercounts);
}

void
SCALERRESET() {
  if (debug&1) printf("SCALERRESET\n");
  /* Reset counter */
  if (ioctl(fd,AM9513_RESET,0) < 0) {
    perror("SCALERRESET-ioctl():");
  }

}

void
AMDIO_IN(unsigned char * byte) {
  if (ioctl(fd,AM9513_AUX_RD,&am) < 0) {
    fprintf(stderr,"ioctl failed!\n");
  }
  *byte = am.aux;
  if (debug & 1) printf("AMDIO_IN: 0x%02x\n",*byte);
}

void
AMDIO_OUT(unsigned char * byte) {
  if (debug & 1) printf("AMDIO_OUT: 0x%02x\n",*byte);

  am.aux = *byte;
  if (ioctl(fd,AM9513_AUX_WR,&am) < 0) {
    fprintf(stderr,"ioctl failed!\n");
  }      
}

void
AMDIO_RELAY(int * bit, int * state) {
  unsigned char mask = 0;
  if ((*bit > 7) || (*bit < 0)) return;
  mask = (0x1 << *bit);
  iobyte &= ~mask;
  
  if (*state) iobyte |= mask;  
  AMDIO_OUT(&iobyte);
}
