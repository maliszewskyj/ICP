/*
	COMMON /instr/ header, insp1,
	1       nsta, mots, mon_add, scl, scl_add, scl_chn, input_add,
	2	misc_switches(5), sw_add, sw_reg, sw_mon_bit, sw_scl_bit, 
	3    	sw_relays(5), pols, pol_add, pol_chn, histo_add,sw_histo_bit,
     1		rout_add, insp2, mot_line(maxtypes),tmp_line, 
	4       logname, mottype(maxmots), vmot1(maxmots), 
     1	        vmot2(maxmots), vmot3(maxmots), 
	5	vmot4(maxmots), base(maxmots), 
     1	        ramp(maxmots), top(maxmots), pulse(maxmots), 
	6	back(maxmots), merr(maxmots), 
     1	        collim(4), mosaic(3), def_ver, terminal, insp1b,
	7	dm, da, z(maxmots), ul(maxmots), ll(maxmots), eff(32), 
     1	        zp(32), wavel, mag_line, term_line, monobt1,
     1		insp3,rkpos1, rkpos2, rkmot
*/
/*
#define MAXTYPES 8
#define MAXMOTS  30
extern struct {
  char header[35];
  char insp1;
  short nsta;
  short nmots;
  short mon_add;
  short scl;
  short scl_add;
  short scl_chn;
  short input_add;
  short misc_switches[5];
  short sw_add;
  short sw_reg;
  short sw_mon_bit;
  short sw_scl_bit;
  short sw_relays[5];
  short pols;
  short pol_add;
  short pol_chn;
  short histo_add;
  short sw_histo_bit;
  short rout_add;
  short insp2;
  int mot_line[MAXTYPES];
  int tmp_line;
  char logname[12];
  int mottype[MAXMOTS];
  float vmot1[MAXMOTS];
  float vmot2[MAXMOTS];
  float vmot3[MAXMOTS];
  float vmot4[MAXMOTS];
  int base[MAXMOTS];
  int ramp[MAXMOTS];
  int top[MAXMOTS];
  int pulse[MAXMOTS];
  int back[MAXMOTS];
  int merr[MAXMOTS];
  int collim[4];
  int mosaic[3];
  int def_ver;
  char terminal[7];
  char insp1b;
  float dm;
  float da;
  float z[MAXMOTS];
  float ul[MAXMOTS];
  float ll[MAXMOTS];
  float eff[32];
  float zp[32];
  float wavel;
  int mag_line;
  int term_line;
  char monobt1[5];
  int insp3;
  float rkpos1;
  float rkpos2;
  int rkmot;
} _CINSTR;
*/
#include "icp.h"
extern int nsta;
extern int pulse[MAXMOTS];
extern int mot_line[MAXTYPES];
extern int tmp_line;
extern int mag_line;
extern int term_line;
extern int scl;
extern float vmot1[MAXMOTS];
extern float vmot2[MAXMOTS];
extern float vmot3[MAXMOTS];
extern float vmot4[MAXMOTS];
extern float z[MAXMOTS];
extern int merr[MAXMOTS];
extern int pulse[MAXMOTS];
//extern char monobt1[5];
