#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXSCRATCH   1024
#define DMONO_CU     1.278
#define TWENTYMINUTE 20
#define FORTYMINUTE  40
#define ENERGYMAX    310.00
#define ENERGYF      1.2
#define ENERGYF_RES  1.1

/*
 * Calculate energy resolution of FANS spectrometer
 * Input: energy (Energy in meV)
 *        dmono  (Monochromator D-spacing in Angstrom)
 *        coll1  (Collimation entering drum in minutes)
 *        coll2  (Collimation exiting  drum in minutes)
 *
 */
float fansres(float energy, float dmono, float col1, float col2) {
  float res, colp1, colp2, rmos, dtheta;
  float de, wavelength, stheta, cot_theta;

  colp1 = col1 / (60 * 57.296);
  colp2 = col2 / (60 * 57.296);
  rmos  = 30.0 / (60 * 57.296);
  dtheta = sqrt(((colp1 * colp2) * (colp1 * colp2) +
		 (colp1 * rmos) * (colp1 * rmos) +
		 (colp2 * rmos) * (colp2 * rmos))/
		(colp1 * colp1 + colp2 * colp2 + 4 * rmos * rmos));
  wavelength = sqrt(81.805 / (energy + ENERGYF));
  stheta = wavelength/(2*dmono);
  if (abs(stheta) < 1.0) {
    cot_theta=sqrt(1-stheta*stheta)/stheta;
  } else {
    cot_theta=0.0;
  }
  de = 2.0 * (energy + ENERGYF) * cot_theta * dtheta;
  res = sqrt(de * de + ENERGYF_RES * ENERGYF_RES);
  return res;
}


/*
 * Name:     FANSVEC
 * Purpose:  FORTRAN wrapper for calculating scan vectors in fractions of
 *           resolution widths
 * Requires: energy0 > 0
 *           npts > 0
 *           0 < defrac < 1
 *           Currently assumes Copper Monochromator, 20' collimation
 * Returns:  Vector of energy steps
 *           (Should probably try to put a reasonable cap on Efinal)
 *           On error *npts=0
 *
 * 
 * Author:   N. C. Maliszewskyj, NIST Center for Neutron Research, Aug 2000
 *
 */
void FANSVEC(float * energy0, int * npts, float * defrac, float *vector) 
{
  float en, de, scratch[1024]; 
  int pt, i;

  if (*npts < 0) {
    *npts = 0;
    return;
  }

  if (*energy0 < 0) {
    *npts = 0;
    return;
  }

  pt = 0;
  if (*energy0 < 25.0) {
    scratch[0] = 25.0;
    for(i=1;i<MAXSCRATCH;i++) {
      de = fansres(scratch[i-1],DMONO_CU,TWENTYMINUTE,TWENTYMINUTE); 
      /* de = fansres(scratch[i-1],DMONO_CU,FORTYMINUTE,FORTYMINUTE);*/
      /* Round to hundredths place */
      scratch[i] = floor((scratch[i-1] - (*defrac)*de)*100)/100.0;
      /* scratch[i] = scratch[i-1] - (*defrac) * de; */
      if (scratch[i] < ((*energy0)-0.01)) break;
      pt++;
    }
    for(i=0;i<pt;i++) {
      *(vector + i) = scratch[pt-i];
    }
  }
  en = 25.0;
  if (en >= ((*energy0)-0.01)) {
    *(vector + pt++) = 25.0;
  }

  while (pt < *npts) {
    de = fansres(en,DMONO_CU,TWENTYMINUTE,TWENTYMINUTE);
    /* Round to hundredths place */
    en = floor((en + (*defrac) * de)*100)/100.0;
    /* en = en + (*defrac) * de; */
    if (en > ENERGYMAX) {
      *npts = pt;
      return;
    }
    if (en > ((*energy0) - 0.01)) *(vector + pt++) = en;
  }
  return;
}
