/*
    am9513 - modular device driver for am9513 counter/timer chipset

    Author: N. C. Maliszewskyj, NIST Center for Neutron Research, August 1997
            P. Klosowski        NIST Center for Neutron Research

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Changes: May 1999 - Modified for Linux 2.2.x kernel
*/

#ifndef AM9513_DEF_H
#define AM9513_DEF_H

/* Define command masks */

#define MASTER_RESET  0xff
#define CMD_LDP       0x00
#define CMD_CLK       0x10
#define CMD_ARM       0x20
#define CMD_LOAD      0x40
#define CMD_LARM      0x60
#define CMD_DSVE      0x80
#define CMD_SAVE      0xa0
#define CMD_DISARM    0xc0
#define CMD_CLR_TC_   0xe0
#define CMD_CLR_TC(a) (CMD_CLR_TC_ + (a & 0x07))
#define CMD_SET_TC_   0xe8
#define CMD_SET_TC(a) (CMD_SET_TC_ + (a & 0x07))
#define CMD_STEP_     0xf0
#define CMD_STEP(a)   (CMD_STEP_ + (a & 0x07))
#define CMD_GLOB_     0xf8
#define CMD_GLOB_RST  0xff
#define CMD_GLOB_STAT 0xf8
#define CMD_GLOB_RDIO 0xfc
#define CMD_GLOB_WRIO 0xfa

/* Addressing constants */

#define CTRL_GROUP_ADDR  0x07
#define MASTER_REG_ADDR  0x17
#define ALARM1_REG_ADDR  0x07
#define ALARM2_REG_ADDR  0x0f
#define STATUS_REF_ADDR  0x1f 

#define CTR_GRP_ADDR(A)        (A & 0x07)
#define MODE_REG_ADDR(A)       (CTR_GRP_ADDR(A))
#define LOAD_REG_ADDR(A)       ((0x01<<3)|CTR_GRP_ADDR(A))
#define HOLD_REG_ADDR(A)       ((0x02<<3)|CTR_GRP_ADDR(A))
#define HOLD_CYCLE_REG_ADDR(A) ((0x03<<3)|CTR_GRP_ADDR(A))

#define S1           0x01
#define S2           0x02
#define S3           0x04
#define S4           0x08
#define S5           0x10


/* Master mode register defines */

#define DIV_BINARY      0
#define DIV_BCD         1 /* BCD division of scaler */
#define DP_SEQ_ENBL     0
#define DP_SEQ_DSBL     1 /* Disable data pointer sequencing */ 

#define BUSWIDTH_16     1 /* 16 bit-wide data bus   */
#define BUSWIDTH_8      0

#define FOUT_GATE_OFF   1
#define FOUT_DIV(A)     (A & 0xf)

#define FOUT_SRC_SRC1   1  /* FOUT sources */
#define FOUT_SRC_SRC2   2
#define FOUT_SRC_SRC3   3
#define FOUT_SRC_SRC4   4
#define FOUT_SRC_SRC5   5
#define FOUT_SRC_GATE1  6
#define FOUT_SRC_GATE2  7
#define FOUT_SRC_GATE3  8
#define FOUT_SRC_GATE4  9
#define FOUT_SRC_GATE5 10
#define FOUT_SRC_F1    11
#define FOUT_SRC_F2    12
#define FOUT_SRC_F3    13
#define FOUT_SRC_F4    14
#define FOUT_SRC_F5    15

#define CMPR_1_ENBL     1 /* Enable comparator 1   */
#define CMPR_1_DSBL     0
#define CMPR_2_ENBL     1 /* Enable comparator 2   */
#define CMPR_2_DSBL     0

#define TOD_DSBL        0 /* Time of day functions */
#define TOD_DIV_05      1
#define TOD_DIV_06      2
#define TOD_DIV_10      3


/* Counter mode register defines -- N.B. Not all combinations work! */

#define GATE_ABSENT     0 /* No gating */
#define GATE_HI_TCN_M   1 /* Gate high       TC   (N-1) */
#define GATE_HIL_GTN_P  2 /* Gate high level GATE (N+1) */
#define GATE_HIL_GTN_M  3 /* Gate high level GATE (N-1) */
#define GATE_HIL_GTN    4 /* Gate high level GATE (N)   */
#define GATE_LOL_GTN    5 /* Gate low  level GATE (N)   */
#define GATE_HIE_GTN    6 /* Gate high edge  GATE (N)   */
#define GATE_LOE_GTN    7 /* Gate low  edge  GATE (N)   */

#define CNT_TRAILING    1 /* Count on trailing edge     */
#define CNT_RISING      0

#define CNT_SRC_TCN_M   0 /* Counter sources */
#define CNT_SRC_SRC1    1
#define CNT_SRC_SRC2    2 
#define CNT_SRC_SRC3    3
#define CNT_SRC_SRC4    4
#define CNT_SRC_SRC5    5
#define CNT_SRC_GATE1   6 
#define CNT_SRC_GATE2   7
#define CNT_SRC_GATE3   8
#define CNT_SRC_GATE4   9
#define CNT_SRC_GATE5  10 
#define CNT_SRC_F1     11 
#define CNT_SRC_F2     12 
#define CNT_SRC_F3     13 
#define CNT_SRC_F4     14 
#define CNT_SRC_F5     15

#define GATE_SPEC_ENB   4 /* Enable special gate      */

#define RELOAD_LOAD     2 /* Reload from load or hold */

#define CNT_ONCE        0 /* Count once               */
#define CNT_REPEAT      1 /* Count repetitively       */

#define CNT_BINARY      0 /* Count in binary          */
#define CNT_BCD         1 /* Count in BCD             */

#define CNT_DOWN        0
#define CNT_UP          1

#define OUT_INACTIVE_L  0 /* Inactive, output low            */
#define OUT_HI_TC       1 /* Active high TC pulse            */
#define OUT_TC_TOGGLE   2 /* TC toggled                      */
#define OUT_INACTIVE_H  4 /* Inactive, output high impedance */
#define OUT_LO_TC       5 /* Active low TC pulse             */

/* Board-specific defines */
#define CLK_1MHZ 0
#define CLK_5MHZ 1
#define CLK_EXT  2

/* Module-specific defines */

#define AM9513_SIGNATURE 9513
#define AM9513_MAJOR       51
#define MAXCHIPS           10


/*******************/
/* Data structures */
/*******************/

typedef struct {
  unsigned group   : 3;
  unsigned element : 2;
  unsigned cmd_code: 3;
} dataptr;

/* Status register */
typedef struct {
  unsigned byte     : 1; /* The byte toggle */
  unsigned out1     : 1; /* Five OUT bits; top two bits unassigned */
  unsigned out2     : 1;
  unsigned out3     : 1;
  unsigned out4     : 1;
  unsigned out5     : 1;
  unsigned not_used : 2;
} status_reg;

/* Master mode register */
typedef struct {
  unsigned day_mode     :2;
  unsigned compare_1    :1;
  unsigned compare_2    :1;
  unsigned FOUT_source  :4;
  unsigned FOUT_divisor :4;
  unsigned FOUT_gate    :1;
  unsigned data_base    :1;
  unsigned data_ptr     :1;
  unsigned scaler       :1;
} master_mode_reg;

/* Counter mode register */
typedef struct {
  unsigned output    :3;
  unsigned direction :1;
  unsigned base      :1;
  unsigned control   :3;
  unsigned source    :4;
  unsigned edge      :1;
  unsigned gate      :3;
} count_mode_reg;

/* A single counter set */
typedef struct {
  union{
    count_mode_reg mode;
    unsigned short sMode;
  } uMode;
  unsigned short load;
  unsigned short hold;
} channel;


/* Am9513 chip set */
typedef struct {
  unsigned int    signature;
  union { 
    master_mode_reg master;
    unsigned short sMaster;
  } uMaster;
  channel         counter[5];
  union{
    status_reg      status;
    unsigned char cStatus;
  } uStatus;
  unsigned short  alarm1;
  unsigned short  alarm2;
  unsigned char aux;
} Am9513_type;

/* CIO-CTR10HD Interrupt & Control Register */
typedef struct {
  unsigned interrupt:3;
  unsigned clk1     :2;
  unsigned clk2     :2;
  unsigned waitstate:1;
} Ctr10_ICR_Reg;

typedef struct {
  union {
    Ctr10_ICR_Reg  icr;
    unsigned char cicr;
  } uICR;
} Ctr_ICR;


/* Define ioctl commands                 */
/* Registered command and function range */


#define AM9513IOC     'o'
#define AM9513MINFUN 0x00
#define AM9513MAXFUN 0xff


/* ioctl commands */
/* Separate commands into subclasses */

#define AM9513_CMD_MASK 0xe0 /* 1110 0000 */
#define AM9513_INQ_     0x00 /* 0000 0000 get information for counter set */
#define AM9513_ARM_     0x20 /* 0010 0000 arm counter group               */
#define AM9513_LOAD_    0x40 /* 0100 0000 load counter from load register */
#define AM9513_DIS_     0x80 /* 1000 0000 disarm counter                  */
#define AM9513_READ_    0x60 /* 0110 0000 read contents of hold register  */
#define AM9513_CNF_     0xa0 /* 1010 0000 configure counter group         */
#define AM9513_CMD_     0xe0 /* 1110 0000 global command                  */

#define AM9513_RESET      _IO(AM9513IOC,0xff)
#define AM9513_INQ_ALL    _IOR(AM9513IOC,0x00,Am9513_type)
#define AM9513_INQ(a)     _IOR(AM9513IOC,(AM9513_INQ_|(a & 0x1f)),Am9513_type)
#define AM9513_CNF_ALL    _IOW(AM9513IOC,AM9513_CNF_,Am9513_type)
#define AM9513_CNF(a)     _IOW(AM9513IOC,(AM9513_CNF_|(a & 0x1f)),Am9513_type)
#define AM9513_ARM_ALL    _IO(AM9513IOC,(AM9513_ARM_|0x1f))
#define AM9513_ARM(a)     _IO(AM9513IOC,(AM9513_ARM_|(a & 0x1f)))
#define AM9513_DIS_ALL    _IO(AM9513IOC,(AM9513_DIS_|0x1f))
#define AM9513_DIS(a)     _IO(AM9513IOC,(AM9513_DIS_|(a & 0x1f)))
#define AM9513_LOAD_ALL   _IOW(AM9513IOC,(AM9513_LOAD_|0x1f),Am9513_type)
#define AM9513_LOAD(a)    _IOW(AM9513IOC,(AM9513_LOAD_|(a & 0x1f)),Am9513_type)
#define AM9513_READ_ALL   _IOR(AM9513IOC,(AM9513_READ_|0x1f),Am9513_type)
#define AM9513_READ(a)    _IOR(AM9513IOC,(AM9513_READ_|(a & 0x1f)),Am9513_type)
#define AM9513_STEP(a)    _IO(AM9513IOC,CMD_STEP(a))
#define AM9513_CLR_TC(a)  _IO(AM9513IOC,CMD_CLR_TC(a))
#define AM9513_SET_TC(a)  _IO(AM9513IOC,CMD_SET_TC(a))
#define AM9513_CMD(a)     _IO(AM9513IOC,(AM9513_CMD_|(a & 0x1f)))
#define AM9513_AUX_RD     _IOR(AM9513IOC,CMD_GLOB_RDIO,Am9513_type)
#define AM9513_AUX_WR     _IOW(AM9513IOC,CMD_GLOB_WRIO,Am9513_type)
#define AM9513_STATUS     _IOR(AM9513IOC,CMD_GLOB_STAT,Am9513_type)

#endif /* AM9513_DEF_H */

