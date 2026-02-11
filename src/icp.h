#ifndef _icp_h
#define _icp_h
#define MAXMOTS     30
#define MAXTYPES    12
#define MAXPOINTS   100
#define NBUF        30
#define MOTBUF_RECL 160
#define LOGBUF_RECL 320
#define STAT_RECL    80
#define QBUF_RECL   320
#define TBUF_RECL   320
#define RBUF_RECL   320
#define BBUF_RECL   320
#define IBUF_RECL   320
#define TRASHMONO   2
#define TRASHANAL   15
#define HOMEDIR     trim(gethome())
#define INSTR_CFG   trim(gethome())//'INSTR.CFG'
#define MOTORS_BUF  trim(gethome())//'MOTORS.BUF'
#define HWLONG_BUF  trim(gethome())//'HWLONG.BUF'
#define ZCOEFFS_CFG trim(gethome())//'ZCOEFFS.CFG'
#define HWCOEFFS_CFG  trim(gethome())//'HWCOEFFS.CFG'
#define MONITOR_REC trim(gethome())//'MONITOR.REC'
#define STATUS_ICP  trim(gethome())//'STATUS.ICP'
#define XPEEK_XR0   trim(gethome())//'XPEEK.XR0'
#define DISTANCES_ICPR trim(gethome())//'DISTANCES.ICPR'
#define SCLFAC_DAT  trim(gethome())//'SCLFAC.DAT'
#define MOTCLE_ICP  trim(gethome())//'MOTCLE.ICP'
#define ASDPARAMS_PAR  trim(gethome())//'ASDPARAMS.PAR'
#define ERRORS_ICP  trim(gethome())//'ERRORS.ICP'
#define LOGBOOK_BUF  trim(gethome())//'LOGBOOK.BUF'
#define MOVE4_CFG    trim(gethome())//'MOVE4.CFG'
#define TRANSLATION_CFG  trim(gethome())//'TRANSLATION.CFG'
#define MOTPOS_BUF  trim(gethome())//'MOTPOS.BUF'
#endif
