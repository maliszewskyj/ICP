#include "icp.h"
	SUBROUTINE Save_File (fname, pts )
C	***********************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Modify The Header Of A Data File After An Abort With Contr_A.C
C		or a Contr_F finishup command 				      C
C     Called from: Run_I_Buf, Run_Q_Buf, Run_T_Buf, Run_B_Buf, Run_X, Run_R   C
C                                                                             C
C     Beat Schmid, August 1989                                                C
C     Modified extensively by Gayle Greene				      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface

	INTEGER*4    pts,  !number of points completed in file
     1			opts, j,  maxtry, ios, lchar, imot,
     1			status,notries/20/,openfai/30/, !from $fordef,
     1			nofiles, ifile, context, lpts,
     1			kj
	REAL*4       ec, es, ef, tmp, it, hc, kc, lc, hs, ks, ls,
     1			 hf, a, ai ,af, qz,qzi,qx,qxi,s,si,qold
	CHARACTER*12 fname, locname,file_name
	CHARACTER*80 res_fname
	CHARACTER*90 out
	REAL*4 zeropos/0./
	character*4 filtyps/'ABCD'/
	logical copyfile
	CHARACTER*1 typ
	CHARACTER*16 str16


#define lchar len_trim


C------------------------FIRST DETERMINE FILE TYPE --------------------------C
	IF (NSTA.EQ.8) RETURN	!don't change header for bt8 - too complex
	maxtry = 0
 	OPEN (UNIT=1, FILE= fname, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1    STATUS= 'OLD', IOSTAT=ios)
	DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	   IF (pts.eq.0)RETURN	!no points collected yet. Must be no file.
	   CALL libwait(.2,status)
	   OPEN (UNIT=1, FILE= fname, FORM= 'FORMATTED',
	1	ACCESS= 'SEQUENTIAL',STATUS= 'OLD', IOSTAT=ios)
	   maxtry = maxtry + 1
	ENDDO
	IF (ios .NE. 0)RETURN	!error opening file
	
 	READ (1, '(a)',err=250) out ! Read old header
	CLOSE (1,err=250)
	IF (NSTA.EQ.1) THEN
	   READ (UNIT= out(29:30), FMT= *,err=250) j ! Read file type
	   IF (j .NE. 1) GOTO 250 !do not update any files
	   typ = 'I'
	ELSE
	   typ = out(37:37)
	ENDIF

	IF (polarized) THEN
	   nofiles = 5		!.bt2 (in case P+ but no flip_state),.ba2,.bb2,.bc2,.bd2
	ELSE
	   nofiles = 1
	ENDIF

	lpts = pts
	locname = fname
	DO ifile = 1, nofiles
	   copyfile = .TRUE.
	   IF (polarized .and. ifile .ne. 1)THEN !use fname first time through
	      locname(11:11) = filtyps(ifile-1:ifile-1)
	      IF (locname .EQ. fname)THEN
		 copyfile = .FALSE. !already copied this one
	      ELSE
		 file_name=locname
		 context = 0
		 CALL FIND_FILE (File_Name, res_fname, context,status)
		 if (status.eq.0) copyfile = .FALSE. !file not found with that name
		 CALL FIND_FILE_END (context)
	      ENDIF		!locname eq fname
	   ENDIF		!polarized and not first file

	   IF (copyfile)THEN	!copy files with updated header
	      maxtry = 0
 430	      OPEN (UNIT=2, FILE= locname, FORM= 'FORMATTED',
	1	   ACCESS= 'SEQUENTIAL',STATUS= 'OLD', ERR= 440)
	      goto 445
 440	      maxtry = maxtry + 1
	      IF (maxtry .LT. 50) THEN
		 CALL libwait(.2,status)
		 GOTO 430
	      ELSE
		 OPEN (UNIT=2, FILE= locname, FORM= 'FORMATTED',
	1	      ACCESS= 'SEQUENTIAL',STATUS= 'OLD',ERR= 200)
	      ENDIF

 445	      OPEN (UNIT=3, FILE= locname, FORM= 'FORMATTED',
	1	   ACCESS= 'SEQUENTIAL',STATUS= 'NEW', err=200)

	      READ  (2, '(a)',err=150) out ! Read old header
	      IF (NSTA.EQ.1)THEN
		 READ (UNIT=out(69:73), FMT= '(i5)',err=150) opts ! original # of pts
		 WRITE (UNIT= out(69:73), FMT= '(i5)',err=150) lpts ! Write modified header
	      ELSE
		 READ (UNIT=out(64:68), FMT= '(i5)',err=150) opts ! original # of pts
 10		 WRITE (UNIT= out(64:68), FMT= '(i5)',err=150) lpts ! Write modified header
	      ENDIF
 20	      WRITE (3, '(a)',err=150) out(1:lchar(out))

	      READ  (2, '(a)',err=150) out ! Copy comment line
	      WRITE (3, '(a)',err=150) out(1:lchar(out))
	      READ  (2, '(a)',err=150) out ! Copy sample comment
	      WRITE (3, '(a)',err=150) out(1:lchar(out))

	      IF (typ .EQ. 'R')THEN !refl file
		 READ  (2, '(a)',err=150) out ! Copy mosaic, collimation,
		 WRITE (3, '(a)',err=150) out(1:lchar(out)) ! temp & H-field
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
		 READ  (2, '(a)',err=150) out
		 READ  (unit=out(6:),fmt=*,err=150)
	1	      qz, qzi, qold
		 WRITE (3, '(a3,3x,3f11.6)',err=150)  ' Qz', 
	1	      qz, qzi, qz + qzi*(lpts-1.)
		 READ  (2, '(a)',err=150) out 
		 READ  (unit=out(6:),fmt=*,err=150)
	1	      qx, qxi, qold
		 WRITE (3, '(a3,3x,3f11.6)',err=150)  ' Qx', 
	1	      qx, qxi, qx + qxi*(lpts-1.)

		 DO kj= 1, 4
		    READ  (2, '(a)',err=150) out 
		    READ  (unit=out(3:),fmt=*,err=150)
	1		 imot,s, si, qold
		    WRITE (3, '(1x,a1,i1,3f11.3)',err=150)'S',  imot, 
	1		 s, si, s + si*(lpts-1.)
		 END DO
	      ELSEIF (typ .EQ. 'X')THEN
		 READ  (2, '(a)',err=150) out ! Copy mosaic, collimation,
		 WRITE (3, '(a)',err=150) out(1:lchar(out)) ! temp & H-field
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
		 READ  (2, '(a)',err=150) out
		 DO WHILE (out(2:4) .NE. 'Mot')
		    READ (unit = out,fmt=*,err=150)imot,a,ai,af,j
		    WRITE (3, '(i3,3x,3f11.5,2x,I1)',err=150)  imot, 
	1		 a, ai, a + ai*(lpts-1.),j
		    READ  (2, '(a)',err=150) out
		 ENDDO
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
	      ELSEIF (typ .EQ. 'Q' .OR. typ .EQ. 'T' .OR. typ .EQ. 'B') THEN 
		 READ  (2, '(a)',err=150) out ! Copy Collim & Orientation
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
		 READ  (2, '(a)',err=150) out ! Copy Lattice Params
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))

		 READ  (2, '(a)',err=150) out
		 READ (out(1:), FMT= '(2f10.3,f13.3,5x,2f8.3,5x,f8.3,2x,f8.3)',err=150) !energy
	1	      ec, es, ef, dm, da, tmp, it
		 ec = ec - IFIX( 0.5 * (opts - lpts)) * es
		 WRITE (out(1:10),fmt='(f10.3)',err=150)ec
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
c       WRITE (3, '(2f10.3,f13.3,5x,2f8.3,5x,f8.3,2x,f8.3)',err=150) 
c	1 	    ec, es, ef, dm, da, tmp, it
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))

		 IF (magnctr)THEN
		    READ  (2, '(3(f7.3,1x),2x,3(f7.4,1x),2x,f9.4,a16)',err=150) ! Reciproc. space
	1		 hc, kc, lc, hs, ks, ls, hf, str16
		 ELSE		!not magnctr
		    READ  (2, '(3(f7.3,1x),2x,3(f7.4,1x),2x,f9.4)',err=150) ! Reciproc. space
	1		 hc, kc, lc, hs, ks, ls, hf
		 ENDIF		!not magnctr
		 hc = hc - IFIX( 0.5 * (opts - lpts)) * hs
		 kc = kc - IFIX( 0.5 * (opts - lpts)) * ks
		 lc = lc - IFIX( 0.5 * (opts - lpts)) * ls
		 IF (magnctr) THEN
		    WRITE (3, '(3(f7.3,1x),2x,3(f7.4,1x),2x,f9.4,a16)',err=150)
	1		 hc, kc, lc, hs, ks, ls, hf, str16
		 ELSE		!not magnctr
		    WRITE (3, '(3(f7.3,1x),2x,3(f7.4,1x),2x,f9.4)',err=150)
	1		 hc, kc, lc, hs, ks, ls, hf
		 ENDIF		!magnctr
		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
	      ELSEIF (typ .EQ. 'I') THEN ! (* const increment file *)
		 READ  (2, '(a)',err=150) out ! Copy mosaic, collimation,
		 WRITE (3, '(a)',err=150) out(1:lchar(out)) ! temp & H-field

		 READ  (2, '(a)',err=150) out ! Copy comment line
		 WRITE (3, '(a)',err=150) out(1:lchar(out))
	      ENDIF		!typ choice
	      IF (typ .EQ. 'I' .OR. typ .EQ. 'B')THEN
		 DO j= 1, 6	! Copy modified angle range
		    IF (nsta.eq.1)then
		       IF ( (j .eq. 3 .or. j .eq. 4) .and. motfl(j) )THEN
			  READ  (2, '(a)',err=150) out
			  IF (.not. fixed(j)) THEN
			     READ(unit=out,fmt=*,err=150)imot,a,ai,af
			     WRITE (3, '(i3,3x,3f9.4)',err=150)  imot, 
	1			  a, ai, a + ai*(lpts-1.)
			  ELSE	!fixed
			     WRITE (3, '(a)',err=150) out(1:lchar(out))
			  ENDIF !fixed
		       ENDIF
		    ELSE	!not BT1
		       READ  (2, '(a)',err=150) out
		       IF (motfl(j)) THEN
			  IF (.not. fixed(j)) THEN
			     READ(unit=out,fmt=*,err=150)imot,a,ai,af
			     WRITE (3, '(i3,3x,3f11.5)',err=150)  j, 
	1			  a, ai, a + ai*(lpts-1.)
			  ELSE	!fixed
			     WRITE (3, '(a)',err=150) out(1:lchar(out))
			  ENDIF !fixed
		       ELSE	!motor not in use
			  WRITE (3, '(i3,3x,3f11.5)',err=150)j,zeropos,zeropos,zeropos
		       ENDIF
		    END IF
		 END DO		!i = 1,6
	      END IF		!type I or B

 100	      READ  (2, '(a)', END= 99,err=150) out ! Copy rest of file
	      WRITE (3, '(a)',err=150) out(1:lchar(out))
	      GOTO 100
 99	      CLOSE (2,iostat=ios)
	      CLOSE (3,err=200)
	      CALL Make_Backup(locname)
	   ENDIF		!copyfile
	ENDDO			!for each of nofiles
	return

C       err return, do not eliminate file

 150	close (3,iostat=ios)

 200	close(2,iostat=ios)
	CALL Make_Backup(locname)
 	return
 250	close(1,iostat=ios)
	return
	END


