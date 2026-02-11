# 
# Make file for "icp" Instrument Control Program
# $Id$
#
# Because this source code is intended to make targets for both 
# linux and vms, the FORTRAN is maintained in .F files which
# are FORTRAN source liberally mixed with C preprocessor statements.
#
# Intention at the present is to use the FORTRAN 2008 compliant 
# gfortran compiler. Modules are presently in the ../libs directory.
# .mod files are used in compilation, but we have to link against
# the .o files in that directory
#
FC = gfortran
CPP = /lib/cpp
PERL = /usr/bin/perl
# Directory containing FORTRAN MODULES
MDIR = ../mods
TAR  = /bin/tar
CPPOPTS = -P -traditional

#FFLAGS= -g -ffixed-line-length-132 -M$(MDIR) -I$(MDIR)
FFLAGS= -g -ffixed-line-length-132 -fall-intrinsics -I$(MDIR)
COPTS = -g -Wall

LIBS = -lcurses -lreadline -lm 

COBJ = serial.o rs232_cnf.o rs232_acs.o rs232_x.o lxgetstring.o ttycatch.o \
	udpbroad.o libwait.o amscaler.o vmectrl.o envhome.o \
	rs232_tc.o chkpriv.o fansres.o rs232_hc.o rs232_gpib.o \
	globfile.o trace.o rs232_pc.o \
	tcp_io.o tcp_aim.o ez_m.o pico.o pi.o zeta.o \
	icoil.o center.o \
	enet_gpib.o 

IOBJ = smgini.o readef.o  cluster64.o \
	icpabort.o addlog.o align_peak.o \
	align_xtal.o archfile.o  \
	bragg_pos.o bragg_reclat.o \
	bufop.o calcul.o check_channel.o \
	check_heater.o date_time.o \
	check_sequence.o  contrend.o derive_curve.o \
	dmp_data.o dmp_qpos.o dmp_refl.o dmp_xbuf.o \
	do_sequence.o \
	check_file_sequence.o do_file_sequence.o \
	drive_q.o drive_qrefl.o \
	howl_file.o dry_i_buf.o \
	dry_b_buf.o dry_q_buf.o dry_t_buf.o \
	dry_r_buf.o dry_x_buf.o \
	exam_peak.o file_name.o find_file.o \
	findhalfh.o find_peak.o fit1gauss.o \
	focus.o fz.o geometry.o dtrig.o \
	get_asctime.o getdir.o getval.o \
	gobroad.o gofind_peak.o \
	goodbye.o hiber.o howlong.o howl_seq.o \
	inlimits.o instrini.o isdigit.o \
	limprob.o logfile.o memcurpos.o readlastpos.o \
	make_backup.o namescan.o \
	new_temp_field.o nominit.o \
	nomon.o nopriv.o nosuper.o \
	notinit.o not_implemented.o \
	parse_align.o parse_check.o \
	parse_contr.o parse_dir.o \
	parse_fp.o parse_pbuf.o \
	parse_rs.o putstr.o \
	read_buf.o putcontr.o putcontr_nt.o \
	putcontr_nf.o putline.o putline_nt.o \
	putline_ncr.o putline_nf.o \
	putline_reverse.o putline_wrapword.o \
	refl_pos.o \
	read_string.o rock.o run_i_buf.o  \
	run_b_buf.o run_q_buf.o run_t_buf.o \
	run_r_buf.o run_x_buf.o \
	save_file.o save_mrat.o scan_range.o \
	simulate_mrat.o stampfile.o stamplog.o \
	status_file.o status_line.o \
	test_ststcs.o time_com.o type_file.o \
	upcase.o valid_words.o verify_motors.o \
	dartz.o writefit.o \
	wrongpass.o tconfig.o \
	init_motors.o drive.o \
	iang.o rdangl.o yield.o yield_or_abort.o \
	cntime.o scaler.o readscl.o \
	clref.o setef.o \
	find_file_end.o \
	control.o putmax.o \
	readmon.o  count.o \
	sched_action.o timeyet.o ext_to_int_time.o \
	abs_time.o secdiff.o hold.o holds.o \
	rdtemp.o mag_lib.o hpvolt.o get_hpvolt.o\
	forward_700.o check_helium.o calc_mag_wait.o ad201_file.o \
	ad201_wrdata.o tdb_wrdata.o scan_i.o \
	lia_file.o lia_status.o lia_wrdata.o lia_head.o \
	mconfig.o init_ad201.o init_furnace.o\
	init_lia.o init_ltc.o init_lr400.o init_lr700.o \
	init_tdb.o init_oxford.o init_oxmag.o sendpacket.o ad232empty.o \
	tdb_file.o k_to_ohms.o chebev.o\
	ohms_to_k.o sns_t_v.o sns_v_t.o hdw_t_v.o hdw_v_t.o \
	asd_on.o asd_off.o asd_image.o asd_timed.o read_asdfile.o \
	zero_asd.o read_asd.o iget.o iset.o vset.o set_flippers.o \
	reset_display.o foca_energy.o defa.o \
	vmedrive.o zetadrive.o \
	pidrive.o trashdrive.o sethome.o check_limits.o go_limit.o\
        camac_init.o rlyoff.o rlyon.o readhisto.o gcam.o\
        read_relay.o gpib_listen.o gpib_unlisten.o wrpol.o \
        gpib_talk.o igpib.o gpib_execute.o asd_wrpol.o get_asd_block.o\
        kfetch.o setup_vm.o  calc_realpos.o  calc_virtpos.o \
        yield_home.o newmono.o home.o rock_count.o  vdrive.o  v_iang.o\
        motpar.o dmp_asd.o refl_time.o run_polar.o run_r_polar.o \
        run_xpolar.o run_p_buf.o readpass.o update_motcle.o \
        setname.o getpriv.o  setmag.o setmag_oxford.o careful_approach.o\
        temp_init.o srlia232.o lk232.o lr232.o sc232.o oxmag232.o \
        lr700_232.o nl232.o rd232.o frn232.o tdb232.o ox232.o ltc232.o\
        shear232.o om232.o newom_232.o ad232one.o ad232in.o ad232out.o \
        ad232.o \
        putchars.o putchars_blink.o putchars_bold.o put_help.o \
        putchars_rev.o contr_ring_bell.o cursor_abs.o \
        cursor_off.o cursor_on.o help_ring_bell.o scroll_display.o\
        priv_bypass.o \
        password_ok.o \
	fetch_tofile.o restore_fromfile.o dmp_dbuf.o dry_d_buf.o  \
	run_dpolar.o run_d_buf.o rdkeith.o \
	init_asd.o rd_trj.o run_trj.o dmp_trj.o wait_temp.o \
	ezdrive.o picodrive.o \
	fitsine.o

FLAM =  fndlam.o cnan.o cnt.o istop.o lamzer.o lohi.o peakpo.o
FLAM += scale.o smoth.o rdfiles.o hdwinit.o logfile_fl.o

MODS =  $(MDIR)/asd.o $(MDIR)/histo.o $(MDIR)/dartz.o $(MDIR)/eventflag.o
MODS += $(MDIR)/focus.o $(MDIR)/globals.o $(MDIR)/magnmodel.o
MODS += $(MDIR)/instr.o $(MDIR)/icpsig.o $(MDIR)/motors.o
MODS += $(MDIR)/polar.o $(MDIR)/polfig.o $(MDIR)/refl.o $(MDIR)/smg_prep.o
MODS += $(MDIR)/tc.o $(MDIR)/tc_careful.o $(MDIR)/tdb.o $(MDIR)/trj.o 
MODS += $(MDIR)/vdata.o $(MDIR)/virtmot.o $(MDIR)/xray.o $(MDIR)/zcoeffs.o
MODS += $(MDIR)/homedir.o $(MDIR)/hwlong.o $(MDIR)/lia.o $(MDIR)/ad201.o
MODS += $(MDIR)/fndlam.o

FOBJ = icp_main.o $(IOBJ)

TOBJ =  testpeak.o exam_peak.o lsq.o exam_dummy.o putcontr_nt.o read_string.o \
	scroll_display.o findhalfh.o cursor_abs.o fit1gauss.o putcontr_nf.o \
	lxgetstring.o putcontr.o get_asctime.o writefit.o 

.SUFFIXES: $(.SUFFIXES) .F
# With readline 4.1 need to explicitly link against libcurses.so

#ABSOFT_OPTS=-reclen32

all: icp

icp: $(COBJ) $(FOBJ)
	$(FC) $(FOPTS) -o $@ $(FOBJ) $(COBJ) $(LIBS) $(MODS)

# Example of how to test a piece of ICP without compiling icp
#testsine: testsine.o $(COBJ) $(IOBJ)
#	$(FC) $(FOPTS) -o $@ testsine.o $(IOBJ) $(COBJ) $(LIBS)

imotors: imotors.o rdfiles.o $(COBJ) $(IOBJ)
	$(FC) $(FOPTS) -o $@ imotors.o rdfiles.o $(COBJ) $(IOBJ) $(MODS) $(LIBS)

imot: imot.o rdfiles.o $(COBJ) $(IOBJ)
	$(FC) $(FOPTS) -o $@ imot.o rdfiles.o $(COBJ) $(IOBJ) $(MODS) $(LIBS) 

fndlam: $(FLAM) $(COBJ) $(IOBJ)
	$(FC) $(FOPTS) -o $@ $(FLAM) $(COBJ) $(IOBJ) $(MODS) $(LIBS)

testpeak: $(TOBJ)
	$(FC) $(FOPTS) -o $@ $(TOBJ) $(LIBS)
fsource:
	for a in *.src; do
	echo $a | $(PERL) -ne 's/\.src/\.f/; print' | xargs ${MAKE} ;\
	done
vmstar:
	for a in *.src; do \
	echo $$a | $(PERL) -ne 's/\.src/\.for/; print' ; \
	echo $$a | $(PERL) -ne 's/\.src/\.for/; print' | xargs ${MAKE} -s ;\
	done
	${TAR} cf icpvms.tar *.for
.c.o:
	$(CC) $(COPTS) -c $*.c
.F.o:
	$(FC) $(FFLAGS) -c $*.F
.f.o:
#	export ABSOFT_RT_FLAGS='-reclen32'
	$(FC) $(FOPTS) -c $*.f
.src.f:
	$(CPP) $(CPPOPTS) $*.src |$(PERL) -ne 'print if /\S/' > $*.f
.src.for:
	$(CPP) $(CPPOPTS) -DVMS $*.src |$(PERL) -ne 'print if /\S/' > $*.for
clean:
	rm -f *.o *~ *.f *.for
realclean: clean
	rm -f icp 
