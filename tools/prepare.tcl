#!/usr/bin/wish
#
# Prepare - Instrument Control Program buffer editor
#
#           Dr. Nicholas C. Maliszewskyj - NIST Center for Neutron Research
#
# Changelog - June 2000 Initial revision in CVS - NCM
#             Jan  2001 Automon, Sequence editor, Configuration - NCM/BHT
#             Feb  2001 Customize further by adding mon choices - NCM/BHT
#             Aug  2001 Change null padding to space padding in buffer writes
#             Sep  2001 Modify to run with either wish or cwsh 
#                        (text-based wish) - JC/NCM
#             May  2002 Automon fixes, use of classes to dictate fonts
#                       add DisableWidget command, etc          - BHT
#             Jul  2002 Enable Bragg buffers - NCM
#             Jan  2003 Finally correct problem of shifting elements in 
#                       buffer summary window. Make sure changes registered
#                       after bufop - NCM
#             Mar  2003 Add support for diffraction buffers - NCM
#             Mar  2004 Change packing of GUI to permit expansion of buf list
#                       Fix buffer copy bug - KOD
#             Mar  2006 Change range of increment scans for BT1 reflecting
#                       changes to monochromators at instrument - NCM
set version {$Id: prepare.tcl,v 1.56 2012/03/28 14:02:42 nickm Exp $}
# set tcl_precision 12
#set config(root) $env(HOME)
set config(root) /usr/local/icp
switch -regexp [info nameofexecutable] {
   "cwsh"  { set text_only 1 }
   default { wm withdraw .; update; set text_only 0 }
}

#=Global variables=============================================================
# ibuf_fields
# ibuf
# ibufdisp
# qbuf_fields
# qbuf
# qbufdisp
# bbuf_fields
# bbuf
# bbufdisp
# tbuf_fields
# tbuf
# tbufdisp
# rbuf_fields
# rbuf
# rbufdisp
# dbuf_fields
# dbuf
# dbufdisp
#
# changed
# monrec
set ibuf_fields { a1beg a1end a1inc a2beg a2end a2inc a3beg a3end a3inc \
                  a4beg a4end a4inc a5beg a5end a5inc a6beg a6end a6inc \
                  comment ter th th0 mon mt mpf t0 it tw pts \
	          flip1 flip2 flip3 flip4 hf ihf hfw hfh }

set qbuf_fields { comment a b c aa bb cc ec es ef eft \
	          hc kc lc hs ks ls pts \
		  hkl11 hkl12 hkl13 a1 hkl21 hkl22 hkl23 hkl1 hkl2 \
                  tmp it tw th0 ter hf mon mpf mt flip1 flip2 flip3 flip4 \
		  ihf hfw hfh th }

set bbuf_fields { comment a b c aa bb cc ec es ef eft \
	          hc kc lc hs ks ls pts \
		  hkl11 hkl12 hkl13 a1 hkl21 hkl22 hkl23 hkl1 hkl2 \
                  tmp it tw th0 ter hf mon mpf mt flip1 flip2 flip3 flip4 \
		  a_1 a_2 a_3 a_4 a_5 a_6 \
		  i_1 i_2 i_3 i_4 i_5 i_6 \
		  r_3 r_4 \
		  ihf hfw hfh th }

set tbuf_fields { comment ec es ef m3 \
                  tmp it tw th0 ter hf mon mpf pts \
                  mt ihf hfw hfh th }

set rbuf_fields { comment pts qx qxi qz qzi scan_mode \
                  s_1 s_2 s_3 s_4 i_1 i_2 i_3 i_4 \
		  tmp it tw th0 ter hf mon0 mon1 exp mpf \
                  mt flip1 flip2 flip3 flip4 numx sx ix \
                  ihf hfw hfh th }

set dbuf_fields { comment lpn_1 lpn_2 lpn_3 lpn_4 lpn_5 lpn_6 \
                  lpts_1 lpts_2 lpts_3 lpts_4 lpts_5 lpts_6 \
                  mn_1 mn_2 mn_3 mn_4 mn_5 mn_6 \
                  a_1 a_2 a_3 a_4 a_5 a_6 \
		  i_1 i_2 i_3 i_4 i_5 i_6 \
		  xyz_1 xyz_2 xyz_3 xyz_4 xyz_5 xyz_6 \
		  a_xyz_1 a_xyz_2 a_xyz_3 a_xyz_4 a_xyz_5 a_xyz_6 \
		  i_xyz_1 i_xyz_2 i_xyz_3 i_xyz_4 i_xyz_5 i_xyz_6 \
		  pts hf phi psi phi_inc psi_inc \
		  tmp it tw th0 ter mon mpf mt flip1 flip2 flip3 flip4 \
		  ihf hfw hfh th }

array set changed { ibufdisp 0 qbufdisp 0 bbufdisp 0 tbufdisp 0 rbufdisp 0 dbufdisp 0}

#set monrec(mfile)    "$env(HOME)/cfg/MONITOR.REC"
set monrec(mfile)    "$config(root)/cfg/MONITOR.REC"
set monrec(prefix)   "deflt"
set monrec(user)     ""
set monrec(sample)   ""
set monrec(quantity) 1
set monrec(power)    20
set monrec(beamh)    2
set monrec(beamw)    1
set monrec(mono)     none
set monrec(ana)      none
set monrec(filter)   none
set monrec(coll)     20-20-99-99
set monrec(efix)     14.70
set Pi                3.1415926535897932384626433832795

#=Configuration================================================================

#
# Initialize configuration variables used to customize GUI
proc ConfigInit { } {
    global config env
    
    set config(cfgfile) $config(root)/cfg/INSTR.CFG

#    set config(nsta) -1
    set config(ibuf_state) normal
    set config(tbuf_state) disabled
    set config(qbuf_state) disabled
    set config(bbuf_state) disabled
    set config(rbuf_state) disabled
    set config(dbuf_state) disabled

    set config(ibuf_mots) {1 2 3 4 5 6}
    set config(pola_state) normal

    set config(ibufentry) {}
    set config(tbufentry) {}
    set config(qbufentry) {}
    set config(bbufentry) {}
    set config(rbufentry) {}

    set config(default_buf) increment

    set config(mode)       2  ;# Edit mode: 0=simple, 1=tempctrl, 2=magctrl
    set config(mono)       default
    set config(mono_list)  default

    set config(instr)      default ;# Desired instrument configuration
    set config(instr_list) default ;# List of instrument configurations

    set config(paranoid)   0 ;# Check for buffer changes to display dialog
}

# Read INSTR.CFG ... just enough to read nsta for the moment
proc ConfigRead { } {
    global config

    if [info exists config(nsta)] { return }
    if [catch {open $config(cfgfile) r} f] {
	puts "Could not open config file $config(cfgfile)"
	return
    }
    gets $f
    gets $f input
    regsub -all {([ ]+)} [string trimleft $input] { } input
    set fields [split $input]
    set nsta [lindex $fields 0]
    set nmots [lindex $fields 1]
    # Skip garbage
    for {set i 0} {$i < 8} {incr i} {
	gets $f 
    }

    set config(nsta) $nsta
    # Take care of extra lines for histogramming memory
    if {$nsta == -1 || $nsta == -7 || $nsta == 8} {
	gets $f 
	gets $f 
    }
    
    set config(motlist) {}
    for {set i 0} {$i < $nmots} {incr i} {
	gets $f input
	regsub -all {([ ]+)} [string trimleft $input] { } input
	set fields [split $input]
	lappend config(motlist) [lindex $fields 0]
    }
    set config(motlist) [lsort -integer $config(motlist)]
    close $f
}

#
# Apply some instrument-specific customizations
#
proc ConfigCruft { } {
    global env config text_only

    if {![info exists config(nsta)]} {
	# Note: tk_dialog returns index of selected button
        if $text_only {	
           set iname [ck_dialog .instr "Choose Instrument" \
                   "Choose the instrument you will use" \
                   XR0 BT1 BT2 BT4 BT5 BT7 BT8 BT9 NG1 NG5 NG7]
        } else {
	   set iname [tk_dialog .instr "Choose Instrument" \
		   "Choose the instrument you will use" \
		   question 0 XR0 BT1 BT2 BT4 BT5 BT7 BT8 BT9 NG1 NG5 NG7]
	}

	switch $iname {
            0 { set config(nsta) 0 }
	    1 { set config(nsta) 1 }
	    2 { set config(nsta) 2 }
	    3 { set config(nsta) 4 }
	    4 { set config(nsta) 5 }
	    5 { set config(nsta) 7 }
	    6 { set config(nsta) 8 }
	    7 { set config(nsta) 9 }
	    8 { set config(nsta) -1 }
	    9 { set config(nsta) -5 }
	    10 { set config(nsta) -7 }
	    default { set config(nsta) 10 }
	}
    }

    switch -- $config(nsta) {
	0 {
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(ibuf_mots)  { 1 2 3 4 5 6 }
	    set config(pola_state) disabled
	    set config(mono_list)  {PG002}
	}
	1 {
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(ibuf_mots)  { 3 4 }
	    set config(pola_state) disabled
	    set config(mode)       0 
	    set config(mono_list)  {Ge311 Cu311 Ge733}
	    set config(instr_list) {}

	    foreach mon $config(mono_list) {
		lappend config(instr_list) "$mon-15'"
	    }
	    foreach mon $config(mono_list) {
		lappend config(instr_list) "$mon-7'"
	    }

	    set config(instr) "Cu311-15'"

	    # Store monitor rates in cps
	    set config(cps-Ge311-15') 2100
	    set config(cps-Cu311-15') 1100
	    set config(cps-Ge733-15')  600
	    set config(cps-Ge311-7')  1050
	    set config(cps-Cu311-7')   550
	    set config(cps-Ge733-7')   300
	    # Instrument scientist values for monitor rates
	    if {[catch {source $config(root)/cfg/bt1-mrat.dat} errmsg]} {
		puts "BT-1 mrat source error = $errmsg"
	    }
	    if [ConfigBT1] { return }
	    set config(paranoid)         1
	}
	2 {
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(qbuf_state) normal
	    set config(bbuf_state) normal
	    set config(mono_list)  {PG002 Heusler}
	    set config(ibuf_mots)  { 1 2 3 4 5 6 }
	}
	4 {
	    set config(default_buf) trash
	    set config(rbuf_state) disabled
	    set config(pola_state) disabled
	    set config(qbuf_state) normal
	    set config(bbuf_state) normal
	    set config(tbuf_state) normal
	    set config(ibuf_mots)  { 1 2 3 4 5 6 }
	    set config(mono_list)  {Ge311 Cu311}
	}
	5 {
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(ibuf_mots)  { 1 2 3 4 5 6 }
	    set config(pola_state) disabled
	    set config(mono_list)  {Si220}
	}
	7 {
	    set config(default_buf) increment
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(ibuf_mots)  { 3 4 5 6 }
	    set config(pola_state) disabled
	    set config(mono_list)  {PG002}
	}
	8 {
	    set config(default_buf) diffraction
	    set config(ibuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(dbuf_state) normal
	    set config(pola_state) disabled
	    set config(mono_list)  {PG002}
	}
	9 {
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(pola_state) disabled
	    set config(qbuf_state) normal
	    set config(bbuf_state) normal
	    set config(dbuf_state) normal
	    set config(mono_list)  {Si111 PG002}
	}
	-1 {
	    set config(default_buf) increment
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	}
        -5 {
	    set config(default_buf) increment
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(qbuf_state) normal
	    set config(bbuf_state) normal
	}
	-7 {
	    set config(default_buf) reflectivity
	    set config(ibuf_state) disabled
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) normal
	    set config(pola_state) disabled
	    set config(mono_list)  {PG002}
	}
	-11 {
	    set config(default_buf) increment
	    set config(qbuf_state) disabled
	    set config(bbuf_state) disabled
	    set config(tbuf_state) disabled
	    set config(rbuf_state) disabled
	    set config(dbuf_state) normal
	}
    }

    # If bragg or trash buffers enabled, load shared library for calculations

    set config(mono) [lindex $config(mono_list) 0]
}

#=BT1 specific routines========================================================

# Reads and returns the monochromator setting for BT1
proc GetBT1Mono {} {
    global env config
    if {![file exists $config(root)/cfg/MOTORS.BUF]} {
	return
    }
    set f [open $config(root)/cfg/MOTORS.BUF]
    fconfigure $f -encoding binary
    seek $f [expr 160 * 33 + 10 * 4] start
    set input [read $f 5]
    set monobt1 $input

    # Get current instrument configuration
    switch -glob $monobt1 {
	G*7* { return Ge733 }
	G*3* { return Ge311 }
    }
    return Cu311
}

# check for change in BT1 monochromator 
proc CheckBT1MonoChange {} {
    global config text_only
    # if we do not have a saved value, don't check
    if {$config(BT1mono-asread) == ""} return
    set newmono [GetBT1Mono]
    if {$config(BT1mono-asread) != $newmono && $config(mono) != $newmono} {
	if $text_only {
	    set change [ck_dialog .change "Monochromator Changed!" \
		    "The monochromator setting on BT1 has changed. Set monochromator to $newmono?" \
		    Yes No]
	} else {
	    set change [tk_dialog .change "Monochromator Changed!" \
		    "The monochromator setting on BT1 has changed. Set monochromator to $newmono?" \
		    error 0 Yes No]
	}
	if {$change == 0} {
	    set config(mono) $newmono
	    set config(instr) "$newmono-15'"
	    IBufCalcBT1
	}
	# either way, set the saved value to the new
	set config(BT1mono-asread) $newmono
    }
}

# Read additional configuration information for BT1
#      Current monochromator and collimation
#      T+/T- flag
# No safety yet - should "protect" this with catch statements
proc ConfigBT1 { args } {
    global config env

    set config(BT1mono-asread) ""
    if {![file exists $config(root)/cfg/MOTORS.BUF]} {
	return 0
    } 
    set f [open $config(root)/cfg/MOTORS.BUF]
    fconfigure $f -encoding binary

    seek $f [expr 160 * 33] start
    set input [read $f 4]
    binary scan $input i collim
    
    seek $f [expr 160 * 33 + 10 * 4] start
    set input [read $f 5]
    set monobt1 $input

    seek $f [expr 160 * 34 + 32*4] start
    set input [read $f 4]
    binary scan $input i tempctr

    # Set default edit mode
    if {$tempctr} {
	set config(mode) 1 ;# Temperature control
    } else {
	set config(mode) 0 ;# Expert/Magnet control
    }

    # Get current instrument configuration
    switch -glob $monobt1 {
	GE311 { set config(mono) Ge311 }
	GE733 { set config(mono) Ge733 }
	default {
	    set config(mono) Cu311
	}
    }
    set config(BT1mono-asread) $config(mono)

    switch $collim {
	7 {       set coll "7'"  }
	default { set coll "15'" }
    }

    set config(instr) "$config(mono)-$coll" 
    close $f
    return 1
} 

# invoked when the instrument configuration changes
proc ConfigChange { args } {
    global config

    switch -- $config(nsta) {
	1 {
	    switch -glob $config(instr) {
		G*311* { set config(mono) Ge311 }
		G*733* { set config(mono) Ge733 }
		default {
		    set config(mono) Cu311
		}
	    }
	    # Enforce rules for BT1 after each monochromator change
	    IBufCalcBT1 
	}
    }
}


# enable & disable widgets here
proc DisableWidget {wlist mode} {
    if {$mode} {
	foreach {state bg fg} {normal white black} {} 
    } else {
	foreach {state bg fg} {disabled gray86 gray} {}
    }
    foreach w $wlist {
	switch [winfo class $w] {
	    Label {$w config -fg $fg}
	    Checkbutton {$w config -fg $fg -state $state}
	    Button -
	    Entry {$w config -fg $fg -state $state -bg $bg}
	    default {puts "$w class is [winfo class $w]"}
	}
    }
}
#=I BUFFER=====================================================================

proc IBufEntryCreate { parent } {
    global ibufdisp config text_only

    set config(ibufentry) $parent

    label $parent.bufnolabel -textvariable ibufdisp(label)
    label $parent.commentlabel -text "Comment:"
    entry $parent.comment -width 35 -textvariable ibufdisp(comment) 
    if $text_only {
       grid $parent.bufnolabel -row 0 -column 0 -sticky w
       grid $parent.commentlabel -row 0 -column 1 -ipadx 3 -sticky e
       grid $parent.comment -row 0 -column 2 -columnspan 6 -sticky ew
       set width 5
    } else {
       grid $parent.bufnolabel -row 0 -column 0
       grid $parent.commentlabel -row 0 -column 1
       grid $parent.comment -row 0 -column 2 -columnspan 6 -sticky ew
       set width 7
    }
    set row 1
    foreach i $config(ibuf_mots) {
	label $parent.a${i}beglabel -text "A${i}-beg:"
	entry $parent.a${i}beg -width $width -textvariable ibufdisp(a${i}beg)
	label $parent.a${i}inclabel -text "Inc-A${i}:"
	entry $parent.a${i}inc -width $width -textvariable ibufdisp(a${i}inc)
	label $parent.a${i}endlabel -text "A${i}-end:"
	entry $parent.a${i}end -width $width -textvariable ibufdisp(a${i}end)
        if $text_only {
           grid $parent.a${i}beglabel -row $row -column 1 -ipadx 1 -sticky w
           grid $parent.a${i}beg -row $row -column 2 -ipadx 1 -sticky w
           grid $parent.a${i}inclabel -row $row -column 3 -ipadx 1 -sticky w
           grid $parent.a${i}inc -row $row -column 4 -ipadx 1 -sticky w
           grid $parent.a${i}endlabel -row $row -column 5 -ipadx 1 -sticky w
           grid $parent.a${i}end -row $row -column 6 -ipadx 1 -sticky w
        } else {
           grid $parent.a${i}beglabel -row $row -column 1
           grid $parent.a${i}beg -row $row -column 2
           grid $parent.a${i}inclabel -row $row -column 3
           grid $parent.a${i}inc -row $row -column 4
           grid $parent.a${i}endlabel -row $row -column 5
           grid $parent.a${i}end -row $row -column 6
        }
	bind $parent.a${i}beg <Return>   "IBufAngleBinding ibufdisp a${i}beg w"
	bind $parent.a${i}inc <Return>   "IBufAngleBinding ibufdisp a${i}inc w"
	bind $parent.a${i}end <Return>   "IBufAngleBinding ibufdisp a${i}end w"
	bind $parent.a${i}beg <FocusOut> "IBufAngleBinding ibufdisp a${i}beg w"
	bind $parent.a${i}inc <FocusOut> "IBufAngleBinding ibufdisp a${i}inc w"
	bind $parent.a${i}end <FocusOut> "IBufAngleBinding ibufdisp a${i}end w"
        if {!$text_only} {

	bind $parent.a${i}beg <<Paste>>  "IBufAngleBinding ibufdisp a${i}beg w"
	bind $parent.a${i}inc <<Paste>>  "IBufAngleBinding ibufdisp a${i}inc w"
	bind $parent.a${i}end <<Paste>>  "IBufAngleBinding ibufdisp a${i}end w"

        }
	incr row
    }
    if $text_only {
       # blank line
       label $parent.blank -text " "
       grid $parent.blank -row $row -column 0
       incr row
    }
    frame $parent.t
    label $parent.tlabel -text "Temp/H:"
    if $text_only {
       grid $parent.tlabel -row $row -column 0 -sticky w
       grid $parent.t -row $row -column 1 -columnspan 7 -sticky w
    } else {
       grid $parent.tlabel -row $row -column 0
       grid $parent.t -row $row -column 1 -columnspan 6
    }
    label $parent.t.t0label -text "T0: "
    entry $parent.t.t0 -textvariable ibufdisp(t0)
    label $parent.t.itlabel -text "Inc-T: "
    entry $parent.t.it -textvariable ibufdisp(it)
    label $parent.t.twlabel -text "Wait: "
    entry $parent.t.tw -textvariable ibufdisp(tw)
    label $parent.t.errlabel -text "Err: "
    entry $parent.t.err -textvariable ibufdisp(ter)
    label $parent.t.hld0label -text "Hld0: "
    entry $parent.t.hld0 -textvariable ibufdisp(th0)
    label $parent.t.hldlabel -text "Hld: "
    entry $parent.t.hld -textvariable ibufdisp(th)
    button $parent.t.field -text FIELD -command IBufFieldDialog
    label $parent.timelabel -text "Time:"

    pack $parent.t.t0label $parent.t.t0 $parent.t.itlabel $parent.t.it \
	    $parent.t.twlabel $parent.t.tw $parent.t.errlabel $parent.t.err \
	    $parent.t.hld0label $parent.t.hld0 $parent.t.hldlabel \
	    $parent.t.hld $parent.t.field -side left

    if $text_only {
       $parent.t.t0   configure -width 4
       $parent.t.it   configure -width 4
       $parent.t.tw   configure -width 4
       $parent.t.err  configure -width 4
       $parent.t.hld0 configure -width 4
       $parent.t.hld  configure -width 4
    } else {
       $parent.t.t0   configure -width 6
       $parent.t.it   configure -width 6
       $parent.t.tw   configure -width 4
       $parent.t.err  configure -width 4
       $parent.t.hld0 configure -width 4
       $parent.t.hld  configure -width 4
    }

    incr row
    frame $parent.time

    if $text_only {
       grid $parent.timelabel -row $row -column 0 -sticky w
       grid $parent.time -row $row -column 1 -columnspan 7 -sticky w
    } else {
       grid $parent.timelabel -row $row -column 0
       grid $parent.time -row $row -column 1 -columnspan 6
    }
    label $parent.time.monlabel -text "Monit:"
    entry $parent.time.mon -width 7 -textvariable ibufdisp(mon)
    label $parent.time.prelabel -text "Prefac:"
    entry $parent.time.pre -width 4 -textvariable ibufdisp(mpf)
    label $parent.time.mtlabel -text "M-typ:"
    button $parent.time.pol -text "POLARIZE" -command IBufPolDialogShow \
            -state $config(pola_state)
    if {$config(pola_state) == "disabled"} {
	DisableWidget $parent.time.pol 0
    } else {
	DisableWidget $parent.time.pol 1
    }
    button $parent.time.amon -text "AUTOMON" -command AutomonShow
    if $text_only {
       ck_optionMenu $parent.time.mt ibufdisp(mt) TIME NEUT
       pack $parent.time.monlabel -side left
       pack $parent.time.mon -side left -padx 1
       pack $parent.time.prelabel -side left
       pack $parent.time.pre -side left -padx 1
       pack $parent.time.mtlabel -side left
       pack $parent.time.mt -side left
       pack $parent.time.pol -side left
       pack $parent.time.amon -side left -padx 1
    } else {
       tk_optionMenu $parent.time.mt ibufdisp(mt) TIME NEUT
       pack $parent.time.monlabel $parent.time.mon $parent.time.prelabel \
	       $parent.time.pre $parent.time.mtlabel $parent.time.mt \
	       $parent.time.pol $parent.time.amon -side left
    }

    frame $parent.pts
    label $parent.pts.label -text "NPTS: "
    entry $parent.pts.val -textvariable ibufdisp(pts) -width 6

    pack $parent.pts.label $parent.pts.val -side left
    grid $parent.pts -row 1 -column 7

    bind $parent.pts.val <Return>   IBufCalcAngles
    bind $parent.pts.val <FocusOut> IBufCalcAngles
    bind $parent.comment <Return>   IBufCommentBinding
    bind $parent.comment <FocusOut>   IBufCommentBinding
    

    return $parent
}

# Edit mode modifications
proc IBufEntryMod { } {
    global ibufdisp config text_only

    set parent $config(ibufentry)
    if {$config(mode) == 0} {
	set state disabled
        if $text_only {
	   set color white
        } else {
           set color gray
        }
    } else {
	set state normal
        if $text_only {
           set color white
        } else {
	   set color black
        }
    }
    foreach w "$parent.tlabel [winfo children $parent.t]" {
	switch [winfo class $w] {
	    Label {$w config -fg $color}
	    Entry {$w config -fg $color -state $state}
	}
    }

    if {$config(mode) == 2} {
	set state normal
        if $text_only {
           set color white
        } else {
           set color black
        }
    } else {
	set state disabled
        if $text_only {
           set color white
        } else {
           set color gray
        }
    }
    if {$config(nsta) == 1} {
	foreach i $config(ibuf_mots) {
	    foreach l "a${i}beglabel a${i}inclabel a${i}endlabel" {
		$parent.$l config -fg $color
	    }
	    foreach e "a${i}beg a${i}inc a${i}end" {
		$parent.$e config -state $state -fg $color
	    }
	} 
	$parent.pts.label config -fg $color
	$parent.pts.val config -state $state -fg $color
    }
    foreach l {t.itlabel t.hldlabel} {
	$parent.$l config -fg $color
    }
    foreach e {t.it t.hld t.field} {
	$parent.$e config -state $state -fg $color
    }

    # Enforce rules for BT1
    if { $config(nsta) == 1 } { IBufCalcBT1 }
}

proc IBufReadRec { file rec } {
    upvar $rec r
    set input [read $file 320]
    binary scan $input \
	    A50sfffffffffffffffffffffA4iiiiffff \
	    r(comment) pad1 \
	    r(a1beg) r(a2beg) r(a3beg) r(a4beg) r(a5beg) r(a6beg) \
	    r(a1inc) r(a2inc) r(a3inc) r(a4inc) r(a5inc) r(a6inc) \
	    r(pts) r(hf) r(t0) r(it) r(tw) r(th0) r(ter) \
	    r(mon) r(mpf) r(mt) \
	    r(flip1) r(flip2) r(flip3) r(flip4) \
	    r(ihf) r(hfw) r(hfh) r(th)
}

proc IBufReadFile { } {
    global ibuf ibuf_fields

    set f [open IBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	IBufReadRec $f icpin
	foreach fld $ibuf_fields {
	    # for a*end values compute
	    if {[regexp {a([0-9]+)end} $fld junk n]} {
		set icpin($fld) [expr $icpin(a${n}beg) + \
		$icpin(a${n}inc) * ($icpin(pts) - 1)]
	    }
	    # now store in ibuf array
	    if {![catch {set icpin($fld)} result]} {
		set ibuf($i,$fld) $result
	    }
	}
    }
    # Read last record which contains run sequence information
    set contents [read $f 320]
    binary scan $contents A320 contents
    set ibuf(sequence) [string trimright $contents]
    close $f
    
    set ibuf(mtime) [file mtime IBUFFER.BUF]
}
 
proc IBufCheckFld { } {
    global ibufdisp 
    set flds {a1beg a2beg a3beg a4beg a5beg a6beg \
	    a1inc a2inc a3inc a4inc a5inc a6inc \
	    pts hf t0 it tw th0 ter mon mpf ihf hfw hfh th }
    foreach fld $flds {
	# Check to make sure these are floating point numbers 
	if [catch { expr 1.0 * $ibufdisp($fld) } result] {
	    set ibufdisp($fld) "--------"
	    return -code error "Parameter \"$fld\" must be floating point number"
	}
    }
}

proc IBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set recval [binary format \
	    A50sfffffffffffffffffffffA4iiiiffff \
	    $r(comment) $pad1 \
	    $r(a1beg) $r(a2beg) $r(a3beg) $r(a4beg) $r(a5beg) $r(a6beg) \
	    $r(a1inc) $r(a2inc) $r(a3inc) $r(a4inc) $r(a5inc) $r(a6inc) \
	    $r(pts) $r(hf) $r(t0) $r(it) $r(tw) $r(th0) $r(ter) \
	    $r(mon) $r(mpf) $r(mt) \
	    $r(flip1) $r(flip2) $r(flip3) $r(flip4) \
	    $r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A320 $recval]
    puts -nonewline $file $record
    string length $record
}


proc IBufWriteFile { } {
    global ibuf ibuf_fields

    if [file exists IBUFFER.BUF] {
	set f [open IBUFFER.BUF r+]
	fconfigure $f -encoding binary
	seek $f [expr 320 * 30] start
	set sequence [read $f 320]
	set ibuf(sequence) [string trimright $sequence]	
	seek $f 0
    } else {
	set f [open IBUFFER.BUF w]
	fconfigure $f -encoding binary
    }
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $ibuf_fields {
	    set icpout($fld) $ibuf($i,$fld)
	}
	IBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A320 $ibuf(sequence)]
    close $f

    set ibuf(mtime) [file mtime IBUFFER.BUF]
}

proc IBufInit { } {
    global ibuf config
    array set ibuf {buftype Increment}
    for {set i 0} {$i < 30} {incr i} {
	set ibuf($i,comment) "empty"
	for {set j 1} {$j <= 6} {incr j} {
	    set ibuf($i,a${j}beg)   0.0
	    set ibuf($i,a${j}inc)   0.0
	    set ibuf($i,a${j}end)   0.0
	}
	set ibuf($i,t0)    0
	set ibuf($i,it)    0
	set ibuf($i,tw)    0
	set ibuf($i,ter)   0
	set ibuf($i,th0)   0
	set ibuf($i,th)    0
	set ibuf($i,mon)   1000
	set ibuf($i,mpf)   1
	set ibuf($i,mt)    TIME
	set ibuf($i,pts)   1
	set ibuf($i,flip1) 0
	set ibuf($i,flip2) 0
	set ibuf($i,flip3) 0
	set ibuf($i,flip4) 0
	set ibuf($i,hf)    0
	set ibuf($i,ihf)   0
	set ibuf($i,hfw)   0
	set ibuf($i,hfh)   0
    }
    set ibuf(currentrec)   0
    set ibuf(mtime)        0

    if {$config(nsta) == 1} {
	for {set i 0} {$i < 30} {incr i} {
	    set ibuf($i,mpf) 4
	    set ibuf($i,mt)  NEUT
	}
    }

    set ibuf(sequence) ""
}

proc IBufList { } {
    global text_only
    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
	$lbox insert end [IBufFormatRec $i]
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "IBufDisp \[$lbox curselection\]"
    if $text_only {
       set bind_type <Linefeed>
    } else {
       set bind_type <Return>
    }
    bind $lbox $bind_type "IBufDisp \[$lbox curselection\]"
}

proc IBufFormatRec { recno } {
    global ibuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $ibuf($recno,comment)]
    append output [format " %6.2f "  $ibuf($recno,a4beg)]
    append output [format "%6.2f "   $ibuf($recno,a4end)]
    append output [format "%5.2f "   $ibuf($recno,a4inc)]
    append output [format "%2.0f\* " $ibuf($recno,mpf)]
    append output [format "%6.0f "   $ibuf($recno,mon)]
    append output [binary format A4  $ibuf($recno,mt)]
    append output [format " %6.1f"   $ibuf($recno,t0)]
    append output [format "%4.0f "   $ibuf($recno,th0)]
 
    return $output
}

proc IBufDisp { recno } {
    global ibufdisp ibuf ibuf_fields changed config

    set mtime [file mtime IBUFFER.BUF]
    if {$mtime != $ibuf(mtime)} {
	IBufReadFile
	IBufList
    }
    
    if { $config(nsta) == 1 } { CheckBT1MonoChange }
    if { $config(paranoid)} { CheckBufChange }

    foreach fld $ibuf_fields {
	set ibufdisp($fld) $ibuf($recno,$fld)
    }
    set num [format "%2d" [expr $recno + 1]]
    set ibufdisp(label) "IBuffer No: $num"
    set ibufdisp(current) $recno
    # Fix up several fields
    set flfields { a1beg a1end a1inc a2beg a2end a2inc a3beg a3end a3inc \
	    a4beg a4end a4inc a5beg a5end a5inc a6beg a6end a6inc }

    foreach f $flfields {
	set ibufdisp($f) [fltrim $ibufdisp($f) 10000]
    }

    if [catch {expr $ibufdisp(pts) * 1} ibufdisp(pts)] {
	set ibufdisp(pts) 1
    }

    IBufCalcAngles

    # clear changed flag
    set changed(ibufdisp) 0

    # Enforce rules for BT1
    if { $config(nsta) == 1 } { IBufCalcBT1 }

    # switch focus to listbox so Return & arrowkeys work 
    # and force selection (for 1st time)
    focus .l.list
    .l.list selection set $recno
}

proc IBufUpdate { } {
    global ibufdisp ibuf changed config

    if [catch {IBufCheckFld} result] {
	return -code error $result
    }

    if { $config(nsta) == 1 } { CheckBT1MonoChange }

    set recno $ibufdisp(current)
    foreach fld [array names ibufdisp] {
	set ibuf($recno,$fld) $ibufdisp($fld)
    }
    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [IBufFormatRec $recno]
    # update buffer information on disk
    IBufWriteFile
    set changed(ibufdisp) 0
}

proc IBufGetSequence { } {
    global ibuf
    if {![file exists IBUFFER.BUF]} {
	return
    }
    set f [open IBUFFER.BUF r]
    fconfigure $f -encoding binary
    seek $f [expr 320 * 30] start
    set sequence [read $f 320]
    set ibuf(sequence) [string trimright $sequence]
}

proc IBufFieldDialog { } {
    global ibufdisp ibuf text_only

    toplevel .field

    label .field.title -text "MAGNETIC FIELD"
    label .field.initlab -text "Initial Field (user units):"
    label .field.incrlab -text "Field Increment (user units):"
    label .field.waitlab -text "Max Wait Per Point (secs) :"
    entry .field.initentry -textvariable ibufdisp(hf)  -width 10
    entry .field.increntry -textvariable ibufdisp(ihf) -width 10
    entry .field.waitentry -textvariable ibufdisp(hfw) -width 10
    label .field.t1 -text "(Wait used in non-persistent mode on SC only)"
    label .field.t2 -text "(Reply 0 to get default optimum wait times)"
    button .field.dismiss -text "Dismiss" -command IBufFieldDialogDismiss
    if $text_only {
       grid .field.title     -row 0 -column 0 -columnspan 2 -sticky news
       label .field.blank1 -text " "
       grid .field.blank1    -row 1
       grid .field.initlab   -row 2 -column 0 -sticky w
       grid .field.incrlab   -row 3 -column 0 -sticky w
       grid .field.waitlab   -row 4 -column 0 -sticky w
       grid .field.initentry -row 2 -column 1
       grid .field.increntry -row 3 -column 1
       grid .field.waitentry -row 4 -column 1
       label .field.blank2 -text " "
       grid .field.blank2    -row 5
       grid .field.t1        -row 6 -column 0 -columnspan 2 -sticky ew
       grid .field.t2        -row 7 -column 0 -columnspan 2 -sticky ew
       label .field.blank3 -text " "
       grid .field.blank3    -row 8
       grid .field.dismiss   -row 9 -column 0 -columnspan 2 -sticky ew
       place .field -relheight 1.0 -relwidth 1.0
       focus .field
    } else {
       wm protocol .field WM_DELETE_WINDOW ".field.dismiss invoke"
       grid .field.title     -row 0 -column 0 -columnspan 2 -sticky news
       grid .field.initlab   -row 1 -column 0
       grid .field.incrlab   -row 2 -column 0
       grid .field.waitlab   -row 3 -column 0
       grid .field.initentry -row 1 -column 1
       grid .field.increntry -row 2 -column 1
       grid .field.waitentry -row 3 -column 1
       grid .field.t1        -row 4 -column 0 -columnspan 2 -sticky ew
       grid .field.t2        -row 5 -column 0 -columnspan 2 -sticky ew
       grid .field.dismiss   -row 6 -column 0 -columnspan 2 -sticky ew
    }
    return .field
}

proc IBufFieldDialogDismiss { } {
    global text_only
    if $text_only {
       place forget .field
       focus .
    }
    destroy .field
}

proc IBufPolDialogBuild { } {
    global ibufdisp text_only
    set p [toplevel .polarization]
    label $p.title -text "Polarization Setup"
    set f [frame $p.f]
    set d [button $p.dismiss -text "Dismiss" -command IBufPolDialogHide]
    wm protocol $p WM_DELETE_WINDOW "$d invoke"
    label $f.flip1t -text "Flipper 1"  
    label $f.flip2t -text "Flipper 2"  
    label $f.flipet -text "Execute ?"  
    frame $f.flip1
    frame $f.flip2
    frame $f.flipe
    label $f.flip1.a -text "OFF" 
    label $f.flip1.b -text "ON"  
    label $f.flip1.c -text "OFF" 
    label $f.flip1.d -text "ON"  
    label $f.flip2.a -text "OFF" 
    label $f.flip2.b -text "OFF" 
    label $f.flip2.c -text "ON"  
    label $f.flip2.d -text "ON"  
    #toggle_create $f.flipe.a ibufdisp(flip1)
    #toggle_create $f.flipe.b ibufdisp(flip2)
    #toggle_create $f.flipe.c ibufdisp(flip3)
    #toggle_create $f.flipe.d ibufdisp(flip4)
    tk_optionMenu $f.flipe.a ibufdisp(flip1) 0 1
    tk_optionMenu $f.flipe.b ibufdisp(flip2) 0 1
    tk_optionMenu $f.flipe.c ibufdisp(flip3) 0 1
    tk_optionMenu $f.flipe.d ibufdisp(flip4) 0 1

    if {!$text_only} {
	$f.flip1t configure -relief ridge
	$f.flip2t configure -relief ridge
	$f.flipet configure -relief ridge
	$f.flip1  configure -relief ridge -borderwidth 2
	$f.flip2  configure -relief ridge -borderwidth 2
	$f.flipe  configure -relief ridge -borderwidth 2
	$f.flip1.a configure -pady 5
	$f.flip1.b configure -pady 5
	$f.flip1.c configure -pady 5
	$f.flip1.d configure -pady 5
	$f.flip2.a configure -pady 5
	$f.flip2.b configure -pady 5
	$f.flip2.c configure -pady 5
	$f.flip2.d configure -pady 5
    }

    pack $p.title $f $d -side top -fill x
    grid $f.flip1t $f.flip2t $f.flipet -sticky ew
    grid $f.flip1 $f.flip2 $f.flipe -sticky ew
    pack $f.flip1.a $f.flip1.b $f.flip1.c $f.flip1.d -side top
    pack $f.flip2.a $f.flip2.b $f.flip2.c $f.flip2.d -side top
    pack $f.flipe.a $f.flipe.b $f.flipe.c $f.flipe.d -side top -fill x
    return $p
}

proc IBufPolDialogHide { } {
    global text_only 
    if $text_only {
	destroy .polarization
    } else {
	wm withdraw .polarization
    }
}

proc IBufPolDialogShow { } {
    global text_only

    if $text_only {
       catch { destroy .polarization }
       IBufPolDialogBuild
       place .polarization -relheight 1.0 -relwidth 1.0
       focus .polarization
    } else {
	if [winfo exists .polarization] {
	    wm deiconify .polarization
	} else {
	    IBufPolDialogBuild
	}
    }
}

proc IBufCalcAngles {{anglist {1 2 3 4 5 6}}} {
    global ibufdisp
    if [catch {expr $ibufdisp(pts) * 1} ibufdisp(pts)] {
	set ibufdisp(pts) "------"
	return
    }
    
    foreach i $anglist {
	set beg "a${i}beg"
	set inc "a${i}inc"
	set end "a${i}end"
	if [catch {expr $ibufdisp($beg) * 1.0} ibufdisp($beg)] {
	    set ibufdisp($beg) "------"
	    return
	}
	if [catch {expr $ibufdisp($inc) * 1.0} ibufdisp($inc)] {
	    set ibufdisp($inc) "------"
	    return
	}
	set ibufdisp($end) [expr $ibufdisp($beg) + \
		$ibufdisp($inc) * ($ibufdisp(pts) - 1)]
    }
    update
}

#
# end = beg + pts * inc
# pts = (end - beg) / inc
#
proc IBufAngleBinding { args } {
    global ibufdisp
    set largs [llength $args]

#    puts [join $args " "]
    if {$largs < 3} { return }
    set key [lindex $args 1]
    if {[string compare $key "pts"] == 0} {
	return [IBufCalcAngles]
    }

    if {![regexp {a([0-9])(beg|inc|end)} $key match axis which]} {
	return
    }
    set beg "a${axis}beg"
    set inc "a${axis}inc"
    set end "a${axis}end"
    if [catch {expr $ibufdisp($beg) * 1.0} ibufdisp($beg)] {
	set ibufdisp($beg) "******"
	return
    }
    if [catch {expr $ibufdisp($inc) * 1.0} ibufdisp($inc)] { 
	set ibufdisp($inc) "******"
	return
    }
    if [catch {expr $ibufdisp($end) * 1.0} ibufdisp($end)] {
	set ibufdisp($end) "******"
	return
    }

    switch $which {
	beg {
	}
	end {
	    if {$ibufdisp($inc) == 0} {
		return
	    }
	    set pts [expr ($ibufdisp($end)-$ibufdisp($beg))/$ibufdisp($inc) + 1]
	    set ibufdisp(pts) $pts
	}
	inc {
	}
    }
    return [IBufCalcAngles]
}

proc IBufCommentBinding { } {
    global ibufdisp

    if {[string length $ibufdisp(comment)] > 50} {
	set ibufdisp(comment) [string range $ibufdisp(comment) 0 49]
    }
}

# Special rules for BT1
proc IBufCalcBT1 {} {
    global ibufdisp config

# Disable "expert" feature per J. Stalick 7/21/09
#    if {$config(mode) != 2} {
	set varlist {a3beg a3end a3inc a4inc pts it th}
	set vallist {0.    0.    0.    0.05  201  0  0}
	if {$config(mono) == "Ge311"} {
	    lappend varlist a4beg a4end
	    lappend vallist 1.3   11.3
	} else {
	    lappend varlist a4beg a4end
	    lappend vallist 2.8 12.8
	}
	if {$config(mode) == 0} {
	    lappend varlist "t0"
	    lappend vallist "0"
	}
	foreach f $varlist v $vallist {
	    if {$ibufdisp($f) != $v} {set ibufdisp($f) $v}
	}
#    }
}

proc IBufKeyBinding { } {
    bind all <KeyPress-F1> IBufUpdate
    bind all <KeyPress-F2> BufopShow
    bind all <KeyPress-F3> {notebook_display .e ibuf}
    bind all <KeyPress-F4> {notebook_display .e blank}
}

#=Q BUFFER=====================================================================

proc QBufEntryCreate { p } {
    global qbuf qbufdisp config text_only

    set config(qbufentry) $p
    
    # Create widgets
    label $p.bufnolabel -textvariable qbufdisp(label)
    label $p.commentlabel -text "Comment:"
    entry $p.comment -textvariable qbufdisp(comment)


    label $p.latlabel -text "Lattice:"
    label $p.alabel -text "a:"
    entry $p.a -textvariable qbufdisp(a) -width 6
    label $p.blabel -text "b:"
    entry $p.b -textvariable qbufdisp(b) -width 6
    label $p.clabel -text "c:"
    entry $p.c -textvariable qbufdisp(c) -width 6
    label $p.aalabel -text "aa:"
    entry $p.aa -textvariable qbufdisp(aa) -width 6
    label $p.bblabel -text "bb:"
    entry $p.bb -textvariable qbufdisp(bb) -width 6
    label $p.cclabel -text "cc:"
    entry $p.cc -textvariable qbufdisp(cc) -width 6

    label $p.energylabel -text "Energy:"
    label $p.eclabel -text "EC:"
    entry $p.ec -textvariable qbufdisp(ec) -width 6
    label $p.eslabel -text "ES:"
    entry $p.es -textvariable qbufdisp(es) -width 6
    label $p.eflabel -text "EF:"
    entry $p.ef -textvariable qbufdisp(ef) -width 6
    label $p.eftlabel -text "EFT:"
    if {!$text_only} {
       tk_optionMenu $p.eft qbufdisp(eft) ANA MON
    } else {
       ck_optionMenu $p.eft qbufdisp(eft) ANA MON
    }

    label $p.spacelabel -text "Space:"
    label $p.hclabel -text "HC:"
    entry $p.hc -textvariable qbufdisp(hc) -width 6
    label $p.kclabel -text "KC:"
    entry $p.kc -textvariable qbufdisp(kc) -width 6
    label $p.lclabel -text "LC:"
    entry $p.lc -textvariable qbufdisp(lc) -width 6
    label $p.hslabel -text "HS:"
    entry $p.hs -textvariable qbufdisp(hs) -width 6
    label $p.kslabel -text "KS:"
    entry $p.ks -textvariable qbufdisp(ks) -width 6
    label $p.lslabel -text "LS:"
    entry $p.ls -textvariable qbufdisp(ls) -width 6

    label $p.orientlabel -text "Orient:"
    frame $p.orient
    label $p.orient.hkl1label -text "(h k l):"
    entry $p.orient.hkl1 -textvariable qbufdisp(hkl1) -width 6
    label $p.orient.a1label -text "Angle:"
    entry $p.orient.a1 -textvariable qbufdisp(a1) -width 6
    label $p.orient.hkl2label -text "(h k l)':"
    entry $p.orient.hkl2 -textvariable qbufdisp(hkl2) -width 6
    if $text_only {
       $p.orient.hkl1 configure -width 8
       $p.orient.hkl2 configure -width 8
       grid $p.orient.hkl1label -row 0 -column 0
       grid $p.orient.hkl1      -row 0 -column 1 -padx 2
       grid $p.orient.a1label   -row 0 -column 2
       grid $p.orient.a1        -row 0 -column 3 -padx 2
       grid $p.orient.hkl2label -row 0 -column 4
       grid $p.orient.hkl2      -row 0 -column 5 -padx 2
    } else {
       pack $p.orient.hkl1label $p.orient.hkl1 $p.orient.a1label \
	    $p.orient.a1 $p.orient.hkl2label $p.orient.hkl2 -side left
    }

    label $p.templabel -text "Temp/H:"
    frame $p.temp
    label $p.temp.t0label -text "T0:"
    entry $p.temp.t0 -textvariable qbufdisp(tmp) -width 6
    label $p.temp.tinclabel -text "Inc-T:"
    entry $p.temp.tinc -textvariable qbufdisp(it) -width 6
    label $p.temp.twlabel -text "Wait:"
    entry $p.temp.tw -textvariable qbufdisp(tw) -width 6
    label $p.temp.terlabel -text "Err:"
    entry $p.temp.ter -textvariable qbufdisp(ter) -width 6
    label $p.temp.th0label -text "Hld0:"
    entry $p.temp.th0 -textvariable qbufdisp(th0) -width 6
    label $p.temp.thlabel -text "Hld:"
    entry $p.temp.th -textvariable qbufdisp(th) -width 6
    button $p.temp.field -text FIELD -command QBufFieldDialog
    if $text_only {
       $p.temp.t0   configure -width 4
       $p.temp.tinc configure -width 4
       $p.temp.tw   configure -width 4
       $p.temp.ter  configure -width 4
       $p.temp.th0  configure -width 4
       $p.temp.th   configure -width 4
       grid $p.temp.t0label   -row 0 -column 0
       grid $p.temp.t0        -row 0 -column 1  -padx 1
       grid $p.temp.tinclabel -row 0 -column 2
       grid $p.temp.tinc      -row 0 -column 3  -padx 1
       grid $p.temp.twlabel   -row 0 -column 4
       grid $p.temp.tw        -row 0 -column 5  -padx 1
       grid $p.temp.terlabel  -row 0 -column 6
       grid $p.temp.ter       -row 0 -column 7  -padx 1
       grid $p.temp.th0label  -row 0 -column 8
       grid $p.temp.th0       -row 0 -column 9  -padx 1
       grid $p.temp.thlabel   -row 0 -column 10
       grid $p.temp.th        -row 0 -column 11 -padx 1
       grid $p.temp.field     -row 0 -column 12 
    } else {
       pack $p.temp.t0label $p.temp.t0 $p.temp.tinclabel $p.temp.tinc \
	    $p.temp.twlabel $p.temp.tw $p.temp.terlabel $p.temp.ter \
	    $p.temp.th0label $p.temp.th0 $p.temp.thlabel $p.temp.th \
	    $p.temp.field -side left
    }

    label $p.timelabel -text "Time:"
    frame $p.time 
    label $p.time.monlabel -text "Monitor:"
    entry $p.time.mon -textvariable qbufdisp(mon) -width 6
    label $p.time.mpflabel -text "Prefac:"
    entry $p.time.mpf -textvariable qbufdisp(mpf) -width 6
    label $p.time.mtlabel -text "M-typ:"
    if {!$text_only} {
       tk_optionMenu $p.time.mt qbufdisp(mt) TIME NEUT
    } else {
       ck_optionMenu $p.time.mt qbufdisp(mt) TIME NEUT
    }
    button $p.time.pol -text "POLARIZE" -command QBufPolDialogShow \
            -state $config(pola_state)


    label $p.time.nptslabel -text "NPTS:"
    entry $p.time.npts -textvariable qbufdisp(pts) -width 6
    if $text_only {
       grid $p.time.monlabel  -row 0 -column 0
       grid $p.time.mon       -row 0 -column 1 -padx 2
       grid $p.time.mpflabel  -row 0 -column 2
       grid $p.time.mpf       -row 0 -column 3 -padx 2
       grid $p.time.mtlabel   -row 0 -column 4
       grid $p.time.mt        -row 0 -column 5 -padx 1
       grid $p.time.pol       -row 0 -column 6
       grid $p.time.nptslabel -row 0 -column 7
       grid $p.time.npts      -row 0 -column 8 -padx 2
    } else {
       pack $p.time.monlabel $p.time.mon $p.time.mpflabel $p.time.mpf \
	    $p.time.mtlabel $p.time.mt $p.time.pol $p.time.nptslabel \
	    $p.time.npts \
	    -side left
    }

    # Pack widgets into parent
    if $text_only {
       grid $p.bufnolabel -row 0 -column 0 -sticky w
       grid $p.commentlabel -row 0 -column 2 -sticky e
       grid $p.comment -row 0 -column 3 -columnspan 6 -sticky ew -padx 3
       grid $p.latlabel -row 1 -column 0 -sticky w
       grid $p.alabel   -row 1 -column 1  -padx 1
       grid $p.a        -row 1 -column 2
       grid $p.blabel   -row 1 -column 3  -padx 1
       grid $p.b        -row 1 -column 4
       grid $p.clabel   -row 1 -column 5  -padx 1
       grid $p.c        -row 1 -column 6
       grid $p.aalabel  -row 1 -column 7  -padx 1
       grid $p.aa       -row 1 -column 8
       grid $p.bblabel  -row 1 -column 9  -padx 1
       grid $p.bb       -row 1 -column 10
       grid $p.cclabel  -row 1 -column 11 -padx 1
       grid $p.cc       -row 1 -column 12
       grid $p.energylabel $p.eclabel $p.ec $p.eslabel $p.es $p.eflabel $p.ef \
            $p.eftlabel $p.eft -sticky w
       grid $p.spacelabel $p.hclabel $p.hc $p.kclabel $p.kc $p.lclabel $p.lc \
            $p.hslabel $p.hs $p.kslabel $p.ks $p.lslabel $p.ls -sticky w
       grid $p.orientlabel -row 4 -column 0 -sticky w
       grid $p.orient      -row 4 -column 1 -columnspan 12 -sticky w
       label $p.blank -text " "
       grid $p.blank -row 5 -column 0
       grid $p.templabel   -row 6 -column 0 -sticky w
       grid $p.temp        -row 6 -column 1 -columnspan 12 -sticky w
       grid $p.timelabel   -row 7 -column 0 -sticky w
       grid $p.time        -row 7 -column 1 -columnspan 12 -sticky w
    } else {
       grid $p.bufnolabel $p.commentlabel 
       grid $p.comment     -row 0 -column 2 -columnspan 6 -sticky ew
       grid $p.latlabel $p.alabel $p.a $p.blabel $p.b $p.clabel $p.c \
	    $p.aalabel $p.aa $p.bblabel $p.bb $p.cclabel $p.cc 

       grid $p.energylabel $p.eclabel $p.ec $p.eslabel $p.es $p.eflabel $p.ef \
	    $p.eftlabel $p.eft

       grid $p.spacelabel $p.hclabel $p.hc $p.kclabel $p.kc $p.lclabel $p.lc \
	    $p.hslabel $p.hs $p.kslabel $p.ks $p.lslabel $p.ls 

       grid $p.orientlabel $p.orient - - - - - - 
       grid $p.templabel   $p.temp   - - - - - - 
       grid $p.timelabel   $p.time   - - - - - - 
    }

    # Set up widget bindings
    bind $p.orient.hkl1 <FocusOut> "QBufHKL 1"
    bind $p.orient.hkl1 <Return> "QBufHKL 1"
    bind $p.orient.hkl2 <FocusOut> "QBufHKL 2"
    bind $p.orient.hkl2 <Return> "QBufHKL 2"

    return $p
}

proc QBufEntryMod { } {
    global qbufdisp config text_only

    set parent $config(qbufentry)
    if {$config(mode) == 0} {
	set state disabled
        if {!$text_only} {
           set color gray
        } else {
           set color white
        }
    } else {
	set state normal
        if $text_only {
           set color white
        } else {
           set color black
        }
    }
    foreach w "$parent.templabel [winfo children $parent.temp]" {
	switch [winfo class $w] {
	    Label {$w config -fg $color}
	    Entry {$w config -fg $color -state $state}
	}
    }

    if {$config(mode) == 2} {
	set state normal
        if $text_only {
           set color white
        } else {
           set color black
        }
    } else {
	set state disabled
        if {!$text_only} {
           set color gray
        } else {
           set color white
        }
    }

    foreach l {temp.tinclabel temp.thlabel} {
	$parent.$l config -fg $color
    }
    foreach e {temp.tinc temp.th} {
	$parent.$e config -state $state -fg $color
    }
}


proc QBufInit { } {
    global qbuf 
    array set qbuf {buftype Q}
    for {set i 0} {$i < 30} {incr i} {
	set qbuf($i,comment) "empty"
	set qbuf($i,a)     6.283
	set qbuf($i,b)     0.0
	set qbuf($i,c)     0.0
	set qbuf($i,aa)    90.0
	set qbuf($i,bb)    0.0
	set qbuf($i,cc)    0.0
	set qbuf($i,ec)    0.0
	set qbuf($i,es)    0.0
	set qbuf($i,ef)    14.70
	set qbuf($i,eft)   ANA
	set qbuf($i,hc)    1.0
	set qbuf($i,kc)    0.0
	set qbuf($i,lc)    0.0
	set qbuf($i,hs)    0.1
	set qbuf($i,ks)    0.0
	set qbuf($i,ls)    0.0
	set qbuf($i,hkl11) 1
	set qbuf($i,hkl12) 0
	set qbuf($i,hkl13) 0
	set qbuf($i,a1)    0.0
	set qbuf($i,hkl21) 0
	set qbuf($i,hkl22) 0
	set qbuf($i,hkl23) 1
	set qbuf($i,hkl1)  " 1, 0, 0"
	set qbuf($i,hkl2)  " 0, 0, 1"
	set qbuf($i,tmp)   0
	set qbuf($i,it)    0
	set qbuf($i,tw)    0
	set qbuf($i,ter)   0
	set qbuf($i,th0)   0
	set qbuf($i,th)    0
	set qbuf($i,mon)   1000
	set qbuf($i,mpf)   1
	set qbuf($i,mt)    TIME
	set qbuf($i,pts)   0
	set qbuf($i,flip1) 0
	set qbuf($i,flip2) 0
	set qbuf($i,flip3) 0
	set qbuf($i,flip4) 0
	set qbuf($i,hf)    0
	set qbuf($i,ihf)   0
	set qbuf($i,hfw)   0
	set qbuf($i,hfh)   0
    }
    set qbuf(currentrec)   0
    set qbuf(mtime)        0
}

proc QBufReadRec { file rec } {
    upvar $rec r
    # First seek to correct location
    set input [read $file 50]
    binary scan $input A50 r(comment)
    set input [read $file 2]
    set input [read $file 36]
    binary scan $input fffffffff r(a) r(b) r(c) r(aa) r(bb) r(cc) r(ec) r(es) r(ef) 
    set input [read $file 4]
    binary scan $input A3 r(eft)
    set input [read $file 4]
    binary scan $input f r(hc) 
    set input [read $file 84]
    binary scan $input fffffffffffffffffffff \
	    r(kc) r(lc) r(hs) r(ks) r(ls) r(pts) \
	    r(hkl11) r(hkl12) r(hkl13) r(a1) r(hkl21) r(hkl22) r(hkl23) \
	    r(tmp) r(it) r(tw) r(th0) r(ter) \
	    r(hf) r(mon) r(mpf) 
    set input [read $file 36]
    binary scan $input A4iiiiffff \
	    r(mt) \
	    r(flip1) r(flip2) r(flip3) r(flip4) \
	    r(ihf) r(hfw) r(hfh) r(th)
#    binary scan $input \
#	    A50sfffffffffA3cffffffffffffffffffffffA4iiiiffff \
#	    r(comment) pad1 \
#	    r(a) r(b) r(c) r(aa) r(bb) r(cc) r(ec) r(es) r(ef) r(eft) pad2 \
#	    r(hc) r(kc) r(lc) r(hs) r(ks) r(ls) r(pts) \
#	    r(hkl11) r(hkl12) r(hkl13) r(a1) r(hkl21) r(hkl22) r(hkl23) \
#	    r(tmp) r(it) r(tw) r(th0) r(ter) \
#	    r(hf) r(mon) r(mpf) r(mt) \
#	    r(flip1) r(flip2) r(flip3) r(flip4) \
#	    r(ihf) r(hfw) r(hfh) r(th)
}

proc QBufReadFile { } {
    global qbuf qbuf_fields

    set f [open QBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	seek $f [expr $i * 320]
	QBufReadRec $f icpin
	foreach fld $qbuf_fields {
	    if {![catch {set icpin($fld)} result]} {
		set qbuf($i,$fld) $result
	    }
	}
    }
    close $f
    set qbuf(mtime) [file mtime QBUFFER.BUF]
}

proc QBufCheckFld { } {
    global qbufdisp 
    set flds { a b c aa bb cc ec es ef hc kc lc hs ks ls pts \
	    hkl11 hkl12 hkl13 a1 hkl21 hkl22 hkl23 \
	    tmp it tw th0 ter hf mon mpf ihf hfw hfh th }
    foreach fld $flds {
	# Check to make sure these are floating point numbers 
	if [catch { expr 1.0 * $qbufdisp($fld) } result] {
	    set qbufdisp($fld) "--------"
	    return -code error "Parameter \"$fld\" must be floating point number"
	}
    }
}
 
proc QBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set pad2 0
    set recval [binary format \
	    A50sfffffffffA3cffffffffffffffffffffffA4iiiiffff \
	    $r(comment) $pad1 \
	    $r(a) $r(b) $r(c) $r(aa) $r(bb) $r(cc) $r(ec) \
	    $r(es) $r(ef) $r(eft) $pad2 \
	    $r(hc) $r(kc) $r(lc) $r(hs) $r(ks) $r(ls) $r(pts) \
	    $r(hkl11) $r(hkl12) $r(hkl13) $r(a1) \
	    $r(hkl21) $r(hkl22) $r(hkl23) \
	    $r(tmp) $r(it) $r(tw) $r(th0) $r(ter) \
	    $r(hf) $r(mon) $r(mpf) $r(mt) \
	    $r(flip1) $r(flip2) $r(flip3) $r(flip4) \
	    $r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A320 $recval]
    puts -nonewline $file $record
    string length $record
}

proc QBufWriteFile { } {
    global qbuf qbuf_fields

    set f [open QBUFFER.BUF w]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $qbuf_fields {
	    set icpout($fld) $qbuf($i,$fld)
	}
	QBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A320 "\#"]
    close $f
    set qbuf(mtime) [file mtime QBUFFER.BUF]
}

proc QBufList { } {
    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
	$lbox insert end [QBufFormatRec $i]
    }
    global text_only
    if $text_only {
       $lbox selection set 0 0
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "QBufDisp \[$lbox curselection\]; set changed(qbufdisp) 0"
    if $text_only {
       set bind_type <Linefeed>
    } else {
       set bind_type <Return>
    }
    bind $lbox $bind_type "QBufDisp \[$lbox curselection\]; set changed(qbufdisp) 0"
}

proc QBufFormatRec { recno } {
    global qbuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $qbuf($recno,comment)]
    append output " "
    append output [format "%6.2f " $qbuf($recno,hc)]
    append output [format "%6.2f " $qbuf($recno,kc)]
    append output [format "%6.2f " $qbuf($recno,lc)]
    append output [format "%4.2f " $qbuf($recno,ec)]
    append output [format "%4.2f " $qbuf($recno,es)]
    append output [format "%2.0f\* " $qbuf($recno,mpf)]
    append output [format "%6.0f " $qbuf($recno,mon)]
    append output [binary format A4 $qbuf($recno,mt)]
    return $output
}

proc QBufDisp { recno } {
    global qbufdisp qbuf qbuf_fields config changed

    set num [format "%2d" [expr $recno + 1]]
    set qbufdisp(label) "QBuffer No: $num"
    set qbufdisp(current) $recno
    set mtime [file mtime QBUFFER.BUF]
    if {$mtime != $qbuf(mtime)} {
	QBufReadFile
	QBufList
    }
    if {$config(paranoid)} { CheckBufChange }

    foreach fld $qbuf_fields {
	set qbufdisp($fld) $qbuf($recno,$fld)
    }

    if [catch {expr $qbufdisp(pts) * 1} qbufdisp(pts)] {
	set qbufdisp(pts) 1
    }

    # Trim floating point numbers to sensible decimal places
    set flfields { a b c aa bb cc ec es ef hc kc lc hs ks ls pts \
	    hkl11 hkl12 hkl13 a1 hkl21 hkl22 hkl23 }
    foreach f $flfields {
	set qbufdisp($f) [fltrim $qbufdisp($f) 10000]
    }

    set qbufdisp(hkl1) [format "%2d,%2d,%2d" [expr int($qbufdisp(hkl11))] \
	    [expr int($qbufdisp(hkl12))] [expr int($qbufdisp(hkl13))]]

    set qbufdisp(hkl2) [format "%2d,%2d,%2d" [expr int($qbufdisp(hkl21))] \
	    [expr int($qbufdisp(hkl22))] [expr int($qbufdisp(hkl23))]]

    # clear changed flag
    set changed(qbufdisp) 0
}

proc QBufUpdate { } {
    global qbufdisp qbuf changed

    if [catch {QBufCheckFld} result] {
	return -code error $result
    }

    set recno $qbufdisp(current)
    foreach fld [array names qbufdisp] {
	set qbuf($recno,$fld) $qbufdisp($fld)
    }
    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [QBufFormatRec $recno]
    # update buffer information on disk
    QBufWriteFile

}

proc QBufFieldDialog { } {
    global qbufdisp qbuf text_only

    if [winfo exists .field_q] {
	wm deiconify .field_q
	return .field_q
    }


    toplevel .field_q

    label .field_q.title -text "MAGNETIC FIELD"
    label .field_q.initlab -text "Initial Field (user units):"
    label .field_q.incrlab -text "Field Increment (user units):"
    label .field_q.waitlab -text "Max Wait Per Point (secs) :"
    entry .field_q.initentry -textvariable qbufdisp(hf)  -width 10
    entry .field_q.increntry -textvariable qbufdisp(ihf) -width 10
    entry .field_q.waitentry -textvariable qbufdisp(hfw) -width 10
    label .field_q.t1 -text "(Wait used in non-persistent mode on SC only)"
    label .field_q.t2 -text "(Reply 0 to get default optimum wait times)"
    button .field_q.dismiss -text "Dismiss" -command QBufFieldDialogDismiss
    if $text_only {
       grid .field_q.title     -row 0 -column 0 -columnspan 2 -sticky news
       label .field_q.blank1 -text " "
       grid .field_q.blank1    -row 1
       grid .field_q.initlab   -row 2 -column 0 -sticky w
       grid .field_q.incrlab   -row 3 -column 0 -sticky w
       grid .field_q.waitlab   -row 4 -column 0 -sticky w
       grid .field_q.initentry -row 2 -column 1
       grid .field_q.increntry -row 3 -column 1
       grid .field_q.waitentry -row 4 -column 1
       label .field_q.blank2 -text " "
       grid .field_q.blank2    -row 5
       grid .field_q.t1        -row 6 -column 0 -columnspan 2 -sticky ew
       grid .field_q.t2        -row 7 -column 0 -columnspan 2 -sticky ew
       label .field_q.blank3 -text " "
       grid .field_q.blank3    -row 8
       grid .field_q.dismiss   -row 9 -column 0 -columnspan 2 -sticky ew
       place .field_q -relheight 1.0 -relwidth 1.0
       focus .field_q
    } else {
       wm protocol .field_q WM_DELETE_WINDOW ".field_q.dismiss invoke"
       grid .field_q.title     -row 0 -column 0 -columnspan 2 -sticky news
       grid .field_q.initlab   -row 1 -column 0
       grid .field_q.incrlab   -row 2 -column 0
       grid .field_q.waitlab   -row 3 -column 0
       grid .field_q.initentry -row 1 -column 1
       grid .field_q.increntry -row 2 -column 1
       grid .field_q.waitentry -row 3 -column 1
       grid .field_q.t1        -row 4 -column 0 -columnspan 2 -sticky ew
       grid .field_q.t2        -row 5 -column 0 -columnspan 2 -sticky ew
       grid .field_q.dismiss   -row 6 -column 0 -columnspan 2 -sticky ew
    }
    return .field_q
}

proc QBufFieldDialogDismiss { } {
    global text_only
    wm withdraw .field_q
}

proc QBufPolDialogBuild { } {
    global qbufdisp text_only
    set p [toplevel .polarize_q]
    label $p.title -text "Polarization Setup"
    set f [frame $p.f]
    set d [button $p.dismiss -text "Dismiss" -command QBufPolDialogHide]

    label $f.flip1t -text "Flipper 1"  
    label $f.flip2t -text "Flipper 2"  
    label $f.flipet -text "Execute ?"  
    frame $f.flip1
    frame $f.flip2
    frame $f.flipe
    label $f.flip1.a -text "OFF" 
    label $f.flip1.b -text "ON"  
    label $f.flip1.c -text "OFF" 
    label $f.flip1.d -text "ON"  
    label $f.flip2.a -text "OFF" 
    label $f.flip2.b -text "OFF" 
    label $f.flip2.c -text "ON"  
    label $f.flip2.d -text "ON"  
    #toggle_create $f.flipe.a qbufdisp(flip1)
    #toggle_create $f.flipe.b qbufdisp(flip2)
    #toggle_create $f.flipe.c qbufdisp(flip3)
    #toggle_create $f.flipe.d qbufdisp(flip4)
    tk_optionMenu $f.flipe.a qbufdisp(flip1) 0 1
    tk_optionMenu $f.flipe.b qbufdisp(flip2) 0 1
    tk_optionMenu $f.flipe.c qbufdisp(flip3) 0 1
    tk_optionMenu $f.flipe.d qbufdisp(flip4) 0 1

    if {!$text_only} {
	$f.flip1t configure -relief ridge
	$f.flip2t configure -relief ridge
	$f.flipet configure -relief ridge
	$f.flip1  configure -relief ridge -borderwidth 2
	$f.flip2  configure -relief ridge -borderwidth 2
	$f.flipe  configure -relief ridge -borderwidth 2
	$f.flip1.a configure -pady 5
	$f.flip1.b configure -pady 5
	$f.flip1.c configure -pady 5
	$f.flip1.d configure -pady 5
	$f.flip2.a configure -pady 5
	$f.flip2.b configure -pady 5
	$f.flip2.c configure -pady 5
	$f.flip2.d configure -pady 5
    }

    pack $p.title $f $d -side top -fill x
    grid $f.flip1t $f.flip2t $f.flipet -sticky ew
    grid $f.flip1 $f.flip2 $f.flipe -sticky ew
    pack $f.flip1.a $f.flip1.b $f.flip1.c $f.flip1.d -side top
    pack $f.flip2.a $f.flip2.b $f.flip2.c $f.flip2.d -side top
    pack $f.flipe.a $f.flipe.b $f.flipe.c $f.flipe.d -side top -fill x
    return $p
}

proc QBufPolDialogHide { } {
    global text_only 
    if $text_only {
	destroy .polarize_q
    } else {
	wm withdraw .polarize_q
    }
}

proc QBufPolDialogShow { } {
    global text_only

    if $text_only {
       catch { destroy .polarize_q }
       QBufPolDialogBuild
       place .polarize_q -relheight 1.0 -relwidth 1.0
       focus .polarize_q
    } else {
	if [winfo exists .polarize_q] {
	    wm deiconify .polarize_q
	} else {
	    QBufPolDialogBuild
	}
    }
}

proc QBufHKL { which args } {
    global qbufdisp changed
    if {$which == 1} {
	set no 1
    } else {
	set no 2
    }
    set key "hkl$no"
    if {[catch {scan $qbufdisp($key) "%d,%d,%d" h k l} result]} {
	set qbufdisp($key) "-------"
	return
    }

    # Write string back out again
    if {$which == 1} {
	if {$result == 3} {
	    set qbufdisp(hkl11) $h
	    set qbufdisp(hkl12) $k
	    set qbufdisp(hkl13) $l
	}
	set qbufdisp(hkl1) [format "%2d,%2d,%2d" [expr int($qbufdisp(hkl11))] \
		[expr int($qbufdisp(hkl12))] [expr int($qbufdisp(hkl13))]]
    } else {
	if {$result == 3} {
	    set qbufdisp(hkl21) $h
	    set qbufdisp(hkl22) $k
	    set qbufdisp(hkl23) $l
	}
	set qbufdisp(hkl2) [format "%2d,%2d,%2d" [expr int($qbufdisp(hkl21))] \
		[expr int($qbufdisp(hkl22))] [expr int($qbufdisp(hkl23))]]
    }
    set changed(qbufdisp) 0
}

#=B BUFFER=====================================================================

proc BBufEntryCreate { p } {
    global bbuf bbufdisp config text_only

    # Create widgets
    label $p.bufnolabel -textvariable bbufdisp(label)
    label $p.commentlabel -text "Comment:"
    entry $p.comment -textvariable bbufdisp(comment)


    label $p.latlabel -text "Lattice:"
    label $p.alabel -text "a:"
    entry $p.a -textvariable bbufdisp(a) -width 6
    label $p.blabel -text "b:"
    entry $p.b -textvariable bbufdisp(b) -width 6
    label $p.clabel -text "c:"
    entry $p.c -textvariable bbufdisp(c) -width 6
    label $p.aalabel -text "aa:"
    entry $p.aa -textvariable bbufdisp(aa) -width 6
    label $p.bblabel -text "bb:"
    entry $p.bb -textvariable bbufdisp(bb) -width 6
    label $p.cclabel -text "cc:"
    entry $p.cc -textvariable bbufdisp(cc) -width 6

    label $p.energylabel -text "Energy:"
    label $p.eclabel -text "EC:"
    label $p.ec -textvariable bbufdisp(ec) -width 6
    label $p.eslabel -text "ES:"
    entry $p.es -textvariable bbufdisp(es) -width 6
    label $p.eflabel -text "EF:"
    entry $p.ef -textvariable bbufdisp(ef) -width 6
    label $p.eftlabel -text "EFT:"
    if {!$text_only} {
       tk_optionMenu $p.eft bbufdisp(eft) ANA MON
    } else {
       ck_optionMenu $p.eft bbufdisp(eft) ANA MON
    }

    label $p.spacelabel -text "Space:"
    label $p.hclabel -text "HC:"
    entry $p.hc -textvariable bbufdisp(hc) -width 6
    label $p.kclabel -text "KC:"
    entry $p.kc -textvariable bbufdisp(kc) -width 6
    label $p.lclabel -text "LC:"
    entry $p.lc -textvariable bbufdisp(lc) -width 6
    label $p.hslabel -text "HS:"
    entry $p.hs -textvariable bbufdisp(hs) -width 6
    label $p.kslabel -text "KS:"
    entry $p.ks -textvariable bbufdisp(ks) -width 6
    label $p.lslabel -text "LS:"
    entry $p.ls -textvariable bbufdisp(ls) -width 6

    label $p.orientlabel -text "Orient:"
    frame $p.orient
    label $p.orient.hkl1label -text "(h k l):"
    entry $p.orient.hkl1 -textvariable bbufdisp(hkl1) -width 6
    label $p.orient.a1label -text "Angle:"
    entry $p.orient.a1 -textvariable bbufdisp(a1) -width 6
    label $p.orient.hkl2label -text "(h k l)':"
    entry $p.orient.hkl2 -textvariable bbufdisp(hkl2) -width 6
    pack $p.orient.hkl1label $p.orient.hkl1 $p.orient.a1label \
	    $p.orient.a1 $p.orient.hkl2label $p.orient.hkl2 -side left

    label $p.templabel -text "Temp/H:"
    frame $p.temp
    label $p.temp.t0label -text "T0:"
    entry $p.temp.t0 -textvariable bbufdisp(tmp) -width 6
    label $p.temp.tinclabel -text "Inc-T:"
    entry $p.temp.tinc -textvariable bbufdisp(it) -width 6
    label $p.temp.twlabel -text "Wait:"
    entry $p.temp.tw -textvariable bbufdisp(tw) -width 6
    label $p.temp.terlabel -text "Err:"
    entry $p.temp.ter -textvariable bbufdisp(ter) -width 6
    label $p.temp.th0label -text "Hld0:"
    entry $p.temp.th0 -textvariable bbufdisp(th0) -width 6
    button $p.temp.field -text FIELD -command BBufFieldDialog
    pack $p.temp.t0label $p.temp.t0 $p.temp.tinclabel $p.temp.tinc \
	    $p.temp.twlabel $p.temp.tw $p.temp.terlabel $p.temp.ter \
	    $p.temp.th0label $p.temp.th0 $p.temp.field -side left

    label $p.timelabel -text "Time:"
    frame $p.time 
    label $p.time.monlabel -text "Monitor:"
    entry $p.time.mon -textvariable bbufdisp(mon) -width 6
    label $p.time.mpflabel -text "Prefac:"
    entry $p.time.mpf -textvariable bbufdisp(mpf) -width 6
    label $p.time.mtlabel -text "M-typ:"
    if {!$text_only} {
       tk_optionMenu $p.time.mt bbufdisp(mt) TIME NEUT
    } else {
       ck_optionMenu $p.time.mt bbufdisp(mt) TIME NEUT
    }
    button $p.time.pol -text "POLARIZE" -command BBufPolDialogShow \
            -state $config(pola_state)

    pack $p.time.monlabel $p.time.mon $p.time.mpflabel $p.time.mpf \
	    $p.time.mtlabel $p.time.mt $p.time.pol -side left

    label $p.anglabel -text "Angles:"
    label $p.i3lab    -text "Inc-3:"
    entry $p.i3       -textvariable bbufdisp(i_3) -width 6
    label $p.i4lab    -text "Inc-4:"
    entry $p.i4       -textvariable bbufdisp(i_4) -width 6
    label $p.r3lab    -text "A3-range:"
    entry $p.r3       -textvariable bbufdisp(r_3) -width 6
    label $p.r4lab    -text "A4-range:"
    entry $p.r4       -textvariable bbufdisp(r_4) -width 6   

    label $p.nptslabel -text "NPTS:"
    entry $p.npts -textvariable bbufdisp(pts) -width 6


    # Pack widgets into parent
    grid $p.bufnolabel $p.commentlabel 
    grid $p.comment     -row 0 -column 2 -columnspan 6 -sticky ew
    grid $p.latlabel $p.alabel $p.a $p.blabel $p.b $p.clabel $p.c \
	    $p.aalabel $p.aa $p.bblabel $p.bb $p.cclabel $p.cc 

    grid $p.energylabel $p.eclabel $p.ec $p.eslabel $p.es $p.eflabel $p.ef \
	    $p.eftlabel $p.eft

    grid $p.spacelabel $p.hclabel $p.hc $p.kclabel $p.kc $p.lclabel $p.lc \
	    $p.hslabel $p.hs $p.kslabel $p.ks $p.lslabel $p.ls 

    grid $p.orientlabel $p.orient - - - - - - 
    grid $p.templabel   $p.temp   - - - - - - 
    grid $p.timelabel   $p.time   - - - - - - 

    grid $p.anglabel   $p.i3lab $p.i3 $p.r3lab $p.r3 $p.nptslabel $p.npts
    grid x             $p.i4lab $p.i4 $p.r4lab $p.r4

    # Set up widget bindings
    bind $p.orient.hkl1 <FocusOut> "BBufHKL 1"
    bind $p.orient.hkl1 <Return> "BBufHKL 1"
    bind $p.orient.hkl2 <FocusOut> "BBufHKL 2"
    bind $p.orient.hkl2 <Return> "BBufHKL 2"

    bind $p.i3 <Return>   "BBufAngleBinding bbufdisp i_3 w"
    bind $p.i3 <FocusOut> "BBufAngleBinding bbufdisp i_3 w"
    bind $p.i4 <Return>   "BBufAngleBinding bbufdisp i_4 w"
    bind $p.i4 <FocusOut> "BBufAngleBinding bbufdisp i_4 w"
    bind $p.r3 <Return>   "BBufAngleBinding bbufdisp r_3 w"
    bind $p.r3 <FocusOut> "BBufAngleBinding bbufdisp r_3 w"
    bind $p.r4 <Return>   "BBufAngleBinding bbufdisp r_4 w"
    bind $p.r4 <FocusOut> "BBufAngleBinding bbufdisp r_4 w"

    bind $p.npts <Return>   "BBufCalcRange"
    bind $p.npts <FocusOut> "BBufCalcRange"

    return $p
}

proc BBufInit { } {
    global bbuf 
    array set bbuf {buftype Bragg}
    for {set i 0} {$i < 30} {incr i} {
	set bbuf($i,comment) "empty"
	set bbuf($i,a)     5.000
	set bbuf($i,b)     5.000
	set bbuf($i,c)     5.000
	set bbuf($i,aa)    90.00
	set bbuf($i,bb)    90.00
	set bbuf($i,cc)    90.00
	set bbuf($i,ec)    0.0
	set bbuf($i,es)    0.0
	set bbuf($i,ef)    14.70
	set bbuf($i,eft)   ANA
	set bbuf($i,hc)    1.0
	set bbuf($i,kc)    0.0
	set bbuf($i,lc)    0.0
	set bbuf($i,hs)    0.1
	set bbuf($i,ks)    0.0
	set bbuf($i,ls)    0.0
	set bbuf($i,hkl11) 1
	set bbuf($i,hkl12) 0
	set bbuf($i,hkl13) 0
	set bbuf($i,a1)    0.0
	set bbuf($i,hkl21) 0
	set bbuf($i,hkl22) 0
	set bbuf($i,hkl23) 1
	set bbuf($i,hkl1)  " 1, 0, 0"
	set bbuf($i,hkl2)  " 0, 0, 1"
	set bbuf($i,tmp)   0
	set bbuf($i,it)    0
	set bbuf($i,tw)    0
	set bbuf($i,ter)   0
	set bbuf($i,th0)   0
	set bbuf($i,th)    0
	set bbuf($i,mon)   1000
	set bbuf($i,mpf)   1
	set bbuf($i,mt)    TIME
	set bbuf($i,pts)   1
	set bbuf($i,flip1) 0
	set bbuf($i,flip2) 0
	set bbuf($i,flip3) 0
	set bbuf($i,flip4) 0
	set bbuf($i,a_1)   0.0
	set bbuf($i,a_2)   0.0
	set bbuf($i,a_3)   0.0
	set bbuf($i,a_4)   0.0
	set bbuf($i,a_5)   0.0
	set bbuf($i,a_6)   0.0
	set bbuf($i,i_1)   0.0
	set bbuf($i,i_2)   0.0
	set bbuf($i,i_3)   0.0
	set bbuf($i,i_4)   0.0
	set bbuf($i,i_5)   0.0
	set bbuf($i,i_6)   0.0
	set bbuf($i,r_3)   0.0
	set bbuf($i,r_4)   0.0
	set bbuf($i,hf)    0
	set bbuf($i,ihf)   0
	set bbuf($i,hfw)   0
	set bbuf($i,hfh)   0
    }
    set bbuf(currentrec)   0
    set bbuf(mtime)        0
}

proc BBufReadRec { file rec } {
    upvar $rec r
    set input [read $file 320]
    binary scan $input \
	    A50sfffffffffA3cffffffffffffffffffffffA4iiiiffffffffffffffff \
	    r(comment) pad1 \
	    r(a) r(b) r(c) r(aa) r(bb) r(cc) r(ec) r(es) r(ef) r(eft) pad2 \
	    r(hc) r(kc) r(lc) r(hs) r(ks) r(ls) r(pts) \
	    r(hkl11) r(hkl12) r(hkl13) r(a1) r(hkl21) r(hkl22) r(hkl23) \
	    r(tmp) r(it) r(tw) r(th0) r(ter) \
	    r(hf) r(mon) r(mpf) r(mt) \
	    r(flip1) r(flip2) r(flip3) r(flip4) \
	    r(a_1) r(a_2) r(a_3) r(a_4) r(a_5) r(a_6) \
	    r(i_1) r(i_2) r(i_3) r(i_4) r(i_5) r(i_6) \
	    r(ihf) r(hfw) r(hfh) r(th)
}

proc BBufReadFile { } {
    global bbuf bbuf_fields

    set f [open BBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	BBufReadRec $f icpin
	foreach fld $bbuf_fields {
	    if {![catch {set icpin($fld)} result]} {
		set bbuf($i,$fld) $result
	    }
	}
    }
    close $f
    set bbuf(mtime) [file mtime BBUFFER.BUF]
}
 
proc BBufCheckFld { } {
    global bbufdisp 
    set flds { a b c aa bb cc ec es ef  hc kc lc hs ks ls pts \
	    hkl11 hkl12 hkl13 a1 hkl21 hkl22 hkl23 \
	    tmp it tw th0 ter hf mon mpf \
	    a_1 a_2 a_3 a_4 a_5 a_6 i_1 i_2 i_3 i_4 i_5 i_6 \
	    ihf hfw hfh th }
    foreach fld $flds {
	# Check to make sure these are floating point numbers 
	if [catch { expr 1.0 * $bbufdisp($fld) } result] {
	    set bbufdisp($fld) "--------"
	    return -code error "Parameter \"$fld\" must be floating point number"
	}
    }
}

proc BBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set pad2 0
    set recval [binary format \
	    A50sfffffffffA3cffffffffffffffffffffffA4iiiiffffffffffffffff \
	    $r(comment) $pad1 \
	    $r(a) $r(b) $r(c) $r(aa) $r(bb) $r(cc) $r(ec) \
	    $r(es) $r(ef) $r(eft) $pad2 \
	    $r(hc) $r(kc) $r(lc) $r(hs) $r(ks) $r(ls) $r(pts) \
	    $r(hkl11) $r(hkl12) $r(hkl13) $r(a1) \
	    $r(hkl21) $r(hkl22) $r(hkl23) \
	    $r(tmp) $r(it) $r(tw) $r(th0) $r(ter) \
	    $r(hf) $r(mon) $r(mpf) $r(mt) \
	    $r(flip1) $r(flip2) $r(flip3) $r(flip4) \
	    $r(a_1) $r(a_2) $r(a_3) $r(a_4) $r(a_5) $r(a_6) \
	    $r(i_1) $r(i_2) $r(i_3) $r(i_4) $r(i_5) $r(i_6) \
	    $r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A320 $recval]
    puts -nonewline $file $record
    string length $record
}

proc BBufWriteFile { } {
    global bbuf bbuf_fields

    set f [open BBUFFER.BUF w]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $bbuf_fields {
	    set icpout($fld) $bbuf($i,$fld)
	}
	BBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A320 "\#"]
    close $f
    set bbuf(mtime) [file mtime BBUFFER.BUF]
}

proc BBufList { } {
    global text_only
    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
	$lbox insert end [BBufFormatRec $i]
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "BBufDisp \[$lbox curselection\]"
    if $text_only {
       set bind_type <Linefeed>
    } else {
       set bind_type <Return>
    }
    bind $lbox $bind_type "BBufDisp \[$lbox curselection\]"
}

proc BBufFormatRec { recno } {
    global bbuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $bbuf($recno,comment)]
    append output " "
    append output [format "%6.2f " $bbuf($recno,hc)]
    append output [format "%6.2f " $bbuf($recno,kc)]
    append output [format "%6.2f " $bbuf($recno,lc)]
    append output [format "%4.2f " $bbuf($recno,ec)]
    append output [format "%4.2f " $bbuf($recno,es)]
    append output [format "%2.0f\* " $bbuf($recno,mpf)]
    append output [format "%6.0f " $bbuf($recno,mon)]
    append output [binary format A4 $bbuf($recno,mt)]
    return $output
}

proc BBufDisp { recno } {
    global bbufdisp bbuf bbuf_fields config

    set mtime [file mtime BBUFFER.BUF]
    if {$mtime != $bbuf(mtime)} {
	BBufReadFile
	BBufList
    }

    if {$config(paranoid)} { CheckBufChange }

    foreach fld $bbuf_fields {
	set bbufdisp($fld) $bbuf($recno,$fld)
    }
    set num [format "%2d" [expr $recno + 1]]
    set bbufdisp(label) "BBuffer No: $num"
    set bbufdisp(current) $recno
    if [catch {expr $bbufdisp(pts) * 1} bbufdisp(pts)] {
	set bbufdisp(pts) 1
    }

    set bbufdisp(hkl1) [format "%2d,%2d,%2d" [expr int($bbufdisp(hkl11))] \
	    [expr int($bbufdisp(hkl12))] [expr int($bbufdisp(hkl13))]]

    set bbufdisp(hkl2) [format "%2d,%2d,%2d" [expr int($bbufdisp(hkl21))] \
	    [expr int($bbufdisp(hkl22))] [expr int($bbufdisp(hkl23))]]

    BBufCalcRange

    # clear changed flag
    set changed(bbufdisp) 0
}

proc BBufUpdate { } {
    global bbufdisp bbuf changed

    if [catch {BBufCheckFld} result] {
	return -code error $result
    }

    set recno $bbufdisp(current)
    foreach fld [array names bbufdisp] {
	set bbuf($recno,$fld) $bbufdisp($fld)
    }
    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [BBufFormatRec $recno]
    # update buffer information on disk
    BBufWriteFile

    set changed(bbufdisp) 0
}

proc BBufFieldDialog { } {
    global bbufdisp bbuf text_only

    if [winfo exists .field_b] {
	wm deiconify .field_b
	return .field_b
    }

    toplevel .field_b

    label .field_b.title -text "MAGNETIC FIELD"
    label .field_b.initlab -text "Initial Field (user units):"
    label .field_b.incrlab -text "Field Increment (user units):"
    label .field_b.waitlab -text "Max Wait Per Point (secs) :"
    entry .field_b.initentry -textvariable bbufdisp(hf)  -width 10
    entry .field_b.increntry -textvariable bbufdisp(ihf) -width 10
    entry .field_b.waitentry -textvariable bbufdisp(hfw) -width 10
    label .field_b.t1 -text "(Wait used in non-persistent mode on SC only)"
    label .field_b.t2 -text "(Reply 0 to get default optimum wait times)"
    button .field_b.dismiss -text "Dismiss" -command BBufFieldDialogDismiss
    if $text_only {
       grid .field_b.title     -row 0 -column 0 -columnspan 2 -sticky news
       label .field_b.blank1 -text " "
       grid .field_b.blank1    -row 1
       grid .field_b.initlab   -row 2 -column 0 -sticky w
       grid .field_b.incrlab   -row 3 -column 0 -sticky w
       grid .field_b.waitlab   -row 4 -column 0 -sticky w
       grid .field_b.initentry -row 2 -column 1
       grid .field_b.increntry -row 3 -column 1
       grid .field_b.waitentry -row 4 -column 1
       label .field_b.blank2 -text " "
       grid .field_b.blank2    -row 5
       grid .field_b.t1        -row 6 -column 0 -columnspan 2 -sticky ew
       grid .field_b.t2        -row 7 -column 0 -columnspan 2 -sticky ew
       label .field_b.blank3 -text " "
       grid .field_b.blank3    -row 8
       grid .field_b.dismiss   -row 9 -column 0 -columnspan 2 -sticky ew
       place .field_b -relheight 1.0 -relwidth 1.0
       focus .field_b
    } else {
       wm protocol .field_b WM_DELETE_WINDOW ".field_b.dismiss invoke"
       grid .field_b.title     -row 0 -column 0 -columnspan 2 -sticky news
       grid .field_b.initlab   -row 1 -column 0
       grid .field_b.incrlab   -row 2 -column 0
       grid .field_b.waitlab   -row 3 -column 0
       grid .field_b.initentry -row 1 -column 1
       grid .field_b.increntry -row 2 -column 1
       grid .field_b.waitentry -row 3 -column 1
       grid .field_b.t1        -row 4 -column 0 -columnspan 2 -sticky ew
       grid .field_b.t2        -row 5 -column 0 -columnspan 2 -sticky ew
       grid .field_b.dismiss   -row 6 -column 0 -columnspan 2 -sticky ew
    }
    return .field_b
}

proc BBufFieldDialogDismiss { } {
    global text_only

    wm withdraw .field_b
}

proc BBufPolDialogBuild { } {
    global bbufdisp text_only
    set p [toplevel .polarize_b]
    label $p.title -text "Polarization Setup"
    set f [frame $p.f]
    set d [button $p.dismiss -text "Dismiss" -command BBufPolDialogHide]

    label $f.flip1t -text "Flipper 1"  
    label $f.flip2t -text "Flipper 2"  
    label $f.flipet -text "Execute ?"  
    frame $f.flip1
    frame $f.flip2
    frame $f.flipe
    label $f.flip1.a -text "OFF" 
    label $f.flip1.b -text "ON"  
    label $f.flip1.c -text "OFF" 
    label $f.flip1.d -text "ON"  
    label $f.flip2.a -text "OFF" 
    label $f.flip2.b -text "OFF" 
    label $f.flip2.c -text "ON"  
    label $f.flip2.d -text "ON"  
    #toggle_create $f.flipe.a bbufdisp(flip1)
    #toggle_create $f.flipe.b bbufdisp(flip2)
    #toggle_create $f.flipe.c bbufdisp(flip3)
    #toggle_create $f.flipe.d bbufdisp(flip4)
    tk_optionMenu $f.flipe.a bbufdisp(flip1) 0 1
    tk_optionMenu $f.flipe.b bbufdisp(flip2) 0 1
    tk_optionMenu $f.flipe.c bbufdisp(flip3) 0 1
    tk_optionMenu $f.flipe.d bbufdisp(flip4) 0 1

    if {!$text_only} {
	$f.flip1t configure -relief ridge
	$f.flip2t configure -relief ridge
	$f.flipet configure -relief ridge
	$f.flip1  configure -relief ridge -borderwidth 2
	$f.flip2  configure -relief ridge -borderwidth 2
	$f.flipe  configure -relief ridge -borderwidth 2
	$f.flip1.a configure -pady 5
	$f.flip1.b configure -pady 5
	$f.flip1.c configure -pady 5
	$f.flip1.d configure -pady 5
	$f.flip2.a configure -pady 5
	$f.flip2.b configure -pady 5
	$f.flip2.c configure -pady 5
	$f.flip2.d configure -pady 5
    }

    pack $p.title $f $d -side top -fill x
    grid $f.flip1t $f.flip2t $f.flipet -sticky ew
    grid $f.flip1 $f.flip2 $f.flipe -sticky ew
    pack $f.flip1.a $f.flip1.b $f.flip1.c $f.flip1.d -side top
    pack $f.flip2.a $f.flip2.b $f.flip2.c $f.flip2.d -side top
    pack $f.flipe.a $f.flipe.b $f.flipe.c $f.flipe.d -side top -fill x
    return $p
}

proc BBufPolDialogHide { } {
    global text_only 
    if $text_only {
	destroy .polarize_b
    } else {
	wm withdraw .polarize_b
    }
}

proc BBufPolDialogShow { } {
    global text_only

    if $text_only {
       catch { destroy .polarize_b }
       BBufPolDialogBuild
       place .polarize_b -relheight 1.0 -relwidth 1.0
       focus .polarize_b
    } else {
	if [winfo exists .polarize_b] {
	    wm deiconify .polarize_b
	} else {
	    BBufPolDialogBuild
	}
    }
}

proc BBufHKL { which args } {
    global bbufdisp
    if {$which == 1} {
	set no 1
    } else {
	set no 2
    }
    set key "hkl$no"
    if {[catch {scan $bbufdisp($key) "%d,%d,%d" h k l} result]} {
	set bbufdisp($key) "-------"
	return
    }

    # Write string back out again
    if {$which == 1} {
	if {$result == 3} {
	    set bbufdisp(hkl11) $h
	    set bbufdisp(hkl12) $k
	    set bbufdisp(hkl13) $l
	}
	set bbufdisp(hkl1) [format "%2d,%2d,%2d" [expr int($bbufdisp(hkl11))] \
		[expr int($bbufdisp(hkl12))] [expr int($bbufdisp(hkl13))]]
    } else {
	if {$result == 3} {
	    set bbufdisp(hkl21) $h
	    set bbufdisp(hkl22) $k
	    set bbufdisp(hkl23) $l
	}
	set bbufdisp(hkl2) [format "%2d,%2d,%2d" [expr int($bbufdisp(hkl21))] \
		[expr int($bbufdisp(hkl22))] [expr int($bbufdisp(hkl23))]]
    }
}

proc BBufCalcRange { args } {
    global bbufdisp

    if {$bbufdisp(pts) < 0} { set bbufdisp(pts) 1 }
    set bbufdisp(r_3) [expr ($bbufdisp(pts)-1) * $bbufdisp(i_3)]
    set bbufdisp(r_4) [expr ($bbufdisp(pts)-1) * $bbufdisp(i_4)]
}

proc BBufAngleBinding { args } {
    global bbufdisp

    if {3 > [llength $args]} { return }
    set key [lindex $args 1]
    if {[string compare $key "npts"] == 0} { 
	return [BBufCalcRange]
    }
    if {![regexp {(i|r)_(3|4)} $key match which axis]} {
	return
    }

    switch $which {
	r {
	    set ikey i_${axis}
	    if {$bbufdisp($ikey) == 0} { 
		set bbufdisp($key) 0
		return
	    }
	    set bbufdisp(pts) [expr floor($bbufdisp($key)/$bbufdisp($ikey))+1]
	    return [BBufCalcRange]
	}
	i {
	    return [BBufCalcRange]
	}
    }
}

#=T BUFFER=====================================================================

# Configuration variables for FANS resolution calculation
set res(col1)   20.0
set res(col2)   20.0
set res(dmono)  1.278
set res(e_fin)  1.2
set res(de_fin) 1.1
set res(e_max)  260.0

# Variables used in calculation of FANS resolution
set res(colp1)  [expr $res(col1) / (60.0 * 57.296)]
set res(colp2)  [expr $res(col2) / (60.0 * 57.296)]
set res(rmos)   [expr 30.0 / (60.0 * 57.296)]
set res(dtheta) [expr sqrt((($res(colp1) * $res(colp2)) * \
	($res(colp1) * $res(colp2)) + \
	($res(colp1) * $res(rmos)) * ($res(colp1) * $res(rmos)) + \
	($res(colp2) * $res(rmos)) * ($res(colp2) * $res(rmos))) / \
	($res(colp1) * $res(colp1) + $res(colp2) * $res(colp2) + \
	4 * $res(rmos) * $res(rmos)))]

proc fansres { energy } {
    global res
    set wavelength [expr sqrt(81.805 / ($energy + 1.2))]
    set stheta [expr $wavelength / (2 * $res(dmono))]
    if {[expr abs($stheta)] < 1.0} {
	set cot_theta [expr sqrt(1 - $stheta*$stheta) / $stheta]
    } else {
	set cot_theta 0.0
    }
    set de [expr 2.0 * ($energy + $res(e_fin)) * $cot_theta * $res(dtheta)]
    return [expr sqrt($res(de_fin) * $res(de_fin) + $de * $de)]
}

proc fansvec { energy0 {npts 1} {de_frac 1.0} } {
    global res
    if { $npts <= 0 } {
	return -code error "Enter positive number of points"
    }
    if { $energy0 < 0 } {
	return -code error "Enter positive starting energy"
    }
    
    set vector ""
    if {$energy0 < 25.0} {
	set en 25.0
	set scratch ""
	while {$en >= $energy0} {
	    lappend scratch $en
	    set de [fansres $en]
	    #set en [expr $en - $de_frac * $de]
	    set en [expr floor(($en - $de_frac * $de) * 100) / 100.0]
	}
	set vector [lrange [lsort -real -increasing $scratch] 0 [expr $npts-1]]
    } 

    set en 25.0
    if {([llength $vector] < $npts) && ($en >= $energy0)} {
	lappend vector $en
    }

    while {[llength $vector] < $npts} {
	set de [fansres $en]
	#set en [expr $en + $de_frac * $de]
	set en [expr floor(($en + $de_frac * $de) * 100) / 100.0]
	if {$en > $res(e_max)} { break }
	if {$en < $energy0} { continue }
	lappend vector $en
    }
    return $vector
}

proc TBufScanBinding { args } {
    global tbufdisp
    # puts $args
    set vec [fansvec $tbufdisp(ec) 1000 $tbufdisp(es)]
    set tbufdisp(ec) [lindex $vec 0]
    # Unset active traces
    # TBufTraceOff

    # Ptno binding
    set nargs [llength $args]

    if {$nargs > 2} {
	switch [lindex $args 1] {
	    ef {
		for {set i 0} { $i < [llength $vec]} {incr i} {
		    if {$i > $tbufdisp(ef)} { break }
		}
		set tbufdisp(pts) $i
		set tbufdisp(ef) [lindex $vec [expr $i - 1]]
	    }
	    default {
		if {[llength $vec] > $tbufdisp(pts)} {
		    set tbufdisp(ef) [lindex [lrange $vec 0 \
			    [expr int($tbufdisp(pts)-1)]] end]
		    set changed(tbufdisp) 0
		} elseif {[llength $vec] < $tbufdisp(pts)} {
		    set tbufdisp(pts) [llength $vec]
		    set tbufdisp(ef) [lindex $vec end]
		} else {
    		    set tbufdisp(ef) [lindex $vec end]
		    set changed(tbufdisp) 0
		}
	    }
	}
    }
    update
    # Reestablish traces
    #TBufTraceOn
}

proc TBufTraceOn { } {
    uplevel #0 trace variable tbufdisp(ec) w TBufScanBinding
    uplevel #0 trace variable tbufdisp(es) w TBufScanBinding
    uplevel #0 trace variable tbufdisp(ef) w TBufScanBinding
    uplevel #0 trace variable tbufdisp(pts) w TBufScanBinding
}

proc TBufTraceOff { } {
    uplevel #0 trace vdelete tbufdisp(ec) w TBufScanBinding
    uplevel #0 trace vdelete tbufdisp(es) w TBufScanBinding
    uplevel #0 trace vdelete tbufdisp(ef) w TBufScanBinding
    uplevel #0 trace vdelete tbufdisp(pts) w TBufScanBinding
}

proc TBufReadRec { file rec } {
    upvar $rec r
    set input [read $file 320]
    binary scan $input \
	    A50sfffcc3fffffffffA4ffff \
	    r(comment) pad1 \
	    r(ec) r(es) r(ef) \
	    r(m3) pad2\
	    r(tmp) r(it) r(tw) r(th0) r(ter) \
	    r(hf) r(mon) r(mpf) r(pts) \
	    r(mt) \
	    r(ihf) r(hfw) r(hfh) r(th)
}

proc TBufReadFile { } {
    global tbuf tbuf_fields

    set f [open TBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	TBufReadRec $f icpin
	foreach fld $tbuf_fields {
	    if {![catch {set icpin($fld)} result]} {
		set tbuf($i,$fld) $result
	    }
	}
    }
    close $f
    set tbuf(mtime) [file mtime TBUFFER.BUF]
}
 
proc TBufCheckFld { } {
    global tbufdisp 
    set flds { ec es ef tmp it tw th0 ter hf mon mpf pts ihf hfw hfh th}
    foreach fld $flds {
	# Check to make sure these are floating point numbers 
	if [catch { expr 1.0 * $tbufdisp($fld) } result] {
	    set tbufdisp($fld) "--------"
	    return -code error "Parameter \"$fld\" must be floating point number"
	}
    }
}

proc TBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set pad2 "   "
    set recval [binary format \
	    A50sfffcA3fffffffffA4ffff \
	    $r(comment) $pad1 \
	    $r(ec) $r(es) $r(ef) \
	    $r(m3) $pad2\
	    $r(tmp) $r(it) $r(tw) $r(th0) $r(ter) \
	    $r(hf) $r(mon) $r(mpf) $r(pts) \
	    $r(mt) \
	    $r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A320 $recval]
    puts -nonewline $file $record
    string length $record
}


proc TBufWriteFile { } {
    global tbuf tbuf_fields

    set f [open TBUFFER.BUF w]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $tbuf_fields {
	    set icpout($fld) $tbuf($i,$fld)
	}
	TBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A320 "\#"]
    close $f
    set tbuf(mtime) [file mtime TBUFFER.BUF]
}

proc TBufInit { } {
    global tbuf 
    array set tbuf {buftype Increment}
    for {set i 0} {$i < 30} {incr i} {
	set tbuf($i,comment) "empty"
	set tbuf($i,ec)    10.0
	set tbuf($i,es)    1.0
	set tbuf($i,ef)    11.1
	set tbuf($i,m3)    0
	set tbuf($i,tmp)   300
	set tbuf($i,it)    0
	set tbuf($i,tw)    0
	set tbuf($i,th0)   0
	set tbuf($i,ter)   1
	set tbuf($i,hf)    0
	set tbuf($i,mon)   1000
	set tbuf($i,mpf)   1
	set tbuf($i,pts)   1.0
	set tbuf($i,mt)    TIME
	set tbuf($i,ihf)   0.0
	set tbuf($i,hfw)   0.0
	set tbuf($i,hfh)   0.0
	set tbuf($i,th)    0.0
    }
    set tbuf(currentrec)   0
    set tbuf(mtime)        0
}

proc TBufList { } {
    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
	$lbox insert end [TBufFormatRec $i]
    }
    global text_only
    if $text_only {
       $lbox selection set 0 0
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "TBufDisp \[$lbox curselection\]; set changed(tbufdisp) 0"
    if $text_only {
       set bind_type <Linefeed>
    } else {
       set bind_type <Return>
    }
    bind $lbox $bind_type "TBufDisp \[$lbox curselection\]; set changed(tbufdisp) 0"
}

proc TBufFormatRec { recno } {
    global tbuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $tbuf($recno,comment)]
    append output [format " %6.2f "   $tbuf($recno,ec)]
    append output [format "%6.2f "   $tbuf($recno,es)]
    append output [format "%6.2f "   $tbuf($recno,ef)]
    append output [format "%4.0f "   $tbuf($recno,pts)]
    append output [format "%2.0f\* " $tbuf($recno,mpf)]
    append output [format "%6.0f "   $tbuf($recno,mon)]
    append output [binary format A4 $tbuf($recno,mt)]
    return $output
}

proc TBufDisp { recno } {
    global tbufdisp tbuf tbuf_fields config

    set mtime [file mtime TBUFFER.BUF]
    if {$mtime != $tbuf(mtime)} {
	TBufReadFile
	TBufList
    }

    if { $config(paranoid) } { CheckBufChange }

    foreach fld $tbuf_fields {
	set tbufdisp($fld) $tbuf($recno,$fld)
    }
    set num [format "%2d" [expr $recno + 1]]
    set tbufdisp(label) "TBuffer No: $num"
    set tbufdisp(current) $recno

    set tbufdisp(es) [fltrim $tbufdisp(es) 10000]
    if [catch {expr $tbufdisp(pts) * 1} tbufdisp(pts)] {
	set tbufdisp(pts) 1
    }

    # Clear changed flag
    set changed(tbufdisp) 0

}

proc TBufUpdate { } {
    global tbufdisp tbuf changed

    if [catch {TBufCheckFld} result] {
	return -code error $result
    }
    set recno $tbufdisp(current)
    foreach fld [array names tbufdisp] {
	set tbuf($recno,$fld) $tbufdisp($fld)
    }
    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [TBufFormatRec $recno]
    # update buffer information on disk
    TBufWriteFile

    set changed(tbufdisp) 0
}

proc TBufEntryCreate { p } {
    global tbufdisp config text_only

    set config(tbufentry) $p

    label $p.bufnolabel -textvariable tbufdisp(label)
    label $p.commentlabel -text "Comment:"
    entry $p.comment -width 35 -textvariable tbufdisp(comment)

    label $p.energylab -text "Energy:"
    frame $p.energy

    label $p.energy.eclab -text "E Start:"
    entry $p.energy.ec -width 6 -textvariable tbufdisp(ec)
    label $p.energy.eslab -text "E Step (x dE):"
    if {!$text_only} {
       tk_optionMenu $p.energy.es tbufdisp(es) 0.125 0.2 0.25 0.4 0.5 1.0
    } else {
       ck_optionMenu $p.energy.es tbufdisp(es) 0.125 0.2 0.25 0.4 0.5 1.0
    }
#    entry $p.energy.es -width 6 -textvariable tbufdisp(es)
    label $p.energy.eflab -text "E Final:"
    entry $p.energy.ef -width 6 -textvariable tbufdisp(ef)
    if $text_only {
       grid $p.energy.eclab -row 0 -column 0
       grid $p.energy.ec    -row 0 -column 1 -padx 1
       grid $p.energy.eslab -row 0 -column 2
       grid $p.energy.es    -row 0 -column 3 -padx 1
       grid $p.energy.eflab -row 0 -column 4
       grid $p.energy.ef    -row 0 -column 5 -padx 1
    } else {
       pack $p.energy.eclab $p.energy.ec \
	    $p.energy.eslab $p.energy.es \
	    $p.energy.eflab $p.energy.ef -side left
    }

    label $p.ang3label -text "Angle 3:"
    frame $p.ang3
    label $p.ang3.m3lab -text "M3:"
    #checkbutton $p.ang3.m3 -variable tbufdisp(m3) 
    set tbufdisp(m3) 1
    toggle_create $p.ang3.m3 tbufdisp(m3)
    pack $p.ang3.m3lab $p.ang3.m3 -side left

    label $p.templabel -text "Temp/H:"
    frame $p.temp
    label $p.temp.t0label -text "T0:"
    entry $p.temp.t0 -width 6 -textvariable tbufdisp(tmp)
    label $p.temp.tinclabel -text "Inc-T:"
    entry $p.temp.tinc -width 6 -textvariable tbufdisp(it)
    label $p.temp.twlabel -text "Wait:"
    entry $p.temp.tw -width 3 -textvariable tbufdisp(tw)
    label $p.temp.terlabel -text "Err:"
    entry $p.temp.ter -width 3 -textvariable tbufdisp(ter)
    label $p.temp.th0label -text "Hld0:"
    entry $p.temp.th0 -width 3 -textvariable tbufdisp(th0)
    label $p.temp.thlabel -text "Hld:"
    entry $p.temp.th -width 3 -textvariable tbufdisp(th)
    if $text_only {
       grid $p.temp.t0label   -row 0 -column 0
       grid $p.temp.t0        -row 0 -column 1  -padx 1
       grid $p.temp.tinclabel -row 0 -column 2
       grid $p.temp.tinc      -row 0 -column 3  -padx 1
       grid $p.temp.twlabel   -row 0 -column 4
       grid $p.temp.tw        -row 0 -column 5  -padx 1
       grid $p.temp.terlabel  -row 0 -column 6
       grid $p.temp.ter       -row 0 -column 7  -padx 1
       grid $p.temp.th0label  -row 0 -column 8
       grid $p.temp.th0       -row 0 -column 9  -padx 1
       grid $p.temp.thlabel   -row 0 -column 10
       grid $p.temp.th        -row 0 -column 11 -padx 1
    } else {
       pack $p.temp.t0label $p.temp.t0 \
	    $p.temp.tinclabel $p.temp.tinc \
	    $p.temp.twlabel $p.temp.tw \
	    $p.temp.terlabel $p.temp.ter \
	    $p.temp.th0label $p.temp.th0 \
	    $p.temp.thlabel $p.temp.th -side left
    }

    label $p.timelabel -text "Time:"
    frame $p.time
    label $p.time.monlabel -text "Monitor:"
    entry $p.time.mon -width 6 -textvariable tbufdisp(mon)
    label $p.time.mpflabel -text "Prefac:"
    entry $p.time.mpf -width 3 -textvariable tbufdisp(mpf)
    label $p.time.mtlabel -text "M-typ:"
    if {!$text_only} {
       tk_optionMenu $p.time.mt tbufdisp(mt) TIME NEUT
    } else {
       ck_optionMenu $p.time.mt tbufdisp(mt) TIME NEUT
    }
    if $text_only {
       grid $p.time.monlabel -row 0 -column 0
       grid $p.time.mon      -row 0 -column 1 -padx 1
       grid $p.time.mpflabel -row 0 -column 2
       grid $p.time.mpf      -row 0 -column 3 -padx 1
       grid $p.time.mtlabel  -row 0 -column 4
       grid $p.time.mt       -row 0 -column 5 -padx 1
    } else {
       pack $p.time.monlabel $p.time.mon \
	    $p.time.mpflabel $p.time.mpf \
	    $p.time.mtlabel $p.time.mt -side left 
    }

    frame $p.pts
    label $p.pts.label -text "NPTS: "
    entry $p.pts.val -textvariable tbufdisp(pts) -width 6
    pack $p.pts.label $p.pts.val -side left

    # pack widgets 
    if $text_only {
       grid $p.bufnolabel -row 0 -column 0 -columnspan 2 -sticky w
       grid $p.commentlabel -row 0 -column 2 -padx 3
       grid $p.comment -row 0 -column 3 -columnspan 6 -sticky ew
       grid $p.energylab -row 1 -column 0 -sticky w
       grid $p.energy -row 1 -column 1 -columnspan 5 -padx 2 -sticky w
       grid $p.pts -row 1 -column 7
       label $p.blank -text " "
       grid $p.blank -row 2 -column 0
       grid $p.templabel -row 3 -column 0 -sticky w
       grid $p.temp -row 3 -column 1 -columnspan 7 -padx 2 -sticky w
       grid $p.timelabel -row 4 -column 0 -sticky w
       grid $p.time -row 4 -column 1 -columnspan 6 -padx 2 -sticky w
    } else {
       grid $p.bufnolabel -row 0 -column 0
       grid $p.commentlabel -row 0 -column 1 
       grid $p.comment -row 0 -column 2 -columnspan 6 -sticky ew
       grid $p.energylab -row 1 -column 0
       grid $p.energy -row 1 -column 1 -columnspan 5 -sticky w
       grid $p.pts -row 1 -column 7
#       grid $p.ang3label -row 2 -column 0
#       grid $p.ang3 -row 2 -column 1 -columnspan 6 -sticky w
       grid $p.templabel -row 2 -column 0
       grid $p.temp -row 2 -column 1 -columnspan 6 -sticky w
       grid $p.timelabel -row 3 -column 0
       grid $p.time -row 3 -column 1 -columnspan 6 -sticky w
    }

    # Set variable traces
#    TBufTraceOn
    trace variable tbufdisp(es) w TBufScanBinding
    bind $p.pts.val   <Return>   {TBufScanBinding tbufdisp pts w}
#    bind $p.pts.val   <FocusOut> {TBufScanBinding tbufdisp pts w}
    bind $p.energy.ef <Return>   {TBufScanBinding tbufdisp ef w}
#    bind $p.energy.ef <FocusOut> {TBufScanBinding tbufdisp ef w}
    bind $p.energy.ec <Return>   {TBufScanBinding tbufdisp ec w}
#    bind $p.energy.ec <FocusOut> {TBufScanBinding tbufdisp ec w}

    if {!$text_only} {

    bind $p.pts.val   <<Paste>>  {TBufScanBinding tbufdisp pts w}
    bind $p.energy.ef <<Paste>>  {TBufScanBinding tbufdisp ef w}
    bind $p.energy.ec <<Paste>>  {TBufScanBinding tbufdisp ec w}

    }

    return $p
}

proc TBufEntryMod { } {
    global tbufdisp config text_only

    set parent $config(tbufentry)
    if {$config(mode) == 0} {
	set state disabled
        if {!$text_only} {
           set color gray
        } else {
           set color white
        }
    } else {
	set state normal
        if $text_only {
           set color white
        } else {
           set color black
        }
    }
    foreach w "$parent.templabel [winfo children $parent.temp]" {
	switch [winfo class $w] {
	    Label {$w config -fg $color}
	    Entry {$w config -fg $color -state $state}
	}
    }

    if {$config(mode) == 2} {
	set state normal
        if $text_only {
           set color white
        } else {
           set color black
        }
    } else {
	set state disabled
        if {!$text_only} {
           set color gray
        } else {
           set color white
        }
    }

    foreach l {temp.tinclabel temp.thlabel} {
	$parent.$l config -fg $color
    }
    foreach e {temp.tinc temp.th} {
	$parent.$e config -state $state -fg $color
    }
}


proc TBufKeyBinding { } {
    bind all <KeyPress-F1> TBufUpdate
    bind all <KeyPress-F2> BufopShow
    bind all <KeyPress-F3> {notebook_display .e tbuf}
    bind all <KeyPress-F4> {notebook_display .e blank}
}

#==============================================================================

#	   CHARACTER*50 com 
# 	   REAL         pts
#	   REAL		qx,qxi,qZ,qZi
#	   REAL         scan_mode
#	   REAL		s(4)
#	   REAL		i(4)
#	   REAL         tmp , it , tw , th0 , ter 
#	   REAL         hf , mon0 , mon1,exp,mpf 
#	   CHARACTER*4  mt
#	   LOGICAL      flip_state(4)
#	   INTEGER*4	numx !number of xtra motor
#	   REAL		sx,ix
#	   REAL         ihf , hfw , hfh , th 

set rbuf_fields { comment pts qx qxi qz qzi scan_mode \
                  s_1 s_2 s_3 s_4 i_1 i_2 i_3 i_4 \
		  tmp it tw th0 ter hf mon0 mon1 exp mpf \
                  mt flip1 flip2 flip3 flip4 numx sx ix \
                  ihf hfw hfh th }


proc RBufReadRec { file rec } {
    upvar $rec r
    set input [read $file 320]
    binary scan $input \
	    A50sffffffffffffffffffffffffA4iiiiiffffff \
	    r(comment) pad1 \
	    r(pts) r(qx) r(qxi) r(qz) r(qzi) r(scan_mode) \
	    r(s_1) r(s_2) r(s_3) r(s_4) r(i_1) r(i_2) r(i_3) r(i_4) \
	    r(tmp) r(it) r(tw) r(th0) r(ter) \
	    r(hf) r(mon0) r(mon1) r(exp) r(mpf) \
	    r(mt) r(flip1) r(flip2) r(flip3) r(flip4) \
	    r(numx) r(sx) r(ix) r(ihf) r(hfw) r(hfh) r(th)
    if {$r(numx) == 10} { set r(numx) 13 } ;# _Wierd_ bug
}

proc RBufReadFile { } {
    global rbuf rbuf_fields

    set f [open RBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	RBufReadRec $f icpin
	foreach fld $rbuf_fields {
	    if {![catch {set icpin($fld)} result]} {
		set rbuf($i,$fld) $result
	    }
	}
    }
    close $f
    set rbuf(mtime) [file mtime RBUFFER.BUF]
}
 
proc RBufCheckFld { } {
    global rbufdisp 
    set flds { tmp it tw th0 ter hf mon0 mon1 exp mpf pts ihf hfw hfh th}
    foreach fld $flds {
	# Check to make sure these are floating point numbers 
	if [catch { expr 1.0 * $rbufdisp($fld) } result] {
	    set rbufdisp($fld) "--------"
	    return -code error "Parameter \"$fld\" must be floating point number"
	}
    }
}

proc RBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set pad2 "   "

    set recval [binary format \
	    A50sffffffffffffffffffffffffA4iiiiiffffff \
	    $r(comment) $pad1 \
	    $r(pts) $r(qx) $r(qxi) $r(qz) $r(qzi) $r(scan_mode) \
	    $r(s_1) $r(s_2) $r(s_3) $r(s_4) $r(i_1) $r(i_2) $r(i_3) $r(i_4) \
	    $r(tmp) $r(it) $r(tw) $r(th0) $r(ter) \
	    $r(hf) $r(mon0) $r(mon1) $r(exp) $r(mpf) \
	    $r(mt) $r(flip1) $r(flip2) $r(flip3) $r(flip4) \
	    $r(numx) $r(sx) $r(ix) $r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A320 $recval]
    puts -nonewline $file $record
    string length $record
}


proc RBufWriteFile { } {
    global rbuf rbuf_fields

    set f [open RBUFFER.BUF w]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $rbuf_fields {
	    set icpout($fld) $rbuf($i,$fld)
	}
	RBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A320 "\#"]
    close $f
    set rbuf(mtime) [file mtime RBUFFER.BUF]
}

proc RBufInit { } {
    global rbuf 
    array set rbuf {buftype Reflectivity}
    for {set i 0} {$i < 30} {incr i} {
	set rbuf($i,comment) "empty"
	set rbuf($i,scan_mode) 1
	set rbuf($i,pts)   31
	set rbuf($i,qx)    0.0
	set rbuf($i,qxi)   0.0
	set rbuf($i,qxe)   0.0
	set rbuf($i,qz)    0.005
	set rbuf($i,qzi)   0.0005
	set rbuf($i,qze)   0.020
	set rbuf($i,s_1)   0.1
	set rbuf($i,s_2)   0.1
	set rbuf($i,s_3)   0.5
	set rbuf($i,s_4)   0.0
	set rbuf($i,i_1)   0.01
	set rbuf($i,i_2)   0.01
	set rbuf($i,i_3)   0.02
	set rbuf($i,i_4)   0.0
	set rbuf($i,e_1)   0.0
	set rbuf($i,e_2)   0.0
	set rbuf($i,e_3)   0.0
	set rbuf($i,e_4)   0.0
	set rbuf($i,tmp)   300.0
	set rbuf($i,it)    0.0
	set rbuf($i,tw)    0.0
	set rbuf($i,th0)   0.0
	set rbuf($i,ter)   1.0
	set rbuf($i,hf)    0
	set rbuf($i,mon0)  1000
	set rbuf($i,mon1)  1000
	set rbuf($i,exp)   1.0
	set rbuf($i,mpf)   1.0
	set rbuf($i,mt)    TIME
	set rbuf($i,flip1) 0
	set rbuf($i,flip2) 0
	set rbuf($i,flip3) 0
	set rbuf($i,flip4) 0
	set rbuf($i,numx)  0
	set rbuf($i,sx)    0.0
	set rbuf($i,ix)    0.0
	set rbuf($i,ihf)   0.0
	set rbuf($i,hfw)   0.0
	set rbuf($i,hfh)   0.0
	set rbuf($i,th)    0.0
    }
    set rbuf(currentrec)   0
    set rbuf(mtime)        0
}

proc RBufList { } {
    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
        $lbox insert end [RBufFormatRec $i]
    }
    global text_only
    if $text_only {
       $lbox selection set 0 0
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "RBufDisp \[$lbox curselection\]; set changed(rbufdisp) 0"
    if $text_only {
       set bind_type <Linefeed>
    } else {
       set bind_type <Return>
    }
    bind $lbox $bind_type "RBufDisp \[$lbox curselection\]; set changed(rbufdisp) 0"
}                                                                               
proc RBufFormatRec { recno } {
    global rbuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $rbuf($recno,comment)]
    append output [format "  %8.6f"  $rbuf($recno,qz)]
    append output [format "  %8.6f"  $rbuf($recno,qze)]
    append output [format "  %8.6f"  $rbuf($recno,qx)]
    append output [format "  %8.6f"  $rbuf($recno,qxe)]
    append output [format "  %2.0f\* " $rbuf($recno,mpf)]
    append output "varies"
    append output [format " %4.0f"   $rbuf($recno,pts)]
    return $output
}                                                                               
proc RBufDisp { recno } {
    global rbufdisp rbuf rbuf_fields config
 
    set mtime [file mtime RBUFFER.BUF]
    if {$mtime != $rbuf(mtime)} {
        RBufReadFile
        RBufList
    }
 
    if {$config(paranoid)} { CheckBufChange } 
 
    foreach fld $rbuf_fields {
        set rbufdisp($fld) $rbuf($recno,$fld)
    }
    set num [format "%2d" [expr $recno + 1]]
    set rbufdisp(label)   "RBuffer No: $num"
    set rbufdisp(current) $recno

    set flfields { qx qxi qz qzi s_1 i_1 s_2 i_2 s_3 i_3 s_4 i_4 sx ix}

    foreach f $flfields {
        set rbufdisp($f) [fltrim $rbufdisp($f) 10000]
    }

 
    if [catch {expr $rbufdisp(pts) * 1} rbufdisp(pts)] {
        set rbufdisp(pts) 1
    }
 
    RBufCalcAngles

    # Clear changed flag
    set changed(rbufdisp) 0
 
}

proc RBufUpdate { } {
    global rbufdisp rbuf changed
 
    if [catch {RBufCheckFld} result] {
        return -code error $result
    }
    set recno $rbufdisp(current)
    foreach fld [array names rbufdisp] {
        set rbuf($recno,$fld) $rbufdisp($fld)
    }
    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [RBufFormatRec $recno]
    # update buffer information on disk
    RBufWriteFile
 
    set changed(rbufdisp) 0
}

proc RBufEntryCreate { p } {
    global rbufdisp config text_only

    set config(rbufentry) $p

    label $p.bufnolabel -textvariable rbufdisp(label)
    label $p.commentlabel -text "Comment:"
    entry $p.comment -width 35 -textvariable rbufdisp(comment)
    if $text_only {
       grid $p.bufnolabel   -row 0 -column 0 -sticky w
       grid $p.commentlabel -row 0 -column 1 -ipadx 3 -sticky e
       grid $p.comment      -row 0 -column 2 -columnspan 6 -sticky ew
       set width 5
    } else {
       grid $p.bufnolabel    -row 0 -column 0
       grid $p.commentlabel  -row 0 -column 1
       grid $p.comment       -row 0 -column 2 -columnspan 6 -sticky ew
       set width 7
    }
    label $p.qzlabel  -text "qz-beg:"
    entry $p.qz       -textvariable rbufdisp(qz)  -width 8
    label $p.qzilabel -text "Inc-qz:"
    entry $p.qzi      -textvariable rbufdisp(qzi) -width 8
    label $p.qzelabel -text "qz-end:"
    entry $p.qze      -textvariable rbufdisp(qze) -width 8

    label $p.qxlabel  -text "qx-beg:"
    entry $p.qx       -textvariable rbufdisp(qx)  -width 8
    label $p.qxilabel -text "Inc-qx:"
    entry $p.qxi      -textvariable rbufdisp(qxi) -width 8
    label $p.qxelabel -text "qx-end:"
    entry $p.qxe      -textvariable rbufdisp(qxe) -width 8
    
    grid $p.qzlabel  -row 1 -column 1 -sticky w
    grid $p.qz       -row 1 -column 2 -sticky w
    grid $p.qzilabel -row 1 -column 3 -sticky w
    grid $p.qzi      -row 1 -column 4 -sticky w
    grid $p.qzelabel -row 1 -column 5 -sticky w
    grid $p.qze      -row 1 -column 6 -sticky w
    grid $p.qxlabel  -row 2 -column 1 -sticky w
    grid $p.qx       -row 2 -column 2 -sticky w
    grid $p.qxilabel -row 2 -column 3 -sticky w
    grid $p.qxi      -row 2 -column 4 -sticky w
    grid $p.qxelabel -row 2 -column 5 -sticky w
    grid $p.qxe      -row 2 -column 6 -sticky w

    set row 3
    foreach i {1 2 3 4} {
	label $p.s_${i}label -text "S${i}-beg:"
	entry $p.s_${i}      -textvariable rbufdisp(s_$i) -width 8
	label $p.i_${i}label -text "S${i}-inc:"
	entry $p.i_${i}      -textvariable rbufdisp(i_$i) -width 8
	label $p.e_${i}label -text "S${i}-end:"
	entry $p.e_${i}      -textvariable rbufdisp(e_$i) -width 8

	grid $p.s_${i}label -row $row -column 1 -sticky w
	grid $p.s_${i}      -row $row -column 2 -sticky w
	grid $p.i_${i}label -row $row -column 3 -sticky w
	grid $p.i_${i}      -row $row -column 4 -sticky w
	grid $p.e_${i}label -row $row -column 5 -sticky w
	grid $p.e_${i}      -row $row -column 6 -sticky w
	incr row
    }

    frame $p.xm
    label $p.xm.mlabel  -text "XM-num:"
    entry $p.xm.m       -textvariable rbufdisp(numx) -width 2

    label $p.slabel  -text "XM-beg:"
    entry $p.s       -textvariable rbufdisp(sx) -width 8
    label $p.silabel -text "XM-inc:"
    entry $p.si      -textvariable rbufdisp(ix) -width 8
    label $p.selabel -text "XM-end:"
    entry $p.se      -textvariable rbufdisp(ex) -width 8
    
    frame $p.sm
    label $p.sm.slabel -text "scan_mode:"
    entry $p.sm.s      -textvariable rbufdisp(scan_mode) -width 1

    grid  $p.xm.mlabel $p.xm.m -sticky w
    grid  $p.sm.slabel $p.sm.s -sticky w
    grid  $p.xm $p.slabel $p.s $p.silabel $p.si $p.selabel $p.se \
	    $p.sm -sticky w

    # Sample environment fields
    
    label $p.templabel -text "Temp/H:"
    frame $p.temp
    label $p.temp.t0label -text "T0:"
    entry $p.temp.t0 -width 6 -textvariable rbufdisp(tmp)
    label $p.temp.tinclabel -text "Inc-T:"
    entry $p.temp.tinc -width 6 -textvariable rbufdisp(it)
    label $p.temp.twlabel -text "Wait:"
    entry $p.temp.tw -width 3 -textvariable rbufdisp(tw)
    label $p.temp.terlabel -text "Err:"
    entry $p.temp.ter -width 3 -textvariable rbufdisp(ter)
    label $p.temp.th0label -text "Hld0:"
    entry $p.temp.th0 -width 3 -textvariable rbufdisp(th0)
    label $p.temp.thlabel -text "Hld:"
    entry $p.temp.th -width 3 -textvariable rbufdisp(th) 
    if $text_only {
       grid $p.temp.t0label   -row 0 -column 0
       grid $p.temp.t0        -row 0 -column 1  -padx 1
       grid $p.temp.tinclabel -row 0 -column 2
       grid $p.temp.tinc      -row 0 -column 3  -padx 1
       grid $p.temp.twlabel   -row 0 -column 4
       grid $p.temp.tw        -row 0 -column 5  -padx 1
       grid $p.temp.terlabel  -row 0 -column 6
       grid $p.temp.ter       -row 0 -column 7  -padx 1
       grid $p.temp.th0label  -row 0 -column 8
       grid $p.temp.th0       -row 0 -column 9  -padx 1
       grid $p.temp.thlabel   -row 0 -column 10
       grid $p.temp.th        -row 0 -column 11 -padx 1
    } else {
       pack $p.temp.t0label $p.temp.t0 \
            $p.temp.tinclabel $p.temp.tinc \
            $p.temp.twlabel $p.temp.tw \
            $p.temp.terlabel $p.temp.ter \
            $p.temp.th0label $p.temp.th0 \
            $p.temp.thlabel $p.temp.th -side left
    }

    grid $p.templabel $p.temp - - - - - - -sticky w

    label $p.timelabel -text "Time:"
    frame $p.time
    label $p.time.mpflabel -text "Prefac:"
    entry $p.time.mpf  -width 3 -textvariable rbufdisp(mpf)
    label $p.time.mon0label -text "Mon0:"
    entry $p.time.mon0 -width 6 -textvariable rbufdisp(mon0)
    label $p.time.mon1label -text "Mon1:"
    entry $p.time.mon1 -width 6 -textvariable rbufdisp(mon1)
    label $p.time.explabel -text "Exp:"
    entry $p.time.exp  -width 6 -textvariable rbufdisp(exp)
    
    label $p.time.mtlabel -text "M-typ:"
    if {!$text_only} {
       tk_optionMenu $p.time.mt rbufdisp(mt) TIME NEUT
    } else {
       ck_optionMenu $p.time.mt rbufdisp(mt) TIME NEUT
    }
    if $text_only {
	grid $p.time.mpflabel  -row 0 -column 0
	grid $p.time.mpf       -row 0 -column 1 -padx 1
	grid $p.time.mon0label -row 0 -column 2
	grid $p.time.mon0      -row 0 -column 3 -padx 1
	grid $p.time.mon1label -row 0 -column 4
	grid $p.time.mon1      -row 0 -column 5 -padx 1
	grid $p.time.explabel  -row 0 -column 6
	grid $p.time.exp       -row 0 -column 7 -padx 1
	grid $p.time.mtlabel   -row 0 -column 8
	grid $p.time.mt        -row 0 -column 9 -padx 1
    } else {
	pack $p.time.mpflabel  $p.time.mpf  \
	     $p.time.mon0label $p.time.mon0 \
	     $p.time.mon1label $p.time.mon1 \
	     $p.time.explabel  $p.time.exp  \
	     $p.time.mtlabel   $p.time.mt -side left
    }

    grid $p.timelabel $p.time - - - - - - -sticky w

    # Number of points
    frame $p.pts
    label $p.pts.label -text "NPTS: "
    entry $p.pts.val -textvariable rbufdisp(pts) -width 6
    pack $p.pts.label $p.pts.val -side left                                     
    grid $p.pts -row 4 -column 7

    bind $p.pts.val <Return>   RBufCalcAngles
    bind $p.pts.val <FocusOut> RBufCalcAngles
    bind $p.qz  <Return>   "RBufAngleBinding rbufdisp qz  w"
    bind $p.qzi <Return>   "RBufAngleBinding rbufdisp qzi w"
    bind $p.qze <Return>   "RBufAngleBinding rbufdisp qze w"
    bind $p.qx  <Return>   "RBufAngleBinding rbufdisp qx  w"
    bind $p.qxi <Return>   "RBufAngleBinding rbufdisp qxi w"
    bind $p.qxe <Return>   "RBufAngleBinding rbufdisp qxe w"
    bind $p.qz  <FocusOut> "RBufAngleBinding rbufdisp qz  w"
    bind $p.qzi <FocusOut> "RBufAngleBinding rbufdisp qzi w"
    bind $p.qze <FocusOut> "RBufAngleBinding rbufdisp qze w"
    bind $p.qx  <FocusOut> "RBufAngleBinding rbufdisp qx  w"
    bind $p.qxi <FocusOut> "RBufAngleBinding rbufdisp qxi w"
    bind $p.qxe <FocusOut> "RBufAngleBinding rbufdisp qxe w"

    return $p
}

# end = beg + (pts-1) * inc
# pts = (end - beg) / inc + 1
proc RBufCalcAngles { } {
    global rbufdisp
    if [catch {expr $rbufdisp(pts) * 1} rbufdisp(pts)] {
        set rbufdisp(pts) "------"
        return
    }

    foreach i {qx qz} {
	set beg $i
	set inc "${i}i"
	set end "${i}e"
        if [catch {expr $rbufdisp($beg) * 1.0} rbufdisp($beg)] {
            set rbufdisp($beg) "------"
            return
        }
        if [catch {expr $rbufdisp($inc) * 1.0} rbufdisp($inc)] {
            set rbufdisp($inc) "------"
            return
        }
        set rbufdisp($end) [expr $rbufdisp($beg) + \
                $rbufdisp($inc) * ($rbufdisp(pts) - 1)]
    }

    foreach i {x _1 _2 _3 _4} {
	set beg "s$i"
	set inc "i$i"
	set end "e$i"
        if [catch {expr $rbufdisp($beg) * 1.0} rbufdisp($beg)] {
            set rbufdisp($beg) "------"
            return
        }
        if [catch {expr $rbufdisp($inc) * 1.0} rbufdisp($inc)] {
            set rbufdisp($inc) "------"
            return
        }
        set rbufdisp($end) [expr $rbufdisp($beg) + \
                $rbufdisp($inc) * ($rbufdisp(pts) - 1)]
    }
}

proc RBufAngleBinding { args } {
    global rbufdisp
    set largs [llength $args]

    if {$largs < 3} { return }
    set key [lindex $args 1]
    if {[string compare $key "pts"] == 0} {
        return [RBufCalcAngles]
    }
    
    if {[string compare $key "qxe"] == 0} {
	if [catch {expr $rbufdisp(qxe) * 1.0} rbufdisp(qxe)] {
	    set rbufdisp(qxe) "------"
	    return
	}
	if {$rbufdisp(qxi) == 0.0} {
	    set rbufdisp(qxe) $rbufdisp(qx)
	    set rbufdisp(pts) 1
	} else {
	    set rbufdisp(pts) [expr (($rbufdisp(qxe) - $rbufdisp(qx))/$rbufdisp(qxi)) + 1]
	}
	
    }

    if {[string compare $key "qze"] == 0} {
	if [catch {expr $rbufdisp(qze) * 1.0} rbufdisp(qze)] {
	    set rbufdisp(qze) "------"
	    return
	}
	if {$rbufdisp(qzi) == 0.0} {
	    set rbufdisp(qze) $rbufdisp(qz)
	    set rbufdisp(pts) 1
	} else {
	    set rbufdisp(pts) [expr (($rbufdisp(qze) - $rbufdisp(qz))/$rbufdisp(qzi)) + 1]
	}
	
    }

    return [RBufCalcAngles]
}

#=D BUFFER=====================================================================

proc DBufWriteRec { file rec } {
    upvar $rec r

    set pad1 0
    set pad2 "   "
    set recval [binary format \
	"A50siiiiiiiiiiiiiiiiiiffffffffffffiiiiiifffffffffffffffffffffffffA4iiiiffff" \
	$r(comment) $pad1 \
	$r(lpn_1) $r(lpn_2) $r(lpn_3) $r(lpn_4) $r(lpn_5) $r(lpn_6) \
	$r(lpts_1) $r(lpts_2) $r(lpts_3) $r(lpts_4) $r(lpts_5) $r(lpts_6) \
	$r(mn_1) $r(mn_2) $r(mn_3) $r(mn_4) $r(mn_5) $r(mn_6) \
	$r(a_1) $r(a_2) $r(a_3) $r(a_4) $r(a_5) $r(a_6) \
	$r(i_1) $r(i_2) $r(i_3) $r(i_4) $r(i_5) $r(i_6) \
	$r(xyz_1) $r(xyz_2) $r(xyz_3) $r(xyz_4) $r(xyz_5) $r(xyz_6) \
	$r(a_xyz_1) $r(a_xyz_2) $r(a_xyz_3) $r(a_xyz_4) $r(a_xyz_5) $r(a_xyz_6) \
	$r(i_xyz_1) $r(i_xyz_2) $r(i_xyz_3) $r(i_xyz_4) $r(i_xyz_5) $r(i_xyz_6) \
	$r(pts) $r(hf) $r(phi) $r(psi) $r(phi_inc) $r(psi_inc) \
	$r(tmp) $r(it) $r(tw) $r(th0) $r(ter) $r(mon) $r(mpf) $r(mt) \
	$r(flip1) $r(flip2) $r(flip3) $r(flip4) \
	$r(ihf) $r(hfw) $r(hfh) $r(th)]
    set record [binary format A336 $recval]
    puts -nonewline $file $record
    string length $record
}

proc DBufWriteFile { } {
    global dbuf dbuf_fields

    set f [open DBUFFER.BUF w]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
	foreach fld $dbuf_fields {
	    set icpout($fld) $dbuf($i,$fld)
	}
	DBufWriteRec $f icpout
    }
    puts -nonewline $f [binary format A336 "\#"]
    close $f
    set dbuf(mtime) [file mtime DBUFFER.BUF]
}

proc DBufReadRec { file rec } {
    upvar $rec r
    set input [read $file 336]
    binary scan $input \
	A50siiiiiiiiiiiiiiiiiiffffffffffffiiiiiifffffffffffffffffffffffffA4iiiiffff \
	r(comment) pad1 \
	r(lpn_1) r(lpn_2) r(lpn_3) r(lpn_4) r(lpn_5) r(lpn_6) \
	r(lpts_1) r(lpts_2) r(lpts_3) r(lpts_4) r(lpts_5) r(lpts_6) \
	r(mn_1) r(mn_2) r(mn_3) r(mn_4) r(mn_5) r(mn_6) \
	r(a_1) r(a_2) r(a_3) r(a_4) r(a_5) r(a_6) \
	r(i_1) r(i_2) r(i_3) r(i_4) r(i_5) r(i_6) \
	r(xyz_1) r(xyz_2) r(xyz_3) r(xyz_4) r(xyz_5) r(xyz_6) \
	r(a_xyz_1) r(a_xyz_2) r(a_xyz_3) r(a_xyz_4) r(a_xyz_5) r(a_xyz_6) \
	r(i_xyz_1) r(i_xyz_2) r(i_xyz_3) r(i_xyz_4) r(i_xyz_5) r(i_xyz_6) \
	r(pts) r(hf) r(phi) r(psi) r(phi_inc) r(psi_inc) \
	r(tmp) r(it) r(tw) r(th0) r(ter) r(mon) r(mpf) r(mt) \
	r(flip1) r(flip2) r(flip3) r(flip4) \
	r(ihf) r(hfw) r(hfh) r(th)
}

proc DBufReadFile { } {
    global dbuf dbuf_fields

    set f [open DBUFFER.BUF r]
    fconfigure $f -encoding binary
    for {set i 0} {$i < 30} {incr i} {
        DBufReadRec $f icpin
        foreach fld $dbuf_fields {
            if {![catch {set icpin($fld)} result]} {
                set dbuf($i,$fld) $result
            }
        }
    }
    close $f
    set dbuf(mtime) [file mtime DBUFFER.BUF]
}

proc DBufList { } {

    # Clear out our list if it contains anything.
    set lbox .l.list
    set start [expr int([lindex [$lbox yview] 0] * [$lbox index end])]
    $lbox delete 0 end
    $lbox configure -font TkFixedFont
    for {set i 0} {$i < 30} {incr i} {
        $lbox insert end [DBufFormatRec $i]
    }

    $lbox yview $start
    bind $lbox <ButtonRelease-1> "DBufDisp \[$lbox curselection\]"
    set bind_type <Return>
    bind $lbox $bind_type "DBufDisp \[$lbox curselection\]"
}

proc DBufFormatRec { recno } {
    global dbuf
    set output [format "%2d " [expr $recno + 1]]
    append output [binary format A20 $dbuf($recno,comment)]
    append output [format " %4d "    $dbuf($recno,mn_1)]
    append output [format "%6.2f "   $dbuf($recno,a_1)]
    append output [format "%6.2f "   $dbuf($recno,i_1)]
    append output [format "%6.2f "   $dbuf($recno,e_1)]
    append output [format "%2.0f\* " $dbuf($recno,mpf)]
    append output [format "%6.0f "   $dbuf($recno,mon)]
    append output [binary format A4  $dbuf($recno,mt)]

    return $output
}

proc DBufDisp { recno } {
    global dbufdisp dbuf dbuf_fields changed config

    set mtime [file mtime DBUFFER.BUF]
    if {$mtime != $dbuf(mtime)} {
        DBufReadFile
        DBufList
    }

    if { $config(paranoid)} { CheckBufChange }

    foreach fld $dbuf_fields {
        set dbufdisp($fld) $dbuf($recno,$fld)
    }
    set num [format "%2d" [expr $recno + 1]]
    set dbufdisp(label) "DBuffer No: $num"
    set dbufdisp(current) $recno
    # Fix up several fields

    DBufCalcAngles
    # clear changed flag
    set changed(dbufdisp) 0

    # switch focus to listbox so Return & arrowkeys work
    # and force selection (for 1st time)
    focus .l.list
    .l.list selection set $recno
}

proc DBufUpdate { } {
    global dbufdisp dbuf changed config

    if [catch {DBufCheckFld} result] {
        return -code error $result
    }

    set recno $dbufdisp(current)
    foreach fld [array names dbufdisp] {
        set dbuf($recno,$fld) $dbufdisp($fld)
    }

    # update display on the list
    .l.list delete $recno
    .l.list insert $recno [DBufFormatRec $recno]
    # update buffer information on disk
    DBufWriteFile
    set changed(dbufdisp) 0
}


proc DBufInit { } {
    global dbuf 
    array set dbuf {buftype Diffraction}
    for {set i 0} {$i < 30} {incr i} {
	set dbuf(${i},comment)  empty
	set dbuf(${i},lpn_1)    1
	set dbuf(${i},lpn_2)    1
	set dbuf(${i},lpn_3)    1
	set dbuf(${i},lpn_4)    1
	set dbuf(${i},lpn_5)    1
	set dbuf(${i},lpn_6)    1
	set dbuf(${i},lpts_1)   0
	set dbuf(${i},lpts_2)   0
	set dbuf(${i},lpts_3)   0
	set dbuf(${i},lpts_4)   0
	set dbuf(${i},lpts_5)   0
	set dbuf(${i},lpts_6)   0
	set dbuf(${i},mn_1)     0
	set dbuf(${i},mn_2)     0
	set dbuf(${i},mn_3)     0
	set dbuf(${i},mn_4)     0
	set dbuf(${i},mn_5)     0
	set dbuf(${i},mn_6)     0
	set dbuf(${i},a_1)      0
	set dbuf(${i},a_2)      0
	set dbuf(${i},a_3)      0
	set dbuf(${i},a_4)      0
	set dbuf(${i},a_5)      0
	set dbuf(${i},a_6)      0
	set dbuf(${i},i_1)      0
	set dbuf(${i},i_2)      0
	set dbuf(${i},i_3)      0
	set dbuf(${i},i_4)      0
	set dbuf(${i},i_5)      0
	set dbuf(${i},i_6)      0
	set dbuf(${i},e_1)      0
	set dbuf(${i},e_2)      0
	set dbuf(${i},e_3)      0
	set dbuf(${i},e_4)      0
	set dbuf(${i},e_5)      0
	set dbuf(${i},e_6)      0
	set dbuf(${i},xyz_1)    0
	set dbuf(${i},xyz_2)    0
	set dbuf(${i},xyz_3)    0
	set dbuf(${i},xyz_4)    0
	set dbuf(${i},xyz_5)    0
	set dbuf(${i},xyz_6)    0
	set dbuf(${i},a_xyz_1)  0
	set dbuf(${i},a_xyz_2)  0
	set dbuf(${i},a_xyz_3)  0
	set dbuf(${i},a_xyz_4)  0
	set dbuf(${i},a_xyz_5)  0
	set dbuf(${i},a_xyz_6)  0
	set dbuf(${i},i_xyz_1)  0
	set dbuf(${i},i_xyz_2)  0
	set dbuf(${i},i_xyz_3)  0
	set dbuf(${i},i_xyz_4)  0
	set dbuf(${i},i_xyz_5)  0
	set dbuf(${i},i_xyz_6)  0
	set dbuf(${i},e_xyz_1)  0
	set dbuf(${i},e_xyz_2)  0
	set dbuf(${i},e_xyz_3)  0
	set dbuf(${i},e_xyz_4)  0
	set dbuf(${i},e_xyz_5)  0
	set dbuf(${i},e_xyz_6)  0
	set dbuf(${i},pts)      1
	set dbuf(${i},hf)       0
	set dbuf(${i},phi)      0
	set dbuf(${i},psi)      0
	set dbuf(${i},phi_inc)  0
	set dbuf(${i},psi_inc)  0
	set dbuf(${i},tmp)      300
	set dbuf(${i},it)       0
	set dbuf(${i},tw)       0
	set dbuf(${i},th0)      0
	set dbuf(${i},ter)      0
	set dbuf(${i},mon)      1
	set dbuf(${i},mpf)      1
	set dbuf(${i},mt)       NEUT
	set dbuf(${i},flip1)    0
	set dbuf(${i},flip2)    0
	set dbuf(${i},flip3)    0
	set dbuf(${i},flip4)    0
	set dbuf(${i},ihf)      0
	set dbuf(${i},hfw)      0
	set dbuf(${i},hfh)      0
	set dbuf(${i},th)       0
    }
    set dbuf(currentrec)   0
    set dbuf(mtime)        0
}

proc DBufEntryCreate { p } {
    global dbuf dbufdisp config 

    # Create widgets
    label $p.bufnolabel -textvariable dbufdisp(label)
    label $p.commentlabel -width 8 -text "Comment:"
    entry $p.comment -textvariable dbufdisp(comment)
    label $p.lpn_l   -width 4 -text "Loop"
    label $p.mn_l    -width 3 -text "Mot"
    label $p.xyz_l   -width 3 -text "xyz"
    label $p.a_l     -width 7 -text "a_Beg"
    label $p.i_l     -width 7 -text "a_Inc"
    label $p.e_l     -width 7 -text "a_End"
    label $p.a_xyz_l -width 7 -text "xyz_Beg"
    label $p.i_xyz_l -width 7 -text "xyz_Inc"
    label $p.e_xyz_l -width 7 -text "xyz_End"
    label $p.lpts_l  -width 4 -text "\#pts"
    label $p.phi_l   -width 5 -text "phi"
    label $p.psi_l   -width 5 -text "psi"

    entry $p.lpn_1   -width 2 -textvariable dbufdisp(lpn_1)
    entry $p.mn_1    -width 2 -textvariable dbufdisp(mn_1)
    entry $p.xyz_1   -width 1 -textvariable dbufdisp(xyz_1)
    entry $p.a_1     -width 7 -textvariable dbufdisp(a_1)
    entry $p.i_1     -width 7 -textvariable dbufdisp(i_1)
    entry $p.e_1     -width 7 -textvariable dbufdisp(e_1)
    entry $p.a_xyz_1 -width 7 -textvariable dbufdisp(a_xyz_1)
    entry $p.i_xyz_1 -width 7 -textvariable dbufdisp(i_xyz_1)
    entry $p.e_xyz_1 -width 7 -textvariable dbufdisp(e_xyz_1)
    entry $p.lpts_1  -width 4 -textvariable dbufdisp(lpts_1)
    
    entry $p.phi     -width 5 -textvariable dbufdisp(phi)
    entry $p.psi     -width 5 -textvariable dbufdisp(psi)

    entry $p.lpn_2   -width 2 -textvariable dbufdisp(lpn_2)
    entry $p.mn_2    -width 2 -textvariable dbufdisp(mn_2)
    entry $p.xyz_2   -width 1 -textvariable dbufdisp(xyz_2)
    entry $p.a_2     -width 7 -textvariable dbufdisp(a_2)
    entry $p.i_2     -width 7 -textvariable dbufdisp(i_2)
    entry $p.e_2     -width 7 -textvariable dbufdisp(e_2)
    entry $p.a_xyz_2 -width 7 -textvariable dbufdisp(a_xyz_2)
    entry $p.i_xyz_2 -width 7 -textvariable dbufdisp(i_xyz_2)
    entry $p.e_xyz_2 -width 7 -textvariable dbufdisp(e_xyz_2)
    entry $p.lpts_2  -width 4 -textvariable dbufdisp(lpts_2)
    
    entry $p.lpn_3   -width 2 -textvariable dbufdisp(lpn_3)
    entry $p.mn_3    -width 2 -textvariable dbufdisp(mn_3)
    entry $p.xyz_3   -width 1 -textvariable dbufdisp(xyz_3)
    entry $p.a_3     -width 7 -textvariable dbufdisp(a_3)
    entry $p.i_3     -width 7 -textvariable dbufdisp(i_3)
    entry $p.e_3     -width 7 -textvariable dbufdisp(e_3)
    entry $p.a_xyz_3 -width 7 -textvariable dbufdisp(a_xyz_3)
    entry $p.i_xyz_3 -width 7 -textvariable dbufdisp(i_xyz_3)
    entry $p.e_xyz_3 -width 7 -textvariable dbufdisp(e_xyz_3)
    entry $p.lpts_3  -width 4 -textvariable dbufdisp(lpts_3)

    entry $p.lpn_4   -width 2 -textvariable dbufdisp(lpn_4)
    entry $p.mn_4    -width 2 -textvariable dbufdisp(mn_4)
    entry $p.xyz_4   -width 1 -textvariable dbufdisp(xyz_4)
    entry $p.a_4     -width 7 -textvariable dbufdisp(a_4)
    entry $p.i_4     -width 7 -textvariable dbufdisp(i_4)
    entry $p.e_4     -width 7 -textvariable dbufdisp(e_4)
    entry $p.a_xyz_4 -width 7 -textvariable dbufdisp(a_xyz_4)
    entry $p.i_xyz_4 -width 7 -textvariable dbufdisp(i_xyz_4)
    entry $p.e_xyz_4 -width 7 -textvariable dbufdisp(e_xyz_4)
    entry $p.lpts_4  -width 4 -textvariable dbufdisp(lpts_4)

    entry $p.lpn_5   -width 2 -textvariable dbufdisp(lpn_5)
    entry $p.mn_5    -width 2 -textvariable dbufdisp(mn_5)
    entry $p.xyz_5   -width 1 -textvariable dbufdisp(xyz_5)
    entry $p.a_5     -width 7 -textvariable dbufdisp(a_5)
    entry $p.i_5     -width 7 -textvariable dbufdisp(i_5)
    entry $p.e_5     -width 7 -textvariable dbufdisp(e_5)
    entry $p.a_xyz_5 -width 7 -textvariable dbufdisp(a_xyz_5)
    entry $p.i_xyz_5 -width 7 -textvariable dbufdisp(i_xyz_5)
    entry $p.e_xyz_5 -width 7 -textvariable dbufdisp(e_xyz_5)
    entry $p.lpts_5  -width 4 -textvariable dbufdisp(lpts_5)

    entry $p.lpn_6   -width 2 -textvariable dbufdisp(lpn_6)
    entry $p.mn_6    -width 2 -textvariable dbufdisp(mn_6)
    entry $p.xyz_6   -width 1 -textvariable dbufdisp(xyz_6)
    entry $p.a_6     -width 7 -textvariable dbufdisp(a_6)
    entry $p.i_6     -width 7 -textvariable dbufdisp(i_6)
    entry $p.e_6     -width 7 -textvariable dbufdisp(e_6)
    entry $p.a_xyz_6 -width 7 -textvariable dbufdisp(a_xyz_6)
    entry $p.i_xyz_6 -width 7 -textvariable dbufdisp(i_xyz_6)
    entry $p.e_xyz_6 -width 7 -textvariable dbufdisp(e_xyz_6)
    entry $p.lpts_6  -width 4 -textvariable dbufdisp(lpts_6)

    label $p.pts_l   -text "#Pts:"
    label $p.pts     -width 5 -anchor w -textvariable dbufdisp(pts) 

    label $p.templabel -text "Temp/H:"
    frame $p.temp
    label $p.temp.t0label -text "T0:"
    entry $p.temp.t0 -textvariable dbufdisp(tmp) -width 6
    label $p.temp.tinclabel -text "Inc-T:"
    entry $p.temp.tinc -textvariable dbufdisp(it) -width 6
    label $p.temp.twlabel -text "Wait:"
    entry $p.temp.tw -textvariable dbufdisp(tw) -width 6
    label $p.temp.terlabel -text "Err:"
    entry $p.temp.ter -textvariable dbufdisp(ter) -width 6
    label $p.temp.th0label -text "Hld0:"
    entry $p.temp.th0 -textvariable dbufdisp(th0) -width 6    
    button $p.temp.field -text FIELD -command DBufFieldDialog -state disabled
    pack $p.temp.t0label $p.temp.t0 $p.temp.tinclabel $p.temp.tinc \
            $p.temp.twlabel $p.temp.tw $p.temp.terlabel $p.temp.ter \
            $p.temp.th0label $p.temp.th0 $p.temp.field -side left

    label $p.timelabel -text "Time:"
    frame $p.time
    label $p.time.monlabel -text "Monitor:"
    entry $p.time.mon -textvariable dbufdisp(mon) -width 6
    label $p.time.mpflabel -text "Prefac:"
    entry $p.time.mpf -textvariable dbufdisp(mpf) -width 6
    label $p.time.mtlabel -text "M-typ:"
    tk_optionMenu $p.time.mt dbufdisp(mt) TIME NEUT
    pack $p.time.monlabel $p.time.mon $p.time.mpflabel $p.time.mpf \
            $p.time.mtlabel $p.time.mt -side left

    grid $p.bufnolabel $p.commentlabel - $p.comment - - - - - - - - - -sticky ew
    grid x $p.lpn_l $p.mn_l $p.xyz_l $p.a_l $p.i_l $p.e_l \
	$p.a_xyz_l $p.i_xyz_l $p.e_xyz_l $p.lpts_l $p.phi_l $p.psi_l 
    grid x $p.lpn_1 $p.mn_1 $p.xyz_1 $p.a_1 $p.i_1 $p.e_1 \
	$p.a_xyz_1 $p.i_xyz_1 $p.e_xyz_1 $p.lpts_1 $p.phi $p.psi 
    grid x $p.lpn_2 $p.mn_2 $p.xyz_2 $p.a_2 $p.i_2 $p.e_2 \
	$p.a_xyz_2 $p.i_xyz_2 $p.e_xyz_2 $p.lpts_2 
    grid x $p.lpn_3 $p.mn_3 $p.xyz_3 $p.a_3 $p.i_3 $p.e_3 \
	$p.a_xyz_3 $p.i_xyz_3 $p.e_xyz_3 $p.lpts_3 $p.pts_l $p.pts
    grid x $p.lpn_4 $p.mn_4 $p.xyz_4 $p.a_4 $p.i_4 $p.e_4 \
	$p.a_xyz_4 $p.i_xyz_4 $p.e_xyz_4 $p.lpts_4 
    grid x $p.lpn_5 $p.mn_5 $p.xyz_5 $p.a_5 $p.i_5 $p.e_5 \
	$p.a_xyz_5 $p.i_xyz_5 $p.e_xyz_5 $p.lpts_5 
    grid x $p.lpn_6 $p.mn_6 $p.xyz_6 $p.a_6 $p.i_6 $p.e_6 \
	$p.a_xyz_6 $p.i_xyz_6 $p.e_xyz_6 $p.lpts_6 
    grid $p.templabel   $p.temp   - - - - - -
    grid $p.timelabel   $p.time   - - - - - -


    bind $p.lpts_1 <Return> DBufCalcAngles
    bind $p.lpts_2 <Return> DBufCalcAngles
    bind $p.lpts_3 <Return> DBufCalcAngles
    bind $p.lpts_4 <Return> DBufCalcAngles
    bind $p.lpts_5 <Return> DBufCalcAngles
    bind $p.lpts_6 <Return> DBufCalcAngles

    bind $p.lpts_1 <FocusOut> DBufCalcAngles
    bind $p.lpts_2 <FocusOut> DBufCalcAngles
    bind $p.lpts_3 <FocusOut> DBufCalcAngles
    bind $p.lpts_4 <FocusOut> DBufCalcAngles
    bind $p.lpts_5 <FocusOut> DBufCalcAngles
    bind $p.lpts_6 <FocusOut> DBufCalcAngles

    bind $p.a_1 <Return> DBufCalcAngles
    bind $p.a_2 <Return> DBufCalcAngles
    bind $p.a_3 <Return> DBufCalcAngles
    bind $p.a_4 <Return> DBufCalcAngles
    bind $p.a_5 <Return> DBufCalcAngles
    bind $p.a_6 <Return> DBufCalcAngles

    bind $p.a_1 <FocusOut> DBufCalcAngles
    bind $p.a_2 <FocusOut> DBufCalcAngles
    bind $p.a_3 <FocusOut> DBufCalcAngles
    bind $p.a_4 <FocusOut> DBufCalcAngles
    bind $p.a_5 <FocusOut> DBufCalcAngles
    bind $p.a_6 <FocusOut> DBufCalcAngles

    bind $p.i_1 <Return> DBufCalcAngles
    bind $p.i_2 <Return> DBufCalcAngles
    bind $p.i_3 <Return> DBufCalcAngles
    bind $p.i_4 <Return> DBufCalcAngles
    bind $p.i_5 <Return> DBufCalcAngles
    bind $p.i_6 <Return> DBufCalcAngles

    bind $p.i_1 <FocusOut> DBufCalcAngles
    bind $p.i_2 <FocusOut> DBufCalcAngles
    bind $p.i_3 <FocusOut> DBufCalcAngles
    bind $p.i_4 <FocusOut> DBufCalcAngles
    bind $p.i_5 <FocusOut> DBufCalcAngles
    bind $p.i_6 <FocusOut> DBufCalcAngles
}

proc DBufCheckFld { } {
    global dbufdisp
    return
    set flds {a_1 a_2 a_3 a_4 a_5 a_6 \
	      i_1 i_2 i_3 i_4 i_5 i_6 \
	      a_xyz_1 a_xyz_2 a_xyz_3 a_xyz_4 a_xyz_5 a_xyz_6 \
	      i_xyz_1 i_xyz_2 i_xyz_3 i_xyz_4 i_xyz_5 i_xyz_6 \
	      lpts_1  lpts_2  lpts_3  lpts_4  lpts_5 lpts_6 \
	      hf tmp it tw th0 ter mon mpf ihf hfw hfh th }
    foreach fld $flds {
        # Check to make sure these are floating point numbers
        if [catch { expr 1.0 * $dbufdisp($fld) } result] {
            set dbufdisp($fld) "--------"
            return -code error "Parameter $fld must be floating point number"
        }
    }

    
}

proc DBufCalcAngles {{anglist {1 2 3 4 5 6}}} {
    global dbufdisp


    array set ploop { pts_1 0 pts_2 0 pts_3 0 pts_4 0 pts_5 0 pts_6 0 }
    set maxloop 1
    foreach i $anglist {
	if [catch {expr $dbufdisp(lpn_$i) * 1} dbufdisp(lpn_$i)] {
	    set dbufdisp(lpn_$i) 1
	    return
	}

	if [catch {expr $dbufdisp(lpts_$i) * 1} dbufdisp(lpts_$i)] {
	    set dbufdisp(lpts_$i) "------"
	    return
	}

	if {$dbufdisp(lpn_$i) < 1} { set dbufdisp(lpn_$i) 1 }
	if {$dbufdisp(lpn_$i) > 6} { set dbufdisp(lpn_$i) 6 }
	set iloop $dbufdisp(lpn_$i)
	# Probably not good enough
	if {$iloop > $maxloop} { set maxloop $iloop }
	#
	if { $ploop(pts_$iloop) == 0 } {
	    set ploop(pts_$i) $dbufdisp(lpts_$i)
	} else {
	    set dbufdisp(lpts_$i) $ploop(pts_$iloop)
	}
    }

    set pts 0
    for {set i 1} {$i <= $maxloop} {incr i} {
	if { $ploop(pts_$i) > 0 } {
	    if {$pts == 0} { 
		set pts $ploop(pts_$i)
	    } else {
		set pts [expr $pts * $ploop(pts_$i)]
	    }
	}
    }
    set dbufdisp(pts) $pts

    foreach i $anglist {
	set abeg "a_$i"
	set ainc "i_$i"
	set aend "e_$i"
	set xbeg "a_xyz_$i"
	set xinc "i_xyz_$i"
	set xend "e_xyz_$i"



	if [catch {expr $dbufdisp($abeg) * 1.0} ibufdisp($abeg)] {
	    set dbufdisp($abeg) "------"
	    set dbufdisp($aend) "------"
	    return
	}
	if [catch {expr $dbufdisp($ainc) * 1.0} dbufdisp($ainc)] {
	    set dbufdisp($ainc) "------"
	    set dbufdisp($aend) "------"
	    return
	}
	if [catch {expr $dbufdisp($xbeg) * 1.0} ibufdisp($xbeg)] {
	    set dbufdisp($xbeg) "------"
	    set dbufdisp($xend) "------"
	    return
	}
	if [catch {expr $dbufdisp($xinc) * 1.0} dbufdisp($xinc)] {
	    set dbufdisp($xinc) "------"
	    set dbufdisp($xend) "------"
	    return
	}
	set dbufdisp($aend) [expr $dbufdisp($abeg) + \
		$dbufdisp($ainc) * ($dbufdisp(lpts_$i) - 1)]
	set dbufdisp($xend) [expr $dbufdisp($xbeg) + \
		$dbufdisp($xinc) * ($dbufdisp(lpts_$i) - 1)]
    }
    update
}


#==============================================================================

proc BufopEntryCreate { parent } {
    global bufop text_only
    set cl [label $parent.clab -text "Command:" -anchor w]
    set ce [entry $parent.cent -textvariable bufop(command) -width 30]
    set rt [label $parent.result  -height 6 -anchor nw]
    set ex [button $parent.dismiss -text "Return" -command BufopHide]

    if {!$text_only} {
       $rt configure -font TkFixedFont -relief sunken -justify left
    }

    grid $cl $ce -sticky ew
    grid $rt -   -sticky ew
    grid $ex -   -sticky ew
    set bufop(msgwid) $rt

    # Setup bindings
    bind $ce <KeyPress-Return> BufopAction

}

proc BufopShow { } {
    global bufop
    notebook_display .e bufop
}

proc BufopHide { } {
    global mode bufop ibufdisp bbufdisp tbufdisp qbufdisp rbufdisp dbufdisp
    switch $mode {
	bragg {
	    BBufDisp $bbufdisp(current)
	    notebook_display .e bbuf
	}
	increment {
	    IBufDisp $ibufdisp(current)
	    notebook_display .e ibuf
	}
	q {
	    QBufDisp $qbufdisp(current)
	    notebook_display .e qbuf
	}
	trash {
	    TBufDisp $tbufdisp(current)
	    notebook_display .e tbuf
	}
	reflectivity {
	    RBufDisp $rbufdisp(current)
	    notebook_display .e rbuf
	}
	diffraction {
	    DBufDisp $dbufdisp(current)
	    notebook_display .e dbuf
	}
    }
}

proc BufopMsg { msg } {
    global bufop
    $bufop(msgwid) configure -text $msg
}

#
# Parse bufop(command) and take the appropriate action
#
proc BufopAction { } {
    global bufop mode ibuf tbuf qbuf rbuf dbuf bbuf ibufdisp tbufdisp qbufdisp rbufdisp dbufdisp bbufdisp
    set bufop(command) [string tolower $bufop(command)]

    if [regexp {copy} $bufop(command)] {
	return [BufopCopy]
    }

    if {![regexp = $bufop(command)]} {
	BufopMsg "Syntax: parameter = value"
    }
    set parts [split $bufop(command) "="]
    set parm [string trimleft [string trimright [lindex $parts 0]]]
    set val  [lindex $parts 1]

    # First test to see if parameter exists in hash
    switch $mode {
	bragg {
	    set exists [info exists bbuf(0,$parm)]
	}
	increment {
	    set exists [info exists ibuf(0,$parm)]
	}
	trash {
	    set exists [info exists tbuf(0,$parm)]
	}
	q {
	    set exists [info exists qbuf(0,$parm)]
	}
	reflectivity {
	    set exists [info exists rbuf(0,$parm)]
	}
	diffraction {
	    set exists [info exists dbuf(0,$parm)]
	}
	default {
	    set exists 0
	}
    }
    if {!$exists} {
	BufopMsg "Parameter $parm does not exist\n in $mode buffers"
	return
    }

    # Take care of "special" syntax
    if {[string compare $parm "mt"] == 0} {
	if [regexp n $val] {
	    set val NEUT
	} else {
	    set val TIME
	}
    }

    # Set values
    for {set i 0} {$i < 30} {incr i} {
	switch $mode {
	    bragg {
		set bbuf($i,$parm) $val
	    }
	    increment {
		set ibuf($i,$parm) $val
	    }
	    trash {
		set tbuf($i,$parm) $val
	    }
	    q {
		set qbuf($i,$parm) $val
	    }
	    reflectivity {
		set rbuf($i,$parm) $val
	    }
	    diffraction {
		set dbuf($i,$parm) $val
	    }
	}
    }
    BufopMsg "Updating buffers"
    # Now update the buffers and displays
    #### Make sure we update the entry mask as well!!!
    switch $mode {
	bragg {
	    BBufWriteFile
	    BBufList
	}
	increment {
	    IBufWriteFile
	    IBufList
	}
	q {
	    QBufWriteFile
	    QBufList
	}
	trash {
	    TBufWriteFile
	    TBufList
	}
	reflectivity {
	    RBufWriteFile
	    RBufList
	}
	diffraction {
	    DBufWriteFile
	    DBufList
	}
    }
    after 500 {BufopMsg ""}
}

#
# Syntax: copy n1-n2,m - copy buffers n1 to n2 to buffers m to m+(n2-n1)
#         need to know which buffers are active
#
#
proc BufopCopy { } {
    global mode bufop ibufdisp bbufdisp qbufdisp rbufdisp tbufdisp dbufdisp
    
    if {![regexp {copy ([^,]+),(.+)} $bufop(command) match src trg]} {
	BufopMsg "Syntax:\ncopy src,trg"
    }

    # Parse source string - possibly fragile
    if {[regexp {([0-9]+)-([0-9]+)} $src match srclo srchi]} {
	set srclist {}
	for {set i $srclo} {$i <= $srchi} {incr i} {
	    lappend srclist $i
	}
    } else {
	set srclist [expr $src * 1]
    }

    foreach s $srclist {
	#puts "copying $s -> $trg"
	if { $trg <= 30 } {
	    BufopTransfer [expr $s - 1] [expr $trg - 1]
	}
	incr trg
    }
    # Update buffers and displays
    BufopMsg "Updating buffers"
    # Now update the buffers and displays
    switch $mode {
	bragg {
	    BBufWriteFile
	    BBufDisp $bbufdisp(current)
	    BBufList
	}
	increment {
	    IBufWriteFile
	    IBufDisp $ibufdisp(current)
	    IBufList
	}
	q {
	    QBufWriteFile
	    QBufDisp $qbufdisp(current)
	    QBufList
	}
	trash {
	    TBufWriteFile
	    TBufDisp $tbufdisp(current)
	    TBufList
	}
	reflectivity {
	    RBufWriteFile
	    RBufDisp $rbufdisp(current)
	    RBufList
	}
	diffraction {
	    DBufWriteFile
	    DBufDisp $dbufdisp(current)
	    DBufList
	}
    }
    after 500 {BufopMsg ""}
}

proc BufopTransfer { src trg } {
    global mode ibuf tbuf qbuf rbuf bbuf dbuf ibuf_fields qbuf_fields tbuf_fields rbuf_fields bbuf_fields dbuf_fields

    switch $mode {
	bragg {
	    foreach fld $bbuf_fields {
		set bbuf($trg,$fld) $bbuf($src,$fld)
	    }
	}
	increment {
	    foreach fld $ibuf_fields {
		set ibuf($trg,$fld) $ibuf($src,$fld)
	    }
	}
	q {
	    foreach fld $qbuf_fields {
		set qbuf($trg,$fld) $qbuf($src,$fld)
	    }
	}
	trash {
	    foreach fld $tbuf_fields {
		set tbuf($trg,$fld) $tbuf($src,$fld)
	    }
	}
	reflectivity {
	    foreach fld $rbuf_fields {
		set rbuf($trg,$fld) $rbuf($src,$fld)
	    }
	}
	diffraction {
	    foreach fld $dbuf_fields {
		set dbuf($trg,$fld) $dbuf($src,$fld)
	    }
	}
    }
}

proc MsgShow { args } {
    global msgpane
    if {[llength $args] > 0} {
	$msgpane configure -text [lindex $args 0]
    }
    notebook_display .e msg
}

proc MsgHide { } {
    global mode 
    switch $mode {
	bragg {
	    notebook_display .e bbuf
	}
	increment {
	    notebook_display .e ibuf
	}
	q {
	    notebook_display .e qbuf
	}
	trash {
	    notebook_display .e tbuf
	}
	reflectivity {
	    notebook_display .e rbuf
	}
	diffraction {
	    notebook_display .e dbuf
	}
    }
}

#
# Check to see whether the buffer file has changed on the disk under us
#
proc BufChangeCheck { } {
    global mode ibuf ibufdisp qbuf qbufdisp tbuf tbufdisp bbuf bbufdisp \
	    rbuf rbufdisp dbuf dbufdisp changed text_only

    switch $mode {
	increment {
	    set mtime [file mtime IBUFFER.BUF]
	    if { $mtime != $ibuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		IBufReadFile
		IBufList
		IBufDisp $ibufdisp(current)
#		set changed(ibufdisp) 0
		return 0
	    } 
	    return 1
	}
	q {
	    set mtime [file mtime QBUFFER.BUF]
	    if { $mtime != $qbuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		QBufReadFile
		QBufList
		QBufDisp $qbufdisp(current)
		set changed(qbufdisp) 0
		return 0
	    } 
	    return 1
	}
	bragg {
	    set mtime [file mtime BBUFFER.BUF]
	    if { $mtime != $bbuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		BBufReadFile
		BBufList
		BBufDisp $bbufdisp(current)
		set changed(bbufdisp) 0
		return 0
	    } 
	    return 1	    
	}
	trash {
	    set mtime [file mtime TBUFFER.BUF]
	    if { $mtime != $tbuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		TBufReadFile
		TBufList
		TBufDisp $tbufdisp(current)
		set changed(tbufdisp) 0
		return 0
	    } 
	    return 1
	}
	reflectivity {
	    set mtime [file mtime RBUFFER.BUF]
	    if { $mtime != $rbuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		RBufReadFile
		RBufList
		RBufDisp $rbufdisp(current)
		set changed(rbufdisp) 0
		return 0
	    } 
	    return 1
	}
	diffraction {
	    set mtime [file mtime DBUFFER.BUF]
	    if { $mtime != $dbuf(mtime) } {
              if {!$text_only} {
		set save [tk_dialog .change "Buffer Changed!" \
		   "The Buffer file has changed on the disk. Save anyway?" \
		   error 0 No Yes]
              } else {
                set save [ck_dialog .change "Buffer Changed!" \
                   "The Buffer file has changed on the disk. Save anyway?" \
                   No Yes]
              }
		if { $save } { return 1 }
		DBufReadFile
		DBufList
		DBufDisp $dbufdisp(current)
		set changed(dbufdisp) 0
		return 0
	    } 
	    return 1
	}
    }
    return 1
}

proc CheckBufChange {} {
    global changed mode text_only

    if {![info exists mode]} { return }

    switch $mode {
	increment { set buf ibufdisp }
	q         { set buf qbufdisp }
	bragg     { set buf bbufdisp }
	trash     { set buf tbufdisp }
	reflectivity { set buf rbufdisp }
	diffraction  { set buf dbufdisp }
	default return
    }
    #puts $buf
    upvar #0 $buf bufdisp

    if {$changed($buf)} {
      if {!$text_only} {
	set save [tk_dialog .change "Buffer Changed!" \
		"$mode Buffer [expr 1+$bufdisp(current)] has been edited, Save?" \
		warning 0 Yes No]
      } else {
        set save [ck_dialog .change "Buffer Changed!" \
                "$mode Buffer [expr 1+$bufdisp(current)] has been edited, Save?" \
                Yes No]
      }
	if { ! $save } { UpdateBuf }
    }
    set changed($buf) 0 ;# Reset changed flag
}

proc UpdateBuf { } {
    global mode
    set finish [BufChangeCheck]
    if { !$finish } { return }
    MonRecCheck
    switch $mode {
	increment {
	    MsgShow "Updating Buffer"
	    if [catch {IBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
	q {
	    MsgShow "Updating Buffer"
	    if [catch {QBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
	bragg {
	    MsgShow "Updating Buffer"
	    if [catch {BBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
	trash {
	    MsgShow "Updating Buffer"
	    if [catch {TBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
	reflectivity {
	    MsgShow "Updating Buffer"
	    if [catch {RBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
	diffraction {
	    MsgShow "Updating Buffer"
	    if [catch {DBufUpdate} result] {
		MsgShow $result
		return
	    }
	    after 500 MsgHide
	}
    }
}

proc MonRecDialogBuild { } {
    global monrec config text_only

    set p [toplevel .datainfo]
    set monrec(top) $p

    if $text_only {
	set color white
    } else {
	set color gray
	wm title $p "Log Sample"
	wm withdraw $p
    }
    
    label $p.userlab -text "Experimenters: "
    entry $p.user -textvariable monrec(user) -width 60
    label $p.samplab -text "Sample:"
    entry $p.samp -textvariable monrec(sample) -width 30
    label $p.quanlab -text "Quantity (g):"
    entry $p.quan -textvariable monrec(quantity) -width 8
    label $p.powerlab -text "Reactor Power: "
    entry $p.power -textvariable monrec(power) -width 8
    label $p.beamhlab -text "Beam Height: " 
    entry $p.beamh -textvariable monrec(beamh) -width 8
    label $p.beamwlab -text "Beam Width:"
    entry $p.beamw -textvariable monrec(beamw) -width 8
    label $p.monolab -text "Monochromator: "
    entry $p.mono -textvariable monrec(mono) -width 8
    label $p.analab -text "Analyzer:"
    entry $p.ana -textvariable monrec(ana) -width 8
    label $p.filtlab -text "Filter:" 
    entry $p.filt -textvariable monrec(filter) -width 8
    label $p.colllab -text "Collimation:"
    entry $p.coll -textvariable monrec(coll) -width 30
    label $p.efixlab -text "Fixed Energy (meV): "
    entry $p.efix -textvariable monrec(efix) -width 8
    
    button $p.finish -text OK -command MonRecWrite

    # Customizations
    if { $config(nsta) == 1 } {
	$p.powerlab config -fg $color
	$p.power config    -fg $color -state disabled
	$p.beamhlab config -fg $color
	$p.beamh config    -fg $color -state disabled
	$p.monolab config  -fg $color
	$p.mono config     -fg $color -state disabled
	$p.analab config   -fg $color
	$p.ana config      -fg $color -state disabled
	$p.filtlab config  -fg $color
	$p.filt config     -fg $color -state disabled
	$p.colllab config  -fg $color
	$p.coll config     -fg $color -state disabled
	$p.efixlab config  -fg $color
	$p.efix config     -fg $color -state disabled
    }

    grid $p.userlab $p.user - - - - -sticky w
    grid $p.samplab $p.samp - - $p.quanlab $p.quan -sticky w
    grid $p.powerlab $p.power $p.beamhlab $p.beamh $p.beamwlab $p.beamw \
	    -sticky w
    grid $p.monolab $p.mono $p.analab $p.ana $p.filtlab $p.filt -sticky w
    grid $p.colllab $p.coll - - $p.efixlab $p.efix -sticky w
    if {!$text_only} {
	grid x x $p.finish - -sticky ew
	wm protocol $p WM_DELETE_WINDOW "$p.finish invoke"
    } else {
	grid $p.finish - -sticky ew
    }

}

proc MonRecDialogShow { } {
    global monrec config text_only
    if $text_only {
       catch { destroy $monrec(top) }
       MonRecDialogBuild
    }
    set monrec(mono) $config(mono)
    if {!$text_only} {
       # center the dialog in the middle of the main
       set x [expr [winfo x .] + [winfo width .]/2 - \
   		[winfo reqwidth $monrec(top)]/2 - [winfo vrootx .]]
       set y [expr [winfo y .] + [winfo height .]/2 - \
   	    [winfo reqheight $monrec(top)]/2 - [winfo vrooty .]]
       wm geom $monrec(top) +$x+$y
       wm transient $monrec(top) .
       wm deiconify $monrec(top)
       # set the grab
       grab $monrec(top)
    } else {
       place $monrec(top) -relheight 1.0 -relwidth 1.0
       focus $monrec(top)
    }
}

proc MonRecDialogHide { } {
    global monrec text_only
    if {!$text_only} {
       # reset the grab
       grab .
       wm withdraw $monrec(top) 
    } else {
       place forget $monrec(top)
       focus .
       destroy $monrec(top)
       .l.list selection set 0 0
    }
}

proc MonRecWrite { } {
    global mode monrec

    set vaxformat "%d-%b-%Y %H:%M"
    set ctformat  "%b %d %Y %H:%M"

    set time [clock format [clock seconds] -format $ctformat]
    if [catch {open $monrec(mfile) a+} file] { 
	puts stderr "Can't open $monrec(mfile)"
	MonRecDialogHide
	return 
    }
    puts $file " $time [pwd] $monrec(prefix)"
    puts $file "    Experimenters: $monrec(user)"
    puts $file "    Sample: $monrec(sample)\t\tQuantity (g): $monrec(quantity)"
    puts $file "    Reactor Power: $monrec(power)\tBeam Height (in): $monrec(beamh)\tWidth: $monrec(beamw)"
    puts $file "    Monochromator: $monrec(mono)\tAnalyzer: $monrec(ana)\tFilter: $monrec(filter)"
    puts $file "    Collimator: $monrec(coll)\t\tFixed Energy (meV): $monrec(efix)"
    close $file

    MonRecDialogHide
}

proc MonRecCheck { } {
    global mode monrec 
    switch $mode {
	increment { set p i }
	q {         set p q }
	trash {     set p t }
	default     return
    }

    global ${p}buf ${p}bufdisp

    set prefix [string range [set ${p}bufdisp(comment)] 0 4]
    # validate prefix
    while {![regexp {[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]} $prefix]} {
	set prefix [GetNewPrefix $prefix]
	set ${p}bufdisp(comment) "${prefix}[string range [set ${p}bufdisp(comment)] 5 end]"
    }
    for { set i 0} { $i < 30 } {incr i} {
	lappend preflist [string range [set ${p}buf(${i},comment)] 0 4]
    }
    if {[lsearch $preflist $prefix] < 0} {
	set monrec(prefix) $prefix
	return [MonRecDialogShow]
    } else {
        global text_only
        if $text_only {
            .l.list selection set 0 0
        }
    }
}

#=Eval entry dialog============================================================

proc EvalEntryCreate { parent } {
    global evalbox text_only

    set evalbox(parent) $parent
    set evalbox(current) ""
    set evalbox(result) ""

    set el [label $parent.label -text "Command:"]
    set ee [entry $parent.entry -textvariable evalbox(current) -width 50]

    set er [label $parent.result -textvariable evalbox(result) \
	    -height 8 -width 60 -anchor nw]
    set ex [button $parent.dismiss -text "Return" -command EvalHide]

    if {!$text_only} {
       $er configure -font TkFixedFont -relief sunken
    }

    grid $el $ee -sticky ew
    grid $er -   -sticky ew
    grid $ex -   -sticky ew    


    bind $ee <Return> EvalAction
    return $parent
}

proc EvalAction { } {
    global evalbox
    if {[string length [string trimright $evalbox(current)]] != 0} {
	catch {uplevel #0 eval $evalbox(current)} evalbox(result)
    }
    set evalbox(current) ""
}

proc EvalShow { } {
    global evalbox
    notebook_display .e evalbox
}

proc EvalHide { } {
    global mode ibufdisp tbufdisp qbufdisp
    switch $mode {
	bragg {
	    BBufDisp $ibufdisp(current)
	    notebook_display .e bbuf
	}
	increment {
	    IBufDisp $ibufdisp(current)
	    notebook_display .e ibuf
	}
	q {
	    QBufDisp $qbufdisp(current)
	    notebook_display .e qbuf
	}
	trash {
	    TBufDisp $tbufdisp(current)
	    notebook_display .e tbuf
	}
    }
}

#=Automon Dialog===============================================================

proc AutomonInit { } {
    global automon env config

    set automon(file) "$config(root)/cfg/HWLONG.BUF"
    set automon(cps)  500
    set automon(hwlconst) 1.0
    set automon(mon)      ?
    set automon(pre)      1
    set automon(base) duration
    set automon(cps)     500
    set automon(savedcps) 500
    set automon(weekday) Sun
    set automon(hours)   1
    set automon(hh)      00
    set automon(mm)      00
    set automon(usewait) 0
    set automon(usehold) 0
    
}

proc AutomonReadFile { } {
    global automon
    if {![file exists $automon(file)]} { return }
    set f [open $automon(file) r]
    fconfigure $f -encoding binary
    set input [read $f 20]
    binary scan $input fffif dlast monang cps cnstover hwlconst
    # Trim values to reasonable precision
#    set automon(cps)      [format %.2f $cps]
    set automon(savedcps) [format %.2f $cps]
    set automon(hwlconst) [format %.1f $hwlconst]
    return
}

proc AutomonBuild { } {
    global automon ibufdisp ibuf text_only

    AutomonInit
    set p [toplevel .automon]
    set automon(top) $p

    if {!$text_only} {
       wm withdraw $automon(top)
    }

    set pm [frame $p.mon]
    label $pm.1 -text "Monitor computation settings"
    label $pm.2 -text "MRAT (cps)"
    entry $pm.cps -textvariable automon(cps)  -width 10
    label $pm.3 -text "ovrhd (sec/pt)"
    entry $pm.hwl -textvariable automon(hwlconst)  -width 5
    grid $pm.1 -columnspan 4
    if $text_only {
       label $pm.blank -text " "
       grid $pm.blank
    }
    grid $pm.2 $pm.cps $pm.3 $pm.hwl

    # Perhaps make this a global option
    global config
    if {$config(nsta) == 1} {
	label $pm.4 -text "Set MRAT to"
        if $text_only {
           set menu [ck_optionMenu $pm.5 automon(amonlbl) junk]
        } else {
           set menu [tk_optionMenu $pm.5 automon(amonlbl) junk]
        }
	$menu delete 0 end
	foreach var $config(instr_list) {
	    $menu add command -label "$var default" \
		    -command \
		    "set automon(amonlbl) $var;\
		     set automon(cps) $config(cps-$var);\
		     set config(instr) $var;\
		     ConfigChange";
	}

	$menu add command -label "last saved MRAT" \
		-command "set automon(amonlbl) {(last saved MRAT)};\
		          AutomonReadFile; set automon(cps) $automon(savedcps)"
	set automon(amonlbl) "(last saved MRAT)"
	grid $pm.4 $pm.5
	grid $pm.4 -sticky e
	grid $pm.5 -sticky w -columnspan 3
    }
    
    set pv1 [frame $p.vars1 -relief groove -bd 4]
    set pv2 [frame $p.vars2]
    checkbutton $pv1.wlab -text Wait -variable automon(usewait)
    checkbutton $pv1.hlab -text Hold -variable automon(usehold)
    entry $pv1.wait -textvariable ibufdisp(tw)  -width 6
    entry $pv1.hold -textvariable ibufdisp(th0) -width 6
    label $pv1.lab -text "Control Delays "
    label $pv2.mlab -text "Monitor:"
    label $pv2.plab -text Prefactors

    label $pv2.mon  -textvariable automon(mon)  -width 10 \
	    -bg lightyellow -justify center -anchor center
    entry $pv2.pre  -textvariable automon(mpf)  -width 4
    button $pv2.compute -text Compute -command AutomonCompute

    set pb [frame $p.base]
    radiobutton $pb.dlab -text "Scan duration (hrs):" -value duration \
	    -variable automon(base)
    radiobutton $pb.elab -text "End at time:"   -value endtime \
	    -variable automon(base)
    entry $pb.hours -textvariable automon(hours) -width 7


    if $text_only {
	ck_optionMenu $pb.weekday automon(weekday) Sun Mon Tue Wed Thu Fri Sat
	ck_optionMenu $pb.hh automon(hh) 00 01 02 03 04 05 06 07 08 09 10 11 \
		12 13 14 15 16 17 18 19 20 21 22 23
	label $pb.colon -text :
	ck_optionMenu $pb.mm automon(mm) 00 05 10 15 20 25 30 35 40 45 50 55
    } else {
	$p.mon configure -relief groove -bd 4
	$pv2.mon configure -relief sunken
	tk_optionMenu $pb.weekday automon(weekday) Sun Mon Tue Wed Thu Fri Sat
	tk_optionMenu $pb.hh automon(hh) 00 01 02 03 04 05 06 07 08 09 10 11 \
		12 13 14 15 16 17 18 19 20 21 22 23
	label $pb.colon -text :
	tk_optionMenu $pb.mm automon(mm) 00 05 10 15 20 25 30 35 40 45 50 55
    }

    set pB [frame $p.buttons]
    button $pB.ok -text OK -command "AutomonCompute; AutomonApply; AutomonHide"
    button $pB.cancel  -text Cancel  -command AutomonHide
    wm protocol $p WM_DELETE_WINDOW "$pB.cancel invoke"

    grid $pv1.lab $pv1.wlab $pv1.wait $pv1.hlab $pv1.hold
    grid $pv2.plab $pv2.pre $pv2.mlab $pv2.mon $pv2.compute
    grid $pv2.mon -sticky ew
 
    grid $pb.dlab $pb.hours                           -in $pb -sticky w
    grid $pb.elab $pb.weekday $pb.hh $pb.colon $pb.mm -in $pb -sticky w

    grid $pB.ok $pB.cancel -row 1

    pack $pm $pv1 $pv2 $pb $pB -side top -expand true -fill x -anchor w
}

proc AutomonShow { } {
    global automon text_only ibufdisp config
    AutomonReadFile
    if $text_only {
       catch { destroy $automon(top) }
       AutomonBuild
       place $automon(top) -relheight 1.0 -relwidth 1.0
       focus $automon(top)
    } else {
       # center the dialog in the middle of the main
       set x [expr [winfo x .] + [winfo width .]/2 - \
   		[winfo reqwidth $automon(top)]/2 - [winfo vrootx .]]
       set y [expr [winfo y .] + [winfo height .]/2 - \
   	    [winfo reqheight $automon(top)]/2 - [winfo vrooty .]]
       wm geom $automon(top) +$x+$y
       wm transient $automon(top) .
       wm deiconify $automon(top) 
       if {$config(mode) == 0} {set mode 0} else {set mode 1}
           set pv1 .automon.vars1
           DisableWidget "$pv1.lab $pv1.wlab $pv1.wait $pv1.hlab $pv1.hold" $mode
           # if this is BT-1 and we are using the MRAT/S value, update to the latest value
           if {$config(nsta) == 1 && $automon(amonlbl) == {(last saved MRAT)}} {
	       AutomonReadFile
	       set automon(cps) $automon(savedcps)
           }
           # round up invalid prefactor value, default is 4
           set automon(mpf) $ibufdisp(mpf)
           set automon(mpf) [expr int($automon(mpf)+0.5)]
           if {$automon(mpf) <= 0} {set automon(mpf) 4}
       grab $automon(top)
    }
}

proc AutomonHide { } {
    global automon text_only

    if $text_only {
       place forget $automon(top)
       focus .
       destroy $automon(top)
       .l.list selection set 0 0
    } else {
       grab release $automon(top)
       wm withdraw $automon(top) 
    }
}

proc AutomonCompute { } {
    global automon ibufdisp

    #AutomonReadFile
    switch $automon(base) {
	endtime {
	    set curtime [clock seconds]
	    set endtime [clock scan \
		    "$automon(weekday) $automon(hh):$automon(mm)"]
	    set total_t [expr $endtime - $curtime]
	}
	default {
	    # duration
	    set total_t [expr 3600 * $automon(hours)]
	}
    }
    
    if {$automon(mpf) <= 0} { set ibufdisp(mpf) 1}

    # Now calculate how much overhead to subtract
    # Do it cheap and dirty for now - assume a constant monitor rate

    # Subtract hold 
    if {$automon(usehold)} {
	set t_ho $ibufdisp(th0)
    } else {
	set t_ho 0
    }
    
    # Subtract hardware overhead
    set t_hw [expr $ibufdisp(pts) * $automon(hwlconst)]
    
    # Subtract wait/pt
    if {$automon(usewait)} {
	set t_tw [expr $ibufdisp(tw) * $ibufdisp(pts)]
    } else {
	set t_tw 0
    }

    set delta_t [expr $total_t - $t_ho - $t_hw - $t_tw]
    set t_pt    [expr $delta_t / $ibufdisp(pts)]
        
    # Get time/prefactor
    set t_pre   [expr $t_pt / $automon(mpf)]
    

    switch $ibufdisp(mt) {
	NEUT {
	    set automon(mon) [expr floor($t_pre * $automon(cps))]
	}
	default {
	    set automon(mon) [expr floor($t_pre)]
	}
    }

}

# Apply monitor value to buffer in question
#
proc AutomonApply { } {
    global automon ibufdisp

    set ibufdisp(mpf) $automon(mpf)
    set ibufdisp(mon) $automon(mon)
    update
    
}

#=Sequence Editor==============================================================

proc SequenceRead { } {
    global env sequence ibuf

    if {![file exists $sequence(file)]} {
	return
    }

    if [file exists IBUFFER.BUF] {
	set f [open IBUFFER.BUF r+]
	fconfigure $f -encoding binary
	seek $f [expr 320 * 30] start
	set contents [read $f 320]
	binary scan $contents A320 contents
	set contents [string trimright $contents]
	set ibuf(sequence) $contents
	close $f
    } else {
	set contents {}
    }

    $sequence(list) delete 0 end
    regsub {#$} $contents {} contents
    foreach s [split $contents \;] {
	$sequence(list) insert end $s
    }
}

proc SequenceWrite { } {
    global ibuf sequence

    set contents [join [$sequence(list) get 0 end] ";"]
    append contents {#}
    set ibuf(sequence) $contents

    if [file exists IBUFFER.BUF] {
	set f [open IBUFFER.BUF r+]
	fconfigure $f -encoding binary
	seek $f [expr 320 * 30] start
	puts -nonewline $f [binary format A320 $ibuf(sequence)]
	close $f
    }
}

proc SequenceEditorBuild { parent } {
    global env sequence text_only

    # initialize array sequence
    set sequence(run)  1
    set sequence(wait) 1
    set sequence(temp) 300.0
    set sequence(cmd)  "NEXT"
    set sequence(file) IBUFFER.BUF
    set sequence(toplevel) [toplevel $parent]

    if {!$text_only} {

    set sequence(up) [image create bitmap -data {
	#define up_width 24
	#define up_height 24
	static unsigned char up_bits[] = {
	    0x00, 0x18, 0x00, 0x00, 0x18, 0x00, 
	    0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
	    0x00, 0x7e, 0x00, 0x00, 0x7e, 0x00, 
	    0x00, 0xff, 0x00, 0x00, 0xff, 0x00,
	    0x80, 0xff, 0x01, 0x80, 0xff, 0x01, 
	    0xc0, 0xff, 0x03, 0xc0, 0xff, 0x03,
	    0xe0, 0xff, 0x07, 0xe0, 0xff, 0x07, 
	    0xf0, 0xff, 0x0f, 0xf0, 0xff, 0x0f,
	    0xf8, 0xff, 0x1f, 0xf8, 0xff, 0x1f, 
	    0xfc, 0xff, 0x3f, 0xfc, 0xff, 0x3f,
	    0xfe, 0xff, 0x7f, 0xfe, 0xff, 0x7f, 
	    0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
    }]

    set sequence(down) [image create bitmap -data {
	#define down_width 24
	#define down_height 24
	static unsigned char down_bits[] = {
	    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
	    0xfe, 0xff, 0x7f, 0xfe, 0xff, 0x7f,
	    0xfc, 0xff, 0x3f, 0xfc, 0xff, 0x3f, 
	    0xf8, 0xff, 0x1f, 0xf8, 0xff, 0x1f,
	    0xf0, 0xff, 0x0f, 0xf0, 0xff, 0x0f,
	    0xe0, 0xff, 0x07, 0xe0, 0xff, 0x07,
	    0xc0, 0xff, 0x03, 0xc0, 0xff, 0x03,
	    0x80, 0xff, 0x01, 0x80, 0xff, 0x01,
	    0x00, 0xff, 0x00, 0x00, 0xff, 0x00,
	    0x00, 0x7e, 0x00, 0x00, 0x7e, 0x00,
	    0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
	    0x00, 0x18, 0x00, 0x00, 0x18, 0x00};
    }]

    wm withdraw $sequence(toplevel)

    }

    set tf [frame $parent.top]
    
    set sequence(list) [ScrolledListbox $tf.l -width 20 -height 8]

    set f [frame $tf.f1]
    set frl [label $f.runlab    -text " Run "]
    set ftl [label $f.templab   -text " Temperature "]
    set fwl [label $f.waitlab   -text " Wait "]
    set fcl [label $f.cmdlab    -text " Command "]
    set fru [label $f.rununits  -text "(Buffer) "]
    set ftu [label $f.tempunits -text "(K) "]
    set fwu [label $f.waitunits -text "(Min) "]
    set fcu [label $f.cmdunits  -text " "]
    set fre $f.runentry
    if $text_only {
       ck_optionMenu $fre sequence(run) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 \
            16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
    } else {
       $tf.f1 configure -bd 2 -relief ridge
       tk_optionMenu $fre sequence(run) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 \
	    16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
    }
    set fte [entry $f.tempentry -width 10 -textvariable sequence(temp)]
    set fwe [entry $f.waitentry -width 10 -textvariable sequence(wait)]
    set fce [entry $f.cmdentry  -width 10 -textvariable sequence(cmd)]
    set fri [button $f.runbut   -text Insert -command SequenceInsertRun]
    set fti [button $f.tempbut  -text Insert -command SequenceInsertTemp]
    set fwi [button $f.waitbut  -text Insert -command SequenceInsertWait]
    set fci [button $f.cmdbut   -text Insert -command SequenceInsertCmd]

    if $text_only {
       label $f.title -text "Sequence Edit"
       label $f.blank -text " "
       grid $f.title -sticky ew -columnspan 4
       grid $f.blank
    }

    grid $frl $fru $fre $fri -sticky w
    grid $fre -sticky we
    grid $ftl $ftu $fte $fti -sticky w
    grid $fwl $fwu $fwe $fwi -sticky w
    grid $fcl $fcu $fce $fci -sticky w

    set bf  [frame $parent.bot]
    set fu  [button $bf.up \
	    -command SequenceRaiseCommand]
    set fl  [button $bf.down \
	    -command SequenceLowerCommand]
    set fd  [button $bf.delete   -text Delete  -width 8 \
	    -command SequenceDeleteElement]
    set fc  [button $bf.clear    -text Clear   -width 8 \
	    -command SequenceClear]
    set fa  [button $bf.apply    -text OK      -width 8 \
	    -command "SequenceWrite;SequenceEditorHide"]
    set fx  [button $bf.dismiss  -text Cancel  -width 8 \
	    -command SequenceEditorHide]

    if $text_only {
       $bf.up   configure -text " /\\ "
       $bf.down configure -text " \\/ "
    } else {
       wm protocol $parent WM_DELETE_WINDOW SequenceEditorHide
       $bf.up   configure -image $sequence(up) -width 50
       $bf.down configure -image $sequence(down) -width 50
    }

    pack $fu $fl $fd $fc $fa $fx -side left -expand true -fill x
    pack $tf.l $f -side left
    pack $tf $bf -side top

    bind $sequence(toplevel) <Destroy> SequenceEditorHide
    return $sequence(toplevel)
}

proc SequenceClear { } {
    global sequence
    $sequence(list) delete 0 end
}

proc SequenceDeleteElement { } {
    global sequence
    set idx [$sequence(list) curselection]
    if {[string length $idx] == 0} {set idx end}
    $sequence(list) delete $idx
}

proc SequenceInsertRun { } {
    global sequence mode
    #set cmd "run buffer $sequence(run)"

    switch $mode {
	increment { set cmd RI }
	q         { set cmd RQ }
	bragg     { set cmd RB }
	trash     { set cmd RT }
    }

    append cmd $sequence(run)
    set idx [$sequence(list) curselection]
    if {[string length $idx] == 0} {set idx end}
    $sequence(list) insert $idx $cmd
    # this little goodie makes it easier to set up sequences of buffers IMHO
    incr sequence(run)
    if {$sequence(run) > 30} {set sequence(run) 1}
}

proc SequenceInsertTemp { } {
    global sequence
    #set cmd "temperature set $sequence(temp)"
    set cmd "ST=$sequence(temp)"
    set idx [$sequence(list) curselection]
    if {[string length $idx] == 0} {set idx end}
    $sequence(list) insert $idx $cmd
}

proc SequenceInsertWait { } {
    global sequence
    #set cmd "wait minutes $sequence(wait)"
    set cmd "HOLD=$sequence(wait)"
    set idx [$sequence(list) curselection]
    if {[string length $idx] == 0} {set idx end}
    $sequence(list) insert $idx $cmd
}

proc SequenceInsertCmd { } {
    global sequence
    set cmd $sequence(cmd)
    set idx [$sequence(list) curselection]
    if {[string length $idx] == 0} {set idx end}
    $sequence(list) insert $idx $cmd
}

proc SequenceEditorShow { } {
    global sequence text_only
    if $text_only {
       catch { destroy $sequence(toplevel) }
       SequenceEditorBuild .sequence
       SequenceRead
       place $sequence(toplevel) -relheight 1.0 -relwidth 1.0
       focus $sequence(toplevel)
    } else {
       SequenceRead
       # center the dialog in the middle of the main
       set x [expr [winfo x .] + [winfo width .]/2 - \
   		[winfo reqwidth $sequence(toplevel)]/2 - [winfo vrootx .]]
       set y [expr [winfo y .] + [winfo height .]/2 - \
   	    [winfo reqheight $sequence(toplevel)]/2 - [winfo vrooty .]]
       wm geom $sequence(toplevel) +$x+$y
       wm transient $sequence(toplevel) .
       wm deiconify $sequence(toplevel)
       grab $sequence(toplevel)
    }
}

proc SequenceEditorHide { } {
    global sequence text_only
    if $text_only {
       place forget $sequence(toplevel)
       focus .
       destroy $sequence(toplevel)
#       .l.list selection set 0 0
    } else {
       wm withdraw $sequence(toplevel)
       grab release $sequence(toplevel)
    }
}

proc SequenceRaiseCommand { } {
    global sequence
    set c [$sequence(list) curselection]
    if {$c == 0 || $c == ""} return
    set val [$sequence(list) get $c]
    $sequence(list) delete $c
    incr c -1
    $sequence(list) insert $c $val
    $sequence(list) selection set $c
}
proc SequenceLowerCommand { } {
    global sequence
    set c [$sequence(list) curselection]
    if {$c == [expr [$sequence(list) size]-1] || $c == ""} return
    set val [$sequence(list) get $c]
    $sequence(list) delete $c
    incr c
    $sequence(list) insert $c $val
    $sequence(list) selection set $c
}

#=Switch buffers===============================================================

proc SwitchBuf { which } {
    global mode changed
    switch $which {
	increment {
	    .l.title configure -text \
		    "No Comment *I-Buffer*   A4-beg A4-end  Inc    Monitor        Temp Hold"
	    IBufReadFile ;# Make sure we get the latest data from disk
	    IBufList
	    notebook_display .e ibuf
	    set mode $which
	    set changed(ibufdisp) 0
	    # this must be done after the resetting the changed flag
	    # or IBufCalcBT1 changes are not noted
	    IBufDisp 0
	}
	q {
	    .l.title configure -text \
		    "No Comment *Q-Buffer      HC     KC     LC   EC   ES     Monitor"
	    QBufReadFile ;# Make sure we get the latest data from disk
	    QBufList
	    QBufDisp 0
	    notebook_display .e qbuf
	    set mode $which
	    set changed(qbufdisp) 0
	}
	bragg {
	    .l.title configure -text \
		    "No Comment *B-Buffer      HC     KC     LC   EC   ES     Monitor"
	    BBufReadFile ;# Make sure we get the latest data from disk
	    BBufList
	    BBufDisp 0
	    notebook_display .e bbuf
	    set mode $which
	    set changed(bbufdisp) 0
	}
	trash {
	    .l.title configure -text \
		    "No Comment *T-Buffer*   Ec-meV Es-xdE Ef-meV NPTS  Monitor "
	    TBufReadFile ;# Make sure we get the latest data from disk
	    TBufList
	    TBufDisp 0
	    notebook_display .e tbuf
	    set mode $which
	    set changed(tbufdisp) 0
	}
	reflectivity {
	    .l.title configure -text \
		    "No Comment *R-Buffer*      qz-beg    qz-end    qx-beg   qx-end    Monitor  NPTS"
	    RBufReadFile ;# Make sure we get the latest data from disk
	    RBufList
	    RBufDisp 0
	    notebook_display .e rbuf
	    set mode $which
	    set changed(rbufdisp) 0
	}
	diffraction {
	    .l.title configure -text \
		    "No Comment *D-Buffer*      A  A-Beg  A-Inc A-End Monitor  NPTS"
	    DBufReadFile ;# Make sure we get the latest data from disk
	    DBufList
	    DBufDisp 0
	    notebook_display .e dbuf
	    set mode $which
	    set changed(dbufdisp) 0
	}
    }
}

###############################################################################
# GUI helper functions
#

proc About { } {
   global text_only
   if $text_only {
      ck_dialog .about {About...} \
       "Prepare\nN.C. Maliszewskyj\n<nickm@nist.gov>" \
       OK
   } else {
      tk_dialog .about {About...} \
       "Prepare\nN.C. Maliszewskyj\n<nickm@nist.gov>" \
       info 0 OK
   }
}

proc ScrolledListbox { parent args } {
    # Create listbox attached to scrollbars, pass through $args
    frame $parent
    eval {listbox $parent.list \
	    -yscrollcommand [list $parent.sy set] } $args
    # Create scrollbar attached to the listbox
    scrollbar $parent.sy -orient vertical \
	    -command [list $parent.list yview]
    pack $parent.sy -side right -fill y
    # pack to allow for resizing
    pack $parent.list -side left -fill both -expand true
    return $parent.list
}

proc ScrolledLabelledListbox { parent {title ""} args } {
    global tcl_platform text_only
    set fontname TkFixedFont

    # Create listbox attached to scrollbars, pass through $args
    frame $parent -class MonoSpc
     eval {listbox $parent.list \
	    -yscrollcommand [list $parent.sy set] } $args
    # Create scrollbar attached to the listbox
    scrollbar $parent.sy -orient vertical \
	    -command [list $parent.list yview]
    label $parent.title -text $title -anchor w

    if {!$text_only} {
       $parent.list configure -font $fontname
       $parent.title configure -borderwidth 4 -font $fontname
    }
    
    pack $parent.title -fill x
    pack $parent.sy -side right -fill y
    pack $parent.list -side left -fill both -expand true
    # pack to allow for resizing

    # Figure out how to expand in main window
    return $parent.list
}

proc fltrim { var { factor 1000 } } {
    set outvar [expr round($var * $factor)/ (1.0 * $factor)]
    return $outvar
}

proc GetNewPrefix {prefix} {
    global text_only monrec
    set p .prefix

    toplevel $p

    if {!$text_only} {
       # center the dialog in the middle of the main
       set x [expr [winfo x .] + [winfo width .]/2 - \
		[winfo reqwidth $p]/2 - [winfo vrootx .]]
       set y [expr [winfo y .] + [winfo height .]/2 - \
	    [winfo reqheight $p]/2 - [winfo vrooty .]]
       wm geom $p +$x+$y
       wm transient $p .
       wm deiconify $p
       # set the grab
       grab $p
       wm title $p "Fix File Name"
    }

    label $p.title -anchor center \
	    -text "Please modify the experiment prefix."
    label $p.title2 -anchor center \
            -text "\"$prefix\" is invalid."
    label $p.expl -text "The first five letters of the comment set the file name for the experiment."
    label $p.expl2 -text "You must use letters and or digits, but no other symbols or spaces."
    if {!$text_only} {
       $p.expl  configure -font "Helvetica -12 italic" -wraplength 200
       $p.expl2 configure -font "Helvetica -12 italic" -wraplength 200
    }
    label $p.newlbl -text "New prefix: "
    entry $p.new -width 5 -textvariable monrec(newprefix)
    set monrec(newprefix) [string trim $prefix]
    button $p.ok -text Change -command {
          global text_only
          if $text_only {
             place forget .prefix
             focus .
          }
          destroy .prefix
          .l.list selection set 0 0
       }
    grid $p.title -columnspan 2 -sticky ew
    label $p.lineskip -text " "
    grid $p.lineskip

    grid $p.title2 -columnspan 2 -sticky ew
    label $p.lineskip2 -text " "
    grid $p.lineskip2
    
    grid $p.expl -columnspan 2
    grid $p.expl2 -columnspan 2
    label $p.lineskip3 -text " "
    grid $p.lineskip3

    if $text_only {
       $p.new configure -width 8
       grid $p.newlbl -row 8 -column 0 -sticky e
       grid $p.new    -row 8 -column 1 -sticky w
    } else {
       grid $p.newlbl $p.new -sticky e
       grid $p.new -sticky w
    }
    label $p.lineskip4 -text " "
    grid $p.lineskip4

    grid $p.ok -columnspan 2

    if $text_only {
       place $p -relheight 1.0 -relwidth 1.0
       focus $p
    }
    tkwait window $p
    return [string range "$monrec(newprefix)     " 0 4]
}

###############################################################################
#
#
option add *Notebook.borderWidth 2 widgetDefault
option add *Notebook.relief sunken widgetDefault

proc notebook_create { win } {
    global nbInfo

    frame $win -class Notebook

    pack propagate $win 0

    set nbInfo($win-count)   0
    set nbInfo($win-pages)   ""
    set nbInfo($win-current) ""

    # Remember to bind to destroy method
    bind $win <Destroy> "notebook_destroy $win"

    return $win
}

proc notebook_destroy { win } {
    global nbInfo

    foreach p [array names nbInfo] {
	if [regexp "^$win" $p] {
	    unset nbInfo($p)
	}
    }
}

proc notebook_page { win name } {
    global nbInfo

    set page "$win.page[incr nbInfo($win-count)]"
    lappend nbInfo($win-pages)  $page
    set nbInfo($win-page-$name) $page

    frame $page

    if {$nbInfo($win-count) == 1} {
	after idle [list notebook_display $win $name]
    }
    
    return $page
}

proc notebook_display {win name} {
    global nbInfo

    set page ""
    if {[info exists nbInfo($win-page-$name)]} {
	set page $nbInfo($win-page-$name)
    } elseif {[winfo exists $win.page$name]} {
	set page $win.page$name
    }
    if {"" == $page} {
	error "bad notebook page \"$name\""
    }
    
    # perform size calculation
    notebook_fix_size $win
    if {"" != $nbInfo($win-current)} {
	pack forget $nbInfo($win-current)
    }
    pack $page -expand yes -anchor nw
    set nbInfo($win-current) $page
    return $page
}

proc notebook_fix_size { win } {
    global nbInfo text_only

    update idletasks

    set maxw 0
    set maxh 0
    foreach page $nbInfo($win-pages) {
	set w [winfo reqwidth $page]
	if {$w > $maxw} {set maxw $w}

	set h [winfo reqheight $page] 
	if {$h > $maxh} {set maxh $h} 
    }

    if $text_only {
       set bd 1
    } else {
       set bd [$win cget -borderwidth]
    }
    set maxw [expr $maxw + 2 * $bd]
    set maxh [expr $maxh + 2 * $bd]
    $win configure -width $maxw -height $maxh
}

proc toggle_create { win boolvar {boolvals {OFF ON}}} {
    global tInfo

    upvar $boolvar b
    if {[llength $boolvals] != 2} {
	return -code error "toggle_create: specify only two possible labels"
    }

    if {$b} {
	set initvar [lindex $boolvals 1]
    } else {
	set initvar [lindex $boolvals 0]
    }
    set tInfo($win-var) $boolvar
    set tInfo($win-lab) $boolvals
    button $win -text $initvar -command "toggle_action $win"

    bind $win <Destroy> "toggle_destroy $win"

    return $win
}

proc toggle_destroy { win } {
    global tInfo
    catch {unset tInfo($win-var)}
    catch {unset tInfo($win-lab)}
}

proc toggle_action { win } {
    global tInfo
    upvar $tInfo($win-var) t
    if {$t} {
	$win configure -text [lindex $tInfo($win-lab) 0]
	set t 0
    } else {
	$win configure -text [lindex $tInfo($win-lab) 1]
	set t 1
    }
    return $t
}

proc toggle_set { win } {
    global tInfo
    upvar #0 $tInfo($win-var) t
    if {$t} {
	$win configure -text [lindex $tInfo($win-lab) 0]
    } else {
	$win configure -text [lindex $tInfo($win-lab) 1]
    }
    return $t
}

# Track changes to displayed buffers
proc bufchanged { args } {
    global changed
    if {[llength $args] < 1} { return }
    set changed([lindex $args 0]) 1
}

proc SetTkDefaultOptions {"basefont 14"} { 

    global font
    set font $basefont

    set Opt(prio)    100
    set Opt(font)         "Helvetica -$basefont"
    set Opt(bold_font)    "Helvetica -$basefont bold"
    set Opt(menu_font)    "Helvetica -$basefont bold"
    set Opt(italic_font)  "Helvetica -$basefont bold italic"
    #set Opt(fixed_font)   -*-courier-medium-r-*-*-14-*-*-*-*-*-*-*
    incr basefont -2
    set Opt(graph_font)    "Helvetica -$basefont"
    set Opt(small_bold_font)    "Helvetica -$basefont bold"
    set Opt(small_font)    "Helvetica -$basefont"
    set Opt(coord_font)    "Courier -$basefont bold"

    option add *Font		$Opt(bold_font) $Opt(prio)
    option add *font		$Opt(bold_font) $Opt(prio)
    option add *Graph*Font	$Opt(graph_font) $Opt(prio)
    option add *Graph.font	$Opt(graph_font) $Opt(prio)
    option add *Canvas.font	$Opt(bold_font) $Opt(prio)
    option add *Button.font	$Opt(bold_font) $Opt(prio)
    option add *Menu.font	$Opt(menu_font) $Opt(prio)
    option add *Menubutton.font	$Opt(menu_font) $Opt(prio)
    option add *Label.font      $Opt(bold_font) $Opt(prio)
    option add *Scale.font	$Opt(italic_font) $Opt(prio)
    option add *TitleFrame.font $Opt(italic_font) $Opt(prio)
    option add *SmallFont.Label.font	$Opt(small_bold_font) $Opt(prio)
    option add *SmallFont.Checkbutton.font	$Opt(small_font) $Opt(prio)
    option add *SmallFont.Button.font	$Opt(small_font) $Opt(prio)
    option add *Coord.Listbox.font	$Opt(coord_font) $Opt(prio)
    option add *HistList.Listbox.font	$Opt(coord_font) $Opt(prio)
    option add *MonoSpc.Label.font	$Opt(coord_font) $Opt(prio)
    option add *MonoSpc.Listbox.font	$Opt(coord_font) $Opt(prio)

    set Opt(bg)           lightgray
    set Opt(fg)           black

    set Opt(dark1_bg)     gray86
    set Opt(dark1_fg)     black
    #set Opt(dark2_bg)     gray77
    #set Opt(dark2_fg)     black
    set Opt(inactive_bg)  gray77
    set Opt(inactive_fg)  black

    set Opt(light1_bg)    gray92

    set Opt(active_bg)    $Opt(dark1_bg)
    set Opt(active_fg)    $Opt(fg)
    set Opt(disabled_fg)  gray55

    set Opt(input_bg)    gray95
    set Opt(output1_bg)   $Opt(dark1_bg)
    set Opt(output2_bg)   $Opt(bg)

    set Opt(select_fg)    black
    set Opt(select_bg)    lightblue

    set Opt(selector)	yellow

    option add *background 		$Opt(bg) 10
    option add *Background		$Opt(bg) $Opt(prio)
    option add *background		$Opt(bg) $Opt(prio)
    option add *Foreground		$Opt(fg) $Opt(prio)
    option add *foreground		$Opt(fg) $Opt(prio)
    option add *activeBackground	$Opt(active_bg) $Opt(prio)
    option add *activeForeground	$Opt(active_fg) $Opt(prio)
    option add *HighlightBackground	$Opt(bg) $Opt(prio)
    option add *selectBackground	$Opt(select_bg) $Opt(prio)
    option add *selectForeground	$Opt(select_fg) $Opt(prio)
    option add *selectBorderWidth	0 $Opt(prio)
    option add *Menu.selectColor	$Opt(selector) $Opt(prio)
    option add *Menubutton.padY		1p $Opt(prio)
    option add *Menubutton.activeBackground	$Opt(select_bg) $Opt(prio)
    option add *Menubutton.background	$Opt(light1_bg) $Opt(prio)
    option add *Button.activeBackground	$Opt(select_bg) $Opt(prio)
    option add *Button.background	$Opt(light1_bg) $Opt(prio)
    option add *Button.borderWidth	2 $Opt(prio)
    option add *Button.anchor		c $Opt(prio)
    option add *Checkbutton.selectColor	$Opt(selector) $Opt(prio)
    option add *Checkbutton.activeBackground	$Opt(select_bg) $Opt(prio)
    option add *Radiobutton.selectColor	$Opt(selector) $Opt(prio)
    option add *Radiobutton.activeBackground	$Opt(select_bg) $Opt(prio)
    option add *Entry.relief		sunken $Opt(prio)
    option add *Entry.background	$Opt(input_bg) $Opt(prio)
    option add *Entry.foreground	black $Opt(prio)
    option add *Entry.insertBackground	black $Opt(prio)
    option add *Label.anchor		w $Opt(prio)
    option add *Label.borderWidth	0 $Opt(prio)
    option add *Listbox.background	$Opt(light1_bg) $Opt(prio)
    option add *Listbox.relief		sunken $Opt(prio)
    option add *Notebook.borderWidth    2        widgetDefault
    option add *Notebook.relief         sunken   widgetDefault
    option add *Scale.foreground	$Opt(fg) $Opt(prio)
    option add *Scale.activeForeground	$Opt(bg) $Opt(prio)
    option add *Scale.background	$Opt(bg) $Opt(prio)
    option add *Scale.sliderForeground	$Opt(bg) $Opt(prio)
    option add *Scale.sliderBackground	$Opt(light1_bg) $Opt(prio)
    option add *Scrollbar.background	$Opt(bg) $Opt(prio)
    option add *Scrollbar.troughColor	$Opt(light1_bg) $Opt(prio)
    option add *Scrollbar.relief	sunken $Opt(prio)
    option add *Scrollbar.borderWidth	1 $Opt(prio)
    option add *Scrollbar.width		15 $Opt(prio)
    option add *Text.background		$Opt(input_bg) $Opt(prio)
    option add *Text.relief		sunken $Opt(prio)
    . config -background                $Opt(bg)
}

# recursive routine to set all 
proc ResizeFont {path} {
    foreach child [winfo children $path] {
        set childtype [winfo class $child]
	# class "FixedFont" should not be resized
	if {$childtype == "FixedFont"} continue
	set font [option get $child font $childtype]
	if {$font != ""} {
	    catch {
		set curfont [$child cget -font]
		if {[string tolower [lindex $curfont 0]] == "symbol"} {
		    $child configure -font "Symbol [lrange $font 1 end]"
		} else {
		    $child configure -font $font
		}
	    }
	}
	ResizeFont $child
    }
}

#------------------------------------------------------------------------------
#	Message box code that centers the message box over the parent.
#          or along the edge, if too close, 
#          but leave a border along +x & +y for reasons I don't remember
#       It also allows the button names to be defined using 
#            -type $list  -- where $list has a list of button names
#       larger messages are placed in a scrolled text widget
#       capitalization is now ignored for -default
#       The command returns the name button in all lower case letters
#       otherwise see  tk_messageBox for a description
#
#       This is a modification of tkMessageBox (msgbox.tcl v1.5)
#
proc MyMessageBox {args} {
    global tkPriv tcl_platform

    set w tkPrivMsgBox
    upvar #0 $w data

    #
    # The default value of the title is space (" ") not the empty string
    # because for some window managers, a 
    #		wm title .foo ""
    # causes the window title to be "foo" instead of the empty string.
    #
    set specs {
	{-default "" "" ""}
        {-icon "" "" "info"}
        {-message "" "" ""}
        {-parent "" "" .}
        {-title "" "" " "}
        {-type "" "" "ok"}
        {-helplink "" "" ""}
    }

    tclParseConfigSpec $w $specs "" $args

    if {[lsearch {info warning error question} $data(-icon)] == -1} {
	error "bad -icon value \"$data(-icon)\": must be error, info, question, or warning"
    }
    if {![string compare $tcl_platform(platform) "macintosh"]} {
      switch -- $data(-icon) {
          "error"     {set data(-icon) "stop"}
          "warning"   {set data(-icon) "caution"}
          "info"      {set data(-icon) "note"}
	}
    }

    if {![winfo exists $data(-parent)]} {
	error "bad window path name \"$data(-parent)\""
    }

    switch -- $data(-type) {
	abortretryignore {
	    set buttons {
		{abort  -width 6 -text Abort -under 0}
		{retry  -width 6 -text Retry -under 0}
		{ignore -width 6 -text Ignore -under 0}
	    }
	}
	ok {
	    set buttons {
		{ok -width 6 -text OK -under 0}
	    }
          if {![string compare $data(-default) ""]} {
		set data(-default) "ok"
	    }
	}
	okcancel {
	    set buttons {
		{ok     -width 6 -text OK     -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	retrycancel {
	    set buttons {
		{retry  -width 6 -text Retry  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	yesno {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
	    }
	}
	yesnocancel {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	default {
#	    error "bad -type value \"$data(-type)\": must be abortretryignore, ok, okcancel, retrycancel, yesno, or yesnocancel"
	    foreach item $data(-type) {
		lappend buttons [list [string tolower $item] -text $item -under 0]
	    }
	}
    }

    if {[string compare $data(-default) ""]} {
	set valid 0
	foreach btn $buttons {
	    if {![string compare [lindex $btn 0] [string tolower $data(-default)]]} {
		set valid 1
		break
	    }
	}
	if {!$valid} {
	    error "invalid default button \"$data(-default)\""
	}
    }

    # 2. Set the dialog to be a child window of $parent
    #
    #
    if {[string compare $data(-parent) .]} {
	set w $data(-parent).__tk__messagebox
    } else {
	set w .__tk__messagebox
    }

    # 3. Create the top-level window and divide it into top
    # and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w $data(-title)
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w $data(-parent)
    if {![string compare $tcl_platform(platform) "macintosh"]} {
	unsupported1 style $w dBoxProc
    }

    frame $w.bot
    pack $w.bot -side bottom -fill both
    frame $w.top
    pack $w.top -side top -fill both -expand 1
    if {$data(-helplink) != ""} {
#	frame $w.help
#	pack $w.help -side top -fill both
	pack [button $w.top.1 -text Help -bg yellow \
		-command "MakeWWWHelp $data(-helplink)"] \
		-side right -anchor ne
	bind $w <Key-F1> "MakeWWWHelp $data(-helplink)"
    }
    if {[string compare $tcl_platform(platform) "macintosh"]} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }

    # 4. Fill the top part with bitmap and message (use the option
    # database for -wraplength and -font so that they can be
    # overridden by the caller).

    option add *Dialog.msg.wrapLength 6i widgetDefault

    if {[string length $data(-message)] > 300} {
	if {![string compare $tcl_platform(platform) "macintosh"]} {
	    option add *Dialog.msg.t.font system widgetDefault
	} else {
	    option add *Dialog.msg.t.font {Times 18} widgetDefault
	}
	frame $w.msg
	grid [text  $w.msg.t  \
		-height 20 -width 55 -relief flat -wrap word \
		-yscrollcommand "$w.msg.rscr set" \
		] -row 1 -column 0 -sticky news
	grid [scrollbar $w.msg.rscr  -command "$w.msg.t yview" \
		] -row 1 -column 1 -sticky ns
	# give extra space to the text box
	grid columnconfigure $w.msg 0 -weight 1
	grid rowconfigure $w.msg 1 -weight 1
	$w.msg.t insert end $data(-message)
    } else {
	if {![string compare $tcl_platform(platform) "macintosh"]} {
	    option add *Dialog.msg.font system widgetDefault
	} else {
	    option add *Dialog.msg.font {Times 18} widgetDefault
	}
	label $w.msg -justify left -text $data(-message)
    }
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {[string compare $data(-icon) ""]} {
	label $w.bitmap -bitmap $data(-icon)
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 5. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $buttons {
	set name [lindex $but 0]
	set opts [lrange $but 1 end]
      if {![llength $opts]} {
	    # Capitalize the first letter of $name
          set capName [string toupper \
		    [string index $name 0]][string range $name 1 end]
	    set opts [list -text $capName]
	}

      eval button [list $w.$name] $opts [list -command [list set tkPriv(button) $name]]

	if {![string compare $name [string tolower $data(-default)]]} {
	    $w.$name configure -default active
	}
      pack $w.$name -in $w.bot -side left -expand 1 -padx 3m -pady 2m

	# create the binding for the key accelerator, based on the underline
	#
	set underIdx [$w.$name cget -under]
	if {$underIdx >= 0} {
	    set key [string index [$w.$name cget -text] $underIdx]
          bind $w <Alt-[string tolower $key]>  [list $w.$name invoke]
          bind $w <Alt-[string toupper $key]>  [list $w.$name invoke]
	}
	incr i
    }

    # 6. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {[string compare $data(-default) ""]} {
      bind $w <Return> [list tkButtonInvoke $w.[string tolower $data(-default)]]
    }

    # 7. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set wp $data(-parent)
    # center the new window in the middle of the parent
    set x [expr [winfo x $wp] + [winfo width $wp]/2 - \
	    [winfo reqwidth $w]/2 - [winfo vrootx $wp]]
    set y [expr [winfo y $wp] + [winfo height $wp]/2 - \
	    [winfo reqheight $w]/2 - [winfo vrooty $wp]]
    # make sure that we can see the entire window
    set xborder 10
    set yborder 25
    if {$x < 0} {set x 0}
    if {$x+[winfo reqwidth $w] +$xborder > [winfo screenwidth $w]} {
	incr x [expr \
		[winfo screenwidth $w] - ($x+[winfo reqwidth $w] + $xborder)]
    }
    if {$y < 0} {set y 0}
    if {$y+[winfo reqheight $w] +$yborder > [winfo screenheight $w]} {
	incr y [expr \
		[winfo screenheight $w] - ($y+[winfo reqheight $w] + $yborder)]
    }
    wm geom $w +$x+$y
    wm deiconify $w

    # 8. Set a grab and claim the focus too.

    catch {set oldFocus [focus]}
    catch {set oldGrab [grab current $w]}
    catch {
	grab $w
	if {[string compare $data(-default) ""]} {
	    focus $w.[string tolower $data(-default)]
	} else {
	    focus $w
	}
    }

    # 9. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    destroy $w
    catch {grab $oldGrab}
    return $tkPriv(button)
}

###############################################################################
# Build GUI
#

proc GUIBuild { } {
   global config msgpane text_only
   if {!$text_only} {
      set cwd [pwd]
      wm title . "ICP Prepare - $cwd"
      set m [frame .mbar -relief raised -borderwidth 2]
   } else {
      set m [frame .mbar]
   }
   set mf [menubutton $m.file    -width 6 -text File    -underline 0 \
	-menu $m.file.menu]
   set mfm [menu $mf.menu]
   $mfm add command -label "About" -underline 0 -command About
   $mfm add command -label "Exit"  -underline 1 -command "CheckBufChange; exit"

   set mb [menubutton $m.buf -width 6 -text Buffer -underline 0 \
	-menu $m.buf.menu]
   set mbm [menu $mb.menu]
   $mbm add command -label "Increment"  -command {SwitchBuf increment} \
	-state $config(ibuf_state)
   $mbm add command -label "Constant Q" -command {SwitchBuf q}         \
	-state $config(qbuf_state)
   $mbm add command -label "Bragg"      -command {SwitchBuf bragg}     \
	-state $config(bbuf_state)
   $mbm add command -label "Trash"      -command {SwitchBuf trash}     \
	-state $config(tbuf_state)
   $mbm add command -label "Reflectivity" -command {SwitchBuf reflectivity} \
	-state $config(rbuf_state)
   $mbm add command -label "Diffraction" -command {SwitchBuf diffraction} \
	-state $config(dbuf_state)

   set ms [menubutton $m.seq -width 8 -text Sequence -underline 0 \
	-menu $m.seq.menu]
   set msm [menu $ms.menu]
   $msm add command -label "Edit" -command {SequenceEditorShow}

   set me [menubutton $m.edit    -width 9 -text "Edit mode"    -underline 0 \
	-menu $m.edit.mode]
   set mem [menu $m.edit.mode]
   foreach opt {0 1 2} lbl {Simple "Temp Control" "Magnet Control"} {
      $mem add radiobutton -command "IBufEntryMod; TBufEntryMod; QBufEntryMod" \
	    -label $lbl -value $opt -variable config(mode)
   }

   set mc [menubutton $m.conf   -width 12 -text Configuration -underline 0 \
	-menu $m.conf.menu]
   set mcm [menu $m.conf.menu]
   foreach opt $config(instr_list) {
      $mcm add radiobutton -command ConfigChange \
	    -label $opt -value $opt -variable config(instr)
   }

   label $m.cfg -textvariable config(instr)
   if {!$text_only} {
      $m.cfg configure -font "Helvetica -14 bold italic"
      pack $mf $mb $ms $me $mc -side left
   } else {
      pack $mf $mb $ms $me $mc -side left -ipadx 1
   }
   pack $m.cfg -side right

   ScrolledLabelledListbox .l \
	"No Comment *I-Buffer*   A4-beg A4-end  Inc    Monitor " \
	-height 6 -width 80

   if {!$text_only} {
      # turn off selection export so raised selection stays on
      .l.list config -exportselection 0
      # looks better
      .l.list config -highlightthickness 0
   }

   notebook_create .e
   set page [notebook_page .e ibuf ]
   IBufEntryCreate $page
   set page [notebook_page .e tbuf]
   TBufEntryCreate $page
   set page [notebook_page .e qbuf]
   QBufEntryCreate $page
   set page [notebook_page .e bbuf]
   BBufEntryCreate $page
   set page [notebook_page .e rbuf]
   RBufEntryCreate $page
   set page [notebook_page .e dbuf]
   DBufEntryCreate $page
   set page [notebook_page .e bufop]
   BufopEntryCreate $page
   set page [notebook_page .e evalbox]
   EvalEntryCreate $page
   set page [notebook_page .e msg  ]
   set msgpane [label $page.label -text "Updating Buffer" ]
   button $page.dismiss -text "Dismiss" -command MsgHide
   pack $page.label -expand true -fill x
   pack $page.dismiss -side bottom
   set page [notebook_page .e blank]
   notebook_display .e blank

   # Pack widgets
   frame .k
   button .k.f1 -text "Update: F1" -width 11 -command UpdateBuf
   button .k.f2 -text "BufOps: F2" -width 11 -command BufopShow
   if $text_only {
      label .k.keys -text \
         "(F10 & Ctrl-U = menu, Tab & Ctrl-N = next, Ctrl-P = prev)"
   }

   pack $m -side top -fill x -anchor nw
   pack .k -side bottom -fill x
   pack .e -side bottom -fill x
   pack .l -side top    -expand true -fill both


   if {!$text_only} {
      pack .k.f1 .k.f2 -side left
   } else {
      pack .k.f1 .k.f2 .k.keys -side left
   }

   # Build additional dialogs
   MonRecDialogBuild
   SequenceEditorBuild .sequence
   AutomonBuild

   # Set key bindings
   bind all <KeyPress-F1> UpdateBuf
   bind all <KeyPress-F2> BufopShow
   bind all <KeyPress-F4> EvalShow

   if {!$text_only} {
       set xmax [expr [winfo screenwidth  .]/2]
       set ymax [expr [winfo screenheight .]/2]
       set x  [expr ($xmax - [winfo reqwidth  .])/2]
       set y  [expr ($ymax - [winfo reqheight .])/2]
       wm geom . +$x+$y
       wm protocol . WM_DELETE_WINDOW "CheckBufChange;exit"
       wm deiconify . ;# Show toplevel window
       wm protocol . WM_DELETE_WINDOW "CheckBufChange;exit"
       wm deiconify . ;# Show toplevel window
       if {$config(nsta) == 1} { wm geometry . 773x423+254+63 }
       update
   }
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Body of the script itself
#
# Check command line arguments
if {[llength $argv] > 0} {
    set instr [string tolower [lindex $argv 0]]
    switch $instr {
	bt0 { set config(nsta) 0 }
	bt1 { set config(nsta) 1 }
	bt2 { set config(nsta) 2 }
	bt4 { set config(nsta) 4 }
	bt5 { set config(nsta) 5 }
	bt7 { set config(nsta) 7 }
	bt8 { set config(nsta) 8 } 
	bt9 { set config(nsta) 9 }
	ng1 { set config(nsta) -1 }
	ng5 { set config(nsta) -5 }
	ng7 { set config(nsta) -7 }
	xr0 { set config(nsta) 0 }
	default {
	    puts stderr "Instrument \"$instr\" not supported"
	    exit
	}
    }
}

ConfigInit  ;# Initialize variables
ConfigRead  ;# Determine configuration
ConfigCruft ;# Make instrument-specific customizations

IBufInit 
QBufInit 
BBufInit 
TBufInit 
RBufInit 
DBufInit 

GUIBuild

# Last minute initializations
#
#

if {1 == $config(nsta)} {
    if $text_only {
	set config(mode) [ck_dialog .instr "Choose Edit Mode" \
		"Choose the buffer setup mode you will use" \
		"Simple" "Temperature Control" "Magnet Control"]
    } else {
	set config(mode) [tk_dialog .instr "Choose Edit Mode" \
		"Choose the buffer setup mode you will use" \
		question $config(mode) "Simple" "Temperature\nControl" "Magnet\nControl"]
    }
}

IBufEntryMod
TBufEntryMod
QBufEntryMod

if {[file exists IBUFFER.BUF] && $config(ibuf_state) == "normal"} { IBufReadFile } else { IBufWriteFile }
if {[file exists QBUFFER.BUF] && $config(qbuf_state) == "normal"} { QBufReadFile } else { QBufWriteFile }
if {[file exists TBUFFER.BUF] && $config(tbuf_state) == "normal"} { TBufReadFile } else { TBufWriteFile }
if {[file exists BBUFFER.BUF] && $config(bbuf_state) == "normal"} { BBufReadFile } else { BBufWriteFile }
if {[file exists RBUFFER.BUF] && $config(rbuf_state) == "normal"} { RBufReadFile } else { RBufWriteFile }
if {[file exists DBUFFER.BUF] && $config(dbuf_state) == "normal"} { DBufReadFile } else { DBufWriteFile }


# Set up changed buffer traces -- for BT1 only
if {$config(paranoid)} {
    trace variable ibufdisp w bufchanged
    trace variable qbufdisp w bufchanged
    trace variable tbufdisp w bufchanged
    trace variable bbufdisp w bufchanged
    trace variable rbufdisp w bufchanged
    trace variable dbufdisp w bufchanged
}

# Set the default buffer type to increment for all instruments (for now)
# Note: this command must be executed after the trace is set so that 
#       changes to the 1st entry are tracked!
SwitchBuf $config(default_buf)

# Add menu and emacs-style key bindings to move from field to field

if $text_only {
   bind all <Control-n> {focus [ck_focusNext %W]}
   bind all <Control-p> {focus [ck_focusPrev %W]}
   bind all <Control-f> {focus [ck_focusNext %W]}
   bind all <Control-b> {focus [ck_focusPrev %W]}
   bind all <Control-u> {focus .mbar.file}
}

# Show current version if we're running from the wish prompt
set version
