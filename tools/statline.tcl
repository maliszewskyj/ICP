#!/usr/bin/wish

set version {$Id$}

proc StatLineInit { } {
    global statline env
#    set statline(flagfile) $env(HOME)/cfg/MOTORS.BUF
#    set statline(motfile)  $env(HOME)/cfg/MOTPOS.BUF
    set statline(flagfile) /usr/local/icp/cfg/MOTORS.BUF
    set statline(motfile)  /usr/local/icp/cfg/MOTPOS.BUF
    set statline(motlist) {}
    for {set i 1} {$i <= 30} {incr i} {
	set key "fixed$i"
	set statline($key) 0
	lappend statline(flaglist) $key
	set key "soft$i"
	set statline($key) "0.0000"
	lappend statline(motlist) $i
    }
    set statline(file) 0
    set statline(statistic) 0
    set statline(tempctr) 0
    set statline(polar) 0
    set statline(psd) 0
    set statline(tdevcfg) 0
    set statline(tdevmot) 0
    set statline(magnctr) 0
    set statline(extr) 0

    lappend statline(flaglist) statistic
    lappend statline(flaglist)  tempctr
    lappend statline(flaglist)  magnctr
    lappend statline(flaglist)  polar
    
    set statline(oncolor) red
    set statline(offcolor) #d9d9d9
    set statline(flgmtime)   0
    set statline(motmtime)   0
}

proc StatLineRead { } {
    global statline

    if {![file exists $statline(flagfile)]} {
	return
    }
    #puts "Reading $statline(flagfile)"
    set f [open $statline(flagfile) r]
    fconfigure $f -encoding binary
    seek $f [expr 160 * 34] start
    set input [read $f 160]
    set statline(input) $input

    binary scan $input iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii \
	    statline(fixed1)  statline(fixed2)  statline(fixed3)  \
	    statline(fixed4)  statline(fixed5)  statline(fixed6)  \
	    statline(fixed7)  statline(fixed8)  statline(fixed9)  \
	    statline(fixed10) statline(fixed11) statline(fixed12) \
	    statline(fixed13) statline(fixed14) statline(fixed15) \
	    statline(fixed16) statline(fixed17) statline(fixed18) \
	    statline(fixed19) statline(fixed20) statline(fixed21) \
	    statline(fixed22) statline(fixed23) statline(fixed24) \
	    statline(fixed25) statline(fixed26) statline(fixed27) \
	    statline(fixed28) statline(fixed29) statline(fixed30) \
	    statline(file)  statline(statistic) statline(tempctr) \
	    statline(polar)   statline(psd)     statline(tdevcfg) \
	    statline(tdevmot) statline(magnctr) 
    set statline(extr) 0
    close $f
}

proc StatLineBuild { p } {
    global statline
    # Build frame for fixed motor flags
    set pf [frame $p.fix ]
    label $pf.fixhdr -text "Motor Status"
    grid $pf.fixhdr -columnspan 6 -in $pf -sticky ew

    set nmot 0
    foreach i $statline(motlist) {
	set key "fixed$i"
	set lab $i
	set pfl [button $pf.$key -text $lab -state disabled \
		-disabledforeground black ]
	uplevel #0 trace variable statline($key) w StatLineChange
	set wkey "${key}_wid"
	set statline($wkey) $pfl

	set key "pos$i"
	set pfp [label $pf.$key -textvariable statline(soft$i) \
		-justify right -anchor e -relief sunken]
#	set row [expr $nmot / 4 + 1]
#	set col [expr $nmot % 4]
#	grid $pfl -row $row -column $col -in $pf -sticky ew
	grid $pfl $pfp - - -in $pf -sticky ew
	incr nmot
    }

    label $pf.flghdr -text "Flags"
    grid $pf.flghdr -columnspan 4 -in $pf -sticky ew

    button $pf.stat -text "S-" -state disabled -disabledforeground black 
    set statline(statistic_wid) $pf.stat
    uplevel #0 trace variable statline(statistic) w StatLineChange
    button $pf.temp -text "T-" -state disabled -disabledforeground black 
    set statline(tempctr_wid) $pf.temp
    uplevel #0 trace variable statline(tempctr) w StatLineChange
    button $pf.magn -text "H-" -state disabled -disabledforeground black 
    set statline(magnctr_wid) $pf.magn
    uplevel #0 trace variable statline(magnctr) w StatLineChange
    button $pf.pola -text "P-" -state disabled -disabledforeground black 
    set statline(polar_wid) $pf.pola
    uplevel #0 trace variable statline(polar) w StatLineChange

    grid $pf.stat $pf.temp $pf.magn $pf.pola -in $pf \
	    -sticky ew

    pack $pf -in $p -side top
}

proc StatLineChange { args } {
    global statline
    if {[llength $args] < 2} { return }
    set key [lindex $args 1]
    set wkey "${key}_wid"
    if ![info exists statline($wkey)] { return } 
    switch $key {
	tempctr {
	    if {$statline($key)} {
		$statline($wkey) configure -text "T+" \
			-background $statline(oncolor)
	    } else {
		$statline($wkey) configure -text "T-" \
			-background $statline(offcolor)
	    }
	}
	magnctr {
	    if {$statline($key)} {
		$statline($wkey) configure -text "H+" \
			-background $statline(oncolor)
	    } else {
		$statline($wkey) configure -text "H-" \
			-background $statline(offcolor)
	    }
	}
	polar {
	    if {$statline($key)} {
		$statline($wkey) configure -text "P+" \
			-background $statline(oncolor)
	    } else {
		$statline($wkey) configure -text "P-" \
			-background $statline(offcolor)
	    }
	}
	statistic {
	    if {$statline($key)} {
		$statline($wkey) configure -text "S+" \
			-background $statline(oncolor)
	    } else {
		$statline($wkey) configure -text "S-" \
			-background $statline(offcolor)
	    }
	}
	default {
	    #puts "StatLineChange $key $wkey"
	    if {$statline($key)} {
		$statline($wkey) configure -background $statline(oncolor)
	    } else {
		$statline($wkey) configure -background $statline(offcolor)
	    }
	}
    }
}

# Periodically read status files
proc StatLinePoll { } {
    global statline
    
    set mtime [file mtime $statline(flagfile)]
    if {$mtime != $statline(flgmtime)} {
	set statline(flgmtime) $mtime
	StatLineRead
#	StatLineFlagUpdate
    }

    set mtime [file mtime $statline(motfile)]
    if {$mtime != $statline(motmtime)} {
	set statline(motmtime) $mtime
	ReadMotPos
    }

    after 500 StatLinePoll
}

proc StatLineFlagUpdate { } {
    global statline
    foreach key $statline(flaglist) {
	StatLineChange statline $key
    }
}

proc ReadInstrCfg { } {
    global statline env

    set statline(cfgfile) "$env(HOME)/cfg/INSTR.CFG"
    if [catch {open $statline(cfgfile) r} f] {
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

    set statline(nsta) $nsta
    # Take care of extra lines for histogramming memory addresses
    if {$nsta == -1 || $nsta == -7 || $nsta == 8} {
	gets $f
	gets $f
    }
    
    set statline(motlist) {}
    for {set i 0} {$i < $nmots} {incr i} {
	gets $f input
	regsub -all {([ ]+)} [string trimleft $input] { } input
	set fields [split $input]
	lappend statline(motlist) [lindex $fields 0]
    }
    set statline(motlist) [lsort -integer $statline(motlist)]
    close $f
}

proc ReadMotPos { } {
    global statline env

    if [catch {open $statline(motfile) r} f] {
	return
    }
    fconfigure $f -encoding binary
    for {set i 1} {$i <= 30} {incr i} {
	seek $f [expr 160 * ($i - 1)] start
	set input [read $f 160]
	binary scan $input ff hard zero
	set statline(hard$i) $hard
	set statline(zero$i) $zero
	set statline(soft$i) [format "%8.4f" [expr $hard - $zero]]
	#puts "$i $hard $zero $statline(soft$i)"
    }
    close $f
}

StatLineInit     ;# Set defaults
ReadInstrCfg     ;# Read instrument configuration
StatLineBuild .

StatLinePoll
