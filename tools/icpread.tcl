
# BT1 parameters
set BT1_scl { 2.700  2.479  2.827  2.483  2.260  2.347  2.011  1.749  \
              1.630  1.360  1.339  1.218  1.058  1.000  1.054  0.953  \
	      0.941  0.985  1.031  1.021  0.982  1.011  0.900  1.118  \
	      0.955  1.056  0.973  0.974  0.943  0.877  0.872  0.820  }

set BT1_z_c { 0.00   1.00   1.29  -0.48   1.53  -0.98   2.03   0.89   \
              1.54   1.28   0.40   0.35   1.53  -1.57   0.63   1.43   \
	     -0.08  -0.01  -0.78   0.16  -1.08  -2.08  -1.23  -0.47   \
	      0.43  -0.27  -2.60   0.88  -1.34   2.24   3.00   4.00   }


# Can't yet handle fpx files, or BT1, or BT8
proc ICPReadHeader { file header } {
    upvar $header h

    gets $file line1
#    if {![regexp {^'([^']*)'[ ]+'([^']*)'[ ]+'([^']*)'} \
#	    $line1 match h(filename) h(date) h(scan)]} {
#	return -code error "Bad header - 1"
#    }

    if {![regexp {^'([^']*)'[ ]+'([^']*)'} \
	    $line1 match h(filename) h(date)]} {
	return -code error "Bad header - 1"
    }

    if {![regexp {([^ ]+)[ ]+([^ ]+)[ ]+'([^']+)'[ ]+([^ ]+)[ ]+'([^']+)'$} \
	    $line1 match h(mon) h(prf) h(base) h(pts) h(treatment)]} {
	return -code error "Bad header - 2"
    }
    set h(mon) [expr 1 * $h(mon)]

    # Determine instrument type from filename: I think it's unmolested by 
    # addrun, etc
    set h(instrtype) ""
    if {[regexp {\.([^\.]+)$} $h(filename) match h(instrtype)]} {
	set h(instrtype) [string toupper $h(instrtype)]
    }

    gets $file line2
    gets $file line1
    set h(comments) $line1

    # Chew up third block of data 
    gets $file line1
    gets $file line2
    regsub -all {([ ]+)} [string trimleft $line1] { } line1
    regsub -all {([ ]+)} [string trimleft $line2] { } line2
    set l1split [split $line1]
    set l2split [split $line2]
    if {[string compare [lindex $l2split 0] "Collimation"] == 0} {
	set h(collimation) "[lrange $l1split 0 3]"
	set l1split [lrange $l1split 4 end]
	set l2split [lrange $l2split 1 end]
    }
    if {[string compare [lindex $l2split 0] "Mosaic"] == 0} {
	set h(mosaic) "[lrange $l1split 0 2]"
	set l1split [lrange $l1split 3 end]
	set l2split [lrange $l2split 1 end]
    }
    if {[string compare [lindex $l2split 0] "Wavelength"] == 0} {
	set h(wavelength) "[lindex $l1split 0]"
	set l1split [lrange $l1split 1 end]
	set l2split [lrange $l2split 1 end]
    }
    if {[string compare [lindex $l2split 0] "Mon1"] == 0} {
	set h(monitor1) "[lindex $l1split 0]"
	set l1split [lrange $l1split 1 end]
	set l2split [lrange $l2split 1 end]
    }
    if {[string compare [lindex $l2split 0] "Exp"] == 0} {
	set h(exponent) "[lindex $l1split 0]"
	set l1split [lrange $l1split 1 end]
	set l2split [lrange $l2split 1 end]
    }

    # If instrument type is bt1, branch off to read the rest of the BT1
    # header info
    if [regexp {BT1} $h(instrtype)] {
	while {[gets $file line1] >= 0} {
	    if [regexp {^ Data} $line1] {
		return
	    }
	}
    }

    # Give up for now & proceed to the end
    while {[gets $file line1] >= 0} {
	if [regexp {(COUNTS|Counts)} $line1] { 
	    set line1 [string trimright [string trimleft $line1]]
	    break 
	}
    }
    regsub -all {(\(|\))} $line1 {} line1
    regsub -all {(\-)}   $line1 "\_" line1
    regsub -all {([ ]+)} $line1 { } line1
    
    set l1split [split $line1]
    
    set h(items) {}
    set i 0
    while {$i < [llength $l1split]} {
	if [regexp {^#(.*)} [lindex $l1split $i] match num] {
	    lappend h(items) "Counts$num"
	    incr i
	} else {
	    lappend h(items) [lindex $l1split $i]
	}
	incr i
    }
    
    return "OK: Success"
}    


proc ICPReadBT8Data { file header data } {
}

proc ICPReadBT1Data { file header data } {
    upvar $data d
    set d(A4) {}
    for {set i 0} {$i < 32} {incr i} {
	set key DATA$i
	set d($key) {}
    }
    while {![eof $file]} {
	gets $file line
	if {![regexp {[^ ]+} $line]} { continue }
	if {![regexp {^\$ M4=[ ]+([\.0-9]+) } $line match point]} {
	    return -code error "Bad BT1 Format"
	}
	lappend d(A4) $point

	set dpts {}
	while {[llength $dpts] < 32} {
	    gets $file line
	    regsub {,[ ]*$} $line "" line
	    foreach pt [split $line ,] {
		lappend dpts $pt
	    }
	}
	for {set i 0} {$i < 32} {incr i} {
	    set key DATA$i
	    lappend d($key) [lindex $dpts $i]
	}
    }
}

proc ICPReadData { file header data } {
    upvar $header h
    upvar $data d

    # create lists to contain data
    foreach vec $h(items) {
	set d($vec) {}
    }

    while {[gets $file line] >= 0} {
	set line [string trimright [string trimleft $line]]
	regsub -all {(\(|\))} $line {} line
	regsub -all {([ ]+)} $line { } line
	set lsplit [split $line]

	set i 0
	foreach vec $h(items) {
	    lappend d($vec) [lindex $lsplit $i]
	    incr i
	}
    }
}

proc ICPReadFile { filename header data_array } {
    upvar $header h
    upvar $data_array d
    if [catch {open $filename r} f] {
	return -code error "Can't open file \"$filename\""
    }
    if [catch {ICPReadHeader $f h} result] {
	close $f
	return -code error $result
    }
    if [catch {ICPReadData $f h d} result] {
	close $f
	return -code error $result
    }
    close $f
}

