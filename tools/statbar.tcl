#!/bin/sh
# the next line restarts this script using nohup and wish \
exec nohup wish "$0" "$@" > statbar.log 2>> statbar.err

#
# Put our process ID in the taskbar.log file
#
puts [pid]

#
# Current working directory
#
set cwd [pwd]

#
# Which instrument is this running on?
#
set instr_name "BT1"

#
# Change the following to reflect the correct path to each executable:
#
set icp_exe     "/usr/local/icp/bin/icpterm"
set prepare_exe "/usr/local/icp/bin/prepare"
set xpeek_exe   "/usr/local/icp/bin/xpeek"

#
# 0 - create a standard window with a title bar that behaves normally
# 1 - create a window with no title bar that hovers above all other windows
#
set use_overrideredirect 1

#
# Hide the default wish window
#
wm withdraw .

#
# Scrolled Listbox routines 
#

proc Scroll_Set { scrollbar geoCmd offset size } {
   if {$offset != 0.0 || $size != 1.0} {
      eval $geoCmd ;# Make sure it is visible
   }
   $scrollbar set $offset $size
}

proc Scrolled_Listbox { f args } {
   frame $f
   listbox $f.list \
      -xscrollcommand [list Scroll_Set $f.xscroll \
         [list grid $f.xscroll -row 1 -column 0 -sticky we]] \
      -yscrollcommand [list Scroll_Set $f.yscroll \
         [list grid $f.yscroll -row 0 -column 1 -sticky ns]]
   eval {$f.list configure} $args
   scrollbar $f.xscroll -orient horizontal -command [list $f.list xview]
   scrollbar $f.yscroll -orient vertical   -command [list $f.list yview]
   grid $f.list -sticky news -row 0 -column 0
   grid $f.xscroll -sticky news -row 1 -column 0
   grid $f.yscroll -sticky news -row 0 -column 1
   grid rowconfigure    $f 0 -weight 1
   grid columnconfigure $f 0 -weight 1
   return $f.list
}

#
# Directory dialog routines
#

proc set_directory { cwd } {
   if {[catch { cd $cwd }]} { make_directory cwd }
   set cwd [pwd]
   # shorten the path if it is long
   if {[string length $cwd] > 25} {
       regsub ^$env(HOME) $cwd ~ cwd
   }
}

proc make_directory { d } {
   global $d
   if {![tk_dialog .mkdir "Directory does not exist!" \
      "The directory [set $d] does not exist.  Would you like to create it?" \
      question 0 No Yes]} return
   if {[catch {file mkdir [set $d]} error_msg]} {
      tk_dialog .error "Failed to create directory!" \
         "Could not create directory [set $d] - Error: $error_msg" \
         error 0 Ok
      return
   }
   cd [set $d]
}

proc get_directory { { d 0 } } {
   global cd.cwd cd.dirlist
   if $d {
      catch {set cd.cwd [.cd.dirlist.list get [.cd.dirlist.list curselection]]}
      if {${cd.cwd} == "<parent>"} {set cd.cwd ..}
   }
   if {[catch { cd [set cd.cwd] }]} { make_directory cd.cwd }
   set cd.cwd [pwd]
   for {set i [[set cd.dirlist] size]} {$i >= 0} {incr i -1} {
      [set cd.dirlist] delete $i
   }
   if {![string match [pwd] "/"]} { [set cd.dirlist] insert end "<parent>" }
   foreach file [lsort [glob -nocomplain "*"]] {
      if {[file isdirectory $file]} {
         [set cd.dirlist] insert end $file
      }
   }
}

proc cd_dialog { cwd } {
   global cd.cwd cd.dirlist cd.done
   if [catch { toplevel .cd }] {
      wm withdraw .cd
      wm deiconify .cd
      return
   }
   wm title .cd "Select Working Directory"
   wm geometry .cd "250x300+[winfo x .taskbar]+150"
   set cd.cwd $cwd
   label .cd.subdirectories -text "Subdirectories" \
	   -font "Helvetica -14 bold"
   set cd.dirlist [Scrolled_Listbox .cd.dirlist -background white -font "Helvetica -12 bold"]
   label .cd.directorylabel -text "Current Directory" \
	   -font "Helvetica -14 bold"
   entry .cd.directoryentry -background white -textvariable cd.cwd -font "Helvetica -14"
   button .cd.accept -text "Use" -command \
      { get_directory 1; set cd.done 1; destroy .cd }
   button .cd.cancel -text "Cancel" -command \
      { set cd.done 0; destroy .cd }
   grid rowconfigure    .cd 1 -weight 1
   grid columnconfigure .cd 0 -weight 1
   grid columnconfigure .cd 1 -weight 1
   grid .cd.directorylabel -row 2 -column 0 -columnspan 2
   grid .cd.directoryentry -row 3 -column 0 -columnspan 2 -sticky news
   grid .cd.subdirectories -row 0 -column 0 -columnspan 2
   grid .cd.dirlist  -row 1 -column 0 -columnspan 2 -sticky news
   grid .cd.cancel -row 4 -column 1 -sticky news
   grid .cd.accept -row 4 -column 0 -sticky news
   get_directory
   wm protocol .cd WM_DELETE_WINDOW \
      { global cd.done; set cd.done 0; destroy .cd }
   bind .cd.directoryentry <Return> get_directory
#   bind [set cd.dirlist]  <Double-ButtonPress-1> { get_directory 1 }
   bind [set cd.dirlist]  <ButtonRelease-1> { get_directory 1 }
   vwait cd.done
   if [set cd.done] { return [pwd] } else { return }
}

##############################################################################
proc StatLineInit { } {
    global statline statval env
    set statline(flagfile) /usr/local/icp/cfg/MOTORS.BUF
    set statline(motfile)  /usr/local/icp/cfg/MOTPOS.BUF
    set statline(statfile) /usr/local/icp/cfg/STATUS.ICP
    set statline(motlist) {}
    for {set i 1} {$i <= 30} {incr i} {
	set key "fixed$i"
	set statline($key) 0
	set key "soft$i"
	set statline($key) "0.0000"
	lappend statline(motlist) $i
    }
    set statval(file) 0
    set statval(statistic) 0
    set statval(tempctr) 0
    set statval(polar) 0
    set statval(psd) 0
    set statval(tdevcfg) 0
    set statval(tdevmot) 0
    set statval(magnctr) 0
    set statline(extr) 0
    
    set statline(oncolor) green
    set statline(offcolor) red
    set statline(statmsg)    {}
    set statline(flgmtime)   0
    set statline(motmtime)   0
    set statline(statmtime)   0

    set statline(debug)   0
}

# read the flag file
proc StatLineRead { } {
    global statline statval

    if {![file exists $statline(flagfile)]} {
	return
    }
    if {$statline(debug)} {puts "Reading $statline(flagfile)"}
    set f [open $statline(flagfile) r]
    seek $f [expr 160 * 34] start
    set input [read $f 160]
    set statline(input) $input

    binary scan $input iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii \
	    statval(fixed1)  statval(fixed2)  statval(fixed3)  \
	    statval(fixed4)  statval(fixed5)  statval(fixed6)  \
	    statval(fixed7)  statval(fixed8)  statval(fixed9)  \
	    statval(fixed10) statval(fixed11) statval(fixed12) \
	    statval(fixed13) statval(fixed14) statval(fixed15) \
	    statval(fixed16) statval(fixed17) statval(fixed18) \
	    statval(fixed19) statval(fixed20) statval(fixed21) \
	    statval(fixed22) statval(fixed23) statval(fixed24) \
	    statval(fixed25) statval(fixed26) statval(fixed27) \
	    statval(fixed28) statval(fixed29) statval(fixed30) \
	    statval(file)  statval(statistic) statval(tempctr) \
	    statval(polar)   statval(psd)     statval(tdevcfg) \
	    statval(tdevmot) statval(magnctr) 
    set statline(extr) 0
    close $f
}

proc StatValChange { args } {
    global statline statval
    if {[llength $args] < 2} { return }
    set key [lindex $args 1]
    if {$statline(debug)} {puts "$key state $statval($key)"}
    switch $key {
	tempctr {
	    if {$statval($key)} {
		$statline(lbl_t) configure -text "T+" \
			-background $statline(oncolor)
	    } else {
		$statline(lbl_t) configure -text "T-" \
			-background $statline(offcolor)
	    }
	}
	statistic {
	    if {$statval($key)} {
		$statline(lbl_s) configure -text "S+" \
			-background $statline(oncolor)
	    } else {
		$statline(lbl_s) configure -text "S-" \
			-background $statline(offcolor)
	    }
	}
    }
}
#	magnctr {
#	    if {$statline($key)} {
#		$statline($wkey) configure -text "H+" \
#			-background $statline(oncolor)
#	    } else {
#		$statline($wkey) configure -text "H-" \
#			-background $statline(offcolor)
#	    }
#	}
#	polar {
#	    if {$statline($key)} {
#		$statline($wkey) configure -text "P+" \
#			-background $statline(oncolor)
#	    } else {
#		$statline($wkey) configure -text "P-" \
#			-background $statline(offcolor)
#	    }
#	}
#	default {
#	    if {$statline($key)} {
#		$statline($wkey) configure -background $statline(oncolor)
#	    } else {
#		$statline($wkey) configure -background $statline(offcolor)
#	    }
#	}
#    }

proc ReportStatFlag { key } {
    global statline statval
    switch $key {
	tempctr {
	    if {$statval($key)} {
		return "Temperature control is enabled"
	    } else {
		return "Temperature control is off"
	    }
	}
	statistic {
	    if {$statval($key)} {
		return "Measurements will be checked for\nstatistical consistency"
	    } else {
		return "Statisical checking of data is disabled"
	    }
	}
    }
}

proc ReportStatusInfo {} {
    global statline
    set text [string trim $statline(statmsg)]
# does this line contain the right delimiters?
    if [regexp {.*:.*/.*\*.*:.*} $statline(statmsg3)] {
# parse out the number of points
	regexp { ([0-9]+)/ *([0-9]+) }  $statline(statmsg3) junk ptn ptd
	# puts "$ptn of $ptd"
# parse out the current time and the time per point
	regexp {([0-9]+):([0-9]+) .* ([0-9]+):([0-9]+) *} \
		$statline(statmsg3) junk nowh nowm timem times
	# remove leading zeros from numbers 
	regexp {0*([0-9]+)} $nowh junk nowh
	regexp {0*([0-9]+)} $nowm junk nowm
	regexp {0*([0-9]+)} $timem junk timem
	regexp {0*([0-9]+)} $times junk times
	
	set delta [expr ($ptd - $ptn)*($times/60. + $timem)/60.]

	set now [clock seconds]
	set then [expr int($delta * 60 * 60 + $now)]
	regexp {0*([0-9]+)} [clock format $then -format %d] junk thenday
	regexp {0*([0-9]+)} [clock format $now  -format %d] junk nowday
	set days [expr $thenday - $nowday]

	append text \n\n
	# format the message
	if {$delta > 1} {
	    set finish "[format "%.2f" $delta] hours"
	} else {
	    set finish "[format "%.1f" [expr 60*$delta]] minutes"
	}
	if {[clock format $then -format %H] > 12} {
	    set time "[clock format $then -format %H:%M] ([clock format $then -format {%I:%M %p}])"
	} else {
	    set time "[clock format $then -format %H:%M]"
	}
	# display the message
	if {$days > 1} then {
	    append text \
		    "Projected finish in $finish \[at $time in $days days\]"
	} elseif {$days > 0} then {
	    append text \
		    "Projected finish in $finish \[at $time tomorrow\]"
	} else {
	    append text \
		    "Projected finish in $finish \[at $time today\]"
	}
    }
    return $text
}

proc ReportAllMotors { } {
    global statline
    set text {}
    foreach i $statline(motlist) {
	append text $statline(soft$i) \n
    }
    return [string trim $text]
}

# Periodically read status files
proc StatLinePoll { } {
    global statline
    
    catch {
	set mtime [file mtime $statline(flagfile)]
	if {$mtime != $statline(flgmtime)} {
	    set statline(flgmtime) $mtime
	    StatLineRead
	}
	
	set mtime [file mtime $statline(motfile)]
	if {$mtime != $statline(motmtime)} {
	    set statline(motmtime) $mtime
	    ReadMotPos
	}
	
	set mtime [file mtime $statline(statfile)]
	if {$mtime != $statline(statmtime)} {
	    set statline(statmtime) $mtime
	    ReadStatusFile
	}
    }

    set statline(id) [after 1000 StatLinePoll]

}

proc ReadInstrCfg { } {
    global statline env

    set statline(cfgfile) "/usr/local/icp/cfg/INSTR.CFG"
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
    for {set i 1} {$i <= 30} {incr i} {
	seek $f [expr 160 * ($i - 1)] start
	set input [read $f 160]
	binary scan $input ff hard zero
	set statline(hard$i) $hard
	set statline(zero$i) $zero
	set statline(soft$i) [format "M%2d: %.4f" $i [expr $hard - $zero]]
	if {$statline(debug)} {puts "$i $hard $zero $statline(soft$i)"}
    }
    close $f
}

proc ReadStatusFile {} {
    global statline env

    if [catch {open $statline(statfile) r} f] {
	return
    }
    if {$statline(debug)} {puts "Reading $statline(statfile)"}
    set statline(statmsg) {}
    set i 0
    while {[string length [set line [read $f 80]]] > 0} {
	incr i
	set line [string trim $line]
	if {$line != ""} {append statline(statmsg) $line \n}
	if {$i == 3} {set statline(statmsg3) $line}
    }
    if {$statline(debug)} {puts "$statline(statmsg)"}
    close $f
}

##############################################################################
#
# Build the taskbar
#

proc build_taskbar { } {
    global cwd instr_name use_overrideredirect
    toplevel .taskbar
    wm withdraw .taskbar
    #---------------------------------------------------------------------
    # this next command seems to be problematic with some window managers
    wm overrideredirect .taskbar $use_overrideredirect
    #---------------------------------------------------------------------
    button .taskbar.start_instr -text "START\n$instr_name" \
	    -command start_instr -height 2
    set var [RegisterPopup .taskbar.start_instr 1]
    uplevel #0 "set $var {Use this button to select\na directory and start\nICP, PREPARE & XPEEK}"
    entry .taskbar.directory -background white -textvariable cwd \
	    -state disabled -relief flat -font "fixed 14 bold" -width 25
    set var [RegisterPopup .taskbar.directory 1]
    uplevel #0 "set $var {This shows the current\ndirectory for ICP & PREPARE\nChange using \"Start BT1\" button.}"
    button .taskbar.extra_prepare -text "EXTRA\nPREPARE" \
	    -command extra_prepare
    set var [RegisterPopup .taskbar.extra_prepare 1]
    uplevel #0 "set $var {Use this button to start\nPREPARE to edit runs in\na different directory}"
    button .taskbar.launch_xpeek -text "LAUNCH\nXPEEK" \
	    -height 2 -command launch_xpeek
    set var [RegisterPopup .taskbar.launch_xpeek 1]
    uplevel #0 "set $var {Use this button to start\na new XPEEK plot}"
    foreach b {start_instr extra_prepare launch_xpeek} {
	.taskbar.$b config -padx 0 -pady 0 -width 0 -font "helvetica 12 bold"
    }
    frame .taskbar.flags 
    global statline
#    foreach l {t r s w} key {tempctr ? statistic ?}
    foreach l {t s} key {tempctr statistic} {

       set statline(lbl_$l) .taskbar.flags.$l
	pack [label $statline(lbl_$l) -text [string toupper ${l}?] -bg blue \
	       -font "helvetica 14 bold" \
	       ] -side left -padx 3 -pady 5 -anchor s
	set var [RegisterPopup $statline(lbl_$l)]
	uplevel #0 "set $var \"ReportStatFlag $key \" "
    }
    label .taskbar.status -bg lightyellow -textvariable statline(statmsg) \
	    -height 3 -width 80 -font "helvetica 10 bold" \
	    -justify left -width 0 -anchor n
    set var [RegisterPopup .taskbar.status 0]
    uplevel #0 "set $var ReportStatusInfo"

    label .taskbar.motor4 -textvariable statline(soft4) \
	    -font "helvetica 14" \
	    -justify center -anchor c
    set var [RegisterPopup .taskbar.motor4 0]
    uplevel #0 "set $var ReportAllMotors"

    grid .taskbar.start_instr   -row 0 -column 1
    grid .taskbar.directory     -row 0 -column 2 -sticky news
    grid .taskbar.extra_prepare -row 0 -column 3
    grid .taskbar.launch_xpeek  -row 0 -column 4
    grid .taskbar.flags         -row 0 -column 5 -sticky s
    grid .taskbar.status        -row 0 -column 6
    grid .taskbar.motor4        -row 0 -column 7 -sticky news
    bind .taskbar <Control-c> confirm_exit
    bind .taskbar <Control-C> confirm_exit
    wm protocol .taskbar WM_DELETE_WINDOW confirm_exit
    if $use_overrideredirect {
	bind .taskbar <Visibility> {
	    if {[string match %W .taskbar] &&
            [string compare %s VisibilityUnobscured]} { raise %W; update }
	}
    }
    wm deiconify .taskbar
    update idletasks
    # place taskbar in upper left corner
    wm geometry .taskbar "+5+0"
}

#
# Procedures called by the taskbar buttons
#

proc start_instr { } {
   global cwd instr_name icp_exe prepare_exe xpeek_exe

    #pleasewait "Starting console, prepare & xpeek..." {} {}

   set user_name [exec whoami]
   set process_list [exec ps -u $user_name]

   if [regexp " icp\n" $process_list] {
      # Should we check the lock file here and verify the PID?
      set icp_running 1
   } else {
      set icp_running 0
   }
   if [regexp " prepare" $process_list] {
      set run_prepare 0
   } else {
      set run_prepare 1
   }
   if [regexp " xpeek" $process_list] {
      set run_xpeek 0
   } else {
      set run_xpeek 1
   }

   if $icp_running {
      tk_dialog .icp_running "ICP is already running" \
      "The ICP application is already running. \
      Please quit ICP and then choose the 'Start $instr_name' option again." \
      warning 0 OK
      #donewait
      return
   } else {
      if {![string equal [cd_dialog $cwd] {}]} {
         set cwd [pwd]
         exec $icp_exe &
      } else {
	  #donewait
	  return
      }
   }
   if $run_prepare {
      exec $prepare_exe &
   }
   if $run_xpeek {
      exec $xpeek_exe  -geometry 521x360+0-45  $instr_name &
   }
    #donewait
}

proc extra_prepare { } {
   global cwd prepare_exe
   if {![string equal [cd_dialog $cwd] {}]} {
      exec $prepare_exe &
   }
}

proc launch_xpeek { } {
   global instr_name xpeek_exe
   exec $xpeek_exe $instr_name &
}

proc confirm_exit { } {
   if [tk_dialog .confirm "Really exit taskbar?" \
      "Are you sure you want to exit taskbar?" question 0 No Yes] exit
}

# register a Pop-up window for widget $box
#   returns a variable name that is created to contain either the message 
#   text or a procedure to be computed when the box is posted
#   the Pop-up is not used if the variable is blank.
set Popup(current) {}
set Popup(ID) {}
proc RegisterPopup {box "delay 0"} {
    global Popup
    if {$delay} {
	bind $box <Enter> "PostPopupDelay $box"
    } else {
	bind $box <Enter> "PostPopup $box"
    }
    set Popup($box) {}
    return Popup($box)
}

proc PostPopupDelay {box} {
    global Popup statline
    if {$statline(debug)} {puts "PostPopupDelay $box"}
    # wait 1 second before posting the box
    set ID [after 1000 "PostPopup $box"]
    # leaving within that second cancels the post
    bind $box <Leave> "after cancel $ID; bind $box <Leave> {}"
}

proc PostPopup {box} {
    global Popup statline

    if {$statline(debug)} {puts "PostPopup $box"}
    set win .transient

    # is a clear pending? 
    if {$Popup(ID) != ""} {
	after cancel $Popup(ID)
	if {$statline(debug)} {puts "dequeue $Popup(ID)"}
	set Popup(ID) {}

	if {$Popup(current) == $box} {
	    # we have reentered the same widget again
	    # remove popup 1 sec after mouse leaves the widget
	    bind $box <Leave> "QueueClearPopup $box $win"
	    return
	}
	# if not, time to make a new Popup -- this deletes the old one
    }
    set Popup(current) $box
    if {[string trim $Popup($box)] == ""} {
	# if the variable is blank -- do nothing
	return
    } elseif {[info proc [lindex $Popup($box) 0]] == ""} {
	# does the variable contain a procedure?
	set text $Popup($box)
    } else {
	if {[catch {set text [eval $Popup($box)]} errmsg]} {
	    set text $Popup($box)
	    if {$statline(debug)} {puts "error = $errmsg"}
	}
    }
    catch {destroy $win}
    toplevel $win -relief flat -bd 2 -bg yellow
    wm overrideredirect $win 1
    wm transient $win
    wm withdraw $win
    
    label $win.label -text $text  \
            -relief flat -bd 0 -highlightthickness 0  -bg yellow \
	    -font "helvetica 13 bold" -justify left
    pack $win.label -side left
    update idletasks
    
    # center the new window 5 pixels below the parent
    set x [expr [winfo rootx $box] + [winfo width $box]/2 - \
	    [winfo reqwidth $win]/2 - [winfo vrootx $box]]
    set y [expr [winfo rooty $box] + [winfo height $box] + 5]
    # make sure that we can see the entire window
    set xborder 10
    set yborder 25
    if {$x < 0} {set x 0}
    if {$x+[winfo reqwidth $win] +$xborder > [winfo screenwidth $win]} {
	incr x [expr \
		[winfo screenwidth $win] - ($x+[winfo reqwidth $win] + $xborder)]
    }
    if {$y < 0} {set y 0}
    if {$y+[winfo reqheight $win] +$yborder > [winfo screenheight $win]} {
	incr y [expr \
		[winfo screenheight $win] - ($y+[winfo reqheight $win] + $yborder)]
    }
    wm geometry  $win "+$x+$y"
    update idletasks
    wm deiconify $win
    # remove popup 1 sec after mouse leaves the widget
    bind $box <Leave> "QueueClearPopup $box $win"
}

proc QueueClearPopup {box win} {
    global Popup statline
    if {$statline(debug)} {puts "queue $Popup(ID)"}
    set Popup(ID) [after 1000 "destroy $win; set Popup(ID) {}"]
    bind $box <Leave> {}
}

# override the bgerror routine with something simple
proc bgerror { args } {
    puts stderr "BGERROR @ [clock format [clock seconds]]: $args"
    global errorInfo errorCode
    puts stderr "errorInfo = $errorInfo"
    puts stderr "errorCode = $errorCode"
# I may want to add  -code break
    return
}

# temporary insertion ? add wait msg support

# tell'em what is happening
#    message    is a text message to display
#    statusvar  is a variable name containing a message that gets updated
#    parent     is the name of the parent window
#    button     defines a button for the window. Element 0 in $button is the
#               text for the button and Element 1 is the command to execute.
proc pleasewait {{message {}} {statusvar {}} {parent .} {button ""}} {
    catch {destroy .msg}
    toplevel .msg
    wm transient .msg [winfo toplevel .]
    pack [frame .msg.f -bd 4 -relief groove] -padx 5 -pady 5
    pack [message .msg.f.m -text "Please wait $message"] -side top
    if {$statusvar != ""} {
        pack [label .msg.f.status -textvariable $statusvar] -side top
    }
    if {$button != ""} {
        pack [button .msg.f.button -text [lindex $button 0] \
                -command [lindex $button 1]] -side top
    }
    wm withdraw .msg
    update idletasks
    # place the message on top of the parent window
    if {$parent != ""} {
	set x [expr [winfo x $parent] + [winfo width $parent]/2 - \
		   [winfo reqwidth .msg]/2 - [winfo vrootx $parent]]
	if {$x < 0} {set x 0}
	set y [expr [winfo y $parent] + [winfo height $parent]/2 - \
		   [winfo reqheight .msg]/2 - [winfo vrooty $parent]]
	if {$y < 0} {set y 0}
    } else {
        set x [expr {[winfo screenwidth .msg]/2 - [winfo reqwidth .msg]/2 \
                - [winfo vrootx .]}]
        set y [expr {[winfo screenheight .msg]/2 - [winfo reqheight .msg]/2 \
                - [winfo vrooty .]}]
     }
    wm geom .msg +$x+$y
    wm deiconify .msg
    global makenew
    set makenew(OldGrab) ""
    set makenew(OldFocus) ""
    # save focus & grab
    catch {set makenew(OldFocus) [focus]}
    catch {set makenew(OldGrab) [grab current .msg]}
    catch {grab .msg}
    update
}
# clear the message
proc donewait {} {
    global makenew
    catch {destroy .msg}
    # reset focus & grab
    catch {
        if {$makenew(OldFocus) != ""} {
            focus $makenew(OldFocus)
        }
    }
    catch {
        if {$makenew(OldGrab) != ""} {
            grab $makenew(OldGrab)
        }
    }
}

#
# Build the user interface and enter the event loop
#
build_taskbar
StatLineInit     ;# Set defaults

trace variable statval w StatValChange
ReadInstrCfg     ;# Read instrument configuration

StatLinePoll
