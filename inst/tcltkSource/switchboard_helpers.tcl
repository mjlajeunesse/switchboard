proc tcltk_eavesdropper_update {theCanvas itemName size theY theColor} {
	$theCanvas move $itemName -1 0
	$theCanvas delete [ $theCanvas find overlapping 8 0 9 [expr {80 * $size - 6}] ]
	set adjustedSize [expr {$size * 2}]
	set adjustedX [expr {80 * $size - $adjustedSize - 5}]
	$theCanvas create rectangle $adjustedX $theY $adjustedX $theY -width $adjustedSize -outline $theColor -tags $itemName
}

proc tcltk_eavesdropper_X_update {theCanvas itemName size theX theColor} {
	$theCanvas move $itemName 0 1
	$theCanvas delete [ $theCanvas find overlapping 5 [expr {80 * $size - 6}] [expr {80 * $size - 6}] [expr {80 * $size - 6}] ]
	set adjustedSize [expr {$size * 2}]
	$theCanvas create rectangle $theX 5 $theX 5 -width $adjustedSize -outline $theColor -tags $itemName
}

proc tcltk_injector_update {theCanvas itemName size theY theColor} {
	$theCanvas move $itemName -1 0
	$theCanvas delete [ $theCanvas find overlapping 20 0 20 [expr {80 * $size - 6}] ]
	set adjustedSize [expr {$size * 2}]
	set adjustedX [expr {80 * $size - $adjustedSize - 5}]
	$theCanvas create rectangle $adjustedX $theY $adjustedX $theY -width $adjustedSize -outline $theColor -tags $itemName
}

proc tcltk_injector_X_update {theCanvas itemName size theX theColor} {
	$theCanvas move $itemName 0 1
	$theCanvas delete [ $theCanvas find overlapping 5 [expr {80 * $size - 20}] [expr {80 * $size - 6}] [expr {80 * $size - 20}] ]
	set adjustedSize [expr {$size * 2}]
	$theCanvas create rectangle $theX 5 $theX 5 -width $adjustedSize -outline $theColor -tags $itemName
}

proc tcltk_injector_2D_update {theCanvas pointName theTime size theX theY theColor} {
	set tempPoint [ $theCanvas create rectangle $theX $theY $theX $theY -width [expr {$size * 2}] -outline $theColor -tags $pointName]
	after $theTime $theCanvas delete $tempPoint
}

proc tcltk_plotMean_Y {theCanvas meanName sdName pointName hidden} {
	if { $hidden == FALSE } {
		$theCanvas coords $meanName -5 -5 -5 -5
		$theCanvas coords $sdName -5 -5 -5 -5
	} else {
		foreach x [$theCanvas find withtag $pointName] {
			lappend theY [lindex [$theCanvas coords $x] 1] 
		}
		if { [llength $theY] > 3 } {
			set theMean [::math::statistics::basic-stats $theY]
		} else {
			return -1
		}
		$theCanvas coords $meanName 17 [lindex $theMean 0] 17 [lindex $theMean 0]
		$theCanvas coords $sdName 18 [expr {[lindex $theMean 0] - [lindex $theMean 4]}] 18 [expr {[lindex $theMean 0] + [lindex $theMean 4]}]
	}
}

proc tcltk_plotMean_X {theCanvas meanName sdName pointName size hidden} {
	if { $hidden == FALSE } {
		$theCanvas coords $meanName -5 -5 -5 -5
		$theCanvas coords $sdName -5 -5 -5 -5
	} else {
		foreach x [$theCanvas find withtag $pointName] {
			lappend theX [lindex [$theCanvas coords $x] 0] 
		}
		if { [llength $theX] > 3 } {
			set theMean [::math::statistics::basic-stats $theX]
		} else {
			return -1
		}
		$theCanvas coords $meanName [lindex $theMean 0] [expr {80 * $size - 17}] [lindex $theMean 0] [expr {80 * $size - 17}]
		$theCanvas coords $sdName   [expr {[lindex $theMean 0] - [lindex $theMean 4]}] [expr {80 * $size - 16}] [expr {[lindex $theMean 0] + [lindex $theMean 4]}] [expr {80 * $size - 16}]
	}
}

proc tcltk_plotN {theCanvas itemName pointName hidden} {
	if { $hidden == FALSE } {
		$theCanvas itemconfigure $itemName -text ""
	} else {
		$theCanvas itemconfigure $itemName -text [concat "N = " [llength [$theCanvas find withtag $pointName]]]
	}
}
	
proc tcltk_plotRegression {theCanvas itemName pointName size hidden} {
	if { $hidden == FALSE } {
		$theCanvas coords $itemName -5 -5 -5 -5
	} else {
		foreach x [$theCanvas find withtag $pointName] {
			set theCoords [$theCanvas coords $x] 
			lappend theX [lindex $theCoords 0] 
			lappend theY [lindex $theCoords 1] 
		}
		if { [llength $theX] > 3 } {
			set theSlope [::math::statistics::linear-model $theX $theY 1]
			$theCanvas coords $itemName 22 [expr {22 * [lindex $theSlope 1] + [lindex $theSlope 0]}] [expr {70 * $size}] [expr {70 * $size * [lindex $theSlope 1] + [lindex $theSlope 0]}]
		} else {
			return -1
		}
	}
}