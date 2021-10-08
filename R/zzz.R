.onLoad <- function(libname, pkgname) {

	tcltk::tcl("source", system.file("tcltkSource", "math.tcl", package = pkgname))
	tcltk::tcl("source", system.file("tcltkSource", "statistics.tcl", package = pkgname))
	tcltk::tcl("source", system.file("tcltkSource", "switchboard_helpers.tcl", package = pkgname))

	if(!("azure" %in% as.character(tcltk::.Tcl("ttk::style theme names"))))
	  tcltk::tcl("source", system.file("tcltkSource", "azure_switchboard.tcl", package = pkgname))
	tcltk::.Tcl(paste("ttk::style theme use", "azure"))

}

.onUnload <- function(libpath)
{
	switchboard_close()
}
