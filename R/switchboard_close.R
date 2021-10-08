#' Closes all switchboard windows.
#'
#' Helps remove all visible/invisible switchboard windows. Place outside of 
#' loop to remove windows.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'
#' for(i in 1:100) 
#'     switchboard(delay = 0.05) %>% percent(i, maximum = 100, label = "100 loops")
#' switchboard_close()
#'
#' }
#'
#' @family dashboard
#' @import tcltk
#' @export
switchboard_close <- function() {

  ## view 'withdrawn' switchboards before destroying
  # tcltk::tcl("wm", "deiconify", as.character(tcltk::tkwinfo("children", ".")))

  ## collect time delay executions and cancel them
  for(anAfter in as.character(tcltk::tcl("after", "info")))
    tcltk::tcl("after", "cancel", anAfter)

  ## collect 'withdrawn' switchboards and remove them
  for(aWindow in as.character(tcltk::tkwinfo("children", ".")))
    tcltk::tcl("destroy", aWindow)
  #OR? tcltk::.Tcl(paste("eval destroy [winfo children .]"))

  ## collect switchboard's tcltk variables and delete
  #OR? tcltk::.Tcl(paste("foreach var [info globals] {unset $var} "))
  globalsTclTk <- as.character(tcltk::.Tcl('info globals'))
  deleteTheseGlobalVar <- globalsTclTk[which(!globalsTclTk %in% c("tcl_version", "tk_library",
                                                                  "tk_version", "selectedButton",
                                                                  "errorCode", "auto_path",
                                                                  "errorInfo", "tk_strictMotif",
                                                                  "auto_index", "env",
                                                                  "checkbutton", "tcl_patchLevel",
                                                                  "tk_patchLevel", "tcl_platform",
                                                                  "tcl_library"))]
  for(i in deleteTheseGlobalVar) tcltk::tcl("unset", i)

  # finally clear switchboard globals
  rm(list = ls(switchboard.env), envir = switchboard.env)
 
  return(NULL)
}